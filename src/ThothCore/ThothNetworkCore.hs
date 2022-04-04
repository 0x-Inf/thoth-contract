{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ThothCore.ThothNetworkCore
    ( initializeThothNetwork
    , endpoints
    , networkCovIdx
    , NetworkInitParams (..)
    , NetworkActivateParams (..)
    ) where

import           Control.Monad               hiding (fmap)
import           Data.Aeson                  (FromJSON, ToJSON)
import qualified Data.Map                    as Map
import           Data.Maybe                  (fromJust)
import           Data.Monoid                 (Last (..))
import           Data.OpenApi.Schema         (ToSchema)
import           Data.Text                   (Text, pack)
import           Data.Void                   (Void)
import           GHC.Generics                (Generic)
import qualified Data.OpenApi.Schema         as OpenApi
import qualified Formatting                  as Formatting
import           Plutus.Contract             as Contract
import           Plutus.Contract.Wallet      (getUnspentOutput)
import           Plutus.Contracts.Currency   as Currency
import qualified PlutusTx
import           PlutusTx.Code               (getCovIdx)
import           PlutusTx.Coverage           (CoverageIndex)
import           PlutusTx.Prelude            hiding (Semigroup(..), unless)
import           PlutusPrelude               (foldl')
import           Ledger                      hiding (mint, singleton)
import           Ledger.TimeSlot
import qualified Ledger.Typed.Scripts        as Scripts
import           Ledger.Ada                  as Ada
import           Ledger.Value                as Value
import           Ledger.Constraints          as Constraints
import           Prelude                     (Semigroup (..), Show (..), String)
import qualified Prelude
import           Text.Printf                 (printf)

import           Utils                       (getCredentials, unsafeTokenNameToHex)



{-# INLINABLE mkNetworkTokenPolicy #-}
-- | The validator for the thoth network token policy
mkNetworkTokenPolicy :: TxOutRef -> TokenName -> Integer -> () -> ScriptContext -> Bool 
mkNetworkTokenPolicy oref tn amt () ctx = traceIfFalse "Utxo not consumed" hasUtxo            &&
                                          traceIfFalse "wrong amount minted" checkMintedAmount
    where 
        info :: TxInfo
        info = scriptContextTxInfo ctx

        hasUtxo :: Bool 
        hasUtxo = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info   

        checkMintedAmount :: Bool 
        checkMintedAmount = case flattenValue (txInfoMint info) of
                                 [(_, tn', amt')] -> tn' == tn && amt' == amt 
                                 _                -> False 

networkTokenPolicy :: TxOutRef -> TokenName -> Integer -> Scripts.MintingPolicy
networkTokenPolicy oref tn amt = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \oref' tn' amt' -> Scripts.wrapMintingPolicy $ mkNetworkTokenPolicy oref' tn' amt' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn
    `PlutusTx.applyCode`
    PlutusTx.liftCode amt

networkTokenCurSymbol :: TxOutRef -> TokenName -> Integer -> CurrencySymbol 
networkTokenCurSymbol oref tn = scriptCurrencySymbol . networkTokenPolicy oref tn 



data Network = Network
    { lastSyncAddress :: Address
    } deriving Show

-- | the datum state data type
data DatumStateData = DatumStateData
    { researcherPaymentPubKeyHash   :: [PubKeyHash]
    , networkScriptAddress          :: Address 
    , networkParams                 :: String
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord, OpenApi.ToSchema, PlutusTx.UnsafeFromData, PlutusTx.FromData, 
                PlutusTx.ToData)

-- PlutusTx.unstableMakeIsData ''DatumStateData

-- | The datum of the network contained in the utxos at script address 
data NetworkDatum = Initialized | Active BuiltinByteString | Dead 
    deriving Show

PlutusTx.unstableMakeIsData ''NetworkDatum

-- | The network update interactions
data NetworkRedeemer = InitNetwork PubKeyHash | InitReseacher | InitGuest | MorphNetwork
    deriving Show 

PlutusTx.unstableMakeIsData ''NetworkRedeemer


{-# INLINABLE mkNetworkValidator #-}
-- | The network on chain Validator
mkNetworkValidator :: Address -> NetworkDatum -> NetworkRedeemer -> ScriptContext -> Bool
mkNetworkValidator addr d r ctx = 
    -- traceIfFalse "Payment Key Hash doesn't have tokens" $ checkAddressHasTokens addr  &&
    traceIfFalse "Not signed by authorized agent" (txSignedBy info (addressPkh addr)) &&
    case (d, r) of 
        (Initialized, InitNetwork pkh)                   -> 
            traceIfFalse "Not signed by the desginated one"         (txSignedBy info pkh)                 &&
            traceIfFalse "There is no output to the current script" tokenOutputToScript                   && 
            traceIfFalse "Check if the datum has been set"          checkIfDatumIsSet                     &&
            traceIfFalse "Output to Researcher zero is missing"     outputsToResearcherZero               && 
            traceIfFalse "Agent hasn't paid tribute to script"      checkBalanceAfterTx 
        _                                         -> False 

        where 
            info :: TxInfo
            info = scriptContextTxInfo ctx

            -- ownHashes :: (ValidatorHash, DatumHash) 
            -- ownHashes = ownHashes :: ScriptContext -> (ValidatorHash, DatumHash) 

            ownInput :: TxOut
            ownInput = case findOwnInput ctx of 
                            Nothing -> traceError "couldn't find an input"
                            Just i  -> txInInfoResolved i 

            ownPkhLockedOutputs :: Maybe TxOut
            ownPkhLockedOutputs = case getContinuingOutputs ctx of 
                                       [] -> traceError "Could not find the produced outputs"
                                       os -> find f os
                                           where 
                                               f :: TxOut-> Bool 
                                               f o = case pubKeyOutput o of
                                                       Nothing   -> False 
                                                       Just pkh' -> True

            scriptOwnOutputs :: [(DatumHash, Value)]
            scriptOwnOutputs = scriptOutputsAt (ownHash ctx) info 

            ownInputValue :: Value 
            ownInputValue = txOutValue ownInput

            transactionInputs :: [TxInInfo]
            transactionInputs = txInfoInputs info

            checkAddressHasTokens :: Address -> Bool 
            checkAddressHasTokens addr' = addr' == txOutAddress ownInput && ownInputValue `Value.geq` (toValue minAdaTxOut <> singleton adaSymbol adaToken 5_000_000)

            addressPkh :: Address -> PubKeyHash
            addressPkh addr'' = case toPubKeyHash addr'' of
                                    Nothing   -> traceError "something went wrong with pkh retrieval"
                                    Just pkh' -> pkh'

            -- TODO: get an actual pubkeyhash ... maybe from another script
            getTheOnePkh :: PubKeyHash
            getTheOnePkh = PubKeyHash emptyByteString

            tokenOutputToScript :: Bool
            tokenOutputToScript = (length $ findContinuingOutputs ctx) >= 1


            checkIfDatumIsSet :: Bool
            checkIfDatumIsSet =  any f scriptOwnOutputs
                where 
                    f :: (DatumHash, Value) -> Bool
                    f (dh, _) = case findDatum dh info of 
                                     Nothing -> False 
                                     Just d  -> True
                                                     

            outputsToResearcherZero :: Bool 
            outputsToResearcherZero =  let value_researcher = pubKeyOutputsAt (addressPkh addr) info 
                                                  in length value_researcher > 1

            checkBalanceAfterTx :: Bool 
            checkBalanceAfterTx = case getContinuingOutputs ctx of 
                                       os -> let value = foldl' (\total v -> total <> v) (lovelaceValueOf 0) $ txOutValue <$> os in 
                                                 value `geq` (lovelaceValueOf 1_000_000)


                -- utxos <- utxosAt addr'
                -- return any f utxos
                --     where
                --         f :: (TxOutRef, ChainIndexTxOut) -> Bool
                --         f (_, o) = getLovelace (fromValue (txOutValue $ toTxOut o)) >= toValue minAdaTxOut


data ThothNetwork
instance Scripts.ValidatorTypes ThothNetwork where 
    type instance DatumType ThothNetwork = NetworkDatum 
    type instance RedeemerType ThothNetwork = NetworkRedeemer

typedNetworkValidator :: Address -> Scripts.TypedValidator ThothNetwork
typedNetworkValidator addr = Scripts.mkTypedValidator @ThothNetwork
    ($$(PlutusTx.compile [|| mkNetworkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode addr)
    $$(PlutusTx.compile [|| wrap ||])
  where
      wrap = Scripts.wrapValidator @NetworkDatum @NetworkRedeemer


networkValidator :: Address -> Validator
networkValidator = Scripts.validatorScript . typedNetworkValidator

networkAddress :: Address -> Ledger.Address
networkAddress = scriptAddress . networkValidator

networkCovIdx :: CoverageIndex
networkCovIdx = getCovIdx $$(PlutusTx.compile [|| mkNetworkValidator ||])

-- PlutusTx.makeLift ''DatumStateData
-- PlutusTx.makeLift ''NetworkDatum
-- PlutusTx.makeLift ''NetworkRedeemer

waitUnitlTimeHasPassed :: AsContractError e => POSIXTime -> Contract w s e ()
waitUnitlTimeHasPassed t = do 
    s1 <- Contract.currentSlot 
    logInfo @String $ "current slot: " ++ show s1 ++ ", waiting until" ++ show t
    void $ awaitTime t >> waitNSlots 1
    s2 <- Contract.currentSlot
    logInfo @String $ "waited until: " ++ show s2 

adjustAndSubmitWith ::( PlutusTx.FromData  (Scripts.DatumType a)
                      , PlutusTx.ToData (Scripts.RedeemerType a)
                      , PlutusTx.ToData (Scripts.DatumType a)
                      , AsContractError e
                      )
                    => ScriptLookups a 
                    -> TxConstraints (Scripts.RedeemerType a) (Scripts.DatumType a)
                    -> Contract w s e CardanoTx

adjustAndSubmitWith lookups constraints = do 
    unbalanced <- adjustUnbalancedTx <$> mkTxConstraints lookups constraints 
    Contract.logDebug @String $ printf "unbalanced: %s" $ show unbalanced
    unsigned <- balanceTx unbalanced
    Contract.logDebug @String $ printf "balanced: %s" $ show unsigned
    signed <- submitBalancedTx unsigned 
    Contract.logDebug @String $ printf "signed: %s" $ show signed 
    return signed 

adjustAndSubmit :: ( PlutusTx.FromData (Scripts.DatumType a)
                   , PlutusTx.ToData (Scripts.RedeemerType a)
                   , PlutusTx.ToData (Scripts.DatumType a)
                   , AsContractError e
                   )
                => Scripts.TypedValidator a
                -> TxConstraints (Scripts.RedeemerType a) (Scripts.DatumType a)
                -> Contract w s e CardanoTx
adjustAndSubmit inst = adjustAndSubmitWith $ Constraints.typedValidatorLookups inst 

data NetworkInitParams = NetworkInitParams
    { rZeroAddress              :: !Address
    , initialNetworkParams      :: !String
    , networkSacrificeAmount    :: !Integer
    , networkTokenName1          :: !TokenName
    , networkTokenInitialSupply1 :: !Integer 
    , networkActiveDatum1        :: !BuiltinByteString
    } deriving (Show, Generic, FromJSON, ToJSON)


initializeThothNetwork :: forall w s. NetworkInitParams -> Contract w s Text ()
initializeThothNetwork nip = do 
    pkh <- Contract.ownPaymentPubKeyHash
    let addr = rZeroAddress nip
        scriptAddress = networkAddress
        tn = networkTokenName1 nip
        amt = networkTokenInitialSupply1 nip
        valHash     = validatorHash $ networkValidator addr
        testDatum   = networkActiveDatum1 nip
    let v       = lovelaceValueOf $ networkSacrificeAmount nip
        lookups = Constraints.typedValidatorLookups (typedNetworkValidator addr)         <>
                  Constraints.otherScript (networkValidator addr) 
        tx      = Constraints.mustPayToOtherScript valHash (Datum $ PlutusTx.toBuiltinData Initialized) v  
    ledgerTx <- submitTxConstraintsWith @ThothNetwork lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ "Researcher zero has initialized network with credential: " ++ show (addressCredential addr)
    now <- currentTime

    waitUnitlTimeHasPassed $ now + 10 
    now1 <- currentTime

    logInfo @String $ "TIme now is: " ++ show now1

    utxos <- fundsAtAddressGeq (networkAddress addr) (Ada.lovelaceValueOf 10_000_000)
    case Map.toList utxos of 
         [(oref, o)] -> do 
                Contract.logDebug @String $ printf "picked UTxo at" ++ (show oref) ++ "with value" ++ (show $ _ciTxOutValue o)
            
                let cs            = networkTokenCurSymbol oref tn amt
                    val           = Value.singleton cs tn amt
                    researcherVal = Value.singleton cs tn 1
                    lookups'      = Constraints.mintingPolicy (networkTokenPolicy oref tn amt)             <>
                                    Constraints.typedValidatorLookups (typedNetworkValidator addr)         <>
                                    Constraints.otherScript (networkValidator addr)                        <>
                                    Constraints.unspentOutputs (Map.singleton oref o)
                    constraints'  = Constraints.mustMintValue val                                          <>
                                    -- Constraints.mustPayToPubKey pkh researcherVal                                    <>
                                    Constraints.mustPayToOtherScript valHash (Datum $ PlutusTx.toBuiltinData (Active testDatum)) val         <>
                                    Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData (InitNetwork (unPaymentPubKeyHash pkh)))

                void $ adjustAndSubmitWith @ThothNetwork lookups' constraints'
                -- ledgerTx <- submitTxConstraintsWith @ThothNetwork lookups constraints
                -- void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                Contract.logInfo @String $ printf "minted: " ++ (show val)

    -- case getCredentials addr of 
    --     Nothing      -> Contract.throwError $ pack $ printf "expected pubkey address, but got" ++ (show addr)
    --     Just (x, my) -> do
    --         oref <- getUnspentOutput
    --         o    <- fromJust <$> Contract.txOutFromRef oref 
    --         Contract.logDebug @String $ printf "picked UTxo at" ++ (show oref) ++ "with value" ++ (show $ _ciTxOutValue o)

  where
      f :: ChainIndexTxOut -> Bool 
      f o = assetClassValueOf (txOutValue $ toTxOut o) (AssetClass (adaSymbol, adaToken)) >= 10



data NetworkActivateParams = NetworkActivateParams
    { networkTokenName          :: !TokenName
    , networkTokenInitialSupply :: !Integer
    , networkActiveDatum        :: !BuiltinByteString
    , rZeroActivateAddress      :: !Address
    } deriving (Show, Generic, FromJSON, ToJSON)

activateThothNetwork :: forall w s. NetworkActivateParams -> Contract w s Text ()
activateThothNetwork nap = do 
    pkh <- Contract.ownPaymentPubKeyHash
    let addr = rZeroActivateAddress nap
    utxos <- fundsAtAddressGeq (networkAddress addr) (Ada.lovelaceValueOf 10_000_000)
    -- utxos <- Map.filter f <$> utxosAt (networkAddress addr)
    case Map.toList utxos of 
         [(oref, o)] -> do 
                Contract.logDebug @String $ printf "picked UTxo at" ++ (show oref) ++ "with value" ++ (show $ _ciTxOutValue o)
                -- TODO: Mint the required tokens and 'store them' in network script address.
                let tn          = networkTokenName nap
                    amt         = networkTokenInitialSupply nap
                    cs          = networkTokenCurSymbol oref tn amt
                    val         = Value.singleton cs tn amt
                    valWAda     = lovelaceValueOf 2_000_000 <> val  -- temp fix, probably use adjustAndSubmitWith
                    valHash     = validatorHash $ networkValidator addr
                    testDatum   = networkActiveDatum nap
                    lookups     = Constraints.mintingPolicy (networkTokenPolicy oref tn amt)             <>
                                  Constraints.typedValidatorLookups (typedNetworkValidator addr)         <>
                                  Constraints.otherScript (networkValidator addr)                        <>
                                  Constraints.unspentOutputs (Map.singleton oref o)
                    constraints = Constraints.mustMintValue val                                          <>
                                --   Constraints.mustPayToTheScript (Active testDatum) valWAda              <>
                                  Constraints.mustPayToOtherScript valHash (Datum $ PlutusTx.toBuiltinData (Active testDatum)) val         <>
                                  Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData (InitNetwork (unPaymentPubKeyHash pkh)))

                void $ adjustAndSubmitWith @ThothNetwork lookups constraints
                -- ledgerTx <- submitTxConstraintsWith @ThothNetwork lookups constraints
                -- void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                Contract.logInfo @String $ printf "minted: " ++ (show val)
        

    now <- currentTime

    waitUnitlTimeHasPassed $ now + 10 
    now1 <- currentTime

    utxosB <- utxosAt (networkAddress addr)
    let orefs   = snd <$> Map.toList utxosB

    case Map.toList utxosB of 
         [(oref', o')] -> do 
                Contract.logDebug @String $ printf "picked UTxo at" ++ (show oref') ++ "with value" ++ (show $ _ciTxOutValue o')
         




type ThothNetworkSchema = 
        Endpoint "init" NetworkInitParams
    .\/ Endpoint "activate" NetworkActivateParams

endpoints :: Contract () ThothNetworkSchema Text ()
endpoints = awaitPromise (init `select` activate) >> endpoints
    where
        init = endpoint @"init" $ \nip -> do
            initializeThothNetwork nip

        activate = endpoint @"activate" $ \nap -> do 
            activateThothNetwork nap 


-- mkSchemaDefinitions ''ThothNetworkSchema

-- myToken :: KnownCurrency
-- myToken = KnownCurrency (ValidatorHash "f") "Token" (TokenName "T" :| [])

-- mkKnownCurrencies ['myToken]
        