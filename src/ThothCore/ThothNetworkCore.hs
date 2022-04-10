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
import           Data.Maybe                  (fromJust, fromMaybe)
import           Data.Monoid                 (Last (..))
import           Data.OpenApi.Schema         (ToSchema)
import           Data.Text                   (Text, pack)
import qualified Data.ByteString.Char8       as BsChar8
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
import           Prelude                     (Semigroup (..), Show (..), String, fromIntegral)
import qualified Prelude
import qualified PlutusPrelude               as PlPrelude
import qualified PlutusTx.Prelude            as PlTxPrelude
import qualified PlutusTx.Ratio              as PlRatio
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



{-# INLINABLE mkNetworkInitTokenPolicy #-}
-- | The validator for the thoth network token policy
mkNetworkInitTokenPolicy :: PaymentPubKeyHash ->  TxOutRef -> TokenName -> POSIXTime -> Integer -> () -> ScriptContext -> Bool
mkNetworkInitTokenPolicy pkh oref tn deadline amt () ctx = 
    traceIfFalse "Utxo not consumed" hasUtxo                &&
    traceIfFalse "wrong amount minted" checkMintedAmount    &&
    traceIfFalse "signature missing" checkCorrectSignature  &&
    traceIfFalse "deadline missed" checkDeadlineReached

  where 
      info :: TxInfo
      info = scriptContextTxInfo ctx

      hasUtxo :: Bool 
      hasUtxo = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info
      
      checkMintedAmount :: Bool 
      checkMintedAmount = case flattenValue (txInfoMint info) of
                                 [(_, tn', amt')] -> tn' == tn && amt' == amt 
                                 _                -> False

      checkCorrectSignature :: Bool 
      checkCorrectSignature = txSignedBy info $ unPaymentPubKeyHash pkh

      checkDeadlineReached :: Bool 
      -- TODO: Make this check whether the deadline is contained in the infoValidRange in realBlockChain
      checkDeadlineReached = txInfoValidRange info `contains` to deadline 


networkInitTokenPolicy :: PaymentPubKeyHash ->  TxOutRef -> TokenName -> POSIXTime -> Integer -> Scripts.MintingPolicy
networkInitTokenPolicy pkh oref tn deadline amt = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \pkh' oref' tn' deadline' amt' -> Scripts.wrapMintingPolicy $ mkNetworkInitTokenPolicy pkh' oref' tn' deadline' amt' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode pkh
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn
    `PlutusTx.applyCode`
    PlutusTx.liftCode deadline
    `PlutusTx.applyCode`
    PlutusTx.liftCode amt

networkInitTokenCurSymbol ::  PaymentPubKeyHash ->  TxOutRef -> TokenName -> POSIXTime -> Integer -> CurrencySymbol
networkInitTokenCurSymbol pkh oref tn deadline = scriptCurrencySymbol . networkInitTokenPolicy pkh oref tn deadline


{-# INLINABLE mkNetworkSpawnTokenPolicy #-}
-- | The validator for the thoth network token policy
mkNetworkSpawnTokenPolicy :: Address -> TxOutRef -> TokenName -> Integer -> () -> ScriptContext -> Bool 
mkNetworkSpawnTokenPolicy scripAddress oref tn amt () ctx = 
    traceIfFalse "Utxo not consumed" hasUtxo                &&
    traceIfFalse "wrong amount minted" checkMintedAmount    &&
    traceIfFalse "signature missing" checkCorrectSignature 

  where

      info :: TxInfo 
      info = scriptContextTxInfo ctx 

      hasUtxo :: Bool 
      hasUtxo = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

      checkMintedAmount :: Bool 
      checkMintedAmount = case flattenValue (txInfoMint info) of
                                 [(_, tn', amt')] -> tn' == tn && amt' == amt 
                                 _                -> False

      checkCorrectSignature :: Bool 
      checkCorrectSignature = True -- txSignedBy info $ unPaymentPubKeyHash pkh


networkSpawnTokenPolicy :: Address -> TxOutRef -> TokenName -> Integer -> Scripts.MintingPolicy      
networkSpawnTokenPolicy sAddress oref tn amt = mkMintingPolicyScript $ 
    $$(PlutusTx.compile [|| \sAddress' oref' tn' amt' -> Scripts.wrapMintingPolicy $ mkNetworkSpawnTokenPolicy sAddress oref' tn' amt' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode sAddress
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn
    `PlutusTx.applyCode`
    PlutusTx.liftCode amt

networkSpawnTokenCurSymbol ::  Address ->  TxOutRef -> TokenName -> Integer -> CurrencySymbol
networkSpawnTokenCurSymbol sAddress oref tn = scriptCurrencySymbol . networkSpawnTokenPolicy sAddress oref tn


data NetworkAttrbutes = NetworkAttrbutes
    { initResearcherAddress :: Address
    , initNetworkToken      :: AssetClass
    , initTributeAmount     :: Integer
    } deriving Show

PlutusTx.makeLift ''NetworkAttrbutes    

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
data NetworkRedeemer = Spawn (PubKeyHash, AssetClass) | InitNetwork (PubKeyHash, Integer) | InitReseacher | InitGuest | MorphNetwork
    deriving Show 

PlutusTx.unstableMakeIsData ''NetworkRedeemer


{-# INLINABLE mkNetworkValidator #-}
-- | The network on chain Validator
mkNetworkValidator :: NetworkAttrbutes -> NetworkDatum -> NetworkRedeemer -> ScriptContext -> Bool
mkNetworkValidator nattr d r ctx = 
    traceIfFalse "Payment Key Hash doesn't have tokens" $ checkAddressHasTokens (initResearcherAddress nattr)  &&
    traceIfFalse "Not signed by authorized agent" (txSignedBy info (addressPkh (initResearcherAddress nattr))) &&
    case (d, r) of 
        (Dead, Spawn (pkh, spwnToken))                                                   -> 
            traceIfFalse "Not signed by the authorized pubkeyhash"  (txSignedBy info (addressPkh $ initResearcherAddress nattr))        &&
            traceIfFalse "Script doesn't contain tribute Ada"       (checkScriptValueAfterTx (initTributeAmount nattr))                 &&
            traceIfFalse "Init Token hasn't been transfered"        (initTokenToInitResearcher $ initResearcherAddress nattr)           &&
            traceIfFalse "Didn't Mint atleast one spawn token"      (checkSpawnTokenInScript spwnToken)                                 &&
            traceIfFalse "Action count is beyond limit"             checkCountDatum                                                     &&
            traceIfFalse "Caller doen't have required token"        checkInitInputsToken                                                &&
            traceIfFalse "Datum Not updated with Network attr"      checkDataInOutDatum 
        (Initialized, InitNetwork (pkh, tribute))                   -> 
            traceIfFalse "Not signed by the valid pubkeyhash"         (txSignedBy info pkh)               &&
            traceIfFalse "There is no output to the current script" tokenOutputToScript                   && 
            traceIfFalse "Check if the datum has been set"          checkIfDatumIsSet                     &&
            traceIfFalse "Output to Researcher zero is missing"     outputsToResearcherZero               && 
            traceIfFalse "Agent hasn't paid tribute to script"      checkBalanceAfterTx                   &&
            traceIfFalse "Script doesn't contain tribute Ada"       (checkScriptValueAfterTx (initTributeAmount nattr))
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
            checkAddressHasTokens addr' = ownInputValue `geq` (toValue minAdaTxOut <> singleton adaSymbol adaToken 5_000_000) -- addr' == txOutAddress ownInput && 

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
            outputsToResearcherZero =  let value_researcher = pubKeyOutputsAt (addressPkh (initResearcherAddress nattr)) info 
                                                  in length value_researcher > 1

            checkBalanceAfterTx :: Bool 
            checkBalanceAfterTx = case getContinuingOutputs ctx of 
                                       os -> let value = foldl' (\total tributeVal -> total <> tributeVal) (lovelaceValueOf 0) $ txOutValue <$> os in 
                                                 value `geq` (lovelaceValueOf 1_000_000)

            thothTokenToResearcher :: Bool 
            thothTokenToResearcher = True -- assetClassValueOf (valuePaidTo info $ unPaymentPubKeyHash (pConstable prison)) (pToken prison) == 1  
            
            checkScriptValueAfterTx :: Integer -> Bool 
            checkScriptValueAfterTx tribute = valueLockedBy info (ownHash ctx) `leq` (lovelaceValueOf tribute)   

            initTokenToInitResearcher :: Address -> Bool 
            initTokenToInitResearcher rAddr = assetClassValueOf (valuePaidTo info $ (addressPkh rAddr)) (initNetworkToken nattr) >= 1 

            checkSpawnTokenInScript :: AssetClass -> Bool 
            checkSpawnTokenInScript spwT = any (\_ -> True) [val `geq` assetClassValue spwT 1 | (dh, val) <- scriptOwnOutputs]

            checkCountDatum :: Bool 
            checkCountDatum = True      

            checkDataInOutDatum :: Bool 
            checkDataInOutDatum = True     

            checkInitInputsToken :: Bool 
            checkInitInputsToken = (assetClassValueOf (txOutValue ownInput) (initNetworkToken nattr) == 1)                            


                -- utxos <- utxosAt addr'
                -- return any f utxos
                --     where
                --         f :: (TxOutRef, ChainIndexTxOut) -> Bool
                --         f (_, o) = getLovelace (fromValue (txOutValue $ toTxOut o)) >= toValue minAdaTxOut


data ThothNetwork
instance Scripts.ValidatorTypes ThothNetwork where 
    type instance DatumType ThothNetwork = NetworkDatum 
    type instance RedeemerType ThothNetwork = NetworkRedeemer

typedNetworkValidator :: NetworkAttrbutes -> Scripts.TypedValidator ThothNetwork
typedNetworkValidator nattr = Scripts.mkTypedValidator @ThothNetwork
    ($$(PlutusTx.compile [|| mkNetworkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode nattr)
    $$(PlutusTx.compile [|| wrap ||])
  where
      wrap = Scripts.wrapValidator @NetworkDatum @NetworkRedeemer
      
      addr = initResearcherAddress nattr


networkValidator :: NetworkAttrbutes -> Validator
networkValidator = Scripts.validatorScript . typedNetworkValidator

networkAddress :: NetworkAttrbutes -> Ledger.Address
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

findAttributesOutput :: AssetClass -> Address -> Contract w s Text (Maybe (TxOutRef, ChainIndexTxOut, NetworkDatum))
findAttributesOutput token scriptAddress = do 
        utxos <- utxosAt $ scriptAddress 
        return $ do 
            (oref, o) <- find f $ Map.toList utxos
            Datum dat <- case _ciTxOutDatum o of 
                                Left _  -> Nothing
                                Right d -> Just d
            pDat <- PlutusTx.fromBuiltinData dat 
            return (oref, o, pDat)
    where 
        f :: (TxOutRef ,ChainIndexTxOut) -> Bool 
        f ( _ , o) = assetClassValueOf (txOutValue $ toTxOut o) token >= 1

data NetworkInitParams = NetworkInitParams
    { rZeroAddress              :: !Address
    , initialNetworkParams      :: !String
    , networkTributeAmount      :: !Integer
    , initNetworkTokenName      :: !TokenName
    , initNetworkDeadline       :: !POSIXTime
    , initNetworkTokenAmount    :: !Integer
    } deriving (Show, Generic, FromJSON, ToJSON)


initializeThothNetwork :: forall w s. NetworkInitParams -> Contract w s Text ()
initializeThothNetwork nip = do 
    pkh <- Contract.ownPaymentPubKeyHash
    now <- currentTime 
    let mintInitDealine =  fromMilliSeconds $ (DiffMilliSeconds ((getPOSIXTime now) + (10 * 1000 * 60)))
    logDebug @String $ printf "Init mint deadline has been set to: " ++ (show mintInitDealine)

    let addr     = rZeroAddress nip
        initTTn  = initNetworkTokenName nip 
        deadline = initNetworkDeadline nip
        initTAmt = initNetworkTokenAmount nip

    utxos <- fundsAtAddressGeq (rZeroAddress nip) (Ada.lovelaceValueOf 10_000_000)
    case Map.toList utxos of 
         [(oref, o)] -> do 
                Contract.logDebug @String $ printf "picked UTxo at" ++ (show oref) ++ "with value" ++ (show $ _ciTxOutValue o) 
                let initCs  = networkInitTokenCurSymbol pkh oref initTTn mintInitDealine initTAmt
                    initTPolicy = networkInitTokenPolicy pkh oref initTTn mintInitDealine initTAmt
                let nattr = NetworkAttrbutes 
                        { initResearcherAddress = addr
                        , initNetworkToken      = Value.assetClass initCs initTTn 
                        , initTributeAmount     = networkTributeAmount nip
                        }
                    scriptAddress = networkAddress nattr
                    valHash       = validatorHash $ networkValidator nattr
                let tributeVal                = lovelaceValueOf $ networkTributeAmount nip
                    initTVal                  = Value.singleton initCs initTTn initTAmt
                    initTSplit                = splitInitToken initTVal
                    initTScriptSplit          = case splitInitToken initTVal of
                                                     Nothing -> Value.singleton initCs initTTn 1
                                                     Just s  -> fst s
                                                -- if isJust initTSplit then fst $ PlPrelude.fromMaybe initTSplit else traceError @String "couldn't find token value"
                    initTResearcherSplit      = case splitInitToken initTVal of
                                                     Nothing -> Value.singleton initCs initTTn 1
                                                     Just s  -> snd s
                                                -- if isJust initTSplit then snd $ PlPrelude.fromMaybe initTSplit else traceError @String "couldn't find token value"
                    scriptValPkg              = tributeVal <> initTScriptSplit
                    lookups                   = Constraints.typedValidatorLookups (typedNetworkValidator nattr)         <>
                                                Constraints.mintingPolicy initTPolicy                                                           <>
                                                Constraints.otherScript (networkValidator nattr) 
                    constraints               = Constraints.mustPayToOtherScript valHash (Datum $ PlutusTx.toBuiltinData Dead) scriptValPkg     <>
                                                Constraints.mustMintValue initTVal                                                                   
                adjustAndSubmitWith @ThothNetwork lookups constraints
                -- ledgerTx <- submitTxConstraintsWith @ThothNetwork lookups tx
                -- void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                logInfo @String $ "Researcher zero has spawned the network with credential: " ++ show (addressCredential addr)
                logInfo @String $ "Researcher zero has minted initToken: " ++ (show initTVal)
                now <- currentTime

                -- return scriptAddress

                waitUnitlTimeHasPassed $ now + 10 
                now1 <- currentTime

                logInfo @String $ "TIme now is: " ++ show now1
                -- let initAssetClass = Value.assetClass initCs initTTn

                -- initToken <- findAttributesOutput 
                -- case findAttributesOutput

                utxos <- Map.filter (\o -> assetClassValueOf (txOutValue $ toTxOut o) (initNetworkToken nattr) >= 1) <$> utxosAt scriptAddress

                case Map.toList utxos of 
                     [(initToref, initTo)] -> do 
                         Contract.logDebug @String $ printf "picked UTxo at" ++ (show initToref) ++ "with value" ++ (show $ _ciTxOutValue initTo)
                         let initTValue       = _ciTxOutValue initTo
                             spawnTAmnt       = deriveSpawnAmount initTValue
                             spawnTTn         = deriveSpawnTokenName initTValue
                             spawnTCs         = networkSpawnTokenCurSymbol scriptAddress initToref spawnTTn spawnTAmnt
                             spawnTPolicy     = networkSpawnTokenPolicy scriptAddress initToref spawnTTn spawnTAmnt
                             spawnTValue      = Value.singleton spawnTCs spawnTTn spawnTAmnt
                             spawnTAssetClass = Value.assetClass spawnTCs spawnTTn 
 
                         let lookups'     = Constraints.typedValidatorLookups (typedNetworkValidator nattr)          <>
                                            Constraints.mintingPolicy spawnTPolicy                                   <>
                                            Constraints.otherScript (networkValidator nattr)
                             constraints' = Constraints.mustSpendScriptOutput initToref (Redeemer $ PlutusTx.toBuiltinData (Spawn (unPaymentPubKeyHash pkh, spawnTAssetClass)))  <>
                                            Constraints.mustPayToOtherScript valHash (Datum $ PlutusTx.toBuiltinData Initialized) spawnTValue                                    <>
                                            Constraints.mustMintValue spawnTValue
                 
                         void $ adjustAndSubmitWith @ThothNetwork lookups' constraints'

                         logInfo @String $ "Researcher zero has minted spawnToken: " ++ (show spawnTValue)



                -- utxos <- fundsAtAddressGeq (networkAddress addr) (Ada.lovelaceValueOf 5_000_000)
                -- case Map.toList utxos of 
                --     [(oref, o)] -> do 
                --             Contract.logDebug @String $ printf "picked UTxo at" ++ (show oref) ++ "with value" ++ (show $ _ciTxOutValue o)
                        
                --             let cs            = networkTokenCurSymbol oref tn amt
                --                 val           = Value.singleton cs tn amt
                --                 researcherVal = Value.singleton cs tn 1
                --                 lookups'      = Constraints.mintingPolicy (networkTokenPolicy oref tn amt)             <>
                --                                 Constraints.typedValidatorLookups (typedNetworkValidator (initResearcherAddress nattr))         <>
                --                                 Constraints.otherScript (networkValidator addr)                        <>
                --                                 Constraints.unspentOutputs (Map.singleton oref o)
                --                 constraints'  = Constraints.mustMintValue val                                          <>
                --                                 -- Constraints.mustPayToPubKey pkh researcherVal                                    <>
                --                                 Constraints.mustPayToOtherScript valHash (Datum $ PlutusTx.toBuiltinData (Active testDatum)) val         <>
                --                                 Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData (InitNetwork (unPaymentPubKeyHash pkh, networkTributeAmount nip)))

                --             void $ adjustAndSubmitWith @ThothNetwork lookups' constraints'
                --             -- ledgerTx <- submitTxConstraintsWith @ThothNetwork lookups constraints
                --             -- void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                --             Contract.logInfo @String $ printf "minted: " ++ (show val)

    -- case getCredentials addr of 
    --     Nothing      -> Contract.throwError $ pack $ printf "expected pubkey address, but got" ++ (show addr)
    --     Just (x, my) -> do
    --         oref <- getUnspentOutput
    --         o    <- fromJust <$> Contract.txOutFromRef oref 
    --         Contract.logDebug @String $ printf "picked UTxo at" ++ (show oref) ++ "with value" ++ (show $ _ciTxOutValue o)

  where
      f :: ChainIndexTxOut -> Bool 
      f o = assetClassValueOf (txOutValue $ toTxOut o) (AssetClass (adaSymbol, adaToken)) >= 10

      deriveSpawnAmount :: Value -> Integer 
      deriveSpawnAmount initV = case Value.flattenValue initV of 
                                           [(cs,tn, amt)]  -> spawnAcitivityMultipliplier * amt 
                                           _               -> 1

      deriveSpawnTokenName :: Value -> TokenName 
      deriveSpawnTokenName initV = case Value.flattenValue initV of 
                                              [(cs,tn, amt)]  -> Value.tokenName $ BsChar8.pack $ show $ unTokenName tn  `appendByteString` appendSpwnNameString
                                              _               -> Value.tokenName $ BsChar8.pack (show emptySpwnNameString)

      appendSpwnNameString :: BuiltinByteString
      appendSpwnNameString =  "spawn tn" -- Data.ByteString.Char8.pack $ show (toInteger (getSlot 1)

      emptySpwnNameString :: BuiltinByteString
      emptySpwnNameString = emptyByteString

spawnAcitivityMultipliplier :: Integer
spawnAcitivityMultipliplier = 5

splitInitToken :: Value -> Maybe (Value, Value)
splitInitToken initVal = if not (Value.isZero initVal) then case flattenValue initVal of 
                                                                 [(cs, tn, amt)] -> Just (shareTAmt amt cs tn)
                                                                 _               -> Nothing
                                                       else Nothing 
    where 
        shareTAmt :: Integer -> CurrencySymbol -> TokenName -> (Value, Value)
        shareTAmt amt cs tn = let split = fromIntegral amt `PlTxPrelude.divide` 2 in 
                                  (Value.singleton cs tn split, Value.singleton cs tn split)   


data NetworkActivateParams = NetworkActivateParams
    { networkTokenName          :: !TokenName
    , networkTokenInitialSupply :: !Integer
    , networkActiveDatum        :: !BuiltinByteString
    , rZeroActivateAddress      :: !Address
    } deriving (Show, Generic, FromJSON, ToJSON)

activateThothNetwork :: forall w s. NetworkActivateParams -> Contract w s Text ()
activateThothNetwork nap = do 
    pkh <- Contract.ownPaymentPubKeyHash
    -- TODO : You need a datum with data to fill nattr
    let addr    = rZeroActivateAddress nap
        dummyCs = Value.currencySymbol "ff"
        dummyTn = Value.tokenName "Dummy"
        nattr   = NetworkAttrbutes 
                { initResearcherAddress = addr
                , initNetworkToken      = Value.assetClass dummyCs dummyTn 
                , initTributeAmount     = 2_000_000
                }
    utxos <- fundsAtAddressGeq (networkAddress nattr) (Ada.lovelaceValueOf 10_000_000)
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
                    valHash     = validatorHash $ networkValidator nattr
                    testDatum   = networkActiveDatum nap
                    lookups     = Constraints.mintingPolicy (networkTokenPolicy oref tn amt)             <>
                                  Constraints.typedValidatorLookups (typedNetworkValidator nattr)         <>
                                --   Constraints.otherScript (networkValidator addr)                        <>
                                  Constraints.unspentOutputs (Map.singleton oref o)
                    constraints = Constraints.mustMintValue val                                          <>
                                  Constraints.mustPayToOtherScript valHash (Datum $ PlutusTx.toBuiltinData (Active testDatum)) val         <>
                                  -- TODO: change the redeemer to reflect change in validator
                                  Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData (InitNetwork (unPaymentPubKeyHash pkh, amt)))  -- TODO

                void $ adjustAndSubmitWith @ThothNetwork lookups constraints
                -- ledgerTx <- submitTxConstraintsWith @ThothNetwork lookups constraints
                -- void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                Contract.logInfo @String $ printf "minted: " ++ (show val)
        

    now <- currentTime

    waitUnitlTimeHasPassed $ now + 10 
    now1 <- currentTime

    utxosB <- utxosAt (networkAddress nattr)
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
        