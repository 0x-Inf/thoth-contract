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
    , ConjureNetworkParams (..)
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
import           Prelude                     (Semigroup (..), Show (..), String, fromIntegral, div)
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



{-# INLINABLE mkNetworkConjureTokenPolicy #-}
-- | The validator for the thoth network token policy
mkNetworkConjureTokenPolicy :: PaymentPubKeyHash ->  TxOutRef -> TokenName -> POSIXTime -> Integer -> () -> ScriptContext -> Bool
mkNetworkConjureTokenPolicy pkh oref tn deadline amt () ctx = 
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


networkConjureTokenPolicy :: PaymentPubKeyHash ->  TxOutRef -> TokenName -> POSIXTime -> Integer -> Scripts.MintingPolicy
networkConjureTokenPolicy pkh oref tn deadline amt = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \pkh' oref' tn' deadline' amt' -> Scripts.wrapMintingPolicy $ mkNetworkConjureTokenPolicy pkh' oref' tn' deadline' amt' ||])
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

networkConjureTokenCurSymbol ::  PaymentPubKeyHash ->  TxOutRef -> TokenName -> POSIXTime -> Integer -> CurrencySymbol
networkConjureTokenCurSymbol pkh oref tn deadline = scriptCurrencySymbol . networkConjureTokenPolicy pkh oref tn deadline


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
    { researcherZeroAddress       :: Address
    , thothNetworkAddresss        :: Address 
    , conjureNetworkToken         :: AssetClass
    , conjureTributeAmount        :: Integer
    , spawnNetworkToken           :: Maybe AssetClass
    , initializeThothNetworkToken :: Maybe AssetClass  
    , activateThothNetworkToken   :: Maybe AssetClass 
    } deriving Show

PlutusTx.makeLift ''NetworkAttrbutes    

PlutusTx.unstableMakeIsData ''NetworkAttrbutes 

-- | the datum state data type
data DatumStateData = DatumStateData
    { researcherPaymentPubKeyHash   :: [PubKeyHash]
    , networkScriptAddress          :: Address 
    , networkParams                 :: String
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord, OpenApi.ToSchema, PlutusTx.UnsafeFromData, PlutusTx.FromData, 
                PlutusTx.ToData)

-- PlutusTx.unstableMakeIsData ''DatumStateData

-- | The datum of the network contained in the utxos at script address 
data NetworkDatum = Dead NetworkAttrbutes | Spawned NetworkAttrbutes | Initialized NetworkAttrbutes | Active BuiltinByteString  
    deriving Show

PlutusTx.unstableMakeIsData ''NetworkDatum

-- | The network update interactions
data NetworkRedeemer = Spawn (PubKeyHash, AssetClass) 
                     | InitializeNetwork (PubKeyHash, AssetClass) 
                     | ActivateNetwork (PubKeyHash, Integer, AssetClass) 
                     | InitializeReseacher (PubKeyHash, AssetClass, Integer)
                     | InitGuest 
                     | MorphNetwork
    deriving Show 

PlutusTx.unstableMakeIsData ''NetworkRedeemer


{-# INLINABLE mkNetworkValidator #-}
-- | The network on chain Validator
mkNetworkValidator :: Address -> NetworkDatum -> NetworkRedeemer -> ScriptContext -> Bool
mkNetworkValidator r0addr d r ctx = 
    -- traceIfFalse "Payment Key Hash doesn't have tokens" $ checkAddressHasTokens r0addr   &&
    traceIfFalse "Not signed by authorized agent" (txSignedBy info (addressPkh r0addr))  &&
    case (d, r) of 
        (Dead attr, Spawn (pkh, spwnToken))                                                   -> 
            traceIfFalse "Not signed by the authorized pubkeyhash"  (txSignedBy info (addressPkh r0addr))                               &&
            -- traceIfFalse "Script doesn't contain tribute Ada"       (checkScriptValueAfterTx (conjureTributeAmount attr))                  &&
            traceIfFalse "Didn't Mint atleast one spawn token"      (checkSpawnTokenInScript spwnToken)                                 &&
            traceIfFalse "Action count is beyond limit"             checkCountDatum                                                     &&
            traceIfFalse "Caller doen't have required token"        (checkInitInputsToken $ conjureNetworkToken attr)                      &&
            traceIfFalse "Datum Not updated with Network attr"      checkDataInOutDatum 
        (Spawned attr, InitializeNetwork (pkh, thothInitToken))          -> 
            traceIfFalse "Not signed by the valid pubkeyhash"                (txSignedBy info pkh)                                             &&
            traceIfFalse "Init Token is send to script"                      (checkInitTokenInScript thothInitToken)                           &&
            traceIfFalse "Init Token hasn't been transfered to researcher"   (initTokenToInitResearcher r0addr $ conjureNetworkToken attr )         &&
            traceIfFalse "Datum not updated after network initialization"    checkDatumForInitializationUpdate
        (Initialized attr, ActivateNetwork (pkh, tribute, effortToken))                   -> 
            -- TODO: Change the checks to reflect the action of 
            traceIfFalse "Not signed by the valid pubkeyhash"         (txSignedBy info pkh)               &&
            traceIfFalse "There is no output to the current script" tokenOutputToScript                   && 
            traceIfFalse "Check if the datum has been set"          checkIfDatumIsSet                     &&
            traceIfFalse "Output to Researcher zero is missing"     outputsToResearcherZero               && 
            traceIfFalse "Agent hasn't paid tribute to script"      checkBalanceAfterTx                   &&
            traceIfFalse "Script doesn't contain tribute Ada"       (checkScriptValueAfterTx (conjureTributeAmount attr))
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
            outputsToResearcherZero =  let value_researcher = pubKeyOutputsAt (addressPkh r0addr) info 
                                                  in length value_researcher > 1

            checkBalanceAfterTx :: Bool 
            checkBalanceAfterTx = case getContinuingOutputs ctx of 
                                       os -> let value = foldl' (\total tributeVal -> total <> tributeVal) (lovelaceValueOf 0) $ txOutValue <$> os in 
                                                 value `geq` (lovelaceValueOf 1_000_000)

            thothTokenToResearcher :: Bool 
            thothTokenToResearcher = True -- assetClassValueOf (valuePaidTo info $ unPaymentPubKeyHash (pConstable prison)) (pToken prison) == 1  
            
            checkScriptValueAfterTx :: Integer -> Bool 
            checkScriptValueAfterTx tribute = valueLockedBy info (ownHash ctx) `geq` (lovelaceValueOf tribute)   

            initTokenToInitResearcher :: Address -> AssetClass -> Bool 
            initTokenToInitResearcher rAddr initNToken = assetClassValueOf (valuePaidTo info $ (addressPkh rAddr)) initNToken >= 1 

            checkSpawnTokenInScript :: AssetClass -> Bool 
            checkSpawnTokenInScript spwT = any (\_ -> True) [val `geq` assetClassValue spwT 1 | (dh, val) <- scriptOwnOutputs]

            checkInitTokenInScript :: AssetClass -> Bool 
            checkInitTokenInScript initT = any (\_ -> True) [val `geq` assetClassValue initT 1 | (dh, val) <- scriptOwnOutputs]

            checkCountDatum :: Bool 
            checkCountDatum = True      

            checkDataInOutDatum :: Bool 
            checkDataInOutDatum = True     

            checkDatumForInitializationUpdate :: Bool 
            checkDatumForInitializationUpdate = True 

            checkInitInputsToken :: AssetClass -> Bool 
            checkInitInputsToken initNToken = (assetClassValueOf (txOutValue ownInput) initNToken >= 1)                            


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

findNetworkAttributesOutput :: AssetClass -> Address -> Contract w s Text (Maybe (TxOutRef, ChainIndexTxOut, NetworkDatum))
findNetworkAttributesOutput token scriptAddress = do 
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

data ConjureNetworkParams = ConjureNetworkParams
    { conjuringResearcherAddress   :: !Address
    , conjureNetworkTributeAmount  :: !Integer
    , conjureNetworkTokenName      :: !TokenName
    , conjureNetworkDeadline       :: !POSIXTime
    } deriving (Show, Generic, FromJSON, ToJSON)


initializeThothNetwork :: forall w s. ConjureNetworkParams -> Contract w s Text ()
initializeThothNetwork cnp = do 
    pkh <- Contract.ownPaymentPubKeyHash
    now <- currentTime 
    let mintInitDealine =  fromMilliSeconds $ (DiffMilliSeconds ((getPOSIXTime now) + (10 * 1000 * 60)))
    logDebug @String $ printf "Init mint deadline has been set to: " ++ (show mintInitDealine)

    let addr     = conjuringResearcherAddress cnp
        conjTTn  = conjureNetworkTokenName cnp 
        deadline = conjureNetworkDeadline cnp
        conjTributeAmt = conjureNetworkTributeAmount cnp
        conjTAmt = deriveConjureAmount conjTributeAmt
        

    utxos <- fundsAtAddressGeq (conjuringResearcherAddress cnp) (Ada.lovelaceValueOf 10_000_000)
    case Map.toList utxos of 
         [(oref, o)] -> do 
                Contract.logDebug @String $ printf "picked UTxo at" ++ (show oref) ++ "with value" ++ (show $ _ciTxOutValue o) 
                let conjCs  = networkConjureTokenCurSymbol pkh oref conjTTn mintInitDealine conjTAmt
                    conjTPolicy = networkConjureTokenPolicy pkh oref conjTTn mintInitDealine conjTAmt
                    scriptAddress = networkAddress addr
                    valHash       = validatorHash $ networkValidator addr
                let nattr = NetworkAttrbutes 
                        { researcherZeroAddress       = addr
                        , thothNetworkAddresss        = scriptAddress
                        , conjureNetworkToken         = Value.assetClass conjCs conjTTn 
                        , conjureTributeAmount        = conjureNetworkTributeAmount cnp
                        , spawnNetworkToken           = Nothing 
                        , initializeThothNetworkToken = Nothing 
                        , activateThothNetworkToken   = Nothing 
                        }
                let tributeVal                = lovelaceValueOf $ conjureNetworkTributeAmount cnp
                    conjTVal                  = Value.singleton conjCs conjTTn conjTAmt
                    conjTSplit                = splitInitToken conjTVal
                    conjTScriptSplit          = case splitInitToken conjTVal of
                                                     Nothing -> Value.singleton conjCs conjTTn 1
                                                     Just s  -> fst s
                    conjTResearcherSplit      = case splitInitToken conjTVal of
                                                     Nothing -> Value.singleton conjCs conjTTn 1
                                                     Just s  -> snd s
                    scriptValPkg              = tributeVal <> conjTScriptSplit
                    lookups                   = Constraints.typedValidatorLookups (typedNetworkValidator addr)         <>
                                                Constraints.mintingPolicy conjTPolicy                                                           <>
                                                Constraints.otherScript (networkValidator addr) 
                    constraints               = Constraints.mustPayToOtherScript valHash (Datum $ PlutusTx.toBuiltinData (Dead nattr)) scriptValPkg         <>
                                                Constraints.mustMintValue conjTVal                                                                   
                adjustAndSubmitWith @ThothNetwork lookups constraints
                -- ledgerTx <- submitTxConstraintsWith @ThothNetwork lookups tx
                -- void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                logInfo @String $ "Network has been conjured with address: " ++  (show scriptAddress)
                logInfo @String $ "Researcher zero has minted conjure token: " ++  (show conjTVal)
                now <- currentTime

                waitUnitlTimeHasPassed $ now + 5_000  
                now1 <- currentTime

                logInfo @String $ "TIme now is: " ++ show now1
                -- let initAssetClass = Value.assetClass initCs initTTn

                -- initToken <- findNetworkAttributesOutput 
                -- case findNetworkAttributesOutput

                utxos <- Map.filter (\o -> assetClassValueOf (txOutValue $ toTxOut o) (conjureNetworkToken nattr) >= 1) <$> utxosAt scriptAddress

                case Map.toList utxos of 
                     [(initToref, initTo)] -> do 
                         Contract.logDebug @String $ printf "picked UTxo at" ++ (show initToref) ++ "with value" ++ (show $ _ciTxOutValue initTo)
                         let initTValue       = _ciTxOutValue initTo
                             spawnTAmnt       = 2
                             spawnTTn         = Value.tokenName "Spawn Thoth"
                             spawnTCs         = networkSpawnTokenCurSymbol scriptAddress initToref spawnTTn spawnTAmnt
                             spawnTPolicy     = networkSpawnTokenPolicy scriptAddress initToref spawnTTn spawnTAmnt
                             spawnTValue      = Value.singleton spawnTCs spawnTTn spawnTAmnt
                             spawnTAssetClass = Value.assetClass spawnTCs spawnTTn 
                             spawnTScriptSplit          = case splitSpawnToken spawnTValue of
                                                              Nothing -> Value.singleton spawnTCs spawnTTn 1
                                                              Just s  -> fst s
                             spawnTResearcherSplit      = case splitSpawnToken spawnTValue of
                                                              Nothing -> Value.singleton spawnTCs spawnTTn 1
                                                              Just s  -> snd s
                             spawnScriptPkg         = initTValue <> spawnTScriptSplit
                         let nattr' = NetworkAttrbutes 
                                  { researcherZeroAddress       = addr
                                  , thothNetworkAddresss        = scriptAddress
                                  , conjureNetworkToken         = Value.assetClass conjCs conjTTn 
                                  , conjureTributeAmount        = conjureNetworkTributeAmount cnp
                                  , spawnNetworkToken           = (Just spawnTAssetClass)
                                  , initializeThothNetworkToken = Nothing 
                                  , activateThothNetworkToken   = Nothing 
                                  }
 
                         let lookups'     = Constraints.typedValidatorLookups (typedNetworkValidator addr)          <>
                                            Constraints.mintingPolicy spawnTPolicy                                   <>
                                            Constraints.otherScript (networkValidator addr)                         <>
                                            Constraints.unspentOutputs (Map.singleton initToref initTo)
                             constraints' = Constraints.mustSpendScriptOutput initToref (Redeemer $ PlutusTx.toBuiltinData (Spawn (unPaymentPubKeyHash pkh, spawnTAssetClass)))  <>
                                            Constraints.mustPayToOtherScript valHash (Datum $ PlutusTx.toBuiltinData (Initialized nattr')) spawnScriptPkg                                    <>
                                            Constraints.mustMintValue spawnTValue
                 
                         void $ adjustAndSubmitWith @ThothNetwork lookups' constraints'

                         logInfo @String $ "Researcher zero has minted spawnToken: " ++ (show spawnTValue)

                        --  return scriptAddress


                -- utxos <- fundsAtAddressGeq (networkAddress addr) (Ada.lovelaceValueOf 5_000_000)
                -- case Map.toList utxos of 
                --     [(oref, o)] -> do 
                --             Contract.logDebug @String $ printf "picked UTxo at" ++ (show oref) ++ "with value" ++ (show $ _ciTxOutValue o)
                        
                --             let cs            = networkTokenCurSymbol oref tn amt
                --                 val           = Value.singleton cs tn amt
                --                 researcherVal = Value.singleton cs tn 1
                --                 lookups'      = Constraints.mintingPolicy (networkTokenPolicy oref tn amt)             <>
                --                                 Constraints.typedValidatorLookups (typedNetworkValidator (conjuringResearcherAddress nattr))         <>
                --                                 Constraints.otherScript (networkValidator addr)                        <>
                --                                 Constraints.unspentOutputs (Map.singleton oref o)
                --                 constraints'  = Constraints.mustMintValue val                                          <>
                --                                 -- Constraints.mustPayToPubKey pkh researcherVal                                    <>
                --                                 Constraints.mustPayToOtherScript valHash (Datum $ PlutusTx.toBuiltinData (Active testDatum)) val         <>
                --                                 Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData (InitNetwork (unPaymentPubKeyHash pkh, conjureNetworkTributeAmount cnp)))

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

      deriveConjureAmount :: Integer -> Integer 
      deriveConjureAmount conjTributeAmt' = (conjTributeAmt' `div` 1_000_000) 

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

splitSpawnToken :: Value -> Maybe (Value, Value)
splitSpawnToken spawnVal = if not (Value.isZero spawnVal) then case flattenValue spawnVal of 
                                                                 [(cs, tn, amt)] -> Just (shareTAmt amt cs tn)
                                                                 _               -> Nothing
                                                       else Nothing 
    where 
        shareTAmt :: Integer -> CurrencySymbol -> TokenName -> (Value, Value)
        shareTAmt amt cs tn = let split = fromIntegral amt `PlTxPrelude.divide` 2 in 
                                  (Value.singleton cs tn split, Value.singleton cs tn split)                                     

-- data NetworkSpawnParams = NetworkSpawnParams
--     { networkTokenName            :: !TokenName
--     , networkScriptAddress        :: !Address
--     } deriving (Show, Generic, FromJSON, ToJSON)



-- spawnThothNetwork :: forall w s. NetworkSpawnParams -> Contract w s Text ()
-- spawnThothNetwork nsp = do 
--     pkh <- Contract.ownPaymentPubKeyHash


    -- 

data NetworkActivateParams = NetworkActivateParams
    { networkTokenName          :: !TokenName
    , networkTokenInitialSupply :: !Integer
    , spawnNetworkAccessToken   :: !AssetClass
    , networkAddress            :: !Address 
    , networkActiveDatum        :: !BuiltinByteString
    , rZeroActivateAddress      :: !Address
    } deriving (Show, Generic, FromJSON, ToJSON)

activateThothNetwork :: forall w s. NetworkActivateParams -> Contract w s Text ()
activateThothNetwork nap = do 
    pkh <- Contract.ownPaymentPubKeyHash

    netAttr <- findNetworkAttributesOutput 
    -- TODO : You need a datum with data to fill nattr
    let addr    = rZeroActivateAddress nap
        dummyCs = Value.currencySymbol "ff"
        dummyTn = Value.tokenName "Dummy"
        nattr = NetworkAttrbutes 
            { researcherZeroAddress       = addr
            , thothNetworkAddresss        = addr
            , conjureNetworkToken         = Value.assetClass dummyCs dummyTn  
            , conjureTributeAmount        = 2_000_000
            , spawnNetworkToken           = Nothing 
            , initializeThothNetworkToken = Nothing 
            , activateThothNetworkToken   = Nothing 
            }
                
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
                                --   Constraints.otherScript (networkValidator addr)                        <>
                                  Constraints.unspentOutputs (Map.singleton oref o)
                    constraints = Constraints.mustMintValue val                                          <>
                                  Constraints.mustPayToOtherScript valHash (Datum $ PlutusTx.toBuiltinData (Active testDatum)) val         
                                  -- TODO: change the redeemer to reflect change in validator
                                --   Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData (InitializeNetwork (unPaymentPubKeyHash pkh, amt)))  -- TODO

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
        Endpoint "conjure" ConjureNetworkParams
    .\/ Endpoint "activate" NetworkActivateParams

endpoints :: Contract () ThothNetworkSchema Text ()
endpoints = awaitPromise (conjure `select` activate) >> endpoints
    where
        conjure = endpoint @"conjure" $ \cnp -> do
            initializeThothNetwork cnp

        activate = endpoint @"activate" $ \nap -> do 
            activateThothNetwork nap 


-- mkSchemaDefinitions ''ThothNetworkSchema

-- myToken :: KnownCurrency
-- myToken = KnownCurrency (ValidatorHash "f") "Token" (TokenName "T" :| [])

-- mkKnownCurrencies ['myToken]
        
