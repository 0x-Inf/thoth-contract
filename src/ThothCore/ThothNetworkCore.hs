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
    ( 
    -- * Functionality for network Initialization        
      conjureThothNetwork
    , initEndpoints
    , conjEndpoint
    , activateEndpoint
    , initializeResearcherEndpoint
    -- * Coverage Testing
    , networkCovIdx
    -- * Utility Functions
    , splitTokenVal
    , adjustAndSubmitWith
    , waitUnitlTimeHasPassed
    -- * Initialization Parameters
    , ConjureNetworkParams (..)
    , NetworkInitializeParams (..)
    , NetworkActivateParams (..)
    , ResearcherInitializeParams (..)
    ) where

import           Control.Monad               hiding (fmap)
import           Control.Lens                hiding (contains, to)
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
import qualified PlutusTx.AssocMap           as AssocMap
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
    $$(PlutusTx.compile [|| \sAddress' oref' tn' amt' -> Scripts.wrapMintingPolicy $ mkNetworkSpawnTokenPolicy sAddress' oref' tn' amt' ||])
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

type ResearcherTxOutRef = TxOutRef

type SpawnTokenTxOutRef = TxOutRef

{-# INLINABLE mkNetworkInitializeTokenPolicy #-}
-- | The validator for the initialize network token 
mkNetworkInitializeTokenPolicy :: Address -> NetworkAttrbutes -> ResearcherTxOutRef -> SpawnTokenTxOutRef -> TokenName -> Integer -> () -> ScriptContext -> Bool 
mkNetworkInitializeTokenPolicy addr nattr roref storef tn amt () ctx = 
    traceIfFalse "Researcher Utxo not consumed" hasResearcherUtxo              &&
    traceIfFalse "Script token Utxo not consumed" hasScriptUtxo                &&
    traceIfFalse "wrong amount minted" checkMintedAmount                       &&
    traceIfFalse "Not signed by researcher zero" checkRZeroSignature           && 
    traceIfFalse "Check spawn token " checkSpawnToken
  where 
      info :: TxInfo 
      info = scriptContextTxInfo ctx 

      hasResearcherUtxo :: Bool 
      hasResearcherUtxo = any (\i -> txInInfoOutRef i == roref) $ txInfoInputs info

      hasScriptUtxo ::  Bool 
      hasScriptUtxo = any (\i -> txInInfoOutRef i == storef) $ txInfoInputs info

      checkMintedAmount :: Bool 
      checkMintedAmount = case flattenValue (txInfoMint info) of
                                 [(_, tn', amt')] -> tn' == tn && amt' == amt 
                                 _                -> False

      checkRZeroSignature :: Bool 
      checkRZeroSignature = case toPubKeyHash (_researcherZeroAddress nattr) of 
                                 Nothing  -> False 
                                 Just pkh -> txSignedBy info pkh && addr == (_researcherZeroAddress nattr)

      checkSpawnToken :: Bool
      checkSpawnToken = True                                  

networkInitializeTokenPolicy :: Address -> NetworkAttrbutes -> ResearcherTxOutRef -> SpawnTokenTxOutRef -> TokenName -> Integer -> Scripts.MintingPolicy
networkInitializeTokenPolicy addr nattr roref storef tn amt = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \addr' nattr' roref' storef' tn' amt' ->  Scripts.wrapMintingPolicy $ mkNetworkInitializeTokenPolicy addr' nattr' roref' storef' tn' amt' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode addr
    `PlutusTx.applyCode`
    PlutusTx.liftCode nattr
    `PlutusTx.applyCode`
    PlutusTx.liftCode roref
    `PlutusTx.applyCode`
    PlutusTx.liftCode storef
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn
    `PlutusTx.applyCode`
    PlutusTx.liftCode amt

networkInitializeTokenCurSym :: Address -> NetworkAttrbutes -> ResearcherTxOutRef -> SpawnTokenTxOutRef -> TokenName -> Integer -> CurrencySymbol
networkInitializeTokenCurSym addr nattr roref storef tn = scriptCurrencySymbol . networkInitializeTokenPolicy addr nattr roref storef tn

type InitTokenTxOutRef = TxOutRef

{-# INLINABLE mkNetworkActivateTokenPolicy #-}
-- | The validator for the initialize network token 
mkNetworkActivateTokenPolicy :: Address -> NetworkAttrbutes -> ResearcherTxOutRef -> InitTokenTxOutRef -> TokenName -> Integer -> () -> ScriptContext -> Bool 
mkNetworkActivateTokenPolicy rAddr nattr rAdaToref initOref tn amt () ctx = 
    traceIfFalse "Researcher Utxo not consumed" hasResearcherUtxo              &&
    traceIfFalse "wrong amount minted" checkMintedAmount                       &&
    traceIfFalse "Not signed by researcher zero" checkRZeroSignature           && 
    traceIfFalse "Check spawn token " checkActivateToken
  where 
      info :: TxInfo 
      info = scriptContextTxInfo ctx 

      hasResearcherUtxo :: Bool 
      hasResearcherUtxo = any (\i -> txInInfoOutRef i == rAdaToref) $ txInfoInputs info

      checkMintedAmount :: Bool 
      checkMintedAmount = case flattenValue (txInfoMint info) of
                                 [(_, tn', amt')] -> tn' == tn && amt' == amt 
                                 _                -> False

      checkRZeroSignature :: Bool 
      checkRZeroSignature = case toPubKeyHash (_researcherZeroAddress nattr) of 
                                 Nothing  -> False 
                                 Just pkh -> txSignedBy info pkh && rAddr == (_researcherZeroAddress nattr)

      checkActivateToken :: Bool
      checkActivateToken = True     

networkActivateTokenPolicy :: Address -> NetworkAttrbutes -> ResearcherTxOutRef -> InitTokenTxOutRef -> TokenName -> Integer -> Scripts.MintingPolicy 
networkActivateTokenPolicy rAddr nattr rAdaToref initOref tn amt = mkMintingPolicyScript $ 
    $$(PlutusTx.compile [|| \addr' nattr' roref' initOref' tn' amt' ->  Scripts.wrapMintingPolicy $ mkNetworkActivateTokenPolicy addr' nattr' roref' initOref' tn' amt' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode rAddr
    `PlutusTx.applyCode`
    PlutusTx.liftCode nattr
    `PlutusTx.applyCode`
    PlutusTx.liftCode rAdaToref
    `PlutusTx.applyCode`
    PlutusTx.liftCode initOref
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn
    `PlutusTx.applyCode`
    PlutusTx.liftCode amt


networkActivateTokenCurSym :: Address -> NetworkAttrbutes -> ResearcherTxOutRef -> InitTokenTxOutRef -> TokenName -> Integer -> CurrencySymbol
networkActivateTokenCurSym rAddr nattr rAdaToref initOref tn = scriptCurrencySymbol . networkActivateTokenPolicy rAddr nattr rAdaToref initOref tn

type AdaTxOutRef = TxOutRef
type ActiveTokenTxOutRef = TxOutRef

{-# INLINABLE mkResearcherInitializeTokenPolicy #-}
-- | The validator for the initialize network token 
mkResearcherInitializeTokenPolicy :: Address -> AdaTxOutRef -> ActiveTokenTxOutRef -> TokenName -> Integer -> POSIXTime -> () -> ScriptContext -> Bool 
mkResearcherInitializeTokenPolicy addr adaOref activeTknOref tn amt deadline () ctx = 
    traceIfFalse "Not signed by correct pkh"         (checkSignature addr)  &&
    traceIfFalse "Minted wrong amount"               checkMintedAmount      && 
    traceIfFalse "Researcher ada token not consumed" hasResearcherAdaUtxo   &&
    traceIfFalse "Active Network token not consumed" hasNetworkActiveToken  && 
    traceIfFalse "Deadline missed"                   checkDeadlineReached
  where
      info :: TxInfo 
      info = scriptContextTxInfo ctx 

      hasResearcherAdaUtxo :: Bool 
      hasResearcherAdaUtxo = any (\i -> txInInfoOutRef i == adaOref) $ txInfoInputs info -- TODO: Check on the plutus-use-cases how this can be changed to use spendsOutputs if current prooves expensive

      hasNetworkActiveToken ::  Bool 
      hasNetworkActiveToken = any (\i -> txInInfoOutRef i == activeTknOref) $ txInfoInputs info

      checkSignature :: Address -> Bool 
      checkSignature addr' = txSignedBy info (addressPkh addr') 

      checkMintedAmount :: Bool                                                           -- TODO: Can use currencyValue and ownSymbol to get expected mint amount; this will remove the need for passing it in
      checkMintedAmount = case flattenValue (txInfoMint info) of
                                 [(_, tn', amt')] -> tn' == tn && amt' == amt 
                                 _                -> False  

      addressPkh :: Address -> PubKeyHash
      addressPkh addr'' = case toPubKeyHash addr'' of
                                    Nothing   -> traceError "something went wrong with pkh retrieval"
                                    Just pkh' -> pkh' 
      
      checkDeadlineReached :: Bool 
      checkDeadlineReached = txInfoValidRange info `contains` to deadline 

researcherInitializeTokenPolicy :: Address -> AdaTxOutRef -> ActiveTokenTxOutRef -> TokenName -> Integer -> POSIXTime -> Scripts.MintingPolicy
researcherInitializeTokenPolicy addr adaOref activeTknOref tn amt deadline = mkMintingPolicyScript $ 
    $$(PlutusTx.compile [|| \addr' adaOref' activeTknOref' tn' amt' deadline' -> Scripts.wrapMintingPolicy $ mkResearcherInitializeTokenPolicy addr' adaOref' activeTknOref' tn' amt' deadline' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode addr
    `PlutusTx.applyCode`
    PlutusTx.liftCode adaOref
    `PlutusTx.applyCode`
    PlutusTx.liftCode activeTknOref
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn
    `PlutusTx.applyCode`
    PlutusTx.liftCode amt
    `PlutusTx.applyCode`
    PlutusTx.liftCode deadline

researcherInitializeTokenCurSym ::  Address -> AdaTxOutRef -> ActiveTokenTxOutRef -> TokenName -> Integer -> POSIXTime -> CurrencySymbol
researcherInitializeTokenCurSym addr adaOref activeTknOref tn amt = scriptCurrencySymbol . researcherInitializeTokenPolicy addr adaOref activeTknOref tn amt
    
-- | The data for network attributes 
data NetworkAttrbutes = NetworkAttrbutes
    { _researcherZeroAddress             :: Address
    -- ^ The initial researcher who sets up the network.
    , _thothNetworkScriptAddresss        :: Address 
    -- ^ The address for the network script.
    , _conjureNetworkToken               :: AssetClass
    -- ^ The token indicating that the network has been conjured. (see note [Initialization Tokens])
    , _conjureTributeAmount              :: Integer
    -- ^ The amount of Ada the initial researcher *donates* to the network script.
     , _conjureActionCount               :: Integer
    -- ^ The number of times researchers have conjured the network.
    , _spawnNetworkToken                 :: Maybe AssetClass
    -- ^ The token minted after spawning the network. (see note [Initialization Tokens])
    , _initializeThothNetworkToken       :: Maybe AssetClass  
    -- ^ The token for initializing the network. (see note [Initialization Tokens])
    , _activateThothNetworkToken         :: Maybe AssetClass
    -- ^ The token indicating that the network is active and can 'create' researchers.
    , _activateThothNetworkTokenAmount   :: Maybe Integer 
    -- ^ This token amount will be useful in making the network behave concurrently.
    , _activeResearchersTokens           :: Maybe Integer
    -- ^ A proxy token for how many researchers are active in the network.
    } deriving Show

{- note [Initialization Tokens]

It certainly seems that we might have redundant tokens here; we still don't have a compelling reason for their inclusion,
and we could just remove some of them.
-}

makeLenses ''NetworkAttrbutes    

PlutusTx.makeLift ''NetworkAttrbutes    

PlutusTx.unstableMakeIsData ''NetworkAttrbutes 

-- | the datum state data type
data DatumStateData = DatumStateData
    { researcherPaymentPubKeyHash   :: [PubKeyHash]
    , datumNetworkAddress           :: Address 
    , networkParams                 :: String
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord, OpenApi.ToSchema, PlutusTx.UnsafeFromData, PlutusTx.FromData, 
                PlutusTx.ToData)

-- PlutusTx.unstableMakeIsData ''DatumStateData

-- | The datum of the network contained in the utxos at script address 
data NetworkDatum = Dead NetworkAttrbutes | Spawned NetworkAttrbutes | Initialized NetworkAttrbutes | Active NetworkAttrbutes  
    deriving Show

PlutusTx.unstableMakeIsData ''NetworkDatum

{-# INLINABLE networkDatum #-}
networkDatum :: Maybe Datum -> Maybe NetworkDatum 
networkDatum d = do  
    Datum d  <- d 
    PlutusTx.fromBuiltinData d

-- | The network update interactions
data NetworkRedeemer = Spawn (PubKeyHash, AssetClass) 
                     | InitializeNetwork (PubKeyHash, AssetClass) 
                     | ActivateNetwork (PubKeyHash, AssetClass) 
                     | InitializeReseacher (Address, AssetClass)
                     | MorphNetwork
    deriving Show 

PlutusTx.unstableMakeIsData ''NetworkRedeemer

{-# INLINABLE mkNetworkValidator #-}
-- | The network on chain Validator
mkNetworkValidator :: Address -> NetworkDatum -> NetworkRedeemer -> ScriptContext -> Bool
mkNetworkValidator rAddr d r ctx = 
    -- traceIfFalse "Payment Key Hash doesn't have tokens" $ checkAddressHasTokens rAddr   &&
    -- traceIfFalse "Not signed by authorized agent" (txSignedBy info (addressPkh rAddr))  &&
    case (d, r) of 
        (Dead attr, Spawn (pkh, spwnToken))                                                   -> 
            traceIfFalse "Not signed by the authorized pubkeyhash"  (txSignedBy info (addressPkh rAddr))                   &&
            traceIfFalse "Script doesn't contain tribute Ada"       (checkScriptValueAfterTx attr)                         &&
            traceIfFalse "Didn't Mint atleast one spawn token"      (checkTokenInScript spwnToken)                         &&
            traceIfFalse "Action count is beyond limit"             (checkCountDatum attr)                                 &&     -- This seems like a redundant check... Think about it
            traceIfFalse "Caller doen't have required token"        (checkInitInputsToken $ _conjureNetworkToken attr)     &&
            traceIfFalse "Datum Not updated with Network attr"      checkDataInOutDatum 
        (Spawned attr, InitializeNetwork (pkh, thothInitToken))          -> 
            traceIfFalse "Not signed by the valid pubkeyhash"                (txSignedBy info pkh)                             &&
            traceIfFalse "Spawn token not in input"                          (checkSpawnTokenInInput attr)                     &&
            traceIfFalse "Init Token not send to script"                     (checkTokenInScript thothInitToken)               &&
            traceIfFalse "Init Token hasn't been transfered to researcher"   (initTokenToInitResearcher attr thothInitToken)   &&
            traceIfFalse "Datum not updated after network initialization"    checkDatumForInitializationUpdate
        (Initialized attr, ActivateNetwork (pkh, activateToken))                   -> 
            -- TODO: Change the checks to reflect the action of 
            traceIfFalse "Not signed by the valid pubkeyhash"       (txSignedBy info pkh)                     &&
            traceIfFalse "There is no output to the current script" tokenOutputToScript                       && 
            traceIfFalse "Check if the datum has been set"          checkIfDatumIsSet                         &&
            traceIfFalse "Value is not coherent across tx"          (checkScriptValueCoherence attr)          &&    
            traceIfFalse "Active Token not send to script"          (checkTokenInScript activateToken)   
        (Active attr, InitializeReseacher (rAddr, reInitToken)) -> 
            traceIfFalse "Not signed by the valid pubkeyhash"       (txSignedBy info (addressPkh rAddr))            &&
            traceIfFalse "researcherToken not send to researcher"   (checkRInitTokenToResearcher rAddr reInitToken) &&
            traceIfFalse "researcherToken not in script"            (checkTokenInScript reInitToken)                &&
            traceIfFalse "researcher has already activated"         (checkResearcherAlreadyActive)                  &&
            traceIfFalse "Active token not in inputs"               (checkActiveTokenInInput attr)                  && 
            traceIfFalse "Active Tokens not increased!"             (checkActiveTokenAmtAft attr)
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

            ownOutput :: TxOut 
            ownOutput = case getContinuingOutputs ctx of 
                             [o] -> o 
                             _   -> traceError "exoected only one game output" 

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
                                                     

            checkScriptValueCoherence :: NetworkAttrbutes -> Bool 
            checkScriptValueCoherence nattr = valueLockedBy info (ownHash ctx) `geq` (lovelaceValueOf (_conjureTributeAmount nattr))

            checkBalanceAfterTx :: Bool 
            checkBalanceAfterTx = case getContinuingOutputs ctx of 
                                       os -> let value = foldl' (\total tributeVal -> total <> tributeVal) (lovelaceValueOf 0) $ txOutValue <$> os in 
                                                 value `geq` (lovelaceValueOf 1_000_000)

            thothTokenToResearcher :: Bool 
            thothTokenToResearcher = True -- assetClassValueOf (valuePaidTo info $ unPaymentPubKeyHash (pConstable prison)) (pToken prison) == 1  
            
            checkScriptValueAfterTx :: NetworkAttrbutes -> Bool 
            checkScriptValueAfterTx attr = valueLockedBy info (ownHash ctx) `geq` (lovelaceValueOf $ _conjureTributeAmount attr)   

            initTokenToInitResearcher :: NetworkAttrbutes -> AssetClass -> Bool 
            initTokenToInitResearcher attr initNToken = valuePaidTo info (addressPkh (_researcherZeroAddress attr)) `geq` assetClassValue initNToken 1 

            checkTokenInScript :: AssetClass -> Bool 
            checkTokenInScript token = any (\_ -> True) [val `geq` assetClassValue token 1 | (dh, val) <- scriptOwnOutputs]

            checkCountDatum :: NetworkAttrbutes ->  Bool 
            checkCountDatum attr = if _conjureActionCount attr > 1 then False else True       

            checkDataInOutDatum :: Bool 
            checkDataInOutDatum = True     

            outputDatumhash :: DatumHash 
            outputDatumhash = case txOutDatumHash ownOutput of 
                                Nothing -> traceError "datumHash missing"
                                Just dh -> dh

            checkDatumForInitializationUpdate :: Bool 
            checkDatumForInitializationUpdate = case networkDatum (findDatum outputDatumhash info) of
                                                     Nothing -> traceError "network datum not found!!"
                                                     Just d -> case d of
                                                                    Dead nattr ->   traceError "wrong datum data constructor" 
                                                                    Spawned nattr  -> traceError "network datum not found!!"
                                                                    Initialized nattr  -> case _initializeThothNetworkToken nattr of 
                                                                                               Nothing -> False 
                                                                                               Just initT -> True 
                                                                    Active bs         -> traceError "wrong datum data constructor" 
                                                                    

            checkInitInputsToken :: AssetClass -> Bool 
            checkInitInputsToken initNToken = (assetClassValueOf (txOutValue ownInput) initNToken >= 1)   

            checkSpawnTokenInInput :: NetworkAttrbutes -> Bool 
            checkSpawnTokenInInput attr = case _spawnNetworkToken attr of 
                                               Nothing -> traceError "error retrieving network attribute"
                                               Just spwToken -> any (\_ -> True) [\inp -> (checkCurrencySymbol inp spwToken) | inp <- transactionInputs]

                where 
                    checkCurrencySymbol :: TxInInfo -> AssetClass -> Bool
                    checkCurrencySymbol txIn spwT = case AssocMap.lookup (fst $ Value.unAssetClass spwT) (getValue $ txOutValue $ txInInfoResolved txIn) of 
                                                         Nothing   -> False 
                                                         Just oMap -> True 

            checkRInitTokenToResearcher :: Address -> AssetClass -> Bool 
            checkRInitTokenToResearcher addr initRToken = valuePaidTo info (addressPkh addr) `geq` assetClassValue initRToken 1  

            checkActiveTokenInInput :: NetworkAttrbutes -> Bool 
            checkActiveTokenInInput attr = case (_activateThothNetworkToken attr) of 
                                                Nothing          -> traceError "Couldn't find active token!"
                                                Just activeToken -> any (\_ -> True) [txOutValue (txInInfoResolved inp) `geq` assetClassValue activeToken 1 |inp <- transactionInputs]

            checkActiveTokenAmtAft :: NetworkAttrbutes -> Bool 
            checkActiveTokenAmtAft attr = case _activateThothNetworkToken attr of 
                                               Nothing -> traceError "Couldn't find active token in datum" 
                                               Just actToken -> case _activeResearchersTokens attr of 
                                                                Nothing -> True -- traceError "No researchers have created tokens"  TODO: This implies that the check is vacuous, think of ways to change that
                                                                Just tkAmt -> any (\_ -> True) [val `geq` assetClassValue actToken tkAmt | (dh, val) <- scriptOwnOutputs]

            checkResearcherAlreadyActive :: Bool 
            checkResearcherAlreadyActive = True 

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

activateNetworkTokenName :: TokenName 
activateNetworkTokenName = Value.tokenName "THOTH ACTIVE"

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
            nDat <- PlutusTx.fromBuiltinData dat 
            return (oref, o, nDat)
    where 
        f :: (TxOutRef ,ChainIndexTxOut) -> Bool 
        f ( _ , o) = assetClassValueOf (txOutValue $ toTxOut o) token >= 1

data ConjureNetworkParams = ConjureNetworkParams
    { conjuringResearcherAddress   :: !Address
    , conjureNetworkTributeAmount  :: !Integer
    , conjureNetworkTokenName      :: !TokenName
    , conjureNetworkDeadline       :: !POSIXTime
    } deriving (Show, Generic, FromJSON, ToJSON)

-- | The Contract for starting the Thoth network script
conjureThothNetwork :: forall w s. ConjureNetworkParams -> Contract (Last (Address, AssetClass)) s Text ()
conjureThothNetwork cnp = do 
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
                        { _researcherZeroAddress           = addr
                        , _thothNetworkScriptAddresss      = scriptAddress
                        , _conjureNetworkToken             = Value.assetClass conjCs conjTTn 
                        , _conjureTributeAmount            = conjureNetworkTributeAmount cnp
                        , _conjureActionCount              = 1
                        , _spawnNetworkToken               = Nothing 
                        , _initializeThothNetworkToken     = Nothing 
                        , _activateThothNetworkToken       = Nothing 
                        , _activateThothNetworkTokenAmount = Nothing
                        , _activeResearchersTokens         = Nothing   
                        }
                let tributeVal                = lovelaceValueOf $ conjureNetworkTributeAmount cnp
                    conjTVal                  = Value.singleton conjCs conjTTn conjTAmt
                    conjTSplit                = splitTokenVal conjTVal
                    conjTScriptSplit          = case splitTokenVal conjTVal of
                                                     Nothing -> Value.singleton conjCs conjTTn 1
                                                     Just s  -> fst s
                    conjTResearcherSplit      = case splitTokenVal conjTVal of
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

                utxos <- Map.filter (\o -> assetClassValueOf (txOutValue $ toTxOut o) (_conjureNetworkToken nattr) >= 1) <$> utxosAt scriptAddress

                case Map.toList utxos of 
                     [(conjToref, conjTo)] -> do 
                         Contract.logDebug @String $ printf "picked UTxo at" ++ (show conjToref) ++ "with value" ++ (show $ _ciTxOutValue conjTo)
                         attrOut <- findNetworkAttributesOutput (_conjureNetworkToken nattr) scriptAddress
                         case attrOut of 
                              Nothing       ->  throwError "Network attributes not found!"
                              Just (noref, no, ndat) -> case ndat of 
                                    Dead nattr'' -> do 
                                       logInfo @String "network has been conjured with datum"
                                       let conjTValue       = _ciTxOutValue conjTo
                                           -- TODO: Maybe change this amount if necessary!
                                           spawnTAmnt       = 2 
                                           spawnTTn         = Value.tokenName "Spawn Thoth"
                                           spawnTCs         = networkSpawnTokenCurSymbol scriptAddress conjToref spawnTTn spawnTAmnt
                                           spawnTPolicy     = networkSpawnTokenPolicy scriptAddress conjToref spawnTTn spawnTAmnt
                                           spawnTValue      = Value.singleton spawnTCs spawnTTn spawnTAmnt
                                           spawnTAssetClass = Value.assetClass spawnTCs spawnTTn 
                                           spawnTScriptSplit          = case splitSpawnToken spawnTValue of
                                                                           Nothing -> Value.singleton spawnTCs spawnTTn 1
                                                                           Just s  -> fst s
                                           spawnTResearcherSplit      = case splitSpawnToken spawnTValue of
                                                                           Nothing -> Value.singleton spawnTCs spawnTTn 1
                                                                           Just s  -> snd s
                                           spawnScriptPkg         = conjTValue <> spawnTScriptSplit
                                           nattr'                 = nattr'' & spawnNetworkToken .~ (Just spawnTAssetClass)
                                       let lookups'     = Constraints.typedValidatorLookups (typedNetworkValidator addr)          <>
                                                          Constraints.mintingPolicy spawnTPolicy                                  <>
                                                          Constraints.otherScript (networkValidator addr)                         <>
                                                          Constraints.unspentOutputs (Map.singleton conjToref conjTo)
                                           constraints' = Constraints.mustSpendScriptOutput conjToref (Redeemer $ PlutusTx.toBuiltinData (Spawn (unPaymentPubKeyHash pkh, spawnTAssetClass)))  <>
                                                          Constraints.mustPayToOtherScript valHash (Datum $ PlutusTx.toBuiltinData (Spawned nattr')) spawnScriptPkg                        <>
                                                          Constraints.mustMintValue spawnTValue
                                
                                       void $ adjustAndSubmitWith @ThothNetwork lookups' constraints'

                                       logInfo @String $ "Researcher zero has minted spawnToken: " ++ (show spawnTValue)
                                       logInfo @String $ "Updated datum with spawnToken: " ++ show (_spawnNetworkToken nattr')
                                       
                                    --    return (scriptAddress, spawnTAssetClass)
                                       tell $ Last $ Just (scriptAddress, spawnTAssetClass)
                                    

                        --  return scriptAddress
                

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

splitTokenVal :: Value -> Maybe (Value, Value)
splitTokenVal initVal = if not (Value.isZero initVal) then case flattenValue initVal of 
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


data NetworkInitializeParams = NetworkInitializeParams
    { initializeNetworkTokenName          :: !TokenName
    , initializeNetworkTokenInitialSupply :: !Integer
    , spawnNetworkAccessToken             :: !AssetClass
    , networkScriptAddress                :: !Address 
    , rZeroActivateAddress                :: !Address
    } deriving (Show, Generic, FromJSON, ToJSON)

-- | The Contract for Initializing the network 
initializeThothNetwork :: forall w s. NetworkInitializeParams -> Contract (Last (Address, AssetClass)) s Text ()
initializeThothNetwork nip = do 
    pkh <- Contract.ownPaymentPubKeyHash
    let scriptAddress  = networkScriptAddress nip 
        spawnNToken    = spawnNetworkAccessToken nip
        rZeroAddr      = rZeroActivateAddress nip
        initTTokenName = initializeNetworkTokenName nip
        initTTokenAmt  = initializeNetworkTokenInitialSupply nip 
    
    netOutput <- findNetworkAttributesOutput spawnNToken scriptAddress
    case netOutput of 
         Nothing -> throwError "Could not find network datum!!"
         Just (noref, no, ndat) -> case ndat of 
              Spawned nattr -> do 
                  logInfo @String "network has been spawned with datum!"
                  let spawnNetworkToken = case _spawnNetworkToken nattr of 
                                               Just st -> st
                                            --    Nothing -> throwError "Could not find spawn token in datum"
                  rUtxos <- Map.filter (\o -> assetClassValueOf (txOutValue $ toTxOut o) spawnNetworkToken >= 1) <$> utxosAt rZeroAddr    --fundsAtAddressGeq rZeroAddr (Ada.lovelaceValueOf 50_000_000)
                  sUtxos <- Map.filter (\o -> assetClassValueOf (txOutValue $ toTxOut o) spawnNetworkToken >= 1) <$> utxosAt scriptAddress
                  case Map.toList rUtxos of 
                    [(rAdaToref, rAdaTo)] -> do 
                         Contract.logDebug @String $ printf "picked UTxo at" ++ (show rAdaToref) ++ "with value" ++ (show $ _ciTxOutValue rAdaTo)
                         case Map.toList sUtxos of 
                            [(rSpwnToref, rSpwnTo)] -> do 
                                Contract.logDebug @String $ printf "picked UTxo at" ++ (show rSpwnToref) ++ "with value" ++ (show $ _ciTxOutValue rSpwnTo)
                                let initTokenCurSymbol   = networkInitializeTokenCurSym rZeroAddr nattr rAdaToref rSpwnToref initTTokenName initTTokenAmt
                                    initTPolicy          = networkInitializeTokenPolicy rZeroAddr nattr rAdaToref rSpwnToref initTTokenName initTTokenAmt
                                    initTokenVal         = Value.singleton initTokenCurSymbol initTTokenName initTTokenAmt
                                    initTAssetClass      = Value.assetClass initTokenCurSymbol initTTokenName 
                                    initTScriptSplit     = case splitTokenVal initTokenVal of
                                                                Nothing -> Value.singleton initTokenCurSymbol initTTokenName 1
                                                                Just s  -> fst s
                                    initTResearcherSplit = case splitTokenVal initTokenVal of
                                                                Nothing -> Value.singleton initTokenCurSymbol initTTokenName 1
                                                                Just s  -> snd s
                                    rAddr                = nattr ^. researcherZeroAddress
                                    nattr'               = nattr & initializeThothNetworkToken .~ (Just initTAssetClass)
                                    netValHash           = case _ciTxOutValidator rSpwnTo of 
                                                              Left vh -> vh
                                                              Right v -> validatorHash v
                                    spawnTScriptValue    = _ciTxOutValue rSpwnTo
                                    spawnTReValue        = _ciTxOutValue rAdaTo
                                    initResearcherPkg    = initTResearcherSplit <> spawnTReValue
                                    initScriptValpkg     = spawnTScriptValue <> initTScriptSplit
                                let lookups     = Constraints.typedValidatorLookups (typedNetworkValidator rZeroAddr)          <>
                                                  Constraints.mintingPolicy initTPolicy                                        <>
                                                  Constraints.otherScript (networkValidator rZeroAddr)                         <>
                                                  Constraints.unspentOutputs (Map.singleton rSpwnToref rSpwnTo)                <>
                                                  Constraints.unspentOutputs (Map.singleton rAdaToref rAdaTo)                  <>
                                                  Constraints.ownPaymentPubKeyHash pkh
                                    constraints = Constraints.mustMintValue initTokenVal                                                                                                           <>
                                                  Constraints.mustSpendPubKeyOutput rAdaToref                                                                                                      <>
                                                  Constraints.mustSpendScriptOutput rSpwnToref (Redeemer $ PlutusTx.toBuiltinData (InitializeNetwork (unPaymentPubKeyHash pkh, initTAssetClass)))  <>
                                                  Constraints.mustPayToOtherScript netValHash (Datum $ PlutusTx.toBuiltinData (Initialized nattr')) initScriptValpkg                               <>
                                                  Constraints.mustPayToPubKey pkh initResearcherPkg

                                adjustAndSubmitWith @ThothNetwork lookups constraints
                                logInfo @String $ "Researcher zero has minted initToken: " ++ (show initTokenVal)
                                tell $ Last $ Just (scriptAddress, initTAssetClass)

                    orefs  -> Contract.logError @String $ "Utxos not right!!" ++ show orefs  -- TODO: Better error message 
                    

data NetworkActivateParams = NetworkActivateParams
    { initNetworkAccessToken                      :: !AssetClass
    , activatenetworkScriptAddress                :: !Address 
    , activateRZeroActivateAddress                :: !Address
    } deriving (Show, Generic, FromJSON, ToJSON)

-- | The Contract for Activating the network 
activateThothNetwork :: forall w s. NetworkActivateParams -> Contract (Last (Address, AssetClass)) s Text ()   
activateThothNetwork nap = do 
    pkh <- Contract.ownPaymentPubKeyHash
    let scriptAddress  = activatenetworkScriptAddress nap 
        initNToken    = initNetworkAccessToken nap
        rZeroAddr      = activateRZeroActivateAddress nap
        activateTTokenName = activateNetworkTokenName

    netOutput <- findNetworkAttributesOutput initNToken scriptAddress 
    case netOutput of 
         Nothing -> traceError "Problem finding network datum!!"
         Just (noref, no, ndat) -> case ndat of 
              Initialized nattr ->  do 
                  logInfo @String "network has been initialized with datum!"
                  let initNetworkToken = case _initializeThothNetworkToken nattr of 
                                              Just st -> st
                      initialActivateTSupply = 1
                      activateTTokenAmt      = case nattr ^. activateThothNetworkTokenAmount of 
                                                    Nothing -> initialActivateTSupply
                  rUtxos <- Map.filter (\o -> assetClassValueOf (txOutValue $ toTxOut o) initNetworkToken >= 1) <$> utxosAt rZeroAddr
                  sUtxos <- Map.filter (\o -> assetClassValueOf (txOutValue $ toTxOut o) initNetworkToken >= 1) <$> utxosAt scriptAddress

                  case Map.toList rUtxos of 
                       [(rAdaToref, rAdaTo)] -> do 
                         Contract.logDebug @String $ printf "picked UTxo at" ++ (show rAdaToref) ++ "with value" ++ (show $ _ciTxOutValue rAdaTo)
                         case Map.toList sUtxos of 
                             [(rInitToref, rInitTo)] -> do 
                                Contract.logDebug @String $ printf "picked UTxo at" ++ (show rAdaToref) ++ "with value" ++ (show $ _ciTxOutValue rInitTo)
                                let activateTokenCurSym      = networkActivateTokenCurSym rZeroAddr nattr rAdaToref rInitToref activateTTokenName activateTTokenAmt
                                    activateTokenPolicy      = networkActivateTokenPolicy rZeroAddr nattr rAdaToref rInitToref activateTTokenName activateTTokenAmt
                                    activateTokenVal         = Value.singleton activateTokenCurSym activateTTokenName activateTTokenAmt
                                    activateTAssetClass      = Value.assetClass activateTokenCurSym activateTTokenName 
                                    netValHash               = case _ciTxOutValidator rInitTo of 
                                                                    Left vh -> vh
                                                                    Right v -> validatorHash v
                                    nattr'                   = nattr & activateThothNetworkToken .~ (Just activateTAssetClass)
                                    initTScriptValue         = _ciTxOutValue rInitTo
                                    -- initTReValue          = _ciTxOutValue rAdaTo
                                    activScriptValPkg        = activateTokenVal <> initTScriptValue

                                let lookups     = Constraints.typedValidatorLookups (typedNetworkValidator rZeroAddr)          <>
                                                  Constraints.mintingPolicy activateTokenPolicy                                <>
                                                  Constraints.otherScript (networkValidator rZeroAddr)                         <>
                                                  Constraints.unspentOutputs (Map.singleton rInitToref rInitTo)                <>
                                                  Constraints.unspentOutputs (Map.singleton rAdaToref rAdaTo)                  
                                    constraints = Constraints.mustMintValue activateTokenVal                                   <>
                                                  Constraints.mustSpendPubKeyOutput rAdaToref                                  <>
                                                  Constraints.mustSpendScriptOutput rInitToref (Redeemer $ PlutusTx.toBuiltinData (ActivateNetwork (unPaymentPubKeyHash pkh, activateTAssetClass)))  <>
                                                  Constraints.mustPayToOtherScript netValHash (Datum $ PlutusTx.toBuiltinData (Active nattr')) activScriptValPkg                               
                                                                                                   
                                adjustAndSubmitWith @ThothNetwork lookups constraints
                                logInfo @String $ "Researcher zero has minted activateToken: " ++ (show activateTokenVal)
                                tell $ Last $ Just (scriptAddress, activateTAssetClass)
                                

data ResearcherInitializeParams = ResearcherInitializeParams
    { activeNetworkAccessToken                   :: !AssetClass
    , activeNetworkScriptAddress                 :: !Address 
    , activatingResearcherAddress                :: !Address
    , activeResearcherTokenName                  :: !TokenName
    , activateResearcherDeadline                 :: !POSIXTime
    } deriving (Show, Generic, FromJSON, ToJSON)

-- | The Contract for Activating a researcher
initializeResearcher :: forall w s. ResearcherInitializeParams -> Contract (Last (AssetClass)) s Text ()   
initializeResearcher rip = do 
    pkh <- Contract.ownPaymentPubKeyHash
    let netScriptAddress = activeNetworkScriptAddress rip 
        researcherAddress = activatingResearcherAddress rip 
        activeReTokenName = activeResearcherTokenName rip 
        activNToken = activeNetworkAccessToken rip 
        activatDeadline = activateResearcherDeadline rip
        activeReTokenAmt = 2 

    netOutput <- findNetworkAttributesOutput activNToken netScriptAddress
    case netOutput of 
         Nothing -> traceError "Problem finding active network datum!!"
         Just (noref, no, ndat) -> case ndat of
                Active nattr -> do 
                    logInfo @String "Found active network attributes!"

                    rAdaUtxos <- fundsAtAddressGeq researcherAddress (Ada.lovelaceValueOf 5_000_000)
                    sUtxos <- Map.filter (\o -> assetClassValueOf (txOutValue $ toTxOut o) activNToken >= 1) <$> utxosAt netScriptAddress

                    case Map.toList rAdaUtxos of 
                       [(rAdaToref, rAdaTo)] -> do 
                            Contract.logDebug @String $ printf "picked UTxo at" ++ (show rAdaToref) ++ "with value" ++ (show $ _ciTxOutValue rAdaTo)
                            case Map.toList sUtxos of 
                                [(activeTknOref, activeTknO)] -> do 
                                    Contract.logDebug @String $ printf "picked UTxo at" ++ (show activeTknOref) ++ "with value" ++ (show $ _ciTxOutValue activeTknO)
                                    let activateReTokenCurSym      = researcherInitializeTokenCurSym researcherAddress rAdaToref activeTknOref activeReTokenName activeReTokenAmt activatDeadline
                                        activateReTokenPolicy      = researcherInitializeTokenPolicy researcherAddress rAdaToref activeTknOref activeReTokenName activeReTokenAmt activatDeadline
                                        activateReTokenVal         = Value.singleton activateReTokenCurSym activeReTokenName activeReTokenAmt
                                        activateReTAssetClass      = Value.assetClass activateReTokenCurSym activeReTokenName
                                        oldActiveResearchers       = case nattr ^. activeResearchersTokens of 
                                                                          Nothing       -> 1 
                                                                          Just reNumber -> reNumber
                                        nattr'                     = nattr & activeResearchersTokens .~ (Just (oldActiveResearchers + 1))
                                        -- nattr'                     = nattr & activeResearchersTokens .~ (Just (oldActiveResearchers + 1))
                                        activeReScriptSplit        = case splitTokenVal activateReTokenVal of
                                                                          Nothing -> Value.singleton activateReTokenCurSym activeReTokenName 1
                                                                          Just s  -> fst s
                                        activeReResearcherSplit        = case splitTokenVal activateReTokenVal of
                                                                          Nothing -> Value.singleton activateReTokenCurSym activeReTokenName 1
                                                                          Just s  -> snd s
                                        activeNTknVal              = _ciTxOutValue activeTknO
                                        activateReScriptValPkg     = activeNTknVal <> activeReScriptSplit
                                        rZeroAddr                  = nattr ^. researcherZeroAddress
                                        netValHash                 = case _ciTxOutValidator activeTknO of 
                                                                          Left vh -> vh
                                                                          Right v -> validatorHash v
                                    let lookups     = Constraints.typedValidatorLookups (typedNetworkValidator rZeroAddr)          <>
                                                      Constraints.mintingPolicy activateReTokenPolicy                              <>
                                                      Constraints.otherScript (networkValidator rZeroAddr)                         <>
                                                      Constraints.unspentOutputs (Map.singleton activeTknOref activeTknO)          <>
                                                      Constraints.unspentOutputs (Map.singleton rAdaToref rAdaTo)
                                        constraints = Constraints.mustMintValue activateReTokenVal                                   <>
                                                      Constraints.mustSpendPubKeyOutput rAdaToref                                    <>
                                                      Constraints.mustSpendScriptOutput activeTknOref (Redeemer $ PlutusTx.toBuiltinData (InitializeReseacher (researcherAddress, activateReTAssetClass)))  <>
                                                      Constraints.mustPayToOtherScript netValHash (Datum $ PlutusTx.toBuiltinData (Active nattr')) activateReScriptValPkg

                                    adjustAndSubmitWith @ThothNetwork lookups constraints
                                    logInfo @String $ "Initialized researcher with address: " ++ (show researcherAddress)
                                    logInfo @String $ "Researcher active with token: " ++ (show activateReTokenVal)
                                    tell $ Last $ Just (activateReTAssetClass)
                        
                       orefs  -> Contract.logError @String $ "Utxos not right!!" ++ show orefs  -- TODO: handle for multiple txOrefs try using head
                _ -> traceError "Didn't find appropriate datum"


type ThothNetworkSchema = 
        Endpoint "conjure" ConjureNetworkParams
    .\/ Endpoint "initialize" NetworkInitializeParams
    .\/ Endpoint "activate" NetworkActivateParams
    .\/ Endpoint "researcherInitialize" ResearcherInitializeParams

conjEndpoint :: Contract (Last (Address, AssetClass)) ThothNetworkSchema Text ()
conjEndpoint = awaitPromise $ endpoint @"conjure" $ \cnp -> do conjureThothNetwork cnp

initEndpoints :: Contract (Last (Address, AssetClass)) ThothNetworkSchema Text ()
initEndpoints = awaitPromise $ (initialize `select` conjure) 
    where
        conjure = endpoint @"conjure" $ \cnp -> do 
            conjureThothNetwork cnp

        initialize = endpoint @"initialize" $ \nip -> do 
            initializeThothNetwork nip 

activateEndpoint :: Contract (Last (Address, AssetClass)) ThothNetworkSchema Text ()
activateEndpoint = awaitPromise $ endpoint @"activate" $ \nap -> do activateThothNetwork nap

initializeResearcherEndpoint :: Contract (Last (AssetClass)) ThothNetworkSchema Text ()
initializeResearcherEndpoint = awaitPromise $ endpoint @"researcherInitialize" $ \rip -> do initializeResearcher rip

-- mkSchemaDefinitions ''ThothNetworkSchema

-- myToken :: KnownCurrency
-- myToken = KnownCurrency (ValidatorHash "f") "Token" (TokenName "T" :| [])

-- mkKnownCurrencies ['myToken]
        
