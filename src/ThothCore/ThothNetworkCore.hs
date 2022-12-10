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
    -- , conjEndpoint
    , activateEndpoint
    , initializeResearcherEndpoint
    -- * Coverage Testing
    , networkCovIdx
    -- * Utility Functions
    , splitTokenVal
    , adjustAndSubmitWith
    , waitUnitlTimeHasPassed
    -- * Script Functions 
    , networkScriptAsCbor
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
import qualified Data.ByteString.Lazy        as LB
import qualified Data.ByteString.Short       as SBS
import           Codec.Serialise             ( serialise )
import           Data.Void                   (Void)
import           GHC.Generics                (Generic)
import qualified Data.OpenApi.Schema         as OpenApi
import qualified Formatting
import           Plutus.Contract             as Contract
import           Plutus.Contract.Wallet      (getUnspentOutput)
import           Plutus.Contracts.Currency   as Currency
import           Plutus.V1.Ledger.Credential as Plutus
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
import           PlutusTx.Builtins.Internal  (BuiltinByteString (..))
import           Text.Printf                 (printf)
import           Cardano.Api                 (serialiseToRawBytesHex, deserialiseFromRawBytes, AsType (AsAssetName), PlutusScript, PlutusScriptV2)
import           Cardano.Api.Shelley hiding  (Address, TxOut, Value)

-- import           Utils                       (getCredentials, unsafeTokenNameToHex)



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
    $$(PlutusTx.compile [|| \oref' tn' amt' -> Scripts.mkUntypedMintingPolicy $ mkNetworkTokenPolicy oref' tn' amt' ||])
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
                                 [(_, tn', amt')] -> tn' == tn && amt' == amt  -- TODO: We could hard-code an amount number here to make sure that only two unique conjure tokens exists.
                                 _                -> False

      checkCorrectSignature :: Bool
      checkCorrectSignature = txSignedBy info $ unPaymentPubKeyHash pkh

      checkDeadlineReached :: Bool
      -- TODO: Make this check whether the deadline is contained in the infoValidRange in realBlockChain
      checkDeadlineReached = txInfoValidRange info `contains` to deadline


networkConjureTokenPolicy :: PaymentPubKeyHash ->  TxOutRef -> TokenName -> POSIXTime -> Integer -> Scripts.MintingPolicy
networkConjureTokenPolicy pkh oref tn deadline amt = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \pkh' oref' tn' deadline' amt' -> Scripts.mkUntypedMintingPolicy $ mkNetworkConjureTokenPolicy pkh' oref' tn' deadline' amt' ||])
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
-- | @mkNetworkSpawnTokenPolicy @scriptaddr@ @oref@ @tn@ @amt@ @()@ @ctx@ makes the validator for minting the Spawn Network token
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
                                 [(_, tn', amt')] -> tn' == tn && amt' == 2 -- see note [mkNetworkSpawnTokenPolicy]
                                 _                -> False

      checkCorrectSignature :: Bool
      checkCorrectSignature = True -- txSignedBy info $ unPaymentPubKeyHash pkh


networkSpawnTokenPolicy :: Address -> TxOutRef -> TokenName -> Integer -> Scripts.MintingPolicy
networkSpawnTokenPolicy sAddress oref tn amt = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \sAddress' oref' tn' amt' -> Scripts.mkUntypedMintingPolicy $ mkNetworkSpawnTokenPolicy sAddress' oref' tn' amt' ||])
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

{- note [mkNetworkSpawnTokenPolicy]
The amount of spawn Tokens minted is hardcoded to two since we want to ensure that the network can be initialized only once with the initializing 
party having one of the tokens and the network script having the other one. 
-}

type ResearcherTxOutRef = TxOutRef

type SpawnTokenTxOutRef = TxOutRef

{-# INLINABLE mkNetworkInitializeTokenPolicy #-}
-- | The validator for the initialize network token 
mkNetworkInitializeTokenPolicy :: Address -> NetworkAttributes -> ResearcherTxOutRef -> SpawnTokenTxOutRef -> TokenName -> Integer -> () -> ScriptContext -> Bool
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

networkInitializeTokenPolicy :: Address -> NetworkAttributes -> ResearcherTxOutRef -> SpawnTokenTxOutRef -> TokenName -> Integer -> Scripts.MintingPolicy
networkInitializeTokenPolicy addr nattr roref storef tn amt = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \addr' nattr' roref' storef' tn' amt' ->  Scripts.mkUntypedMintingPolicy $ mkNetworkInitializeTokenPolicy addr' nattr' roref' storef' tn' amt' ||])
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

networkInitializeTokenCurSym :: Address -> NetworkAttributes -> ResearcherTxOutRef -> SpawnTokenTxOutRef -> TokenName -> Integer -> CurrencySymbol
networkInitializeTokenCurSym addr nattr roref storef tn = scriptCurrencySymbol . networkInitializeTokenPolicy addr nattr roref storef tn

type InitTokenTxOutRef = TxOutRef

{-# INLINABLE mkNetworkActivateTokenPolicy #-}
-- | The validator for the initialize network token 
mkNetworkActivateTokenPolicy :: Address -> NetworkAttributes -> ResearcherTxOutRef -> InitTokenTxOutRef -> TokenName -> Integer -> () -> ScriptContext -> Bool
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

networkActivateTokenPolicy :: Address -> NetworkAttributes -> ResearcherTxOutRef -> InitTokenTxOutRef -> TokenName -> Integer -> Scripts.MintingPolicy
networkActivateTokenPolicy rAddr nattr rAdaToref initOref tn amt = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \addr' nattr' roref' initOref' tn' amt' ->  Scripts.mkUntypedMintingPolicy $ mkNetworkActivateTokenPolicy addr' nattr' roref' initOref' tn' amt' ||])
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


networkActivateTokenCurSym :: Address -> NetworkAttributes -> ResearcherTxOutRef -> InitTokenTxOutRef -> TokenName -> Integer -> CurrencySymbol
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
    $$(PlutusTx.compile [|| \addr' adaOref' activeTknOref' tn' amt' deadline' -> Scripts.mkUntypedMintingPolicy $ mkResearcherInitializeTokenPolicy addr' adaOref' activeTknOref' tn' amt' deadline' ||])
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
data NetworkAttributes = NetworkAttributes
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
and we could just remove some of them. This seems like it was an exercise in creating minting policies. So far we have 
 - conjureNetworkToken    : This token is given to the zeroth researcher after they give up a bit of their to the network script address during the 
                            first stage of initialization, the purpose of this token is that only this one researcher can be able to transition the
                            network to the next state. This token is also created at the network script address, since the token is unique this can 
                            ensure that there is only one valid Thoth network. The On-chain code currently does not enforce that there are only two 
                            of these tokens but that could (should) be changed. 
 - spawnNetworkToken      : The token created at the script address after researcher zero has interacted with the network, it's like an acknowledgement 
                            that indeed the network exists and that one can interact with it. To fully express the semantics of the network at this state
                            we need to ensure that we make it a unique token that lives only at the script address. This is also used in the next transition as 
                            the first true 'network token' to initialize the network. Q: What does it mean for the network to be initialized?
 - initializeNetworkToken : This is the first true contribution return to the researcher from the network. And it also signifies that the network is ready for 
                            business. To get this token the zeroth research has to have also in their possesion the spawn network token. This token also created 
                            'using' a combination of a researcher's spawn token and the network script's spawn token. Since there are many of such tokens, the researcher
                            may at their behest transfer them to any address they would like or put them up for sale. The amount minted also depends on how many lovelace
                            they used to conjure the network. 
 - activateNetworkToken   : This unique token contained only in the script address signifies that the network is fully active and can be used to initialize researchers who 
                            interact with the script. This token is created only when the network is in the 'initialized' state with the initializeNetworkToken(s) and by an address
                            that contains the initializeNetworkToken. This is also the token that's going to be iteratively minted to allow for many more researchers to be 
                            initialized without output 'race conditions'.
On further investigation these seem reasonable. Also should add counters for the tokens.
The way to implement a multiAsset token like structure is to use the fact that a Value can be a map of many currencySymbols with 
various tokenNames and Amounts in each. 
So at the final state of initialization, the scriptAddress will contain one value for the initialization set; which it does according to how the code 
works as of 4/8/2022 and the initial Researcher should have the same. 

-}

makeLenses ''NetworkAttributes

PlutusTx.makeLift ''NetworkAttributes

PlutusTx.unstableMakeIsData ''NetworkAttributes

-- | the datum state data type
data DatumStateData = DatumStateData
    { researcherPaymentPubKeyHash   :: [PubKeyHash]
    , datumNetworkAddress           :: Address
    , networkParams                 :: String
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord, OpenApi.ToSchema, PlutusTx.UnsafeFromData, PlutusTx.FromData,
                PlutusTx.ToData)

-- PlutusTx.unstableMakeIsData ''DatumStateData

-- | The datum of the network contained in the utxos at script address 
data NetworkDatum = Dead NetworkAttributes 
                  -- ^ Represents the datum of the network after researcher zero has created the conjure token and transferred it to the script address and to their address
                  --   The value at their address is like a sourvenier. Q: Can they use it to spawn another network (We'll that other script address has to also have the token)
                  | Spawned NetworkAttributes 
                  -- ^ This represents the state of network after the zeroth researcher has interacted with the network address for the first time. The network can only 
                  --   be in this state once during the lifetime of the network.
                  | Initialized NetworkAttributes 
                  -- ^ This corresponds to the 'state' in which the zeroth researcher has been 'rewarded' some tokens for initializing the network. From this state anyone with the
                  --   initialization tokens can proceed to activate a thoth network, it won't be the original but they can do it. Maybe this should be changed to only allow 
                  --   one address to activate the network. Also maybe enforce that all the initTokens in the script address are burned after network activation.
                  | Active NetworkAttributes
                  -- ^ This represents the fully active network state where new researchers (script addresses) can be created and other network operations can be done. For instance morphing 
                  --   the network script to represent a coherent 'knowledge state'.
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
                     | MorphNetwork (Address)
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
            traceIfFalse "Action count is beyond limit"             (checkCountDatum attr)                                 &&     -- This seems like a redundant check... Think about it.. because we never increase it automatically
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
            traceIfFalse "Datum has not been set"                   checkIfDatumIsSet                         &&
            traceIfFalse "Value is not coherent across tx"          (checkScriptValueCoherence attr)          &&
            traceIfFalse "Active Token not send to script"          (checkTokenInScript activateToken)
        (Active attr, InitializeReseacher (rDatAddr, reInitToken)) ->
            traceIfFalse "Not signed by the valid pubkeyhash"       (txSignedBy info (addressPkh rDatAddr))            &&
            traceIfFalse "researcherToken not send to researcher"   (checkRInitTokenToResearcher rDatAddr reInitToken) &&
            traceIfFalse "researcherToken not in script"            (checkTokenInScript reInitToken)                   &&
            traceIfFalse "researcher has already activated"         (checkResearcherAlreadyActive)                     &&
            traceIfFalse "Active token not in inputs"               (checkActiveTokenInInput attr)                     &&
            traceIfFalse "Active Tokens not increased!"             (checkActiveTokenAmtAft attr)
        (Active attr, MorphNetwork (addr))                          -> True
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
            checkAddressHasTokens addr' = ownInputValue `geq` (toValue minAdaTxOut <> singleton Value.adaSymbol Value.adaToken 5_000_000) -- addr' == txOutAddress ownInput && 

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


            checkScriptValueCoherence :: NetworkAttributes -> Bool
            checkScriptValueCoherence nattr = valueLockedBy info (ownHash ctx) `geq` (lovelaceValueOf (_conjureTributeAmount nattr))

            checkBalanceAfterTx :: Bool
            checkBalanceAfterTx = case getContinuingOutputs ctx of
                                       os -> let value = foldl' (\total tributeVal -> total <> tributeVal) (lovelaceValueOf 0) $ txOutValue <$> os in
                                                 value `geq` (lovelaceValueOf 1_000_000)

            thothTokenToResearcher :: Bool
            thothTokenToResearcher = True -- assetClassValueOf (valuePaidTo info $ unPaymentPubKeyHash (pConstable prison)) (pToken prison) == 1  

            checkScriptValueAfterTx :: NetworkAttributes -> Bool
            checkScriptValueAfterTx attr = valueLockedBy info (ownHash ctx) `geq` (lovelaceValueOf $ _conjureTributeAmount attr)

            initTokenToInitResearcher :: NetworkAttributes -> AssetClass -> Bool
            initTokenToInitResearcher attr initNToken = valuePaidTo info (addressPkh (_researcherZeroAddress attr)) `geq` assetClassValue initNToken 1

            checkTokenInScript :: AssetClass -> Bool
            checkTokenInScript token = any (\_ -> True) [val `geq` assetClassValue token 1 | (dh, val) <- scriptOwnOutputs]

            checkCountDatum :: NetworkAttributes ->  Bool
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

            checkSpawnTokenInInput :: NetworkAttributes -> Bool
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

            checkActiveTokenInInput :: NetworkAttributes -> Bool
            checkActiveTokenInInput attr = case (_activateThothNetworkToken attr) of
                                                Nothing          -> traceError "Couldn't find active token!"
                                                Just activeToken -> any (\_ -> True) [txOutValue (txInInfoResolved inp) `geq` assetClassValue activeToken 1 |inp <- transactionInputs]

            checkActiveTokenAmtAft :: NetworkAttributes -> Bool
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
      wrap = Scripts.mkUntypedValidator @NetworkDatum @NetworkRedeemer


networkValidator :: Address -> Validator
networkValidator = Scripts.validatorScript . typedNetworkValidator

networkValidatorHash :: Address -> ValidatorHash
networkValidatorHash = Scripts.validatorHash . typedNetworkValidator

networkAddress :: Address -> Ledger.Address
networkAddress = scriptHashAddress . networkValidatorHash

networkScript :: Address -> Ledger.Script 
networkScript = Ledger.unValidatorScript . networkValidator

networkScriptAsCbor :: Address -> LB.ByteString
networkScriptAsCbor = serialise . networkValidator

networkScriptAsShortBs :: Address -> SBS.ShortByteString
networkScriptAsShortBs = SBS.toShort . LB.toStrict . serialise . networkScript

apiNetworkScript :: Address -> PlutusScript PlutusScriptV2
apiNetworkScript = PlutusScriptSerialised . networkScriptAsShortBs

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

unsafeTokenNameToHex :: TokenName -> String
unsafeTokenNameToHex = BsChar8.unpack . serialiseToRawBytesHex . fromJust . deserialiseFromRawBytes AsAssetName . getByteString . unTokenName
  where
    getByteString (BuiltinByteString bs) = bs

getCredentials :: Address -> Maybe (PaymentPubKeyHash, Maybe StakePubKeyHash)
getCredentials (Address x y) = case x of
    ScriptCredential _   -> Nothing
    PubKeyCredential pkh ->
      let
        ppkh = PaymentPubKeyHash pkh
      in
        case y of
            Nothing                        -> Just (ppkh, Nothing)
            Just Plutus.StakingPtr {}      -> Nothing
            Just (StakingHash h)           -> case h of
                ScriptCredential _    -> Nothing
                PubKeyCredential pkh' -> Just (ppkh, Just $ StakePubKeyHash pkh')

adjustAndSubmitWith ::( PlutusTx.FromData  (Scripts.DatumType a)
                      , PlutusTx.ToData (Scripts.RedeemerType a)
                      , PlutusTx.ToData (Scripts.DatumType a)
                      , AsContractError e
                      )
                    => ScriptLookups a
                    -> TxConstraints (Scripts.RedeemerType a) (Scripts.DatumType a)
                    -> Contract w s e CardanoTx

adjustAndSubmitWith lookups constraints = do
    unbalanced <- mkTxConstraints lookups constraints
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

-- | The function that gives the datum at the script address for a certain token.
findNetworkAttributesOutput :: AssetClass -> Address -> Contract w s Text (Maybe (TxOutRef, ChainIndexTxOut, NetworkDatum))
findNetworkAttributesOutput token scriptAddress = do
        utxos <- utxosAt scriptAddress
        return $ do
            (oref, o) <- find f $ Map.toList utxos
            Datum dat <- snd $ _ciTxOutScriptDatum o
            nDat <- PlutusTx.fromBuiltinData dat
            return (oref, o, nDat)
    where
        f :: (TxOutRef ,ChainIndexTxOut) -> Bool
        f ( _ , o) = assetClassValueOf (txOutValue $ toTxOut o) token >= 1

-- | This function splits value of tokens for some to go to a researcher and other to the script address.
splitTokenVal :: Value -> Maybe (Value, Value)
splitTokenVal initVal = if not (Value.isZero initVal) then case flattenValue initVal of
                                                                 [(cs, tn, amt)] -> Just (shareTAmt amt cs tn)
                                                                 _               -> Nothing
                                                       else Nothing
    where
        shareTAmt :: Integer -> CurrencySymbol -> TokenName -> (Value, Value)
        shareTAmt amt cs tn = let split = fromIntegral amt `PlTxPrelude.divide` 2 in
                                  (Value.singleton cs tn split, Value.singleton cs tn split)

data ConjureNetworkParams = ConjureNetworkParams
    { conjuringResearcherAddress   :: !Address
    -- ^ This is the address for researcher zero, i.e the 'person' who gets to start all this.
    , conjureNetworkTributeAmount  :: !Integer
    -- ^ This is the amount of 'computational tokens' the zeroth researcher given the script address so as to start the start the thoth network.
    , conjureNetworkTokenName      :: !TokenName
    -- ^ This is the name researcher zero gives to the token used to conjure (out of 'nothing') the network.
    , conjureNetworkDeadline       :: !POSIXTime
    -- ^ This is the deadline the zeroth researcher gives themselves to finish starting the network (not sure if this is needed though)
    , spawnNetworkTokenAmount      :: !Integer
    -- ^ This is an amount that is checked to equal a certain amount when the network script is first involved in a transaction. It has to be 2.
    } deriving (Show, Generic, FromJSON, ToJSON)

-- | The Contract for starting the Thoth network script
conjureThothNetwork :: forall w s. ConjureNetworkParams -> Contract (Last (Address, AssetClass)) s Text ()
conjureThothNetwork cnp = do
    pkh <- Contract.ownFirstPaymentPubKeyHash
    now <- currentTime
    let mintInitDealine =  fromMilliSeconds $ (DiffMilliSeconds ((getPOSIXTime now) + (10 * 1000 * 60))) -- TODO: Change this to a value specified by the params
    logDebug @String $ printf "Init mint deadline has been set to: " ++ (show mintInitDealine)

    let addr     = conjuringResearcherAddress cnp
        conjTTn  = conjureNetworkTokenName cnp
        deadline = conjureNetworkDeadline cnp
        conjTributeAmt = conjureNetworkTributeAmount cnp
        conjTAmt = deriveConjureAmount conjTributeAmt


    utxos <- fundsAtAddressGeq (conjuringResearcherAddress cnp) (Ada.lovelaceValueOf 10_000_000)
    case Map.toList utxos of
         xs          -> do 
                Contract.logDebug @String $ printf  "Found more than one Utxo" ++ (show xs) ++ "at address: " ++ (show $ conjuringResearcherAddress cnp)
                let (oref, o) = head xs
                Contract.logDebug @String $ printf "picked UTxo at" ++ (show oref) ++ "with value" ++ (show $ _ciTxOutValue o)
                let conjCs  = networkConjureTokenCurSymbol pkh oref conjTTn mintInitDealine conjTAmt
                    conjTPolicy = networkConjureTokenPolicy pkh oref conjTTn mintInitDealine conjTAmt
                    scriptAddress = networkAddress addr
                    valHash       = validatorHash $ networkValidator addr
                let nattr = NetworkAttributes
                        { _researcherZeroAddress           = addr
                        , _thothNetworkScriptAddresss      = scriptAddress
                        , _conjureNetworkToken             = Value.assetClass conjCs conjTTn
                        , _conjureTributeAmount            = conjTributeAmt
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
                                                Constraints.plutusV1MintingPolicy conjTPolicy                                                           <>
                                                Constraints.plutusV1OtherScript (networkValidator addr)
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
                                           -- TODO: Maybe change this amount if necessary! i.e don't hardcode it in the off-chain...maybe put it in params
                                           spawnTAmnt       = spawnNetworkTokenAmount cnp
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
                                                          Constraints.plutusV1MintingPolicy spawnTPolicy                                  <>
                                                          Constraints.plutusV1OtherScript (networkValidator addr)                         <>
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
      f o = assetClassValueOf (txOutValue $ toTxOut o) (AssetClass (Value.adaSymbol, Value.adaToken)) >= 10

      deriveConjureAmount :: Integer -> Integer
      deriveConjureAmount conjTributeAmt' = conjTributeAmt' `div` 1_000_000

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
    pkh <- Contract.ownFirstPaymentPubKeyHash
    let scriptAddress  = networkScriptAddress nip
        spawnNToken    = spawnNetworkAccessToken nip
        rZeroAddr      = rZeroActivateAddress nip
        initTTokenName = initializeNetworkTokenName nip
        initTTokenAmt  = initializeNetworkTokenInitialSupply nip    -- This should be calculated using a contribution and returns formula.

    netOutput <- findNetworkAttributesOutput spawnNToken scriptAddress
    case netOutput of
         Nothing -> throwError "Could not find network datum!!"
         Just (noref, no, ndat) -> case ndat of
              Spawned nattr -> do
                  logInfo @String "network has been spawned with datum!"
                  let spawnNetworkToken = case _spawnNetworkToken nattr of
                                               Just st -> st
                                            --    Nothing -> throwError "Could not find spawn token in datum"
                      initTTokenCalulatedAmt = calculateInitializeNetworkTokenInitialSupply nattr
                  researcherUtxos <- Map.filter (\o -> assetClassValueOf (txOutValue $ toTxOut o) spawnNetworkToken >= 1) <$> utxosAt rZeroAddr    --fundsAtAddressGeq rZeroAddr (Ada.lovelaceValueOf 50_000_000)
                  -- Get outputs at the script address that contain the spawnNetworkToken
                  scriptUtxos <- Map.filter (\o -> assetClassValueOf (txOutValue $ toTxOut o) spawnNetworkToken >= 1) <$> utxosAt scriptAddress
                  case Map.toList researcherUtxos of
                    [(rAdaToref, rAdaTo)] -> do
                         Contract.logDebug @String $ printf "picked UTxo at" ++ (show rAdaToref) ++ "with value" ++ (show $ _ciTxOutValue rAdaTo)
                         case Map.toList scriptUtxos of
                            [(scriptSpwnToref, scriptSpwnTo)] -> do
                                Contract.logDebug @String $ printf "picked UTxo at" ++ (show scriptSpwnToref) ++ "with value" ++ (show $ _ciTxOutValue scriptSpwnTo)
                                let initTokenCurSymbol   = networkInitializeTokenCurSym rZeroAddr nattr rAdaToref scriptSpwnToref initTTokenName initTTokenCalulatedAmt
                                    initTPolicy          = networkInitializeTokenPolicy rZeroAddr nattr rAdaToref scriptSpwnToref initTTokenName initTTokenCalulatedAmt
                                    initTokenVal         = Value.singleton initTokenCurSymbol initTTokenName initTTokenCalulatedAmt
                                    initTAssetClass      = Value.assetClass initTokenCurSymbol initTTokenName
                                    initTScriptSplit     = case splitTokenVal initTokenVal of
                                                                Nothing -> Value.singleton initTokenCurSymbol initTTokenName 1
                                                                Just s  -> fst s
                                    initTResearcherSplit = case splitTokenVal initTokenVal of
                                                                Nothing -> Value.singleton initTokenCurSymbol initTTokenName 1
                                                                Just s  -> snd s
                                    rAddr                = nattr ^. researcherZeroAddress
                                    nattr'               = nattr & initializeThothNetworkToken .~ (Just initTAssetClass)
                                    netValHash           = fst (_ciTxOutValidator scriptSpwnTo)                                            
                                    spawnTScriptValue    = _ciTxOutValue scriptSpwnTo
                                    spawnTReValue        = _ciTxOutValue rAdaTo
                                    initResearcherPkg    = initTResearcherSplit <> spawnTReValue
                                    initScriptValpkg     = spawnTScriptValue <> initTScriptSplit
                                let lookups     = Constraints.typedValidatorLookups (typedNetworkValidator rZeroAddr)          <>
                                                  Constraints.plutusV1MintingPolicy initTPolicy                                <>
                                                  Constraints.plutusV1OtherScript (networkValidator rZeroAddr)                 <>
                                                  Constraints.unspentOutputs (Map.singleton scriptSpwnToref scriptSpwnTo)      <>
                                                  Constraints.unspentOutputs (Map.singleton rAdaToref rAdaTo)                  <>
                                                  Constraints.ownPaymentPubKeyHash pkh
                                    constraints = Constraints.mustMintValue initTokenVal                                                                                                           <>
                                                  Constraints.mustSpendPubKeyOutput rAdaToref                                                                                                      <>
                                                  Constraints.mustSpendScriptOutput scriptSpwnToref (Redeemer $ PlutusTx.toBuiltinData (InitializeNetwork (unPaymentPubKeyHash pkh, initTAssetClass)))  <>
                                                  Constraints.mustPayToOtherScript netValHash (Datum $ PlutusTx.toBuiltinData (Initialized nattr')) initScriptValpkg                               <>
                                                  Constraints.mustPayToPubKey pkh initResearcherPkg

                                adjustAndSubmitWith @ThothNetwork lookups constraints
                                logInfo @String $ "Researcher zero has minted initToken: " ++ (show initTokenVal)
                                tell $ Last $ Just (scriptAddress, initTAssetClass)

                    orefs  -> Contract.logError @String $ "Utxos not right!!" ++ show orefs  -- TODO: Better error message 
    where 
        calculateInitializeNetworkTokenInitialSupply :: NetworkAttributes -> Integer 
        calculateInitializeNetworkTokenInitialSupply nattr = (_conjureTributeAmount nattr) * initializationEffortMultiplier

          where
            initializationEffortMultiplier :: Integer 
            initializationEffortMultiplier = 2


data NetworkActivateParams = NetworkActivateParams
    { initNetworkAccessToken                      :: !AssetClass
    , activatenetworkScriptAddress                :: !Address
    , activateRZeroActivateAddress                :: !Address
    } deriving (Show, Generic, FromJSON, ToJSON)

-- | The Contract for Activating the network 
activateThothNetwork :: forall w s. NetworkActivateParams -> Contract (Last (Address, AssetClass)) s Text ()
activateThothNetwork nap = do
    pkh <- Contract.ownFirstPaymentPubKeyHash
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
                  researcherUtxos <- Map.filter (\o -> assetClassValueOf (txOutValue $ toTxOut o) initNetworkToken >= 1) <$> utxosAt rZeroAddr
                  scriptUtxos <- Map.filter (\o -> assetClassValueOf (txOutValue $ toTxOut o) initNetworkToken >= 1) <$> utxosAt scriptAddress

                  case Map.toList researcherUtxos of
                       [(rAdaToref, rAdaTo)] -> do
                         Contract.logDebug @String $ printf "picked UTxo at" ++ (show rAdaToref) ++ "with value" ++ (show $ _ciTxOutValue rAdaTo)
                         case Map.toList scriptUtxos of
                             [(scriptInitToref, scriptInitTo)] -> do
                                Contract.logDebug @String $ printf "picked UTxo at" ++ (show rAdaToref) ++ "with value" ++ (show $ _ciTxOutValue scriptInitTo)
                                let activateTokenCurSym      = networkActivateTokenCurSym rZeroAddr nattr rAdaToref scriptInitToref activateTTokenName activateTTokenAmt
                                    activateTokenPolicy      = networkActivateTokenPolicy rZeroAddr nattr rAdaToref scriptInitToref activateTTokenName activateTTokenAmt
                                    activateTokenVal         = Value.singleton activateTokenCurSym activateTTokenName activateTTokenAmt
                                    activateTAssetClass      = Value.assetClass activateTokenCurSym activateTTokenName
                                    netValHash               = fst (_ciTxOutValidator scriptInitTo)
                                    nattr'                   = nattr & activateThothNetworkToken .~ (Just activateTAssetClass)
                                    initTScriptValue         = _ciTxOutValue scriptInitTo
                                    -- initTReValue          = _ciTxOutValue rAdaTo
                                    activScriptValPkg        = activateTokenVal <> initTScriptValue

                                let lookups     = Constraints.typedValidatorLookups (typedNetworkValidator rZeroAddr)          <>
                                                  Constraints.plutusV1MintingPolicy activateTokenPolicy                                <>
                                                  Constraints.plutusV1OtherScript (networkValidator rZeroAddr)                         <>
                                                  Constraints.unspentOutputs (Map.singleton scriptInitToref scriptInitTo)                <>
                                                  Constraints.unspentOutputs (Map.singleton rAdaToref rAdaTo)
                                    constraints = Constraints.mustMintValue activateTokenVal                                   <>
                                                  Constraints.mustSpendPubKeyOutput rAdaToref                                  <>
                                                  Constraints.mustSpendScriptOutput scriptInitToref (Redeemer $ PlutusTx.toBuiltinData (ActivateNetwork (unPaymentPubKeyHash pkh, activateTAssetClass)))  <>
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
    pkh <- Contract.ownFirstPaymentPubKeyHash
    let netScriptAddress = activeNetworkScriptAddress rip
        researcherAddress = activatingResearcherAddress rip
        activeReTokenName = activeResearcherTokenName rip
        activNToken = activeNetworkAccessToken rip
        activateDeadline = activateResearcherDeadline rip
        activeReTokenAmt = 2

    netOutput <- findNetworkAttributesOutput activNToken netScriptAddress
    case netOutput of
         Nothing -> traceError "Problem finding active network datum!!"
         Just (noref, no, ndat) -> case ndat of
                Active nattr -> do
                    logInfo @String "Found active network attributes!"

                    rAdaUtxos <- fundsAtAddressGeq researcherAddress (Ada.lovelaceValueOf 5_000_000)
                    scriptUtxos <- Map.filter (\o -> assetClassValueOf (txOutValue $ toTxOut o) activNToken >= 1) <$> utxosAt netScriptAddress

                    case Map.toList rAdaUtxos of
                       [(rAdaToref, rAdaTo)] -> do
                            Contract.logDebug @String $ printf "picked UTxo at" ++ (show rAdaToref) ++ "with value" ++ (show $ _ciTxOutValue rAdaTo)
                            case Map.toList scriptUtxos of
                                [(activeTknOref, activeTknO)] -> do
                                    Contract.logDebug @String $ printf "picked UTxo at" ++ (show activeTknOref) ++ "with value" ++ (show $ _ciTxOutValue activeTknO)
                                    let activateReTokenCurSym      = researcherInitializeTokenCurSym researcherAddress rAdaToref activeTknOref activeReTokenName activeReTokenAmt activateDeadline
                                        activateReTokenPolicy      = researcherInitializeTokenPolicy researcherAddress rAdaToref activeTknOref activeReTokenName activeReTokenAmt activateDeadline
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
                                        activeReResearcherSplit    = case splitTokenVal activateReTokenVal of
                                                                          Nothing -> Value.singleton activateReTokenCurSym activeReTokenName 1
                                                                          Just s  -> snd s
                                        activeNTknVal              = _ciTxOutValue activeTknO
                                        adaFromResearcherVal       = _ciTxOutValue rAdaTo
                                        activateReScriptValPkg     = activeNTknVal <> activeReScriptSplit
                                        activateReResearcherValPkg = adaFromResearcherVal <> activeReResearcherSplit
                                        rZeroAddr                  = nattr ^. researcherZeroAddress
                                        netValHash                 = fst (_ciTxOutValidator activeTknO)                                                                          
                                    let lookups     = Constraints.typedValidatorLookups (typedNetworkValidator rZeroAddr)          <>
                                                      Constraints.plutusV1MintingPolicy activateReTokenPolicy                              <>
                                                      Constraints.plutusV1OtherScript (networkValidator rZeroAddr)                         <>
                                                      Constraints.unspentOutputs (Map.singleton activeTknOref activeTknO)          <>
                                                      Constraints.unspentOutputs (Map.singleton rAdaToref rAdaTo)
                                        constraints = Constraints.mustMintValue activateReTokenVal                                   <>
                                                      Constraints.mustSpendPubKeyOutput rAdaToref                                    <>
                                                      Constraints.mustSpendScriptOutput activeTknOref (Redeemer $ PlutusTx.toBuiltinData (InitializeReseacher (researcherAddress, activateReTAssetClass)))  <>
                                                    --   Constraints.mustPayToPubKey pkh activateReResearcherValPkg                                                                                               <>
                                                      Constraints.mustPayToOtherScript netValHash (Datum $ PlutusTx.toBuiltinData (Active nattr')) activateReScriptValPkg

                                    _ <- adjustAndSubmitWith @ThothNetwork lookups constraints
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

-- conjEndpoint :: Contract (Last (Address, AssetClass)) ThothNetworkSchema Text ()
-- conjEndpoint = awaitPromise $ endpoint @"conjure" $ \cnp -> do conjureThothNetwork cnp

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

