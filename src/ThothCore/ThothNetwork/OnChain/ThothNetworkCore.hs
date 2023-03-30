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
{-# LANGUAGE ImportQualifiedPost   #-}

module ThothCore.ThothNetwork.OnChain.ThothNetworkCore
    ( networkCoreValidator
    , networkCoreValidatorHash
    , networkCoreAddress
    , networkCoreScript
    , networkCoreScriptAsCbor
    , networkCoreScriptAsShortBs
    , apiNetworkCoreScript
    -- * Utility Functions
    , saveVal
    ) where 


import           Control.Lens                hiding (contains, to)
import qualified Data.ByteString.Lazy        as LB
import qualified Data.ByteString.Short       as SBS
import           Codec.Serialise             ( serialise )
import           Prelude                     (Show (..), IO)


import qualified PlutusTx
import           PlutusTx.Prelude            hiding (Semigroup(..), unless)
import           Ledger                      hiding (mint, singleton)
import           Ledger.Ada                  (lovelaceValueOf)
import           Cardano.Api.Shelley hiding  (Address, TxOut, Value)
import           Plutus.Script.Utils.V2.Typed.Scripts.Validators as Scripts
import           Plutus.V2.Ledger.Contexts as V2
import Ledger.Value

import qualified Utilities.Serialise       as Serialise 


-- | This represents the universal knowledge state of the network. This is a snapshot of 'all' knowledge.
data KnowledgeState = KnowledgeState
    { _networAddress                 :: Address
    -- ^ The network scriptAddress, used in finding where to spend and pay tokens, for instance when doing a knowledge sync.
    , _networkCoreTokenName          :: TokenName
    -- ^ The name of the the knowledge token.
    , _networkCoreThothToken         :: AssetClass
    -- ^ The networkCore token, this is mostly used to get the mintingPolicy for later mints, e.g when creating a knowledge entry
    , _networkCoreLockedTokenAmounts :: Integer
    -- ^ This is a crude measure of how much knowledge is in the network.
    } deriving Show

makeLenses ''KnowledgeState

PlutusTx.makeLift ''KnowledgeState

PlutusTx.unstableMakeIsData ''KnowledgeState

data PreState = PreState
    { _pledgedAdaTributeAmount    :: Integer
    , _hasMintedInitialThothToken :: Bool
    } deriving Show

makeLenses ''PreState

PlutusTx.makeLift ''PreState

PlutusTx.unstableMakeIsData ''PreState


-- | The datum carried by Utxos in the network core scriptAddress 
data NetworkDatum = Dormant PreState
                  -- ^ This is the 'state' of the network before the first thoth token mint. Basically after setting the scriptAddress.
                  | Active KnowledgeState
                  -- ^ This shows that the network is active and has tokens which can be used in the generation of knowledge.
    deriving Show 

PlutusTx.unstableMakeIsData ''NetworkDatum

-- | The network core update actions
data NetworkRedeemer = CreateKnowledge PubKeyHash
                     -- ^ This creates a knowledge page which is 'tagged' by the knowledge 'type' being created, could be a problem or an observation
                     -- or a work of art like music or a painting.
                     | SyncUniversalKnowledge PubKeyHash
                     -- ^ This is used to add the knowledge in a specific 'knowledge page' to the universal knowledge representation. This is like a proposal 
                     -- that requires full consensum of the knowledge network to be agreed upon, or could have different levels of 'surerity' depending on how
                     -- many 'backers'
                     | MintInitialThothTokens PubKeyHash
                     -- ^ This mints the initital thoth knowledge token distribition. This is what is used to provide the 'site' for storing the 
                     -- reference scripts. 
                     | MintAdditionalThothTokens PubKeyHash
                     -- ^ This is used to ensure that there are always Thoth Tokens at the script which are used to 'create knowledge'. This is a crude way to 
                     -- ensure concurrency.
    deriving Show

PlutusTx.unstableMakeIsData ''NetworkRedeemer

{-# INLINABLE mkNetworkCoreValidator #-}
-- | The network on chain Validator. Used to control the spending of tokens 'locked' in the network core script address
mkNetworkCoreValidator ::NetworkDatum -> NetworkRedeemer -> V2.ScriptContext -> Bool
mkNetworkCoreValidator dat red sc = 
    case (dat,red) of
        (Dormant _, CreateKnowledge _)                    -> False
        (Dormant _, SyncUniversalKnowledge _)             -> False
        (Dormant _, MintAdditionalThothTokens _)          -> False
        (Dormant preState, MintInitialThothTokens pkh)    -> 
            traceIfFalse "The transaction was not signed by the redeemer caller"  (V2.txSignedBy info pkh)                                                                          && 
            traceIfFalse "The script does not have ada to mint thoth tokens"      (checkPledgedTribute preState) && 
            traceIfFalse "The initial mint only happens once"                     (not (_hasMintedInitialThothToken preState))
        (Active _, CreateKnowledge _)           -> True
        (Active _, SyncUniversalKnowledge _)    -> True
        (Active _, MintInitialThothTokens _)    -> True
        (Active _, MintAdditionalThothTokens _) -> True
        where 
            info :: V2.TxInfo
            info = V2.scriptContextTxInfo sc

            checkPledgedTribute :: PreState -> Bool 
            checkPledgedTribute preState = V2.valueLockedBy info (V2.ownHash sc) `geq` lovelaceValueOf (_pledgedAdaTributeAmount preState)


data ThothNetwork
instance Scripts.ValidatorTypes ThothNetwork where 
    type instance DatumType ThothNetwork = NetworkDatum
    type instance RedeemerType ThothNetwork = NetworkRedeemer

typedNetworkCoreValidator :: Scripts.TypedValidator ThothNetwork
typedNetworkCoreValidator = Scripts.mkTypedValidator @ThothNetwork
    $$(PlutusTx.compile [|| mkNetworkCoreValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where 
    wrap = Scripts.mkUntypedValidator @NetworkDatum @NetworkRedeemer

-- typedNetworkCoreValidator :: Address -> Scripts.TypedValidator ThothNetwork
-- typedNetworkCoreValidator addr = Scripts.mkTypedValidator @ThothNetwork
--     ($$(PlutusTx.compile [|| mkNetworkCoreValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode addr)
--     $$(PlutusTx.compile [|| wrap ||])

networkCoreValidator :: Validator    
networkCoreValidator = Scripts.validatorScript  typedNetworkCoreValidator

networkCoreValidatorHash :: ValidatorHash
networkCoreValidatorHash = Scripts.validatorHash  typedNetworkCoreValidator

networkCoreAddress :: Ledger.Address
networkCoreAddress = scriptHashAddress  networkCoreValidatorHash

networkCoreScript :: Ledger.Script 
networkCoreScript = Ledger.unValidatorScript  networkCoreValidator

networkCoreScriptAsCbor :: LB.ByteString
networkCoreScriptAsCbor = serialise  networkCoreValidator

networkCoreScriptAsShortBs :: SBS.ShortByteString
networkCoreScriptAsShortBs = SBS.toShort . LB.toStrict $ serialise  networkCoreScript

apiNetworkCoreScript :: PlutusScript PlutusScriptV2
apiNetworkCoreScript = PlutusScriptSerialised networkCoreScriptAsShortBs



------------------------------------------------------------------------------------------------------------------
------------------------------------------ HELPER FUNCTIONS ------------------------------------------------------

saveVal :: IO () 
saveVal = Serialise.writeValidatorToFile "./assets/networkCore.plutus" networkCoreValidator