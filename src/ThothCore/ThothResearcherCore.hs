{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ThothCore.ThothResearcherCore
    (
    -- * endpoints 
      activateResearcherEndpoint
    , createResearcherPageEndpoint
    -- * Coverage Testing
    , researcherScriptCovIdx
    -- * researcher Activation parameters 
    , ActivateResearcherParams (..)
    , CreateResearcherPageParams (..)
    ) where

import           Control.Monad               hiding (fmap)
import           Control.Lens                hiding (contains, to)
import           Data.Aeson                  (FromJSON, ToJSON)
import qualified Data.Map                    as Map
import           Data.Maybe                  as Maybe
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
import qualified Plutus.V1.Ledger.Contexts   as V
import qualified Plutus.V2.Ledger.Api        as V2API
import           Prelude                     (Semigroup (..), Show (..), String, fromIntegral, div)
import qualified Prelude
import qualified PlutusPrelude               as PlPrelude
import qualified PlutusTx.Prelude            as PlTxPrelude
import qualified PlutusTx.Ratio              as PlRatio
import           Text.Printf                 (printf)

-- import           Utils                       (getCredentials, unsafeTokenNameToHex)

import           ThothCore.ThothNetworkCore
import qualified Plutus.V1.Ledger.Api as V2API
import qualified Plutus.V1.Ledger.Api as V2API


type InitResTokenTxOutRef = TxOutRef
type AdaTxOutRef = TxOutRef

{-# INLINABLE mkResearcherActivateTokenPolicy #-}
mkResearcherActivateTokenPolicy :: Address -> AdaTxOutRef -> InitResTokenTxOutRef -> TokenName -> Integer ->  () -> ScriptContext -> Bool
mkResearcherActivateTokenPolicy addr adaOref initReTknOref tn amt () ctx =
    traceIfFalse "Not signed by correct pkh"          (checkSignature addr)  &&
    traceIfFalse "Minted wrong amount"                checkMintedAmount      &&
    traceIfFalse "Researcher ada token not consumed"  hasResearcherAdaUtxo   &&
    traceIfFalse "Researcher Init token not consumed" hasResearcherInitToken

  where
      info :: TxInfo
      info = scriptContextTxInfo ctx

      addressPkh :: Address -> PubKeyHash
      addressPkh addr'' = case toPubKeyHash addr'' of
                                    Nothing   -> traceError "something went wrong with pkh retrieval"
                                    Just pkh' -> pkh'

      checkSignature :: Address -> Bool
      checkSignature addr' = txSignedBy info (addressPkh addr')

      checkMintedAmount :: Bool                                                           -- TODO: Can use currencyValue and ownSymbol to get expected mint amount; this will remove the need for passing it in
      checkMintedAmount = case flattenValue (txInfoMint info) of
                                 [(_, tn', amt')] -> tn' == tn && amt' == amt
                                 _                -> False

      hasResearcherInitToken :: Bool
      hasResearcherInitToken = V.spendsOutput info (txOutRefId initReTknOref) (txOutRefIdx initReTknOref)

      hasResearcherAdaUtxo :: Bool
      hasResearcherAdaUtxo = V.spendsOutput info (txOutRefId adaOref) (txOutRefIdx adaOref)

-- | The minting policy of the active researcher token 
researcherActivateTokenPolicy :: Address -> AdaTxOutRef -> InitResTokenTxOutRef -> TokenName -> Integer -> Scripts.MintingPolicy
researcherActivateTokenPolicy addr adaOref initTknOref tn amt = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \addr' adaOref' activeTknOref' tn' amt' -> Scripts.mkUntypedMintingPolicy $ mkResearcherActivateTokenPolicy addr' adaOref' activeTknOref' tn' amt' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode addr
    `PlutusTx.applyCode`
    PlutusTx.liftCode adaOref
    `PlutusTx.applyCode`
    PlutusTx.liftCode initTknOref
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn
    `PlutusTx.applyCode`
    PlutusTx.liftCode amt

researcherActivateTokenCurSym ::  Address -> AdaTxOutRef -> InitResTokenTxOutRef -> TokenName -> Integer -> CurrencySymbol
researcherActivateTokenCurSym addr adaOref activeTknOref tn = scriptCurrencySymbol . researcherActivateTokenPolicy addr adaOref activeTknOref tn


type ActionTokenTxOutRef = TxOutRef

{-# INLINABLE mkResearcherEffortTokenPolicy #-}
-- | The minting policy for the effort token to be given to the researcher after valid research actions. see note on Effort for more.
mkResearcherEffortTokenPolicy :: Address -> ActiveResTokenTxOutRef -> ActionTokenTxOutRef -> TokenName -> Integer ->  () -> ScriptContext -> Bool
mkResearcherEffortTokenPolicy addr actResTknOref resActionTknOref tn amt () ctx = True


researcherEffortTokenPolicy :: Address -> ActiveResTokenTxOutRef -> ActionTokenTxOutRef -> TokenName -> Integer ->  Scripts.MintingPolicy
researcherEffortTokenPolicy addr activeTknOref actionTknOref tn amt = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \addr' activeTknOref' actionTknOref' tn' amt' -> Scripts.mkUntypedMintingPolicy $ mkResearcherEffortTokenPolicy addr' activeTknOref' actionTknOref' tn' amt' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode addr
    `PlutusTx.applyCode`
    PlutusTx.liftCode activeTknOref
    `PlutusTx.applyCode`
    PlutusTx.liftCode actionTknOref
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn
    `PlutusTx.applyCode`
    PlutusTx.liftCode amt

researcherEffortTokenCurSym :: Address -> ActiveResTokenTxOutRef -> ActionTokenTxOutRef -> TokenName -> Integer -> CurrencySymbol
researcherEffortTokenCurSym addr activeTknOref actionTknOref tn = scriptCurrencySymbol . researcherEffortTokenPolicy addr activeTknOref actionTknOref tn

{- note  [Effort]

The concept of effort is based on the idea of returns in the collective formalism of the researcher script.
It is a universal way to acknowledge that a particular researcher has done the actions necessary to warrant them having 
possession of the value of the effort token which will be both reflected in the utxo set of their script address and also the 
datum of the relevant output. The semantics of the token is the value the researcher has brought to the thoth network. This value 
will also be reflected in the global state of the network. The value of the effort token together with the actions that a researcher 
does will be used in the calculation of the effort multiplier. 

-}

type ActiveResTokenTxOutRef = TxOutRef

{-# INLINABLE mkResearcherPageTokenPolicy #-}
mkResearcherPageTokenPolicy :: Address -> ActiveResTokenTxOutRef -> TokenName -> Integer ->  () -> ScriptContext -> Bool
mkResearcherPageTokenPolicy addr actResTknOref tn amt () ctx =
    traceIfFalse "Not signed by correct pkh"            (checkSignature addr)  &&
    traceIfFalse "Minted wrong amount"                  checkMintedAmount      &&
    traceIfFalse "Active researcher token not consumed" hasResearcherActiveToken
  where
      info :: TxInfo
      info = scriptContextTxInfo ctx

      addressPkh :: Address -> PubKeyHash
      addressPkh addr'' = case toPubKeyHash addr'' of
                                    Nothing   -> traceError "something went wrong with pkh retrieval"
                                    Just pkh' -> pkh'

      checkSignature :: Address -> Bool
      checkSignature addr' = txSignedBy info (addressPkh addr')

      checkMintedAmount :: Bool                                                           -- TODO: Can use currencyValue and ownSymbol to get expected mint amount; this will remove the need for passing it in
      checkMintedAmount = case flattenValue (txInfoMint info) of
                                 [(_, tn', amt')] -> tn' == tn && amt' == amt && amt' == 1

      hasResearcherActiveToken :: Bool
      hasResearcherActiveToken = V.spendsOutput info (txOutRefId actResTknOref) (txOutRefIdx actResTknOref)



-- | The minting policy for the token that identifies a researcher's page
researcherPageTokenPolicy :: Address ->  ActiveResTokenTxOutRef -> TokenName -> Integer -> Scripts.MintingPolicy
researcherPageTokenPolicy addr activeTknOref tn amt = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \addr' activeTknOref' tn' amt' -> Scripts.mkUntypedMintingPolicy $ mkResearcherPageTokenPolicy addr' activeTknOref' tn' amt' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode addr
    `PlutusTx.applyCode`
    PlutusTx.liftCode activeTknOref
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn
    `PlutusTx.applyCode`
    PlutusTx.liftCode amt

researcherPageTokenCurSym ::  Address -> ActiveResTokenTxOutRef -> TokenName -> Integer -> CurrencySymbol
researcherPageTokenCurSym addr activeTknOref tn = scriptCurrencySymbol . researcherPageTokenPolicy addr activeTknOref tn


-- | The state of 'knowledge' of a researcher 
data ResearcherState =
    ResearcherState
        { _researcherValidatorScriptAddress  :: Address
        -- ^ The address of the researcher script where knowledge representations will be stored 
        , _researcherOwnedAddress            :: Address
        -- ^ The address of the researcher used mainly for validation  
        , _researcherPkh                     :: PubKeyHash
        -- ^ The PublicKeyHash of the researcher
        , _researcherActiveToken             :: Maybe AssetClass
        -- ^ The token showing that the researcher is active
        , _researcherIdentifier              :: BuiltinByteString
        -- ^ This is supposed to be a unique identifier for the researcher in human readable format (not sure we should be storing it here!!)
        , _researcherEffortToken             :: Maybe AssetClass
        -- ^ The reward token for contributing knowledge.
        -- This takes the form of a MultiAsset (if possible) with every knowledge action making it up.
        , _researcherEfforValue              :: Maybe Value
        -- ^ The value of the reward the researcher gets on knowledge contribution.
        -- This is made up of multiple AssetClasses deconstructed into their cs tn and amt. 
        , _researcherEffortMultiplier        :: Maybe Integer
        -- ^ This is a proxy for the history of past contributions which affect how future contributions are calculated.
        -- This will follow the formalization of a collective from the category of Poly
        } deriving Show

makeLenses ''ResearcherState

PlutusTx.makeLift ''ResearcherState
PlutusTx.unstableMakeIsData ''ResearcherState


-- | The state of the researchers 'personal' page 
data PageState =
    PageState
        { pageIdentifierToken    :: AssetClass
        -- ^ The token that will be used in the identification of the page 
        , researcherAddress      :: Address
        -- ^ The address for the researcher who owns this page
        , researcherScripAddress :: Address
        -- ^ The address for the script that controls the researcher page 
         }deriving Show

-- makeLenses ''PageState

PlutusTx.makeLift ''PageState
PlutusTx.unstableMakeIsData ''PageState


-- | This data type represents the contents of a researchers page 
data PageEntry = Notes | Blog | Musings | Papers | Gallery | Code
    deriving Show

PlutusTx.makeLift ''PageEntry
PlutusTx.unstableMakeIsData ''PageEntry

-- | These are the actions a researcher can do when editing their page 
data EditPageOption = Add PageEntry | Remove PageEntry | Modify PageEntry | CreateEntry
    deriving Show

PlutusTx.makeLift ''EditPageOption
PlutusTx.unstableMakeIsData ''EditPageOption

-- | The 'high-level' actions a researcher can do on their page 
data PageOption = Create | Edit EditPageOption
    deriving Show

PlutusTx.makeLift ''PageOption
PlutusTx.unstableMakeIsData ''PageOption


data ResearcherDatum = Active (ResearcherState, Maybe PageState)
    deriving Show

PlutusTx.unstableMakeIsData ''ResearcherDatum


type InitializedToken = AssetClass
type ActivatedToken = AssetClass
type EffortToken = AssetClass

-- | These are the actions of the researcher on their own script address. 
--   They represent the semantics of the researcher in terms of knowledge creation and dissemination. 
data ResearcherRedeemer = ActivateResearcher (PubKeyHash, ActivatedToken, InitializedToken, Integer)
                        | Page (PageOption, PubKeyHash, EffortToken)
                        | Problems
                        | Conversations
                        | Funding
                        | Projects
                        | Resources
                        | Fellowships
                        | Collections
                        | Accreditation
                        | Bounties
    deriving Show

PlutusTx.unstableMakeIsData ''ResearcherRedeemer


{-# INLINABLE mkResearcherValidator #-}
-- | The researcher validator, controls how a researcher can spend tokens in their script address 
mkResearcherValidator :: Address -> ResearcherDatum -> ResearcherRedeemer -> ScriptContext -> Bool
mkResearcherValidator addr d r ctx =
    case (d,r) of
         (Active (resState, Nothing), _) ->
             traceIfFalse "researcher address not in datum" (addr == _researcherOwnedAddress resState)                 &&
             traceIfFalse "not signed by address pkh" (txSignedBy info (addressPkh addr))
         (Active (resState, Nothing), ActivateResearcher (pkh, actToken, initToken, contrAmt)) ->
             traceIfFalse "not signed by the correct pkh"                 (txSignedBy info pkh)                        &&
             traceIfFalse "signing pkh does not match provided address"   (pkh == addressPkh addr)                     &&
             traceIfFalse "pkh does not match datum record"               (pkh == (_researcherPkh resState))           &&
             traceIfFalse "activate token not transferred to researcher"  (activeTokenToResearcher pkh resState)       &&
             traceIfFalse "activate token not locked in script"           (checkTokenInScript actToken)                &&
             traceIfFalse "check if researcher has contributed some ada"  (checkAdaValueInScript contrAmt)
         (Active (resState, pageState), Page (pageOption, pkh, pageToken))                                        ->
             case pageOption of
                  Create ->
                      case pageState of
                           Nothing -> traceError "Page state not given"          -- TODO : mod this to only validate when there wasn't a previous pageState entry
                           Just pState ->
                               traceIfFalse "not signed by the correct pkh"                    (txSignedBy info pkh)                         &&
                               traceIfFalse "the validator address's pkh does not match"       (pkh == addressPkh addr)                      &&
                               traceIfFalse "active researcher token not present in inputs"    (activeTokenInInputs resState)                &&
                            --    traceIfFalse "Effort token not transefered to researcher"       (effortTokenToResearcher pkh resState)        &&
                               traceIfFalse "Page token has not been send to script"           (checkTokenInScript pageToken)
                            --    traceIfFalse "active token has not been returned to researcher" (activeTokenToResearcher pkh resState)

                  Edit editOption ->
                      case pageState of
                           Nothing -> traceError "Can't edit non-existent page state"
                           Just pState ->
                               True
         _               -> False


  where
      info :: TxInfo
      info = scriptContextTxInfo ctx

      ownInputs :: [TxInInfo]
      ownInputs = txInfoInputs info

      scriptOwnOutputs :: [(DatumHash, Value)]
      scriptOwnOutputs = scriptOutputsAt (ownHash ctx) info

      addressPkh :: Address -> PubKeyHash
      addressPkh addr'' = case toPubKeyHash addr'' of
                              Nothing   -> traceError "something went wrong with pkh retrieval"
                              Just pkh' -> pkh'

      activeTokenToResearcher :: PubKeyHash -> ResearcherState -> Bool
      activeTokenToResearcher pkh state = case _researcherActiveToken state of
                                               Nothing -> traceError "Couldn't find asset entry in datum state"
                                               Just actToken -> valuePaidTo info pkh `geq` assetClassValue (actToken) 1

      effortTokenToResearcher :: PubKeyHash -> ResearcherState -> Bool
      effortTokenToResearcher pkh state = case _researcherEffortToken state of
                                               Nothing -> traceError "Couldn't find asset entry in datum state"
                                               Just effToken -> valuePaidTo info pkh `geq` assetClassValue (effToken) 1

      checkTokenInScript :: AssetClass -> Bool
      checkTokenInScript token = any (\_ -> True) [val `geq` assetClassValue token 1 | (dh, val) <- scriptOwnOutputs]

      checkAdaValueInScript :: Integer -> Bool
      checkAdaValueInScript contrAmt' = valueLockedBy info (ownHash ctx) `geq` (lovelaceValueOf contrAmt')

      activeTokenInInputs :: ResearcherState -> Bool
      activeTokenInInputs state = case _researcherActiveToken state of
                                       Nothing -> traceError "could not find the asset"
                                       Just actToken -> any (\val -> val `geq` assetClassValue actToken 1) [txOutValue $ txInInfoResolved inp  | inp <- ownInputs]


{- note [The forbidden Fruit] 

Activating a researcher and getting a token can be seen as the equivalent of Eve eating the 
fruit from the tree of knowledge; only instead of 'getting cursed' the seed from the fruit will 
be the 'basis' for the knowledge this particular researcher will produce and also propagate. 
Without this seed (AssetClass) the researcher will not be able to meaningfully contribute to the 
network's state of knowledge. 

-}


data ThothResearcher
instance Scripts.ValidatorTypes ThothResearcher where
    type instance DatumType ThothResearcher = ResearcherDatum
    type instance RedeemerType ThothResearcher = ResearcherRedeemer


typedResearcherValidator :: Address -> Scripts.TypedValidator ThothResearcher
typedResearcherValidator addr = Scripts.mkTypedValidator @ThothResearcher
    ($$(PlutusTx.compile [|| mkResearcherValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode addr)
    $$(PlutusTx.compile [|| wrap ||])
  where
      wrap = Scripts.mkUntypedValidator @ResearcherDatum @ResearcherRedeemer


researcherValidator :: Address -> Validator
researcherValidator = Scripts.validatorScript . typedResearcherValidator

researcherValidatorHash :: Address -> ValidatorHash 
researcherValidatorHash = Scripts.validatorHash . typedResearcherValidator

researcherScriptAddress :: Address -> Ledger.Address
researcherScriptAddress = scriptHashAddress . researcherValidatorHash

researcherScriptCovIdx :: CoverageIndex
researcherScriptCovIdx = getCovIdx $$(PlutusTx.compile [|| mkResearcherValidator ||])

type ThothResearcherSchema =
            Endpoint "activate_researcher"    ActivateResearcherParams
        .\/ Endpoint "create_researcher_page" CreateResearcherPageParams

data ActivateResearcherParams =
    ActivateResearcherParams
        { researcherNickName              :: BuiltinByteString
        , researcherOwnAddress            :: Address
        , initializedResearcherToken      :: AssetClass
        , contribAmount                   :: Integer
        , activateResearcherTokenName     :: TokenName
        , activateResearcherTokenAmount   :: Integer
        -- ^ Must be at least 2 
        } deriving (Show, Generic, FromJSON, ToJSON)

activateResearcher :: forall w s . ActivateResearcherParams -> Contract (Last (AssetClass, Address)) s Text ()
activateResearcher ActivateResearcherParams{..} = do
    pkh <- Contract.ownPaymentPubKeyHash
    let researcherAddr    = researcherOwnAddress
        researcherId      = researcherNickName
        contrAmt          = contribAmount
        initAccessToken   = initializedResearcherToken
        activateTokenName = activateResearcherTokenName
        activateTokenAmt  = activateResearcherTokenAmount


    adaUtxos <- fundsAtAddressGeq researcherAddr (Ada.lovelaceValueOf contrAmt)
    case Map.toList adaUtxos of
         [(adaOref, adaO)] -> do
                Contract.logDebug @String $ printf "picked UTxo at" ++ (show adaOref) ++ "with value" ++ (show $ _ciTxOutValue adaO)
                let contribVal  = Ada.lovelaceValueOf contrAmt
                    resValAddr  = researcherScriptAddress researcherAddr
                    resValHash  = validatorHash $ researcherValidator researcherAddr
                    researcherState = ResearcherState
                            {  _researcherValidatorScriptAddress = resValAddr
                             , _researcherOwnedAddress           = researcherAddr
                             , _researcherPkh                    = unPaymentPubKeyHash pkh
                             , _researcherActiveToken            = Nothing
                             , _researcherIdentifier             = researcherId
                             , _researcherEffortToken            = Nothing
                             , _researcherEfforValue             = Nothing
                             , _researcherEffortMultiplier       = Nothing
                             }

                let lookups        = Constraints.typedValidatorLookups (typedResearcherValidator researcherAddr)       <>
                                     Constraints.plutusV1OtherScript (researcherValidator researcherAddr)                      <>
                                     Constraints.unspentOutputs (Map.singleton adaOref adaO)
                let constraints    = Constraints.mustSpendPubKeyOutput adaOref                                         <>
                                     Constraints.mustPayToOtherScript resValHash (Datum $ PlutusTx.toBuiltinData (Active (researcherState, Nothing))) contribVal
                _ <- adjustAndSubmitWith @ThothResearcher lookups constraints

                logInfo @String $ "The researcher has contributed: " ++ (show contribVal)
                now <- currentTime

                waitUnitlTimeHasPassed $ now + 5_000
                now1 <- currentTime

                logInfo @String $ "TIme now is: " ++ show now1


                initTokenUtxos <- Map.filter (\adaO -> assetClassValueOf (txOutValue $ toTxOut adaO) initAccessToken >= 1) <$> utxosAt researcherAddr
                case Map.toList initTokenUtxos of
                     [(initOref, initO)] -> do
                            Contract.logDebug @String $ printf "picked Utxo at" ++ (show initOref) ++ "with value" ++ (show $ _ciTxOutValue initO)

                            scritpAdaUtxos <- fundsAtAddressGeq resValAddr (Ada.lovelaceValueOf contrAmt)
                            case Map.toList scritpAdaUtxos of
                                [(scrAdaOref, scrAdaO)] -> do
                                        Contract.logDebug @String $ printf "picked UTxo at" ++ (show scrAdaOref) ++ "with value" ++ (show $ _ciTxOutValue scrAdaO)
                                        researcherDatum <- getResearcherDatum scrAdaO
                                        let activateResearcherTCurSym         = researcherActivateTokenCurSym researcherAddr scrAdaOref initOref activateTokenName activateTokenAmt
                                            activateResearcherTPolicy         = researcherActivateTokenPolicy researcherAddr scrAdaOref initOref activateTokenName activateTokenAmt
                                            activateResearcherTValue          = Value.singleton activateResearcherTCurSym activateTokenName activateTokenAmt
                                            activateResearcherTAssetClass     = Value.assetClass activateResearcherTCurSym activateTokenName
                                            researcherStateDatum              = case researcherDatum of
                                                                                     Active (resState, _ ) -> resState
                                            researcherStateDatum'             = researcherStateDatum & researcherActiveToken .~ (Just activateResearcherTAssetClass)
                                            activScriptSplit                  = case splitTokenVal activateResearcherTValue of
                                                                                    Nothing -> Value.singleton activateResearcherTCurSym activateTokenName 1
                                                                                    Just s  -> fst s
                                            activResearcherSplit              = case splitTokenVal activateResearcherTValue of
                                                                                    Nothing -> Value.singleton activateResearcherTCurSym activateTokenName 1
                                                                                    Just s  -> snd s
                                            scriptAdaValue                    = _ciTxOutValue scrAdaO
                                            scriptValPkg                      = activScriptSplit <> scriptAdaValue
                                        let lookups'       = Constraints.typedValidatorLookups (typedResearcherValidator researcherAddr)       <>
                                                             Constraints.plutusV1MintingPolicy activateResearcherTPolicy                               <>
                                                             Constraints.plutusV1OtherScript (researcherValidator researcherAddr)                      <>
                                                             Constraints.unspentOutputs (Map.singleton scrAdaOref scrAdaO)                     <>
                                                             Constraints.unspentOutputs (Map.singleton initOref initO)
                                            constraints'   = Constraints.mustMintValue activateResearcherTValue                                                                          <>
                                                             Constraints.mustSpendPubKeyOutput initOref                                                                                  <>
                                                             Constraints.mustPayToOtherScript resValHash (Datum $ PlutusTx.toBuiltinData (Active (researcherStateDatum', Nothing))) scriptValPkg          <>
                                                             Constraints.mustSpendScriptOutput scrAdaOref (Redeemer $ PlutusTx.toBuiltinData (ActivateResearcher ((unPaymentPubKeyHash pkh) ,activateResearcherTAssetClass, initAccessToken, contrAmt)))

                                        _ <- adjustAndSubmitWith @ThothResearcher lookups' constraints'

                                        logInfo @String $ "Minted researcher activate token with value: " ++ (show activateResearcherTValue)
                                        tell $ Last $ Just (activateResearcherTAssetClass, resValAddr)
                     initOrefs -> do Contract.logError @String $ "Utxo set not right!!" ++ show initOrefs
         adaInitOrefs -> do Contract.logError @String $ "Utxo set not right!!" ++ show adaInitOrefs


data CreateResearcherPageParams =
    CreateResearcherPageParams
        { researcherPageAddress       :: Address
        -- ^ This is the same as the address for the validator script of the researcher.
        , researcherOwnWalletAddress  :: Address
        -- ^ The address for the researcher
        , researcherPageTokenName     :: TokenName
        -- ^ The token name for the identifier token of the page.
        , researcherActiveAccessToken :: AssetClass
        -- ^ The token that asserts that the researcher has been activated
        } deriving (Show, Generic, FromJSON, ToJSON)

createResearcherPage :: forall w s . CreateResearcherPageParams -> Contract w s Text ()
createResearcherPage CreateResearcherPageParams{..} = do
    pkh <- Contract.ownPaymentPubKeyHash
    Contract.logInfo @String $ "Researcher with pkh: " ++ show pkh ++ " has started page creation"
    let pageAddress    = researcherPageAddress
        researcherAddr = researcherOwnWalletAddress
        pageTokenName  = researcherPageTokenName
        pageTokenAmt   = 1


    activeTokenUtxos <- Map.filter (\o -> assetClassValueOf (txOutValue $ toTxOut o) researcherActiveAccessToken >= 1) <$> utxosAt pageAddress
    case Map.toList activeTokenUtxos of
         [(actOref, actO)] -> do
                Contract.logDebug @String $ "picked Utxo at" ++ show actOref ++ "with value" ++ (show $ _ciTxOutValue actO)
                researcherDatum <- getResearcherDatum actO
                let resPageTokenCurSym             = researcherPageTokenCurSym researcherAddr actOref pageTokenName pageTokenAmt
                    resPageTokenMintPolicy         = researcherPageTokenPolicy researcherAddr actOref pageTokenName pageTokenAmt
                    resPageTokenValue              = Value.singleton resPageTokenCurSym pageTokenName pageTokenAmt
                    resPageTokenAssetClass         = Value.assetClass resPageTokenCurSym pageTokenName
                    researcherStateDatum           = case researcherDatum of
                                                        Active (resState, _ ) -> resState
                    --  researcherStateDatum'          = researcherStateDatum & researcherActiveToken .~ (Just activateResearcherTAssetClass)
                    pageState                      = PageState
                                                            { pageIdentifierToken    = resPageTokenAssetClass
                                                            , researcherAddress      = researcherAddr
                                                            , researcherScripAddress = pageAddress
                                                            }
                    pageState'                     = Just pageState
                    pageOption                     = Create
                    resValHash                     = fst  $ _ciTxOutValidator actO
                    valueFromScript                = _ciTxOutValue actO
                    scriptValPkg                   = valueFromScript <> resPageTokenValue
                let lookups     = Constraints.plutusV1MintingPolicy resPageTokenMintPolicy                                  <>
                                  Constraints.typedValidatorLookups (typedResearcherValidator researcherAddr)       <>
                                  Constraints.plutusV1OtherScript (researcherValidator researcherAddr)                      <>
                                  Constraints.unspentOutputs (Map.singleton actOref actO)
                    tx          = Constraints.mustMintValue  resPageTokenValue                                                                                                        <>
                                  Constraints.mustPayToOtherScript resValHash (Datum $ PlutusTx.toBuiltinData (Active (researcherStateDatum, pageState'))) scriptValPkg               <>
                                  Constraints.mustSpendScriptOutput actOref (Redeemer $ PlutusTx.toBuiltinData (Page (pageOption, (unPaymentPubKeyHash pkh), resPageTokenAssetClass)))

                _ <- adjustAndSubmitWith @ThothResearcher lookups tx
                logInfo @String $ "Minted researcher page ID token with value: " ++ (show resPageTokenValue)



         actOrefs          -> do Contract.logError  @String $ "Utxo set not right!!" ++ show actOrefs


getResearcherDatum :: ChainIndexTxOut -> Contract w s Text ResearcherDatum
getResearcherDatum o =
  case o of
      PublicKeyChainIndexTxOut {} ->
        throwError "no datum for a txout of a public key address"
      ScriptChainIndexTxOut { _ciTxOutScriptDatum } -> do
        pure $ Maybe.fromJust $ V2API.fromBuiltinData $ V2API.toBuiltinData $ Maybe.fromJust $ snd _ciTxOutScriptDatum
        -- getDatum dh
        -- pure $ Maybe.fromJust $ V2API.fromBuiltinData e

        -- maybe (throwError "datum hash wrong type")
        --       pure
        --       (PlutusTx.fromBuiltinData e)
--   where
--     getDatum :: DatumHash -> Contract w s Text Datum
--     getDatum dh =
--       datumFromHash dh >>= \case Nothing -> throwError "datum not found"
                                --  Just d  -> pure d

-- | TODO: You could move the getResearcherDatum to on-chain code with the new types in Plutus


activateResearcherEndpoint :: Contract (Last (AssetClass, Address)) ThothResearcherSchema Text ()
activateResearcherEndpoint = awaitPromise $ endpoint @"activate_researcher" $ \rap -> do activateResearcher rap


createResearcherPageEndpoint :: Contract () ThothResearcherSchema Text ()
createResearcherPageEndpoint = awaitPromise $ endpoint @"create_researcher_page" $ \crpp -> do createResearcherPage crpp