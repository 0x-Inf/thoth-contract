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
    -- * Coverage Testing
    , researcherScriptCovIdx
    -- * researcher Activation parameters 
    , ActivateResearcherParams (..)
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
import           Ledger.Contexts             as V 
import           Prelude                     (Semigroup (..), Show (..), String, fromIntegral, div)
import qualified Prelude
import qualified PlutusPrelude               as PlPrelude
import qualified PlutusTx.Prelude            as PlTxPrelude
import qualified PlutusTx.Ratio              as PlRatio
import           Text.Printf                 (printf)

import           Utils                       (getCredentials, unsafeTokenNameToHex)

import           ThothCore.ThothNetworkCore


type InitResTokenTxOutRef = TxOutRef
type AdaTxOutRef = TxOutRef

{-# INLINABLE mkResearcherActivateTokenPolicy #-}
mkResearcherActivateTokenPolicy :: Address -> AdaTxOutRef -> InitResTokenTxOutRef -> TokenName -> Integer ->  () -> ScriptContext -> Bool 
mkResearcherActivateTokenPolicy addr adaOref initReTknOref tn amt () ctx = 
    traceIfFalse "Not signed by correct pkh"         (checkSignature addr)  &&
    traceIfFalse "Minted wrong amount"               checkMintedAmount      && 
    traceIfFalse "Researcher ada token not consumed" hasResearcherAdaUtxo   &&
    traceIfFalse "Active Network token not consumed" hasResearcherInitToken  

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
    $$(PlutusTx.compile [|| \addr' adaOref' activeTknOref' tn' amt' -> Scripts.wrapMintingPolicy $ mkResearcherActivateTokenPolicy addr' adaOref' activeTknOref' tn' amt' ||])
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

-- | The state of 'knowledge' of a researcher 
data ResearcherState = 
    ResearcherState
        { _researcherValidatorScriptAddress           :: Address
        -- ^ The address of the researcher script where knowledge representations will be stored 
        , _researcherOwnedAddress            :: Address
        -- ^ The address of the researcher used mainly for validation  
        , _researcherPkh                     :: PubKeyHash
        -- ^ The address of the researcher
        , _researcherActiveToken             :: Maybe AssetClass
        -- ^ The token showing that the researcher is active
        , _researcherIdentifier              :: BuiltinByteString
        -- ^ This is supposed to be a unique identifier for the researcher in human readable format
        , _researcherEffortToken             :: Maybe AssetClass
        -- ^ The reward token for contributing knowledge 
        , _researcherEffortMultiplier        :: Maybe Integer
        -- ^ This is a proxy for the history of past contributions which affect how future contributions are calculated.
        -- This will follow the formalization of a collective from the category of Poly
        } deriving Show 


makeLenses ''ResearcherState

PlutusTx.makeLift ''ResearcherState
PlutusTx.unstableMakeIsData ''ResearcherState

data ResearcherDatum = Active ResearcherState 
     deriving Show 

PlutusTx.unstableMakeIsData ''ResearcherDatum   

data PageOption = Create | Edit 
     deriving Show 

PlutusTx.makeLift ''PageOption
PlutusTx.unstableMakeIsData ''PageOption

type InitializedToken = AssetClass
type ActivatedToken = AssetClass

data ResearcherRedeemer = ActivateResearcher (PubKeyHash, ActivatedToken, InitializedToken, Integer)
                        | Page PageOption
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
         (Active resState, _) -> 
             traceIfFalse "researcher address not in datum" (addr == _researcherOwnedAddress resState)             &&
             traceIfFalse "not signed by address pkh" (txSignedBy info (addressPkh addr))               
         (Active resState, ActivateResearcher (pkh, actToken, initToken, contrAmt)) -> 
             traceIfFalse "not signed by the correct pkh"                 (txSignedBy info pkh)                    && 
             traceIfFalse "signing pkh does not match provided address"   (pkh == addressPkh addr)                 && 
             traceIfFalse "activate token not transferred to researcher"  (activeTokenToResearcher pkh resState)   &&
             traceIfFalse "activate token not locked in script"           (checkTokenInScript actToken)            && 
             traceIfFalse "check if researcher has contributed some ada"  (checkAdaValueInScript contrAmt)
         _               -> False 


  where 
      info :: TxInfo 
      info = scriptContextTxInfo ctx

      scriptOwnOutputs :: [(DatumHash, Value)]
      scriptOwnOutputs = scriptOutputsAt (ownHash ctx) info

      addressPkh :: Address -> PubKeyHash
      addressPkh addr'' = case toPubKeyHash addr'' of
                              Nothing   -> traceError "something went wrong with pkh retrieval"
                              Just pkh' -> pkh'
      
      activeTokenToResearcher :: PubKeyHash -> ResearcherState -> Bool 
      activeTokenToResearcher pkh state = case _researcherActiveToken state of 
                                               Nothing -> traceError ""
                                               Just actToken -> valuePaidTo info pkh `geq` assetClassValue (actToken) 1 

      checkTokenInScript :: AssetClass -> Bool 
      checkTokenInScript token = any (\_ -> True) [val `geq` assetClassValue token 1 | (dh, val) <- scriptOwnOutputs]

      checkAdaValueInScript :: Integer -> Bool 
      checkAdaValueInScript contrAmt' = valueLockedBy info (ownHash ctx) `geq` (lovelaceValueOf contrAmt')
      

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
      wrap = Scripts.wrapValidator @ResearcherDatum @ResearcherRedeemer


researcherValidator :: Address -> Validator
researcherValidator = Scripts.validatorScript . typedResearcherValidator

researcherScriptAddress :: Address -> Ledger.Address
researcherScriptAddress = scriptAddress . researcherValidator  

researcherScriptCovIdx :: CoverageIndex
researcherScriptCovIdx = getCovIdx $$(PlutusTx.compile [|| mkResearcherValidator ||])

type ThothResearcherSchema = 
        Endpoint "activate_researcher" ActivateResearcherParams

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

activateResearcher :: forall w s . ActivateResearcherParams -> Contract w s Text () 
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
                             , _researcherEffortMultiplier       = Nothing
                             }

                let lookups        = Constraints.typedValidatorLookups (typedResearcherValidator researcherAddr)       <> 
                                     Constraints.otherScript (researcherValidator researcherAddr)                      <>
                                     Constraints.unspentOutputs (Map.singleton adaOref adaO)                           
                let constraints    = Constraints.mustSpendPubKeyOutput adaOref                                         <>
                                     Constraints.mustPayToOtherScript resValHash (Datum $ PlutusTx.toBuiltinData (Active researcherState)) contribVal 
                adjustAndSubmitWith @ThothResearcher lookups constraints

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
                                                                                     Active resState -> resState
                                            -- researcherStateDatum              = case _ciTxOutDatum scrAdaO of 
                                            --                                          Right d -> case PlutusTx.fromBuiltinData (PlutusTx.toBuiltinData d) of 
                                            --                                                         --  Nothing -> do Contract.logError @String "Didn't get the datum!!"
                                            --                                                          Just reState -> reState
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
                                                             Constraints.mintingPolicy activateResearcherTPolicy                               <> 
                                                             Constraints.otherScript (researcherValidator researcherAddr)                      <>
                                                             Constraints.unspentOutputs (Map.singleton scrAdaOref scrAdaO)                     <>
                                                             Constraints.unspentOutputs (Map.singleton initOref initO) 
                                            constraints'   = Constraints.mustMintValue activateResearcherTValue                                                                          <>
                                                             Constraints.mustSpendPubKeyOutput initOref                                                                                  <> 
                                                             Constraints.mustPayToOtherScript resValHash (Datum $ PlutusTx.toBuiltinData (Active researcherStateDatum')) scriptValPkg          <>
                                                             Constraints.mustSpendScriptOutput scrAdaOref (Redeemer $ PlutusTx.toBuiltinData (ActivateResearcher ((unPaymentPubKeyHash pkh) ,activateResearcherTAssetClass, initAccessToken, contrAmt)))
                                        
                                        adjustAndSubmitWith @ThothResearcher lookups' constraints'

                                        logInfo @String $ "minted researcher activate token with value: " ++ (show activateResearcherTValue)
                     initOrefs -> do Contract.logError @String $ "Utxo set not right!!" ++ show initOrefs

getResearcherDatum :: ChainIndexTxOut -> Contract w s Text ResearcherDatum
getResearcherDatum o =
  case o of
      PublicKeyChainIndexTxOut {} ->
        throwError "no datum for a txout of a public key address"
      ScriptChainIndexTxOut { _ciTxOutDatum } -> do
        (Datum e) <- either getDatum pure _ciTxOutDatum
        maybe (throwError "datum hash wrong type")
              pure
              (PlutusTx.fromBuiltinData e)
  where
    getDatum :: DatumHash -> Contract w s Text Datum
    getDatum dh =
      datumFromHash dh >>= \case Nothing -> throwError "datum not found"
                                 Just d  -> pure d



activateResearcherEndpoint :: Contract () ThothResearcherSchema Text ()
activateResearcherEndpoint = awaitPromise $ endpoint @"activate_researcher" $ \rap -> do activateResearcher rap 