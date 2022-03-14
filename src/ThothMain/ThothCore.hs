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

module ThothMain.ThothCore
    ( tokenPolicy
    , tokenCurSymbol
    , mintThothToken
    , runThothOracle
    , TokenParams (..)
    , ThothOracle (..)
    , ThothOracleSchema
    , ThothOracleParams (..)
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
import           PlutusTx.Prelude            hiding (Semigroup(..), unless)
import           Ledger                      hiding (mint, singleton)
import qualified Ledger.Typed.Scripts        as Scripts
import           Ledger.Ada                  as Ada
import           Ledger.Value                as Value
import           Ledger.Constraints          as Constraints
import           Prelude                     (Semigroup (..), Show (..), String)
import qualified Prelude
import           Text.Printf                 (printf)

import           Utils                       (getCredentials, unsafeTokenNameToHex)


-- TODO: make the validator and policy for initializing the network




{-# INLINABLE mkTokenPolicy #-}
mkTokenPolicy :: TxOutRef -> TokenName -> Integer -> () -> ScriptContext -> Bool 
mkTokenPolicy oref tn amt () ctx = traceIfFalse "UTxo not consumed" hasUTxo             &&
                                   traceIfFalse "wrong amount minted" checkMintedAmount
    where 
        info :: TxInfo
        info = scriptContextTxInfo ctx 
        
        hasUTxo :: Bool 
        hasUTxo = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

        checkMintedAmount :: Bool 
        checkMintedAmount = case flattenValue (txInfoMint info) of 
            [(_, tn', amt')] -> tn' == tn && amt' == amt
            _                -> False


tokenPolicy :: TxOutRef -> TokenName -> Integer -> Scripts.MintingPolicy
tokenPolicy oref tn amt = mkMintingPolicyScript $ 
    $$(PlutusTx.compile [|| \oref' tn' amt' -> Scripts.wrapMintingPolicy $ mkTokenPolicy oref' tn' amt' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn
    `PlutusTx.applyCode`
    PlutusTx.liftCode amt

tokenCurSymbol :: TxOutRef -> TokenName -> Integer -> CurrencySymbol 
tokenCurSymbol oref tn = scriptCurrencySymbol . tokenPolicy oref tn 


data ThothOracle = ThothOracle 
    { oSymbol        :: !CurrencySymbol
    , oOperator      :: !PubKeyHash
    , oFee           :: !Integer
    , oAsset         :: !AssetClass
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord, OpenApi.ToSchema)

PlutusTx.makeLift ''ThothOracle

data ThothOracleRedeemer = Sync | Use | Create
    deriving Show 

PlutusTx.unstableMakeIsData ''ThothOracleRedeemer

{-# INLINABLE thothOracleTokenName#-}
thothOracleTokenName :: TokenName 
thothOracleTokenName = TokenName emptyByteString 

{-# INLINABLE thothOracleAsset #-}
thothOracleAsset :: ThothOracle -> AssetClass
thothOracleAsset oracle = AssetClass (oSymbol oracle, thothOracleTokenName)

{-# INLINABLE thothOracleDatum #-}
thothOracleDatum :: Maybe Datum -> Maybe BuiltinByteString 
thothOracleDatum d = do  
    Datum d  <- d 
    PlutusTx.fromBuiltinData d

{-# INLINABLE thothOracleValue #-}
thothOracleValue :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe Integer
thothOracleValue o f = do 
    dh      <- txOutDatum o 
    Datum d <- f dh 
    PlutusTx.fromBuiltinData d

{-# INLINABLE mkThothOracleValidator #-}
mkThothOracleValidator :: ThothOracle -> BuiltinByteString -> ThothOracleRedeemer -> ScriptContext -> Bool 
mkThothOracleValidator oracle bs r ctx = 
    traceIfFalse "token missing from input"  inputHasToken  &&
    traceIfFalse "token missing from output" outputHasToken &&
    case r of 
        Sync    -> traceIfFalse "invalid output datum" validOutputDatum         &&
                   traceIfFalse "public key has not linked"  True
        Use     -> traceIfFalse "fees not paid"  feesPaid                       &&
                   traceIfFalse "oracle value changed" (outputDatum == Just bs)
        Create  -> traceIfFalse "invalid output datum" validOutputDatum         && 
                   traceIfFalse "fees not paid"  feesPaid                       

  where
    info :: TxInfo 
    info = scriptContextTxInfo ctx 

    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
        Nothing -> traceError "oracle input missing"
        Just i  -> txInInfoResolved i

    inputHasToken :: Bool
    inputHasToken = assetClassValueOf (txOutValue ownInput) (thothOracleAsset oracle) == 1

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one oracle output" 

    outputHasToken :: Bool
    outputHasToken = assetClassValueOf (txOutValue ownOutput) (thothOracleAsset oracle) == 1

    outputDatumhash :: DatumHash 
    outputDatumhash = case txOutDatumHash ownOutput of 
                            Nothing -> traceError "datumHash missing"
                            Just dh -> dh

    outputDatum :: Maybe BuiltinByteString
    outputDatum = case thothOracleDatum (findDatum outputDatumhash info) of
                        Nothing -> traceError "oracle output datum not found"
                        Just d -> Just d
        -- oracleValue ownOutput (`findDatum` info)

    validOutputDatum :: Bool
    validOutputDatum = isJust outputDatum

    feesPaid :: Bool
    feesPaid =
      let
        inVal  = txOutValue ownInput
        outVal = txOutValue ownOutput
      in
        outVal `geq` (inVal <> Ada.lovelaceValueOf (oFee oracle))

data ThothOracling
instance Scripts.ValidatorTypes ThothOracling where 
    type instance DatumType ThothOracling = BuiltinByteString 
    type instance RedeemerType ThothOracling = ThothOracleRedeemer

typeThothOracleValidator :: ThothOracle -> Scripts.TypedValidator ThothOracling 
typeThothOracleValidator oracle = Scripts.mkTypedValidator @ThothOracling 
    ($$(PlutusTx.compile [|| mkThothOracleValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode oracle)
    $$(PlutusTx.compile [|| wrap ||])
  where 
    wrap = Scripts.wrapValidator @BuiltinByteString @ThothOracleRedeemer

thothOracleValidator :: ThothOracle -> Validator 
thothOracleValidator = Scripts.validatorScript . typeThothOracleValidator 

thothOracleAddress :: ThothOracle -> Ledger.Address
thothOracleAddress = scriptAddress . thothOracleValidator

data ThothOracleParams = ThothOracleParams
    { topFees    :: !Integer
    , topSymbol  :: !CurrencySymbol
    , topToken   :: !TokenName
    , topAddress :: !Address 
    } deriving (Show, Generic, FromJSON, ToJSON)



startThothOracle :: forall w s. ThothOracleParams -> Contract w s Text ThothOracle
startThothOracle top = do 
    pkh <- Contract.ownPaymentPubKeyHash
    osc <- mapError (pack . show) (mintContract pkh [(thothOracleTokenName, 1)] :: Contract w s CurrencyError OneShotCurrency)
    let cs  = Currency.currencySymbol osc 
        thothOracle = ThothOracle
            { oSymbol = cs
            , oOperator = unPaymentPubKeyHash pkh
            , oFee      = topFees top
            , oAsset    = AssetClass (topSymbol top, topToken top)
            }
    logInfo @String $ "started the THOTH oracle " ++ show thothOracle
    return thothOracle

syncThothOracle :: forall w s. ThothOracle -> BuiltinByteString -> Contract w s Text ()
syncThothOracle oracle bs = do 
    m <- findOracle oracle
    let c = Constraints.mustPayToTheScript bs $ assetClassValue (thothOracleAsset oracle) 1 <> lovelaceValueOf 2_000_000
    case m of 
        Nothing -> do 
            ledgerTx <- submitTxConstraints (typeThothOracleValidator oracle) c
            awaitTxConfirmed $ getCardanoTxId ledgerTx
            logInfo @String $ "set initial oracle value to " ++ show bs
        Just (oref, o, _) -> do 
            let lookups = Constraints.unspentOutputs (Map.singleton oref o)                   <>
                          Constraints.typedValidatorLookups (typeThothOracleValidator oracle) <>
                          Constraints.otherScript (thothOracleValidator oracle)
                tx      = c <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Sync)
            ledgerTx <- submitTxConstraintsWith @ThothOracling lookups tx
            awaitTxConfirmed $ getCardanoTxId ledgerTx
            -- void $ adjustAndSubmitWith @ThothOracling lookups tx
            logInfo @String $ "updated oracle value to " ++ show bs


findOracle :: forall w s. ThothOracle -> Contract w s Text (Maybe (TxOutRef, ChainIndexTxOut, BuiltinByteString))
findOracle oracle = do 
    utxos <- Map.filter f <$> utxosAt (thothOracleAddress oracle)
    return $ case Map.toList utxos of
        [(oref, o)] -> do 
            Datum d <- case _ciTxOutDatum o of
                                Left _  -> Nothing 
                                Right d -> Just d
            bs <- PlutusTx.fromBuiltinData d 
            return (oref, o, bs)
        _           -> Nothing
  where 
    f :: ChainIndexTxOut -> Bool 
    f o = assetClassValueOf (txOutValue $ toTxOut o) (thothOracleAsset oracle) == 1

type ThothOracleSchema = Endpoint "sync" BuiltinByteString

runThothOracle :: ThothOracleParams -> Contract (Last ThothOracle) ThothOracleSchema Text ()
runThothOracle top = do 
    thothOracle <- startThothOracle top 
    tell $ Last $ Just thothOracle
    go thothOracle
  where 
    go :: ThothOracle -> Contract (Last ThothOracle) ThothOracleSchema Text a 
    go thothOracle = do 
        awaitPromise $ endpoint @"sync" $ syncThothOracle thothOracle
        go thothOracle

    


data TokenParams = TokenParams
    { tpToken     :: !TokenName
    , tpAmount    :: !Integer
    , tpAddress   :: !Address
    } deriving (Prelude.Eq, Prelude.Ord, Generic, FromJSON, ToJSON, ToSchema, Show)

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
    Contract.logDebug @String $ printf "balanced: $s" $ show unsigned
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

mintThothToken :: TokenParams -> Contract w s Text CurrencySymbol
mintThothToken tp = do
    Contract.logDebug @String $ printf "started minting: " ++ show tp 
    let addr = tpAddress tp 
    case getCredentials addr of 
        Nothing      -> Contract.throwError $ pack $ printf "expected pubkey address, but got" ++ (show addr)
        Just (x, my) -> do
            oref <- getUnspentOutput
            o    <- fromJust <$> Contract.txOutFromRef oref 
            Contract.logDebug @String $ printf "picked UTxo at" ++ (show oref) ++ "with value" ++ (show $ _ciTxOutValue o)

            let tn          = tpToken tp
                amt         = tpAmount tp 
                cs          = tokenCurSymbol oref tn amt
                val         = Value.singleton cs tn amt
                c           = case my of 
                    Nothing -> Constraints.mustPayToPubKey x val 
                    Just y  -> Constraints.mustPayToPubKeyAddress x y val 
                lookups     = Constraints.mintingPolicy (tokenPolicy oref tn amt) <>
                              Constraints.unspentOutputs (Map.singleton oref o)
                constraints = Constraints.mustMintValue val          <>
                              Constraints.mustSpendPubKeyOutput oref <>
                              c

            void $ adjustAndSubmitWith @Void lookups constraints
            Contract.logInfo @String $ printf "minted " ++ (show val)
            return cs