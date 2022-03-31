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


data Network = Network
    { lastSyncAddress :: Address
    } deriving Show

-- | the datum state data type
data DatumStateData = DatumStateData
    { researcherPaymentPubKeyHash   :: [PaymentPubKeyHash]
    , networkScriptAddress          :: Address 
    , networkParams                 :: String
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord, OpenApi.ToSchema,
                PlutusTx.UnsafeFromData, PlutusTx.FromData, PlutusTx.ToData)

-- | The datum of the network contained in the utxos at script address 
data NetworkDatum = NetworkDatum (DatumStateData) | Dead 
    deriving Show

PlutusTx.unstableMakeIsData ''NetworkDatum

-- | The network update interactions
data NetworkRedeemer = InitNetwork PaymentPubKeyHash | InitReseacher | InitGuest | MorphNetwork
    deriving Show 

PlutusTx.unstableMakeIsData ''NetworkRedeemer


{-# INLINABLE mkNetworkValidator #-}
-- | The network on chain Validator
mkNetworkValidator :: Address -> NetworkDatum -> NetworkRedeemer -> ScriptContext -> Bool
mkNetworkValidator addr d r ctx = 
    traceIfFalse "Payment Key Hash doesn't have tokens" $ checkAddressHasTokens addr  &&
    traceIfFalse "Not signed by authorized agent" (txSignedBy info (addressPkh addr)) &&
    case (d, r) of 
        (Dead, InitNetwork pkh)                   -> 
            traceIfFalse "Not signed by the desginated one"         (txSignedBy info (getTheOnePkh))      &&
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
            tokenOutputToScript = length scriptOwnOutputs > 1


            checkIfDatumIsSet :: Bool
            checkIfDatumIsSet =  any f scriptOwnOutputs
                where 
                    f :: (DatumHash, Value) -> Bool
                    f (dh, _) = case findDatum dh info of 
                                     Nothing -> False 
                                     Just d  -> True
                                                     

            outputsToResearcherZero :: Bool 
            outputsToResearcherZero =  let value_researcher = pubKeyOutputsAt getTheOnePkh info 
                                                  in length value_researcher >= 1

            checkBalanceAfterTx :: Bool 
            checkBalanceAfterTx = getLovelace (adaLockedBy info (ownHash ctx)) >= 10_000_000

                -- utxos <- utxosAt addr'
                -- return any f utxos
                --     where
                --         f :: (TxOutRef, ChainIndexTxOut) -> Bool
                --         f (_, o) = getLovelace (fromValue (txOutValue $ toTxOut o)) >= toValue minAdaTxOut


data ThothNetworking
instance Scripts.ValidatorTypes ThothNetworking where 
    type instance DatumType ThothNetworking = NetworkDatum 
    type instance RedeemerType ThothNetworking = NetworkRedeemer

typedNetworkValidator :: Address -> Scripts.TypedValidator ThothNetworking
typedNetworkValidator addr = Scripts.mkTypedValidator @ThothNetworking
    ($$(PlutusTx.compile [|| mkNetworkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode addr)
    $$(PlutusTx.compile [|| wrap ||])
  where
      wrap = Scripts.wrapValidator @NetworkDatum @NetworkRedeemer


networkValidator :: Address -> Validator
networkValidator = Scripts.validatorScript . typedNetworkValidator

networkAddress :: Address -> Ledger.Address
networkAddress = scriptAddress . networkValidator

