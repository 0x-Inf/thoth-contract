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


data Network = Network
    { lastSyncAddress :: Address
    } deriving Show

-- | The datum of the network contained in the utxos at script address 
data NetworkDatum = NetworkDatum 
    { researcherPaymentPubKeyHash  :: [PaymentPubKeyHash]
    , networkAddress               :: Address 
    , networkParams                :: String
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord, OpenApi.ToSchema) 

-- | The network update interactions
data NetworkRedeemer = InitNetwork | InitReseacher | InitGuest | MorphNetwork
    deriving Show 

PlutusTx.unstableMakeIsData ''NetworkRedeemer

-- | The network on chain Validator
{-# INLINABLE mkNetworkValidator #-}
mkNetworkValidator :: PaymentPubKeyHash -> NetworkDatum -> NetworkRedeemer -> ScriptContext -> Bool
mkNetworkValidator _ _ _ _ = True 