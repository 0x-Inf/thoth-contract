{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module ThothCoreTest
    ( testTokenMint
    , testThothOracle
    ) where

import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras as Extras
import           Data.Default               (Default (..))
import           Data.Monoid                (Last (..))
import           Data.Text                  (Text)
import qualified Data.Map                   as Map
import           Ledger
import           Ledger.TimeSlot
import           Ledger.Value               as Value 
import           Ledger.Ada                 as Ada
import           Plutus.Contract            as Contract
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           Prelude                    (IO, Semigroup (..), Show (..))
import           Wallet.Emulator.Wallet

import           ThothMain.ThothCore
import           Utils                      (unsafeTokenNameToHex)


testTokenMint :: IO ()
testTokenMint = runEmulatorTraceIO tokenTrace 

tokenTrace :: EmulatorTrace ()
tokenTrace = do 
    let w1 = knownWallet 1 
    void $ activateContractWallet w1 $ void $ mintThothToken @() @Empty TokenParams
        { tpToken   = "PROOF"
        , tpAmount  = 100_000_000_000_000_000
        , tpAddress = mockWalletAddress w1
        }

assetSymbol :: CurrencySymbol
assetSymbol = "ff"

assetToken :: TokenName
assetToken = "THOTH"


testThothOracle :: IO ()
testThothOracle = runEmulatorTraceIO' def emCfg $ thothOracleTrace
    where
        emCfg :: EmulatorConfig
        emCfg = EmulatorConfig (Left $ Map.fromList
            [ (knownWallet 1, v <> Value.singleton assetSymbol assetToken 100_000_000)
            , (knownWallet 2, v)
            , (knownWallet 3, v)
            ]) def def 
        
        v :: Value 
        v = Ada.lovelaceValueOf 1_000_000_000

thothOracleTrace :: EmulatorTrace ()
thothOracleTrace = do 
    let w1 = knownWallet 1
        w2 = knownWallet 2
        w3 = knownWallet 3

        bs  = "I am byte string!!"
        bs1 = "Yet another string"

    let top = ThothOracleParams
                { topFees    = 1_000_000
                , topSymbol  = assetSymbol
                , topToken   = assetToken
                , topAddress = mockWalletAddress w1
                }

    h1 <- activateContractWallet w1 $ void $ runThothOracle top 
    void $ Emulator.waitNSlots 1
    oracle <- getThothOracle h1

    callEndpoint @"sync" h1 bs
    void $ Emulator.waitNSlots 3

    callEndpoint @"sync" h1 bs1
    void $ Emulator.waitNSlots 3

    


  where
    getThothOracle :: ContractHandle (Last ThothOracle) ThothOracleSchema Text -> EmulatorTrace ThothOracle
    getThothOracle h = do
        l <- observableState h
        case l of 
            Last Nothing            -> Emulator.waitNSlots 1 >> getThothOracle h 
            Last (Just thothOracle) -> Extras.logInfo (show thothOracle) >> return thothOracle 



