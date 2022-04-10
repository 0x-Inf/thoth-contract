{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module ThothTestSpec.ThothNetworkCoreTest
    ( testNetworkInit
    , tests
    ) where

import           Control.Monad              hiding (fmap)
import           Control.Lens
import           Control.Monad.Freer.Extras as Extras
import           Data.Default               (Default (..))
import           Data.Monoid                (Last (..))
import           Data.Text                  (Text)
import qualified Data.Map                   as Map
import           Data.IORef
import           System.Exit                (ExitCode (..))
import           Test.Tasty
import qualified Test.Tasty.HUnit           as HUnit
import           Ledger
import           Ledger.TimeSlot
import           Ledger.Value               as Value 
import           Ledger.Ada                 as Ada
import           Plutus.Contract            as Contract
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           Prelude                    (IO, Semigroup (..), Show (..))
import           Wallet.Emulator.Wallet
import           Plutus.Contract.Test
import           Plutus.Contract.Test.Coverage 
import           Plutus.Contract.Test.Coverage.ReportCoverage (writeCoverageReport)

import           ThothCore.ThothNetworkCore
import           Utils                      (unsafeTokenNameToHex)



tests :: TestTree 
tests = checkPredicateOptions
    myOptions 
    "network init trace"
    myPredicate
    networkInitTrace




-- testCoverage :: IO () 
-- testCoverage = do 
--     cref <- newCoverageRef
--     e <- try $ defaultMain $ chechPredicateOptionsCoverage
--         myOptions
--         "network init trace"
--         cref
--         myPredicate
--         myTrace
--     case e of 
--         Left (c :: ExitCode) -> do
--             putStrLn $ "Tasty Exited with: " ++ show c
--             report <- readCoverageRef cref
--             writeCoverageReport "NetworkInitTrace" networkCovIdx report 
--         Right () -> putStrLn $ "unexpected tasty result"

myOptions :: CheckOptions 
myOptions = defaultCheckOptions & emulatorConfig .~ emCfg

myPredicate :: TracePredicate 
myPredicate = 
    walletFundsChange w1 (Ada.lovelaceValueOf (-10_000_000))


emCfg :: EmulatorConfig
emCfg = EmulatorConfig (Left $  Map.fromList [(knownWallet 1, v)]) def def 
    where
        v :: Value 
        v = Ada.lovelaceValueOf 100_000_000


testNetworkInit :: IO () 
testNetworkInit = runEmulatorTraceIO' def emCfg $ networkInitTrace
        


networkAssetSymbol :: CurrencySymbol
networkAssetSymbol = "ff"

networkAssetToken :: TokenName
networkAssetToken = "THOTH"

initNetworkAssetTokenName :: TokenName
initNetworkAssetTokenName = "Init Me"

initNetworkParams :: BuiltinByteString
initNetworkParams = "Thoth one"


networkInitTrace :: EmulatorTrace ()
networkInitTrace = do 
    let w1 = knownWallet 1
        addr = mockWalletAddress w1
        tributeAmount        = 15_000_000
        networkTokenInitSupply = 100_000_000 -- TODO : this should be calculated to induce the whole 'hopping concurrency shnit'
        initDeadline = slotToEndPOSIXTime def 10
        initTokenAmount = 8


    let nip = NetworkInitParams 
                { rZeroAddress              = addr
                , initialNetworkParams      = show initNetworkParams
                , networkTributeAmount      = tributeAmount
                , initNetworkTokenName      = initNetworkAssetTokenName
                , initNetworkDeadline       = initDeadline
                , initNetworkTokenAmount    = initTokenAmount
                }

    let nap = NetworkActivateParams
                { networkTokenName          = networkAssetToken
                , networkTokenInitialSupply = networkTokenInitSupply
                , networkActiveDatum        = initNetworkParams
                , rZeroActivateAddress      = addr
                }

    h1 <- activateContractWallet w1 endpoints
    h2 <- activateContractWallet w1 endpoints
    
    callEndpoint @"init" h1 nip
    void $ Emulator.waitNSlots 6

    -- callEndpoint @"activate" h1 nap 
    -- void $ Emulator.waitNSlots 3




-- checkPredicateOptionsCoverage :: CheckOptions
--                                -> String 
--                                -> CoverageRef
--                                -> TracePredicate
--                                -> EmulatorTrace
--                                -> TestTree
-- checkPredicateOptionsCoverage options nm (CoverageRef ioref) predicate action = 
--     HUnit.testCaseSteps nm $ \step -> do 
--         checkPredicateInner options predicate action step (HUnit.assertBool nm) (\rep -> modifyIORef ioref (rep<>))                            