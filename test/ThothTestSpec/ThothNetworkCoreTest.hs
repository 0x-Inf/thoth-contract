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
    walletFundsChange w1 (Ada.lovelaceValueOf (-15_000_000))
--     walletFundsChange w1 (assetClassValue initToken   5))
--     -- valueAtAddress scripAddress (\v -> if v `geq` Ada.lovelaceValueOf (-15_000_000))
--   where 
--       initToken :: AssetClass 
--       initToken = AssetClass (currency, conjureNetworkAssetTokenName)


emCfg :: EmulatorConfig
emCfg = EmulatorConfig (Left $  Map.fromList [(knownWallet 1, v), (knownWallet 2 , v), (knownWallet 3 , v)]) def def 
    where
        v :: Value 
        v = Ada.lovelaceValueOf 100_000_000


testNetworkInit :: IO () 
testNetworkInit = runEmulatorTraceIO' def emCfg $ networkInitTrace
        


networkAssetSymbol :: CurrencySymbol
networkAssetSymbol = "ff"

networkAssetToken :: TokenName
networkAssetToken = "THOTH"

conjureNetworkAssetTokenName :: TokenName
conjureNetworkAssetTokenName = "Conjure Thoth"

initializeNetworkAssetTokenName :: TokenName
initializeNetworkAssetTokenName = "Initialize Thoth"

activateNetworkAssetTokenName :: TokenName
activateNetworkAssetTokenName = "Thoth Activate"

activateResearcherAssetTokenName :: TokenName
activateResearcherAssetTokenName = "THOTH ONE"

initNetworkParams :: BuiltinByteString
initNetworkParams = "Thoth one"


networkInitTrace :: EmulatorTrace ()
networkInitTrace = do 
    let w1 = knownWallet 1
        w2 = knownWallet 2
        w3 = knownWallet 3
        addr = mockWalletAddress w1
        tributeAmount        = 15_000_000
        networkTokenInitSupply = 100_000_000 -- TODO : this should be calculated to induce the whole 'hopping concurrency shnit' in the off-chain code
        initDeadline = slotToEndPOSIXTime def 10
        initTokenAmount = 8


    let cnp = ConjureNetworkParams 
                { conjuringResearcherAddress   = addr
                , conjureNetworkTributeAmount  = tributeAmount
                , conjureNetworkTokenName      = conjureNetworkAssetTokenName
                , conjureNetworkDeadline       = initDeadline
                }

    h1 <- activateContractWallet w1 initEndpoints
    
    callEndpoint @"conjure" h1 cnp
    void $ Emulator.waitNSlots 10
    Last m <- observableState h1
    case m of 
         Nothing -> Extras.logError $ "Error conjuring network with params: " ++ show cnp 
         Just (scriptAddress, spwnToken) -> do 
             Extras.logError $ "Spawned network with token: " ++ show spwnToken

             let initTokenInitSupply = 2
                    
             let nip = NetworkInitializeParams
                        { initializeNetworkTokenName          = initializeNetworkAssetTokenName
                        , initializeNetworkTokenInitialSupply = initTokenInitSupply
                        , spawnNetworkAccessToken             = spwnToken
                        , networkScriptAddress                = scriptAddress 
                        , rZeroActivateAddress                = addr
                        }

             h2 <- activateContractWallet w1 initEndpoints
             callEndpoint @"initialize" h2 nip 
             void $ Emulator.waitNSlots 3

             Last m <- observableState h2 
             case m of 
                  Nothing -> Extras.logError $ "Error geting last of contract monad with call: " ++ show nip 
                  Just (scripAddress, initToken) -> do 
                      Extras.logError $ "Initialized network with token: " ++ show initToken

                      let nap = NetworkActivateParams
                                  { initNetworkAccessToken                 = initToken
                                  , activatenetworkScriptAddress           = scripAddress 
                                  , activateRZeroActivateAddress           = addr
                                  }

                      h3 <- activateContractWallet w1 activateEndpoint
                      callEndpoint @"activate" h3 nap
                      void $ Emulator.waitNSlots 2

                      Last m <- observableState h3 
                      case m of 
                           Nothing -> Extras.logError $ "Error getting output from contract instance with attr: " ++ show nap 
                           Just (scripAddress, activeToken) -> do 
                                Extras.logError $ "Activated network with token: " ++ show activeToken
                                let reAddr            = mockWalletAddress w2
                                    activateDeadline = slotToEndPOSIXTime def 10

                                let rap = ResearcherActivateParams
                                            { activeNetworkAccessToken    = activeToken
                                            , activeNetworkScriptAddress  = scripAddress
                                            , activatingResearcherAddress = reAddr
                                            , activeResearcherTokenName   = activateResearcherAssetTokenName
                                            , activateResearcherDeadline  = activateDeadline
                                            }
                                
                                h4 <- activateContractWallet w2 activateResearcherEndpoint
                                -- void $ Emulator.waitNSlots 5
                                callEndpoint @"researcherActivate" h4 rap 
                                void $ Emulator.waitNSlots 5

                                let reAddr2            = mockWalletAddress w3
                                    activateDeadline = slotToEndPOSIXTime def 10

                                let rap = ResearcherActivateParams
                                            { activeNetworkAccessToken    = activeToken
                                            , activeNetworkScriptAddress  = scripAddress
                                            , activatingResearcherAddress = reAddr2
                                            , activeResearcherTokenName   = activateResearcherAssetTokenName
                                            , activateResearcherDeadline  = activateDeadline
                                            }
                                
                                h5 <- activateContractWallet w3 activateResearcherEndpoint
                                -- void $ Emulator.waitNSlots 5
                                callEndpoint @"researcherActivate" h5 rap 
                                void $ Emulator.waitNSlots 5

                                -- let reAddr1            = mockWalletAddress w1
                                --     activateDeadline   = slotToEndPOSIXTime def 10

                                -- let rap1 = ResearcherActivateParams
                                --             { activeNetworkAccessToken    = activeToken
                                --             , activeNetworkScriptAddress  = scripAddress
                                --             , activatingResearcherAddress = reAddr1
                                --             , activeResearcherTokenName   = activateResearcherAssetTokenName
                                --             , activateResearcherDeadline  = activateDeadline
                                --             }
                                
                                -- h5 <- activateContractWallet w1 activateResearcherEndpoint
                                -- -- void $ Emulator.waitNSlots 5
                                -- callEndpoint @"researcherActivate" h5 rap1
                                -- void $ Emulator.waitNSlots 5





-- checkPredicateOptionsCoverage :: CheckOptions
--                                -> String 
--                                -> CoverageRef
--                                -> TracePredicate
--                                -> EmulatorTrace
--                                -> TestTree
-- checkPredicateOptionsCoverage options nm (CoverageRef ioref) predicate action = 
--     HUnit.testCaseSteps nm $ \step -> do 
--         checkPredicateInner options predicate action step (HUnit.assertBool nm) (\rep -> modifyIORef ioref (rep<>))                            