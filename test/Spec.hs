module Main
    ( main
    ) where

-- import qualified Spec.Model
import qualified ThothTestSpec.ThothNetworkCoreTest
import qualified ThothTestSpec.ThothNetworkInitTest
import Plutus.Contract.Test
import qualified Plutus.Trace.Emulator as Trace

import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "network Initialization"
    [ ThothTestSpec.ThothNetworkInitTest.tests
    -- , Spec.Model.tests
    ]
