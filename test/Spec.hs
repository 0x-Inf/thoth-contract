module Main
    ( main
    ) where

-- import qualified Spec.Model
import qualified ThothTestSpec.ThothNetworkCoreTest
import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "token sale"
    [ ThothTestSpec.ThothNetworkCoreTest.tests
    -- , Spec.Model.tests
    ]
