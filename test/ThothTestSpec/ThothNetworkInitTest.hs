{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}

module           ThothTestSpec.ThothNetworkInitTest(tests) where

import           Control.Lens
import           Control.Monad (void)
import qualified Data.Map                   as Map
import           Data.Default               (Default (..))

import           Ledger (Value, Address, TokenName)
import qualified Ledger
import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger.Ada as Ada
import           Ledger.Time (POSIXTime)
import           Ledger.TimeSlot
import qualified Ledger.TimeSlot  as TimeSlot
import           Plutus.V1.Ledger.Time (POSIXTime (POSIXTime, getPOSIXTime), POSIXTimeRange)
import           Plutus.Contract.Test
import           Plutus.Script.Utils.V1.Generators (someTokenValue)
import           Plutus.Trace.Emulator (EmulatorConfig)
-- import           Plutus.Trace.Emulator
import           Wallet.Emulator.Wallet


import           ThothCore.ThothNetworkCore
import           ThothCore.ThothResearcherCore

import Test.Tasty


tests :: TestTree
tests = testGroup "network-init"
    [ checkPredicateOptions initOptions "can conjure the network"
        ( walletFundsChange w1 (Ada.adaValueOf (-15) <> Value.singleton conjCs conjTTn conjTAmt)  -- Value.singleton conjCs conjTTn conjTAmt 
          .&&. walletFundsChange w2 mempty
        )
        $ do
            startTime <- scSlotZeroTime <$> getSlotConfig
            -- initDeadline <- slotToEndPOSIXTime slotConfigdef 10
            let conjParams = mkConjureNetworkParams w1Address tributeAmount conjureNetworkAssetTokenName initDeadline spawnTokenAmount

            conjHdl <- activateContractWallet w1 initEndpoints
            callEndpoint @"conjure" conjHdl conjParams
    ]


-- | 'beginningOfTime' corresponds to the Shelley launch date
-- (2020-07-29T21:44:51Z) which is 1596059091000 in POSIX time
-- (number of milliseconds since 1970-01-01T00:00:00Z).
beginningOfTime :: Integer
beginningOfTime = 1596059091000

slotConfigdef :: SlotConfig    
slotConfigdef = SlotConfig{ scSlotLength = 1000, scSlotZeroTime = POSIXTime beginningOfTime }

-- emulatorConfigDef :: EmulatorConfig
-- emulatorConfigDef = EmulatorConfig
--           { _initialChainState = Left defaultDist
--           , _slotConfig = slotConfigdef
--           }

w1Address :: Address 
w1Address = mockWalletAddress w1

tributeAmount :: Integer
tributeAmount = 15_000_000

spawnTokenAmount :: Integer
spawnTokenAmount = 2

conjureNetworkAssetTokenName :: TokenName 
conjureNetworkAssetTokenName = "Conjure Thoth" 

conjureTokenValue :: Integer -> Value 
conjureTokenValue = someTokenValue "Conjure Thoth" 

initDeadline :: POSIXTime 
initDeadline = slotToEndPOSIXTime def 10

mkConjureNetworkParams :: Address -> Integer -> TokenName -> POSIXTime -> Integer -> ConjureNetworkParams
mkConjureNetworkParams addr tributeAmount conjureNetworkAssetTokenName initDeadline spawnTokenAmount = 
    ConjureNetworkParams 
                { conjuringResearcherAddress   = addr
                , conjureNetworkTributeAmount  = tributeAmount
                , conjureNetworkTokenName      = conjureNetworkAssetTokenName
                , conjureNetworkDeadline       = initDeadline
                , spawnNetworkTokenAmount      = spawnTokenAmount
                }



initOptions :: CheckOptions 
initOptions = defaultCheckOptions & emulatorConfig .~ emCfg


emCfg :: EmulatorConfig
emCfg = EmulatorConfig (Left $  Map.fromList [(knownWallet 1, v1), (knownWallet 2 , v2), (knownWallet 3 , v3)]) def  
    where
        v1,v2,v3 :: Value 
        v1 = Ada.lovelaceValueOf 100_000_000
        v2 = Ada.lovelaceValueOf 100_000_000
        v3 = Ada.lovelaceValueOf 100_000_000            