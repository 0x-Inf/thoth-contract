{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module ThothMain.ThothPAB
    ( Address
    , TokenContract (..)
    ) where

import           Data.Aeson                          (FromJSON, ToJSON)
import           Data.OpenApi.Schema                 (ToSchema)
import           GHC.Generics                        (Generic)
import           Ledger                              (Address)
import           Plutus.PAB.Effects.Contract.Builtin (Empty, HasDefinitions (..), SomeBuiltin (..), endpointsToSchemas)
import           Prettyprinter                       (Pretty (..), viaShow)
import           Wallet.Emulator.Wallet              (knownWallet, mockWalletAddress)

import           ThothMain.ThothCore                 as Token


data TokenContract = Mint Token.TokenParams
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON, ToSchema)

instance Pretty TokenContract where 
    pretty = viaShow

instance HasDefinitions TokenContract where 

    getDefinitions         = [Mint exampleTP]

    getContract (Mint tp)  = SomeBuiltin $ Token.mintThothToken @() @Empty tp

    getSchema = const $ endpointsToSchemas @Empty


exampleAddr :: Address
exampleAddr = mockWalletAddress $ knownWallet 1


exampleTP :: Token.TokenParams
exampleTP = Token.TokenParams
    { Token.tpAddress  = exampleAddr
    , Token.tpAmount   = 123456
    , Token.tpToken    = "THOTH"
    }