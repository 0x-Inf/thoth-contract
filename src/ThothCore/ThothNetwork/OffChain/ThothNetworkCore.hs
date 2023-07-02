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

module ThothCore.ThothNetwork.OffChain.ThothNetworkCore
    (-- * Functionality for network Initialization        
      conjureThothNetwork
    , initEndpoints
    -- , conjEndpoint
    , activateEndpoint
    , initializeResearcherEndpoint
    -- * Coverage Testing
    , networkCovIdx
    -- * Utility Functions
    , splitTokenVal
    , adjustAndSubmitWith
    , waitUnitlTimeHasPassed
    -- * Script Functions 
    , networkScriptAsCbor
    -- * Initialization Parameters
    , ConjureNetworkParams (..)
    , NetworkInitializeParams (..)
    , NetworkActivateParams (..)
    , ResearcherInitializeParams (..)
    ) where 

        