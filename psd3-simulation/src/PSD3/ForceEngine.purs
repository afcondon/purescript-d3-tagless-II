-- | Pure Force Engine
-- |
-- | A clean, debuggable force simulation engine.
-- | Uses D3's force calculation algorithms but manages the simulation ourselves.
-- |
-- | Key benefits over d3.forceSimulation:
-- | - Full control over when forces run
-- | - Predictable, debuggable behavior
-- | - Clean PureScript types
-- | - No hidden state or timers
-- |
-- | Usage:
-- | ```purescript
-- | import PSD3.ForceEngine as FE
-- |
-- | main = do
-- |   sim <- FE.create FE.defaultConfig
-- |   FE.setNodes myNodes sim
-- |   FE.addForce (FE.ManyBody "charge" FE.defaultManyBody) sim
-- |   FE.onTick (renderNodes sim) sim
-- |   FE.start sim
-- | ```
module PSD3.ForceEngine
  ( -- * Re-exports from Types
    module Types
    -- * Re-exports from Simulation
  , module Simulation
    -- * Re-exports from Core (low-level)
  , module Core
    -- * Re-exports from Links
  , module Links
  ) where

import PSD3.ForceEngine.Types
  ( SimNode
  , SimLink
  , RawLink
  , SimulationState
  , defaultSimParams
  , ManyBodyConfig
  , CollideConfig
  , LinkConfig
  , CenterConfig
  , ForceXConfig
  , ForceYConfig
  , RadialConfig
  , ForceSpec(..)
  , forceName
  , defaultManyBody
  , defaultCollide
  , defaultLink
  , defaultCenter
  ) as Types

import PSD3.ForceEngine.Simulation
  ( Simulation
  , SimConfig
  , defaultConfig
  , create
  , setNodes
  , setLinks
  , addForce
  , removeForce
  , start
  , stop
  , tick
  , reheat
  , onTick
  , isRunning
  , getAlpha
  , getNodes
  ) as Simulation

import PSD3.ForceEngine.Core
  ( ForceHandle
  , createManyBody
  , createCollide
  , createLink
  , createCenter
  , createForceX
  , createForceY
  , createRadial
  , initializeNodes
  , initializeForce
  , initializeLinkForce
  , applyForce
  , applyForces
  , integratePositions
  , decayAlpha
  , simulationTick
  , logNodes
  ) as Core

import PSD3.ForceEngine.Links
  ( swizzleLinks
  , swizzleLinksByIndex
  , filterLinksToSubset
  ) as Links
