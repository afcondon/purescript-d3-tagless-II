-- | Force Engine Demo
-- |
-- | A simple demo showing the pure force engine in action.
-- | This creates a small force-directed graph with nodes and links.
module PSD3.ForceEngine.Demo
  ( runDemo
  , DemoState
  , DemoNode
  , DemoLink
  ) where

import Prelude

import Data.Array ((..))
import Data.Int (toNumber)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import PSD3.ForceEngine.Core as Core
import PSD3.ForceEngine.Simulation (SimulationNode)
import PSD3.ForceEngine.Simulation as Sim
import PSD3.ForceEngine.Types (ForceSpec(..), defaultManyBody, defaultCollide, defaultLink, defaultCenter)

-- =============================================================================
-- Demo Types
-- =============================================================================

-- | Demo node - extends SimulationNode with index
type DemoNode = SimulationNode (index :: Int)

-- | Demo link
type DemoLink =
  { source :: Int
  , target :: Int
  }

-- | Demo state for rendering
type DemoState =
  { nodes :: Array DemoNode
  , links :: Array DemoLink
  , tickCount :: Int
  }

-- =============================================================================
-- Demo Setup
-- =============================================================================

-- | Create initial demo nodes in a grid
createNodes :: Int -> Array DemoNode
createNodes count =
  map makeNode (0 .. (count - 1))
  where
  makeNode i =
    { id: i
    , x: toNumber (i `mod` 5) * 50.0 - 100.0
    , y: toNumber (i / 5) * 50.0 - 100.0
    , vx: 0.0
    , vy: 0.0
    , fx: Nullable.null
    , fy: Nullable.null
    , index: i
    }

-- | Create links forming a simple chain
createLinks :: Int -> Array DemoLink
createLinks nodeCount =
  map makeLink (0 .. (nodeCount - 2))
  where
  makeLink i = { source: i, target: i + 1 }

-- =============================================================================
-- Demo Runner
-- =============================================================================

-- | Run the force engine demo
-- | Returns a ref to the demo state that updates on each tick
runDemo :: Effect (Ref DemoState)
runDemo = do
  log "[ForceEngine Demo] Starting..."

  -- Create initial data
  let nodes = createNodes 10
  let links = createLinks 10

  -- Create state ref
  stateRef <- Ref.new
    { nodes
    , links
    , tickCount: 0
    }

  -- Create simulation
  sim <- Sim.create Sim.defaultConfig

  -- Set nodes and links
  Sim.setNodes nodes sim
  Sim.setLinks links sim

  -- Add forces
  Sim.addForce (ManyBody "charge" defaultManyBody { strength = -50.0 }) sim
  Sim.addForce (Collide "collide" defaultCollide { radius = 20.0 }) sim
  Sim.addForce (Link "links" defaultLink { distance = 50.0 }) sim
  Sim.addForce (Center "center" defaultCenter) sim

  -- Set tick callback to update state and log progress
  Sim.onTick (do
    currentNodes <- Sim.getNodes sim
    alpha <- Sim.getAlpha sim
    Ref.modify_ (\s -> s
      { nodes = currentNodes
      , tickCount = s.tickCount + 1
      }) stateRef

    -- Log every 50 ticks
    state <- Ref.read stateRef
    when (state.tickCount `mod` 50 == 0) do
      log $ "[ForceEngine Demo] Tick " <> show state.tickCount <> ", alpha=" <> show alpha
      Core.logNodes "Nodes" currentNodes
  ) sim

  -- Start simulation
  log "[ForceEngine Demo] Starting simulation..."
  Sim.start sim

  pure stateRef
