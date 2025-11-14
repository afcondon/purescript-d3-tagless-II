-- | PSD3v2.Capabilities.Simulation
-- |
-- | Force-directed graph simulation capability for PSD3v2.
-- | This module provides a PSD3v2-compatible interface to D3's force simulation engine.
-- |
-- | ## Design Philosophy
-- |
-- | This is a FORK of PSD3.Capabilities.Simulation that uses PSD3v2's Attribute system.
-- | We do NOT rewrite the physics engine - we wrap the existing d3-force FFI.
-- |
-- | Key differences from PSD3v1:
-- | - Uses `PSD3v2.Attribute.Types.Attribute` instead of `SelectionAttribute`
-- | - Step type parameterized by PSD3v2 selection type
-- | - Compatible with PSD3v2's joinData and transitions
-- |
-- | ## Architecture
-- |
-- | ```
-- | User Code (SelectionM + SimulationM2)
-- |     ↓
-- | D3v2 Interpreter (delegates to...)
-- |     ↓
-- | PSD3.Internal.Simulation.Functions (existing FFI)
-- |     ↓
-- | d3-force (physics engine)
-- | ```
module PSD3v2.Capabilities.Simulation where

import Prelude

import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Set (Set)
import PSD3.Data.Node (SimulationNode, D3Link_Unswizzled, D3Link_Swizzled)
import PSD3.Internal.Attributes.Instances (Label)
import PSD3.Internal.FFI (SimulationVariables)
import PSD3.Internal.Simulation.Types (D3SimulationState_, Force)
import PSD3v2.Attribute.Types (Attribute)

-- | A step describes how to update a selection on each simulation tick.
-- |
-- | During force simulation, nodes move 60 times per second. A Step tells
-- | the renderer which selection to update and which attributes to modify.
-- |
-- | Unlike PSD3v1's Step, this uses PSD3v2's Attribute type.
-- |
-- | ```purescript
-- | -- Update node positions on each tick
-- | addTickFunction "nodes" $ Step nodeCircles
-- |   [ cx (_.x)
-- |   , cy (_.y)
-- |   ]
-- |
-- | -- Update link positions on each tick
-- | addTickFunction "links" $ Step linkLines
-- |   [ x1 (\l -> l.source.x)
-- |   , y1 (\l -> l.source.y)
-- |   , x2 (\l -> l.target.x)
-- |   , y2 (\l -> l.target.y)
-- |   ]
-- | ```
data Step sel datum = Step (sel datum) (Array (Attribute datum))

-- | Configuration for initializing a force simulation.
-- |
-- | This mirrors PSD3.Capabilities.Simulation.SimulationConfig but is
-- | type-safe with PSD3v2's patterns.
-- |
-- | ```purescript
-- | { nodes: nodesInSim, links: linksInSim } <- init
-- |   { nodes: myNodeData
-- |   , links: myLinkData
-- |   , forces: [centerForce w h, chargeForce (const (-100.0))]
-- |   , activeForces: Set.fromFoldable ["center", "charge", "link"]
-- |   , config: defaultSimConfig
-- |   , keyFn: _.id
-- |   , ticks: Map.empty
-- |   }
-- | ```
type SimulationConfig a key sel =
  { nodes :: Array (SimulationNode a)
  , links :: Array D3Link_Unswizzled
  , forces :: Array (Force (SimulationNode a))
  , activeForces :: Set Label
  , config :: SimulationVariables
  , keyFn :: SimulationNode a -> key
  , ticks :: Map Label (Step sel (SimulationNode a))  -- Initial tick functions (usually empty)
  }

-- Alias for backward compatibility with initial naming
type SimulationInit a key sel = SimulationConfig a key sel

-- | Configuration for updating a running simulation.
-- |
-- | All fields are optional - only provide what you want to change.
-- |
-- | ```purescript
-- | -- Toggle forces
-- | { nodes, links } <- updateSimulation
-- |   { nodes: Nothing
-- |   , links: Nothing
-- |   , activeForces: Just newForceSet
-- |   , config: Nothing
-- |   , keyFn: _.id
-- |   }
-- |
-- | -- Update data
-- | { nodes, links } <- updateSimulation
-- |   { nodes: Just newNodeArray
-- |   , links: Just newLinkArray
-- |   , activeForces: Nothing
-- |   , config: Nothing
-- |   , keyFn: _.id
-- |   }
-- | ```
type SimulationUpdate a key =
  { nodes :: Maybe (Array (SimulationNode a))
  , links :: Maybe (Array D3Link_Unswizzled)
  , nodeFilter :: Maybe (SimulationNode a -> Boolean)
  , linkFilter :: Maybe (D3Link_Unswizzled -> Boolean)
  , activeForces :: Maybe (Set Label)
  , config :: Maybe SimulationVariables
  , keyFn :: SimulationNode a -> key
  }

-- | SimulationM capability for force-directed graph simulations.
-- |
-- | This is the BASE class for static simulations. Use SimulationM2 for dynamic updates.
-- |
-- | ## Usage Pattern (Static Simulation)
-- |
-- | ```purescript
-- | drawForceGraph :: forall m sel.
-- |   SelectionM sel m =>
-- |   SimulationM sel m =>
-- |   MonadEffect m =>
-- |   MonadState { simulation :: D3SimulationState_ NodeType | r } m =>
-- |   m Unit
-- | drawForceGraph = do
-- |   -- 1. Create DOM with SelectionM
-- |   svg <- select "#chart" >>= appendChild SVG [...]
-- |   nodesGroup <- appendChild Group [...] svg
-- |
-- |   -- 2. Initialize physics with SimulationM
-- |   { nodes, links } <- init {...}
-- |
-- |   -- 3. Join data with SelectionM
-- |   JoinResult { enter } <- joinData nodes "circle" nodesGroup
-- |   circles <- append Circle [...] enter
-- |
-- |   -- 4. Register tick callbacks with SimulationM
-- |   addTickFunction "nodes" $ Step circles [cx (_.x), cy (_.y)]
-- |
-- |   -- 5. Start animation
-- |   start
-- | ```
-- |
-- | ## State Requirement
-- |
-- | Implementations require `MonadState { simulation :: D3SimulationState_ a | r }`.
-- | The simulation state is inherently mutable - we don't hide this.
class (Monad m) <= SimulationM sel m | m -> sel where
  -- | Initialize a force simulation and return simulation-enhanced data.
  -- |
  -- | The simulation adds physics properties (vx, vy) to nodes and swizzles links
  -- | (converts IDs to object references).
  -- |
  -- | Returns nodes and links ready for data joining with SelectionM.
  init :: forall a key. SimulationConfig a key sel -> m { nodes :: Array (SimulationNode a), links :: Array D3Link_Swizzled }

  -- | Register a function to run on every simulation tick.
  -- |
  -- | Tick functions update DOM to match simulation state (~60fps during animation).
  -- |
  -- | Identified by label for removal/replacement.
  addTickFunction :: forall d. Label -> Step sel d -> m Unit

  -- | Remove a tick function by label.
  removeTickFunction :: Label -> m Unit

  -- | Start the simulation animation.
  start :: m Unit

  -- | Stop the simulation animation.
  stop :: m Unit

-- | SimulationM2 extends SimulationM with dynamic update capabilities.
-- |
-- | This type class provides methods for DYNAMIC simulations where data changes.
-- | For static simulations, SimulationM is sufficient.
-- |
-- | ## Usage Pattern (Dynamic Simulation)
-- |
-- | Same as SimulationM but adds:
-- | ```purescript
-- | -- Update data dynamically
-- | { nodes, links } <- update
-- |   { nodes: Just newNodeArray
-- |   , links: Just newLinkArray
-- |   , nodeFilter: Nothing
-- |   , linkFilter: Nothing
-- |   , activeForces: Nothing
-- |   , config: Nothing
-- |   , keyFn: _.id
-- |   }
-- |
-- | -- Re-join updated data to DOM
-- | JoinResult { enter, update, exit } <- joinData nodes "circle" nodesGroup
-- | ...
-- |
-- | -- Reheat and restart
-- | reheat 0.7
-- | start
-- | ```
class (Monad m, SimulationM sel m) <= SimulationM2 sel m | m -> sel where
  -- | Update a running simulation declaratively.
  -- |
  -- | Handles:
  -- | - Data updates (preserving node positions)
  -- | - Force toggling
  -- | - Link swizzling
  -- | - Reheating simulation
  -- |
  -- | All fields in SimulationUpdate are optional. Only provide what you want to change.
  -- |
  -- | Returns updated nodes and swizzled links for re-joining to DOM.
  update :: forall a key. SimulationUpdate a key -> m { nodes :: Array (SimulationNode a), links :: Array D3Link_Swizzled }

  -- | Reheat the simulation (set alpha to given value).
  -- |
  -- | Use this after updating data to re-animate the layout.
  -- | You still need to call `start` (from SimulationM) to begin animation.
  reheat :: Number -> m Unit
