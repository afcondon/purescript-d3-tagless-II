-- | PSD3.Capabilities.Simulation - Force-directed graph simulations
-- |
-- | This module defines the `SimulationM2` type class for creating animated,
-- | force-directed visualizations. Force simulations are physics-based layouts
-- | where nodes and links are positioned through iterative calculations of forces
-- | (gravity, charge, collision, etc.).
-- |
-- | ## What are Force Simulations?
-- |
-- | Force simulations treat your data as a physical system:
-- | - **Nodes** are particles with position (x, y) and velocity
-- | - **Forces** push and pull nodes (like gravity, springs, collision)
-- | - **Links** are springs connecting nodes
-- | - The simulation runs over time, gradually reaching equilibrium
-- |
-- | This creates organic, self-organizing layouts perfect for network graphs,
-- | hierarchies, and clustered data.
-- |
-- | ## Basic Usage Pattern
-- |
-- | ```purescript
-- | import PSD3
-- | import PSD3.Attributes (cx, cy, radius, fill)
-- |
-- | myForceGraph :: forall row m.
-- |   SimulationM2 D3Selection_ m =>
-- |   MonadState { simulation :: D3SimulationState_ | row } m =>
-- |   m Unit
-- | myForceGraph = do
-- |   -- 1. Create SVG container
-- |   svg <- attach "#chart" >>= \r -> appendTo r Svg [width 800.0, height 600.0]
-- |
-- |   -- 2. Load nodes and links into simulation
-- |   nodesInSim <- setNodes myNodeData
-- |   linksInSim <- setLinks myLinkData myNodeData keyFn
-- |
-- |   -- 3. Join data to DOM elements
-- |   circles <- simpleJoin svg Circle nodesInSim keyFn
-- |   lines <- simpleJoin svg Line linksInSim keyFn
-- |
-- |   -- 4. Set up tick function to update positions on each frame
-- |   addTickFunction "nodes" $ Step circles [cx nodeX, cy nodeY]
-- |   addTickFunction "links" $ Step lines [x1 linkSourceX, y1 linkSourceY, ...]
-- |
-- |   -- 5. Start the simulation
-- |   start
-- | ```
-- |
-- | ## Forces
-- |
-- | The simulation applies various forces to nodes:
-- | - **center** - Pulls nodes toward the center
-- | - **charge** - Nodes repel (negative) or attract (positive) each other
-- | - **collide** - Prevents nodes from overlapping
-- | - **link** - Connects nodes like springs
-- | - **x/y** - Pulls nodes toward specific x or y coordinates
-- |
-- | Use `actualizeForces` to enable/disable forces dynamically.
-- |
-- | ## Static vs. Dynamic Data
-- |
-- | **Static simulations** (data doesn't change):
-- | ```purescript
-- | nodesInSim <- setNodes nodeData
-- | linksInSim <- setLinks linkData nodeData keyFn
-- | circles <- simpleJoin svg Circle nodesInSim keyFn
-- | ```
-- |
-- | **Dynamic simulations** (data updates over time):
-- | ```purescript
-- | -- Use update join pattern
-- | result <- updateJoin svg Circle newNodeData keyFn
-- | -- Merge new data into simulation, preserving positions
-- | merged <- mergeNewDataWithSim nodeSelection keyFn linkSelection keyFn rawData
-- | setNodesFromSelection mergedNodes
-- | ```
-- |
-- | ## See Also
-- |
-- | - `PSD3.Internal.Simulation.Types` for `SimVariable`, `Step`, force configuration
-- | - `PSD3.Data.Node` for node and link data types
-- | - [D3 Force](https://d3js.org/d3-force) for the underlying D3.js concepts
module PSD3.Capabilities.Simulation where

import PSD3.Capabilities.Selection (class SelectionM)
import PSD3.Internal.Attributes.Instances (Label)
import PSD3.Internal.Types (D3Simulation_, Datum_, Index_)
import PSD3.Data.Node (D3Link, D3LinkSwizzled, D3_SimulationNode)
import PSD3.Internal.Simulation.Types (SimVariable, Step, Force)
import PSD3.Internal.FFI (SimulationVariables)
import Data.Maybe (Maybe)
import Data.Set (Set)
import Data.Map (Map)
import Prelude (class Eq, class Monad, Unit)

-- | ========================================================================================================
-- | Simplified SimulationM - Record-based initialization for static simulations
-- | ========================================================================================================

-- | Configuration record for initializing a force simulation.
-- |
-- | This provides a simplified, all-in-one initialization approach for static
-- | simulations (where data doesn't change after initialization).
-- |
-- | ## Example Usage
-- |
-- | ```purescript
-- | draw model selector = do
-- |   -- Create DOM structure and join data
-- |   svg <- attach selector >>= appendTo _ Svg [...]
-- |   nodesSelection <- simpleJoin svg Circle model.nodes keyIsID_
-- |   linksSelection <- simpleJoin svg Line model.links keyIsID_
-- |
-- |   -- Initialize simulation with everything at once
-- |   init
-- |     { nodes: model.nodes
-- |     , links: model.links
-- |     , forces: forceLibrary
-- |     , activeForces: Set.fromFoldable ["center", "charge", "collision"]
-- |     , config: defaultConfigSimulation
-- |     , keyFn: keyIsID_
-- |     , ticks: Map.fromFoldable
-- |         [ Tuple "nodes" $ Step nodesSelection [cx datum_.x, cy datum_.y]
-- |         , Tuple "links" $ Step linksSelection [x1 link_.source.x, ...]
-- |         ]
-- |     }
-- |
-- |   start
-- | ```
type SimulationConfig selection d r =
  { nodes :: Array (D3_SimulationNode d)                    -- Node data
  , links :: Array (D3Link String r)                        -- Link data (with String IDs)
  , forces :: Array Force                                   -- Force library (all available forces)
  , activeForces :: Set Label                               -- Which forces to enable initially
  , config :: SimulationVariables                           -- Simulation parameters (alpha, decay, etc.)
  , keyFn :: Datum_ -> Index_                               -- Key function for data binding
  , ticks :: Map Label (Step selection)                     -- Tick functions to update DOM on each frame
  }

-- | Simplified SimulationM - Single init() call for static simulations.
-- |
-- | This type class provides a minimal API for force simulations with static data.
-- | All configuration happens in one `init` call with a record parameter.
-- |
-- | For dynamic simulations with data updates, use SimulationM2 instead.
class (Monad m, SelectionM selection m) <= SimulationM selection m | m -> selection where
  -- | Initialize the simulation with all configuration at once.
  -- |
  -- | Returns the simulation-enhanced nodes and links (with x, y, vx, vy added).
  -- | Use these to create DOM selections, then add tick functions separately.
  init :: forall d r. SimulationConfig selection d r -> m { nodes :: Array (D3_SimulationNode d), links :: Array (D3LinkSwizzled (D3_SimulationNode d) r) }

  -- | Start the simulation animation.
  start :: m Unit

  -- | Stop the simulation animation.
  stop  :: m Unit

-- | ========================================================================================================
-- | SimulationM2 - Declarative API for dynamic simulation updates
-- | ========================================================================================================

-- | Configuration for updating a running simulation.
-- |
-- | All fields are optional (Maybe). Only provide the fields you want to update.
-- | The update function handles all ordering, swizzling, and force management internally.
-- |
-- | ## Example Usage
-- |
-- | ```purescript
-- | -- Update just the active forces
-- | update { nodes: Nothing, links: Nothing, activeForces: Just newForces, config: Nothing, keyFn: keyIsID_ }
-- |
-- | -- Update nodes and links together
-- | { nodes: updatedNodes, links: updatedLinks } <- update
-- |   { nodes: Just newNodeData
-- |   , links: Just newLinkData
-- |   , activeForces: Nothing
-- |   , config: Nothing
-- |   , keyFn: keyIsID_
-- |   }
-- | ```
type SimulationUpdate d r =
  { nodes :: Maybe (Array (D3_SimulationNode d))  -- New node data (replaces existing)
  , links :: Maybe (Array (D3Link String r))      -- New link data (replaces existing)
  , activeForces :: Maybe (Set Label)             -- Which forces to enable (replaces active set)
  , config :: Maybe SimulationVariables           -- Simulation config to update
  , keyFn :: Datum_ -> Index_                     -- Key function for data binding
  }

-- | SimulationM2 extends SimulationM with declarative update capabilities.
-- |
-- | Instead of multiple order-dependent methods, SimulationM2 provides a single
-- | `update` function that handles all complexity internally.
-- |
-- | The update function:
-- | - Handles proper ordering (forces before links, etc.)
-- | - Manages link swizzling automatically
-- | - Preserves node positions when updating data
-- | - Activates/deactivates forces correctly
-- |
-- | For static simulations, use SimulationM's `init`. For dynamic updates, use SimulationM2's `update`.
class (Monad m, SimulationM selection m) <= SimulationM2 selection m | m -> selection where
  -- ** Dynamic Updates **

  -- | Update a running simulation declaratively.
  -- |
  -- | This is the primary method for updating simulations. It handles all complexity:
  -- | - Proper ordering (activates forces before setting links)
  -- | - Link swizzling (converts IDs to object references)
  -- | - Position preservation (maintains x, y when updating data)
  -- | - Force management (enables/disables forces correctly)
  -- |
  -- | All fields in SimulationUpdate are optional. Only provide what you want to change.
  -- |
  -- | ```purescript
  -- | -- Example 1: Toggle forces (e.g., user clicks button)
  -- | update { nodes: Nothing, links: Nothing, activeForces: Just newForceSet, config: Nothing, keyFn: keyIsID_ }
  -- |
  -- | -- Example 2: Update data (e.g., new data arrives)
  -- | { nodes: nodesInSim, links: linksInSim } <- update
  -- |   { nodes: Just newNodeArray
  -- |   , links: Just newLinkArray
  -- |   , activeForces: Nothing  -- Keep current forces
  -- |   , config: Nothing         -- Keep current config
  -- |   , keyFn: keyIsID_
  -- |   }
  -- | -- Now rejoin to DOM with simulation-enhanced data
  -- | result <- updateJoin svg Circle nodesInSim keyIsID_
  -- |
  -- | -- Example 3: Reheat simulation
  -- | update { nodes: Nothing, links: Nothing, activeForces: Nothing, config: Just { alpha: 0.7, ... }, keyFn: keyIsID_ }
  -- | start
  -- | ```
  -- |
  -- | Returns simulation-enhanced nodes and links for joining to DOM.
  update :: forall d r. SimulationUpdate d r -> m { nodes :: Array (D3_SimulationNode d), links :: Array (D3LinkSwizzled (D3_SimulationNode d) r) }

  -- ** Animation (Tick Functions) **

  -- | Register a function to run on every simulation tick (frame).
  -- |
  -- | Tick functions update the DOM to match the simulation's current state.
  -- | Each tick happens ~60 times per second while the simulation runs.
  -- |
  -- | ```purescript
  -- | -- Update circle positions on each tick
  -- | addTickFunction "nodes" $ Step circles [cx datum_.x, cy datum_.y]
  -- |
  -- | -- Update link positions on each tick
  -- | addTickFunction "links" $ Step lines [
  -- |   x1 (_.x <<< link_.source),
  -- |   y1 (_.y <<< link_.source),
  -- |   x2 (_.x <<< link_.target),
  -- |   y2 (_.y <<< link_.target)
  -- | ]
  -- | ```
  -- |
  -- | Tick functions are identified by label, allowing removal/replacement.
  addTickFunction    :: Label -> Step selection -> m Unit

  -- | Remove a tick function by its label.
  -- |
  -- | ```purescript
  -- | removeTickFunction "nodes"  -- Stop updating node positions
  -- | ```
  removeTickFunction :: Label                   -> m Unit

-- RawData type exists to clean up types of mergeNewDataWithSim
type RawData d r id = {
  nodes :: Array (D3_SimulationNode d)
, links :: Array (D3Link id r)
}

type Staging selection d r id = {
    selections :: {
      nodes :: Maybe selection
    , links :: Maybe selection
    }
   -- filter for links given to simulation engine, you don't necessarily want all links to be exerting force
  , linksWithForce :: Datum_ -> Boolean
  , rawdata :: RawData d r id
}
