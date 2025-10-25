-- | PSD3.Capabilities.Simulation - Force-directed graph simulations
-- |
-- | This module defines the `SimulationM` type class for creating animated,
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
-- |   SimulationM D3Selection_ m =>
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
import PSD3.Internal.Simulation.Types (SimVariable, Step)
import Data.Maybe (Maybe)
import Data.Set (Set)
import Prelude (class Eq, class Monad, Unit)

-- | Prevents "boolean blindness" when enabling/disabling forces.
-- |
-- | Instead of passing booleans, pass lists of force labels.
type ForceConfigLists = { enable :: Array Label, disable :: Array Label }

-- | SimulationM extends SelectionM with force simulation capabilities.
-- |
-- | This type class manages the simulation state, forces, and animation loop.
-- | It requires SelectionM because simulations render to selections.
-- |
-- | The functional dependency `m -> selection` means the monad determines
-- | the selection type (e.g., `D3SimM` always uses `D3Selection_`).
class (Monad m, SelectionM selection m) <= SimulationM selection m | m -> selection where
  -- ** Simulation Control **

  -- | Start the simulation animation.
  -- |
  -- | This begins the iterative force calculation process. The simulation will
  -- | run for several hundred ticks, gradually slowing down as it reaches equilibrium.
  -- |
  -- | ```purescript
  -- | start  -- Animation begins, tick functions fire on each frame
  -- | ```
  -- |
  -- | The simulation automatically stops when alpha (animation progress) reaches alphaMin.
  start :: m Unit

  -- | Stop the simulation animation.
  -- |
  -- | Freezes the simulation at its current state. Nodes stop moving.
  -- | Call `start` again to resume.
  -- |
  -- | ```purescript
  -- | stop  -- Freeze the current layout
  -- | ```
  stop  :: m Unit

  -- ** Simulation Configuration **

  -- | Set a simulation variable (alpha, alphaTarget, velocityDecay, etc.).
  -- |
  -- | These control the simulation's behavior:
  -- | - `Alpha` - Current animation progress (1.0 = full energy, 0.0 = stopped)
  -- | - `AlphaTarget` - Minimum alpha to maintain (keeps simulation "warm")
  -- | - `AlphaDecay` - How quickly simulation cools down
  -- | - `VelocityDecay` - Friction applied to node movement
  -- |
  -- | ```purescript
  -- | setConfigVariable $ Alpha 1.0  -- Reheat simulation
  -- | setConfigVariable $ AlphaTarget 0.3  -- Keep simulation running
  -- | ```
  setConfigVariable    :: SimVariable -> m Unit

  -- ** Force Management **

  -- | Enable only the specified forces, disable all others.
  -- |
  -- | Forces are identified by labels like "center", "charge", "link", etc.
  -- | This completely replaces the active force set.
  -- |
  -- | ```purescript
  -- | -- Enable only center and charge forces
  -- | actualizeForces $ Set.fromFoldable ["center", "charge"]
  -- | ```
  -- |
  -- | See `PSD3.Internal.Simulation.Forces` for force definitions.
  actualizeForces:: Set Label -> m Unit

  -- ** Data Management - Type-Safe (Preferred) **

  -- | Load node data into the simulation.
  -- |
  -- | This is the **type-safe** way to set nodes. Use this for static simulations
  -- | where you load data once at initialization.
  -- |
  -- | ```purescript
  -- | let nodes = [{ id: "a", ... }, { id: "b", ... }]
  -- | nodesInSim <- setNodes nodes
  -- | circles <- simpleJoin svg Circle nodesInSim keyFn
  -- | ```
  -- |
  -- | Returns the nodes with simulation properties added (x, y, vx, vy, etc.).
  setNodes :: forall d.   Array (D3_SimulationNode d) -> m (Array (D3_SimulationNode d))

  -- | Load link data into the simulation, validating against nodes.
  -- |
  -- | This is the **type-safe** way to set links. Links reference nodes by ID,
  -- | and this function "swizzles" those IDs into object references that D3 needs.
  -- |
  -- | ```purescript
  -- | let links = [{ source: "a", target: "b", ... }]
  -- | linksInSim <- setLinks links nodes keyFn
  -- | lines <- simpleJoin svg Line linksInSim keyFn
  -- | ```
  -- |
  -- | Invalid links (referencing non-existent nodes) are filtered out.
  -- |
  -- | Returns "swizzled" links where source/target are object references.
  setLinks :: forall d r id. (Eq id) => Array (D3Link id r) -> Array (D3_SimulationNode d) -> (Datum_ -> Index_ ) -> m (Array (D3LinkSwizzled (D3_SimulationNode d) r))

  -- ** Data Management - Selection-Based (For Updates) **

  -- | Update simulation nodes from a selection (less type-safe).
  -- |
  -- | Use this for **dynamic simulations** where data changes over time.
  -- | The selection must contain data matching the simulation node format.
  -- |
  -- | ```purescript
  -- | -- After updating selection with new data
  -- | setNodesFromSelection mergedCircles
  -- | ```
  -- |
  -- | **Warning**: No compile-time type checking. Ensure selection data is correct.
  setNodesFromSelection :: selection -> m Unit

  -- | Update simulation links from a selection (less type-safe).
  -- |
  -- | Use this for **dynamic simulations**. The boolean function filters
  -- | which links should exert force (some links might be visual only).
  -- |
  -- | ```purescript
  -- | setLinksFromSelection mergedLines (\_ -> true)  -- All links have force
  -- | ```
  -- |
  -- | **Warning**: No compile-time type checking. Ensure selection data is correct.
  setLinksFromSelection :: selection -> (Datum_ -> Boolean) -> m Unit

  -- | Merge new data with simulation state, preserving positions.
  -- |
  -- | This is the **key function for dynamic simulations**. It handles the complex
  -- | task of updating simulation data while preserving node positions from the
  -- | previous state (critical for smooth animations).
  -- |
  -- | ```purescript
  -- | -- After general update pattern on selections
  -- | merged <- mergeNewDataWithSim
  -- |   nodeSelection keyFn
  -- |   linkSelection linkKeyFn
  -- |   { nodes: newNodes, links: newLinks }
  -- |
  -- | setNodesFromSelection merged.nodes
  -- | setLinksFromSelection merged.links (\_ -> true)
  -- | ```
  -- |
  -- | This preserves the physical simulation state across data updates.
  mergeNewDataWithSim :: forall d r id. (Eq id) =>
    selection -> -- nodes selection
    (Datum_ -> Index_) -> -- nodes keyFn
    selection -> -- links selection
    (Datum_ -> Index_) -> -- links KeyFn
    RawData d r id -> -- links and nodes raw data
    m { links :: (Array (D3LinkSwizzled (D3_SimulationNode d) r)), nodes :: (Array (D3_SimulationNode d))}

  -- ** Animation (Tick Functions) **

  -- | Get the underlying D3 simulation handle.
  -- |
  -- | Advanced operation - most users won't need this. Used internally
  -- | by tick functions to access simulation properties.
  simulationHandle :: m D3Simulation_

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
