-- | Force Engine Simulation
-- |
-- | High-level API for running force simulations.
-- | This module provides a clean interface for:
-- | - Creating and configuring simulations
-- | - Running the animation loop
-- | - Handling tick callbacks for rendering
-- |
-- | Unlike D3's simulation, we have full control over:
-- | - When forces are applied
-- | - How alpha decays
-- | - When to render
module PSD3.ForceEngine.Simulation
  ( -- * Node Types
    SimulationNode
  , NodeID
  , D3_ID
  , D3_XY
  , D3_VxyFxy
  , D3_FocusXY
    -- * Link Types
  , Link
  , SwizzledLink
    -- * Simulation Types
  , Simulation
  , SimConfig
  , defaultConfig
    -- * Lifecycle
  , create
  , setNodes
  , setLinks
  , addForce
  , removeForce
    -- * Running
  , start
  , stop
  , tick
  , reheat
    -- * Callbacks
  , onTick
    -- * Query
  , isRunning
  , getAlpha
  , getNodes
    -- * Position Updates (for transitions)
  , PositionMap
  , updatePositionsInPlace
  , interpolatePositionsInPlace
  , pinNodesInPlace
  , unpinNodesInPlace
  , pinNodesAtPositions
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Data.Nullable (Nullable)
import Foreign.Object (Object)
import Effect (Effect)
import Type.Row (type (+))
import Effect.Ref (Ref)
import Effect.Ref as Ref
import PSD3.ForceEngine.Core as Core
import PSD3.ForceEngine.Types (ForceSpec(..), defaultSimParams)

-- =============================================================================
-- Types - Node
-- =============================================================================

-- | Node ID type (Int for efficient lookups)
type NodeID = Int

-- | Composable row types for building simulation nodes
-- | These can be combined with (+) to create custom node types
type D3_ID      row = ( id :: NodeID | row )
type D3_XY      row = ( x :: Number, y :: Number | row )
type D3_VxyFxy  row = ( vx :: Number, vy :: Number, fx :: Nullable Number, fy :: Nullable Number | row )
type D3_FocusXY row = ( cluster :: Int, focusX :: Number, focusY :: Number | row )

-- | A simulation node with all required fields for force simulation and transitions.
-- | Extends user data row with id, position (x/y), velocity (vx/vy), and fixed position (fx/fy).
-- |
-- | Example:
-- | ```purescript
-- | type MyNode = SimulationNode (name :: String, group :: Int)
-- | -- Expands to: { id :: Int, x, y, vx, vy, fx, fy, name :: String, group :: Int }
-- | ```
type SimulationNode r = Record (D3_ID + D3_XY + D3_VxyFxy + r)

-- =============================================================================
-- Types - Link
-- =============================================================================

-- | Row-polymorphic link type parameterized by ID type
-- |
-- | Example:
-- | ```purescript
-- | type MyLink = Link Int (value :: Number)
-- | -- Expands to: { source :: Int, target :: Int, value :: Number }
-- | ```
type Link id r = { source :: id, target :: id | r }

-- | Swizzled link where source/target are node object references
-- | After D3 processes links, indices become object references
type SwizzledLink nodeData r =
  { source :: SimulationNode nodeData
  , target :: SimulationNode nodeData
  | r
  }

-- | A running simulation
-- | This is a mutable structure that holds the simulation state
type Simulation row linkRow =
  { nodes :: Ref (Array (SimulationNode row))
  , links :: Ref (Array { source :: Int, target :: Int | linkRow })
  , forces :: Ref (Map String Core.ForceHandle)
  , alpha :: Ref Number
  , config :: SimConfig
  , running :: Ref Boolean
  , cancelAnimation :: Ref (Effect Unit)
  , tickCallback :: Ref (Effect Unit)
  }

-- | Simulation configuration
type SimConfig =
  { alphaMin :: Number
  , alphaDecay :: Number
  , alphaTarget :: Number
  , velocityDecay :: Number
  }

-- | Default simulation configuration
defaultConfig :: SimConfig
defaultConfig =
  { alphaMin: defaultSimParams.alphaMin
  , alphaDecay: defaultSimParams.alphaDecay
  , alphaTarget: defaultSimParams.alphaTarget
  , velocityDecay: defaultSimParams.velocityDecay
  }

-- =============================================================================
-- Lifecycle
-- =============================================================================

-- | Create a new simulation
create :: forall row linkRow. SimConfig -> Effect (Simulation row linkRow)
create config = do
  nodesRef <- Ref.new []
  linksRef <- Ref.new []
  forcesRef <- Ref.new Map.empty
  alphaRef <- Ref.new 1.0
  runningRef <- Ref.new false
  cancelRef <- Ref.new (pure unit)
  tickRef <- Ref.new (pure unit)
  pure
    { nodes: nodesRef
    , links: linksRef
    , forces: forcesRef
    , alpha: alphaRef
    , config
    , running: runningRef
    , cancelAnimation: cancelRef
    , tickCallback: tickRef
    }

-- | Set the nodes for the simulation
-- | This initializes nodes and re-initializes all forces
setNodes :: forall row linkRow.
  Array (SimulationNode row)
  -> Simulation row linkRow
  -> Effect Unit
setNodes nodes sim = do
  -- Initialize nodes (sets index, default vx/vy)
  Core.initializeNodes nodes
  Ref.write nodes sim.nodes

  -- Re-initialize all forces with new nodes
  forces <- Ref.read sim.forces
  for_ (Map.toUnfoldable forces :: Array (Tuple String Core.ForceHandle)) \(Tuple _ handle) -> do
    _ <- Core.initializeForce handle nodes
    pure unit

-- | Set the links for the simulation
-- | This re-initializes any link forces
setLinks :: forall row linkRow.
  Array { source :: Int, target :: Int | linkRow }
  -> Simulation row linkRow
  -> Effect Unit
setLinks links sim = do
  Ref.write links sim.links
  -- TODO: Re-initialize link forces when we track force types

-- | Add a force to the simulation
addForce :: forall row linkRow.
  ForceSpec
  -> Simulation row linkRow
  -> Effect Unit
addForce spec sim = do
  nodes <- Ref.read sim.nodes
  links <- Ref.read sim.links

  -- Create the force handle
  handle <- case spec of
    ManyBody _ config -> do
      let h = Core.createManyBody config
      Core.initializeForce h nodes

    Collide _ config -> do
      let h = Core.createCollide config
      Core.initializeForce h nodes

    Link _ config -> do
      let h = Core.createLink config
      Core.initializeLinkForce h nodes links

    Center _ config -> do
      let h = Core.createCenter config
      Core.initializeForce h nodes

    PositionX _ config -> do
      let h = Core.createForceX config
      Core.initializeForce h nodes

    PositionY _ config -> do
      let h = Core.createForceY config
      Core.initializeForce h nodes

    Radial _ config -> do
      let h = Core.createRadial config
      Core.initializeForce h nodes

  -- Add to forces map
  let name = forceName spec
  Ref.modify_ (Map.insert name handle) sim.forces
  where
  forceName = case _ of
    ManyBody n _ -> n
    Collide n _ -> n
    Link n _ -> n
    Center n _ -> n
    PositionX n _ -> n
    PositionY n _ -> n
    Radial n _ -> n

-- | Remove a force from the simulation
removeForce :: forall row linkRow.
  String
  -> Simulation row linkRow
  -> Effect Unit
removeForce name sim = do
  Ref.modify_ (Map.delete name) sim.forces

-- =============================================================================
-- Running
-- =============================================================================

-- | Start the simulation animation loop
start :: forall row linkRow.
  Simulation row linkRow
  -> Effect Unit
start sim = do
  -- Don't start if already running
  alreadyRunning <- Ref.read sim.running
  unless alreadyRunning do
    Ref.write true sim.running

    -- Start animation loop
    cancel <- Core.startAnimation \_ -> do
      running <- Ref.read sim.running
      if running
        then do
          newAlpha <- tick sim
          -- Continue if alpha > 0, otherwise mark as stopped
          let shouldContinue = newAlpha > 0.0
          unless shouldContinue do
            Ref.write false sim.running
          pure shouldContinue
        else pure false

    Ref.write cancel sim.cancelAnimation

-- | Stop the simulation
stop :: forall row linkRow.
  Simulation row linkRow
  -> Effect Unit
stop sim = do
  Ref.write false sim.running
  cancel <- Ref.read sim.cancelAnimation
  cancel

-- | Run a single tick of the simulation
-- | Returns the new alpha value
tick :: forall row linkRow.
  Simulation row linkRow
  -> Effect Number
tick sim = do
  nodes <- Ref.read sim.nodes
  forces <- Ref.read sim.forces
  alpha <- Ref.read sim.alpha

  -- Apply all forces
  let forceHandles = Array.fromFoldable $ Map.values forces
  Core.applyForces forceHandles alpha

  -- Integrate positions
  Core.integratePositions nodes sim.config.velocityDecay

  -- Decay alpha
  let newAlpha = Core.decayAlpha alpha sim.config.alphaMin sim.config.alphaDecay sim.config.alphaTarget
  Ref.write newAlpha sim.alpha

  -- Call tick callback
  callback <- Ref.read sim.tickCallback
  callback

  pure newAlpha

-- | Reheat the simulation (set alpha to 1)
reheat :: forall row linkRow.
  Simulation row linkRow
  -> Effect Unit
reheat sim = do
  Ref.write 1.0 sim.alpha
  -- If not running, start
  running <- Ref.read sim.running
  unless running $ start sim

-- =============================================================================
-- Callbacks
-- =============================================================================

-- | Set the tick callback
-- | This is called after each simulation tick
onTick :: forall row linkRow.
  Effect Unit
  -> Simulation row linkRow
  -> Effect Unit
onTick callback sim = do
  Ref.write callback sim.tickCallback

-- =============================================================================
-- Query
-- =============================================================================

-- | Check if the simulation is running
isRunning :: forall row linkRow.
  Simulation row linkRow
  -> Effect Boolean
isRunning sim = Ref.read sim.running

-- | Get the current alpha value
getAlpha :: forall row linkRow.
  Simulation row linkRow
  -> Effect Number
getAlpha sim = Ref.read sim.alpha

-- | Get the current nodes (with updated positions)
getNodes :: forall row linkRow.
  Simulation row linkRow
  -> Effect (Array (SimulationNode row))
getNodes sim = Ref.read sim.nodes

-- =============================================================================
-- Position Updates (for tick-driven transitions)
-- =============================================================================

-- | Position map type (keyed by node id as string)
type PositionMap = Object { x :: Number, y :: Number }

-- FFI imports (typed to ensure SimulationNode structure is present)
foreign import updatePositionsInPlace_
  :: forall row. PositionMap -> Ref (Array (SimulationNode row)) -> Effect Unit

foreign import interpolatePositionsInPlace_
  :: forall row. PositionMap -> PositionMap -> Number -> Ref (Array (SimulationNode row)) -> Effect Unit

foreign import pinNodesInPlace_
  :: forall row. Ref (Array (SimulationNode row)) -> Effect Unit

foreign import unpinNodesInPlace_
  :: forall row. Ref (Array (SimulationNode row)) -> Effect Unit

foreign import pinNodesAtPositions_
  :: forall row. PositionMap -> Ref (Array (SimulationNode row)) -> Effect Unit

-- | Update node positions in place from a position map
-- | Mutates the simulation's internal nodes (same objects bound to D3)
updatePositionsInPlace :: forall row linkRow.
  PositionMap
  -> Simulation row linkRow
  -> Effect Unit
updatePositionsInPlace positions sim =
  updatePositionsInPlace_ positions sim.nodes

-- | Interpolate node positions in place between start and target
-- | Progress should be 0.0 to 1.0 (apply easing before calling)
interpolatePositionsInPlace :: forall row linkRow.
  PositionMap  -- ^ Start positions
  -> PositionMap  -- ^ Target positions
  -> Number  -- ^ Progress (0-1, already eased)
  -> Simulation row linkRow
  -> Effect Unit
interpolatePositionsInPlace startPos targetPos progress sim =
  interpolatePositionsInPlace_ startPos targetPos progress sim.nodes

-- | Pin all nodes at their current positions (fx = x, fy = y)
-- | Use before starting a transition to freeze current state
pinNodesInPlace :: forall row linkRow.
  Simulation row linkRow
  -> Effect Unit
pinNodesInPlace sim = pinNodesInPlace_ sim.nodes

-- | Unpin all nodes (fx = null, fy = null)
-- | Use after transition to Grid scene to let forces take over
unpinNodesInPlace :: forall row linkRow.
  Simulation row linkRow
  -> Effect Unit
unpinNodesInPlace sim = unpinNodesInPlace_ sim.nodes

-- | Pin nodes at specific positions from a position map
-- | Sets both x/y and fx/fy - use at end of transition to non-Grid scenes
pinNodesAtPositions :: forall row linkRow.
  PositionMap
  -> Simulation row linkRow
  -> Effect Unit
pinNodesAtPositions positions sim =
  pinNodesAtPositions_ positions sim.nodes
