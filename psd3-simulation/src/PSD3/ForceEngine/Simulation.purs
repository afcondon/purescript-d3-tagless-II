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
-- |
-- | == IMPORTANT: Force Target Caching
-- |
-- | D3's forceX/forceY forces **cache** their target values at initialization time.
-- | This is a significant gotcha when animating between different target positions.
-- |
-- | **The Problem:**
-- |
-- | If you update `node.gridX` and call `reheat`, the force will still use the
-- | old cached values. The simulation appears to run but nodes won't move to
-- | new positions.
-- |
-- | **Wrong approach:**
-- | ```purescript
-- | updateGridXWithFn newXFn sim
-- | reheat sim  -- Nodes don't move!
-- | ```
-- |
-- | **Correct approach:**
-- | ```purescript
-- | -- Use the helper that handles re-initialization:
-- | updateGridXYAndReinit (Just newXFn) Nothing forceXHandle Nothing sim
-- |
-- | -- Or manually re-initialize the force:
-- | updateGridXWithFn newXFn sim
-- | currentNodes <- getNodes sim
-- | _ <- Core.initializeForce forceXHandle currentNodes
-- | reheat sim
-- | ```
-- |
-- | The `updateGridXYAndReinit` function is the recommended way to animate
-- | force-directed transitions as it handles this caching issue automatically.
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
  , createWithCallbacks
  , setNodes
  , setLinks
  , addForce
  , addForceHandle
  , removeForce
  , clearForces
    -- * Running
  , start
  , stop
  , tick
  , reheat
    -- * Callbacks (legacy single-callback API)
  , onTick
    -- * Callbacks (new multi-callback API)
  , setCallbacks
  , getCallbacks
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
    -- * Grid Position Updates (for ForceXGrid/ForceYGrid)
  , updateGridPositionsInPlace
  , updateGridXWithFn
  , updateGridYWithFn
  , updateGridXYAndReinit
    -- * Drag Behavior
  , attachDrag
  , attachGroupDrag
  , attachPinningDrag
    -- * DOM Utilities
  , querySelectorElements
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Nullable (Nullable)
import Foreign.Object (Object)
import Effect (Effect)
import Type.Row (type (+))
import Effect.Ref (Ref)
import Effect.Ref as Ref
import PSD3.ForceEngine.Core as Core
import PSD3.ForceEngine.Types (ForceSpec(..), defaultSimParams)
import PSD3.ForceEngine.Events (SimulationCallbacks)
import Web.DOM.Element (Element)

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
  , prevAlpha :: Ref Number  -- For alpha threshold detection
  , config :: SimConfig
  , running :: Ref Boolean
  , cancelAnimation :: Ref (Effect Unit)
  , tickCallback :: Ref (Effect Unit)  -- Legacy single callback
  , callbacks :: Maybe SimulationCallbacks  -- New multi-callback system
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

-- | Create a new simulation (without callbacks)
-- |
-- | For the new callback-based API, use `createWithCallbacks` instead.
create :: forall row linkRow. SimConfig -> Effect (Simulation row linkRow)
create config = do
  nodesRef <- Ref.new []
  linksRef <- Ref.new []
  forcesRef <- Ref.new Map.empty
  alphaRef <- Ref.new 1.0
  prevAlphaRef <- Ref.new 1.0
  runningRef <- Ref.new false
  cancelRef <- Ref.new (pure unit)
  tickRef <- Ref.new (pure unit)
  pure
    { nodes: nodesRef
    , links: linksRef
    , forces: forcesRef
    , alpha: alphaRef
    , prevAlpha: prevAlphaRef
    , config
    , running: runningRef
    , cancelAnimation: cancelRef
    , tickCallback: tickRef
    , callbacks: Nothing
    }

-- | Create a new simulation with callback system
-- |
-- | Example:
-- | ```purescript
-- | callbacks <- defaultCallbacks
-- | onSimulationTick updateDOM callbacks
-- | onSimulationStop (log "Simulation settled") callbacks
-- | sim <- createWithCallbacks config callbacks
-- | ```
createWithCallbacks :: forall row linkRow.
  SimConfig
  -> SimulationCallbacks
  -> Effect (Simulation row linkRow)
createWithCallbacks config cbs = do
  nodesRef <- Ref.new []
  linksRef <- Ref.new []
  forcesRef <- Ref.new Map.empty
  alphaRef <- Ref.new 1.0
  prevAlphaRef <- Ref.new 1.0
  runningRef <- Ref.new false
  cancelRef <- Ref.new (pure unit)
  tickRef <- Ref.new (pure unit)
  pure
    { nodes: nodesRef
    , links: linksRef
    , forces: forcesRef
    , alpha: alphaRef
    , prevAlpha: prevAlphaRef
    , config
    , running: runningRef
    , cancelAnimation: cancelRef
    , tickCallback: tickRef
    , callbacks: Just cbs
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

-- | Add a pre-initialized force handle to the simulation
-- | Use this when you've created and initialized a force handle manually
-- | (e.g., for dynamic forces not covered by ForceSpec)
addForceHandle :: forall row linkRow.
  String                     -- Force name (used for later removal)
  -> Core.ForceHandle        -- Pre-initialized force handle
  -> Simulation row linkRow
  -> Effect Unit
addForceHandle name handle sim = do
  Ref.modify_ (Map.insert name handle) sim.forces

-- | Remove a force from the simulation
removeForce :: forall row linkRow.
  String
  -> Simulation row linkRow
  -> Effect Unit
removeForce name sim = do
  Ref.modify_ (Map.delete name) sim.forces

-- | Clear all forces from the simulation
-- | Used when switching between force configurations (e.g., grid vs tree vs orbit)
clearForces :: forall row linkRow.
  Simulation row linkRow
  -> Effect Unit
clearForces sim = do
  Ref.write Map.empty sim.forces

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

    -- Fire onStart callback
    invokeStartCallback sim

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
            -- Fire onStop callback
            invokeStopCallback sim
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
  prevAlpha <- Ref.read sim.prevAlpha

  -- Apply all forces
  let forceHandles = Array.fromFoldable $ Map.values forces
  Core.applyForces forceHandles alpha

  -- Integrate positions
  Core.integratePositions nodes sim.config.velocityDecay

  -- Decay alpha
  let newAlpha = Core.decayAlpha alpha sim.config.alphaMin sim.config.alphaDecay sim.config.alphaTarget
  Ref.write newAlpha sim.alpha
  Ref.write alpha sim.prevAlpha  -- Store current as previous for next tick

  -- Call legacy tick callback
  legacyCallback <- Ref.read sim.tickCallback
  legacyCallback

  -- Call new tick callback (if callbacks are configured)
  invokeTickCallback sim

  -- Check for alpha threshold crossings
  checkAlphaThresholds prevAlpha newAlpha sim

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

-- | Set the tick callback (legacy single-callback API)
-- | This is called after each simulation tick
onTick :: forall row linkRow.
  Effect Unit
  -> Simulation row linkRow
  -> Effect Unit
onTick callback sim = do
  Ref.write callback sim.tickCallback

-- | Set the callbacks for a simulation (new multi-callback API)
-- | Note: This replaces any callbacks passed to createWithCallbacks
setCallbacks :: forall row linkRow.
  SimulationCallbacks
  -> Simulation row linkRow
  -> Simulation row linkRow
setCallbacks cbs sim = sim { callbacks = Just cbs }

-- | Get the current callbacks (if any)
getCallbacks :: forall row linkRow.
  Simulation row linkRow
  -> Maybe SimulationCallbacks
getCallbacks sim = sim.callbacks

-- =============================================================================
-- Internal Callback Helpers
-- =============================================================================

-- | Invoke the onStart callback if present
invokeStartCallback :: forall row linkRow.
  Simulation row linkRow
  -> Effect Unit
invokeStartCallback sim = case sim.callbacks of
  Nothing -> pure unit
  Just cbs -> do
    callback <- Ref.read cbs.onStart
    callback

-- | Invoke the onStop callback if present
invokeStopCallback :: forall row linkRow.
  Simulation row linkRow
  -> Effect Unit
invokeStopCallback sim = case sim.callbacks of
  Nothing -> pure unit
  Just cbs -> do
    callback <- Ref.read cbs.onStop
    callback

-- | Invoke the onTick callback if present
invokeTickCallback :: forall row linkRow.
  Simulation row linkRow
  -> Effect Unit
invokeTickCallback sim = case sim.callbacks of
  Nothing -> pure unit
  Just cbs -> do
    callback <- Ref.read cbs.onTick
    callback

-- | Check if alpha crossed any thresholds and invoke callback
-- | Thresholds: 0.5, 0.1, 0.01
checkAlphaThresholds :: forall row linkRow.
  Number  -- ^ Previous alpha
  -> Number  -- ^ New alpha
  -> Simulation row linkRow
  -> Effect Unit
checkAlphaThresholds prevAlpha newAlpha sim = case sim.callbacks of
  Nothing -> pure unit
  Just cbs -> do
    -- Check each threshold
    for_ alphaThresholds \threshold ->
      when (crossedThreshold prevAlpha newAlpha threshold) do
        callback <- Ref.read cbs.onAlphaThreshold
        callback newAlpha
  where
  alphaThresholds = [0.5, 0.1, 0.01]
  crossedThreshold prev new thresh = prev > thresh && new <= thresh

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

-- =============================================================================
-- Grid Position Updates (for ForceXGrid/ForceYGrid forces)
-- =============================================================================

-- FFI for grid position updates
foreign import updateGridPositionsInPlace_
  :: forall row. PositionMap -> Ref (Array (SimulationNode row)) -> Effect Unit

foreign import updateGridXWithFn_
  :: forall row. (SimulationNode row -> Number) -> Ref (Array (SimulationNode row)) -> Effect Unit

foreign import updateGridYWithFn_
  :: forall row. (SimulationNode row -> Number) -> Ref (Array (SimulationNode row)) -> Effect Unit

-- | Update node gridX/gridY positions in place from a positions map
-- | Used to change force targets before reheating the simulation
-- | The positions map is keyed by node.id (as string) -> { x, y }
-- | where x becomes gridX and y becomes gridY
updateGridPositionsInPlace :: forall row linkRow.
  PositionMap
  -> Simulation row linkRow
  -> Effect Unit
updateGridPositionsInPlace positions sim =
  updateGridPositionsInPlace_ positions sim.nodes

-- | Update gridX for all nodes using a function
-- | Useful for toggle animations where only the X target changes
-- | Example: toggle between combined (all same X) and separated (X by department)
updateGridXWithFn :: forall row linkRow.
  (SimulationNode row -> Number)
  -> Simulation row linkRow
  -> Effect Unit
updateGridXWithFn fn sim =
  updateGridXWithFn_ fn sim.nodes

-- | Update gridY for all nodes using a function
-- | Useful for toggle animations where only the Y target changes
updateGridYWithFn :: forall row linkRow.
  (SimulationNode row -> Number)
  -> Simulation row linkRow
  -> Effect Unit
updateGridYWithFn fn sim =
  updateGridYWithFn_ fn sim.nodes

-- | Update gridX and/or gridY for all nodes and re-initialize the forces
-- |
-- | This is the recommended way to animate force-directed transitions.
-- | It handles the D3 force caching issue automatically by:
-- | 1. Updating gridX/gridY values on nodes
-- | 2. Re-initializing the ForceXGrid/ForceYGrid forces (so they pick up new targets)
-- | 3. Reheating the simulation
-- |
-- | IMPORTANT: D3's forceX/forceY cache target values at initialization time.
-- | Simply updating node.gridX/gridY won't make the force see new values.
-- | This function handles that by re-initializing the forces after updating.
-- |
-- | Example:
-- | ```purescript
-- | -- Move nodes to new X positions (Y unchanged)
-- | updateGridXYAndReinit
-- |   (Just (\node -> departmentX node.department))
-- |   Nothing
-- |   forceXHandle
-- |   Nothing
-- |   sim
-- |
-- | -- Move nodes to new X and Y positions
-- | updateGridXYAndReinit
-- |   (Just xFn)
-- |   (Just yFn)
-- |   forceXHandle
-- |   (Just forceYHandle)
-- |   sim
-- | ```
updateGridXYAndReinit :: forall row linkRow.
  Maybe (SimulationNode row -> Number)  -- ^ gridX update function (Nothing = don't update X)
  -> Maybe (SimulationNode row -> Number)  -- ^ gridY update function (Nothing = don't update Y)
  -> Core.ForceHandle  -- ^ ForceXGrid handle (needed if updating X)
  -> Maybe Core.ForceHandle  -- ^ ForceYGrid handle (needed if updating Y)
  -> Simulation row linkRow
  -> Effect Unit
updateGridXYAndReinit mXFn mYFn forceXHandle mForceYHandle sim = do
  -- Update gridX if function provided
  for_ mXFn \xFn ->
    updateGridXWithFn_ xFn sim.nodes

  -- Update gridY if function provided
  for_ mYFn \yFn ->
    updateGridYWithFn_ yFn sim.nodes

  -- Re-initialize forces so they pick up new gridX/gridY values
  currentNodes <- Ref.read sim.nodes

  -- Always re-initialize X force (even if no X update, it's cheap)
  _ <- Core.initializeForce forceXHandle currentNodes

  -- Re-initialize Y force if handle provided
  for_ mForceYHandle \forceYHandle ->
    void $ Core.initializeForce forceYHandle currentNodes

  -- Reheat to start the animation
  reheat sim

-- =============================================================================
-- Drag Behavior
-- =============================================================================

-- | Attach simulation-aware drag behavior to node elements
-- |
-- | This sets up D3 drag handlers that:
-- | 1. Reheat the simulation on drag start
-- | 2. Update fx/fy (fixed position) during drag
-- | 3. Clear fx/fy on drag end (release node)
-- |
-- | Example:
-- | ```purescript
-- | elements <- select ".node" >>= selectAll
-- | attachDrag elements sim
-- | ```
attachDrag :: forall row linkRow.
  Array Element
  -> Simulation row linkRow
  -> Effect Unit
attachDrag elements sim =
  Core.attachDragWithReheat elements (reheat sim)

-- | Attach drag to transformed group elements (like bubble packs)
-- |
-- | Unlike `attachDrag`, this version takes a container selector to
-- | get pointer coordinates in the correct coordinate space. Use this when
-- | dragging `<g>` elements that have `transform` attributes.
-- |
-- | Example:
-- | ```purescript
-- | -- For bubble packs inside a zoom group
-- | packElements <- select ".module-pack" >>= selectAll
-- | attachGroupDrag packElements "#zoom-group" sim
-- | ```
attachGroupDrag :: forall row linkRow.
  Array Element
  -> String  -- ^ Container selector (e.g., "#zoom-group")
  -> Simulation row linkRow
  -> Effect Unit
attachGroupDrag elements containerSelector sim =
  Core.attachGroupDragWithReheat elements containerSelector (reheat sim)

-- | Attach drag with toggle-pinning behavior
-- |
-- | Unlike `attachDrag`, this version keeps nodes pinned after drag ends.
-- | - First drag: pins the node where you drop it
-- | - Subsequent drags: if you barely move (< 3px), unpins the node
-- |
-- | This is useful for force playgrounds and exploration where users want
-- | to "fix" certain nodes in place while letting others float freely.
-- |
-- | Example:
-- | ```purescript
-- | nodeElements <- select ".node" >>= selectAll
-- | attachPinningDrag nodeElements sim
-- | ```
attachPinningDrag :: forall row linkRow.
  Array Element
  -> Simulation row linkRow
  -> Effect Unit
attachPinningDrag elements sim =
  Core.attachPinningDrag elements (reheat sim)

-- =============================================================================
-- DOM Utilities
-- =============================================================================

-- | Query DOM elements by CSS selector
-- |
-- | Convenience re-export from Core for getting element references.
-- |
-- | Example:
-- | ```purescript
-- | nodeCircles <- querySelectorElements "#my-graph circle"
-- | attachPinningDrag nodeCircles sim
-- | ```
querySelectorElements :: String -> Effect (Array Element)
querySelectorElements = Core.querySelectorElements
