-- | SimulationManager - Owns the force simulation for CodeExplorerV2
-- |
-- | This replaces D3's simulation with our own loop using D3's force calculations.
-- | We have full control over:
-- | - When forces are applied
-- | - How alpha decays
-- | - When to render (tick callback)
module Component.CodeExplorerV2.SimulationManager
  ( -- * Types
    ForceHandle
  , SimState
    -- * Simulation lifecycle
  , createSimulation
  , setNodes
  , setLinks
  , start
  , stop
  , reheat
  , setTickCallback
    -- * Force creation (Spago-specific)
  , createCollision
  , createCharge
  , createChargeFiltered
  , createCenter
  , createLink
  , createClusterX
  , createClusterY
  , createRadial
    -- * Force management
  , addForce
  , clearForces
  , initializeForce
  , initializeLinkForce
    -- * Swizzling
  , swizzleLinks
  , SwizzledLink
    -- * Debug
  , logState
  ) where

import Prelude

import D3.Viz.Spago.Model (SpagoSimNode, isModule, isPackage)
import Data.Array as Array
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref

-- =============================================================================
-- Types
-- =============================================================================

-- | Opaque handle to a D3 force
foreign import data ForceHandle :: Type

-- | Link with source/target as node references (swizzled)
type SwizzledLink node linkData =
  { source :: node
  , target :: node
  | linkData
  }

-- | Link with source/target as IDs (raw)
type RawLink linkData =
  { source :: Int
  , target :: Int
  | linkData
  }

-- | Simulation state
type SimState =
  { nodes :: Array SpagoSimNode
  , links :: Array { source :: Int, target :: Int, linktype :: String }
  , forces :: Map String ForceHandle
  , alpha :: Number
  , alphaMin :: Number
  , alphaDecay :: Number
  , alphaTarget :: Number
  , velocityDecay :: Number
  , running :: Boolean
  , tickCallback :: Effect Unit
  }

-- =============================================================================
-- FFI Imports
-- =============================================================================

-- Window storage
foreign import setSimulationInWindow_ :: Ref SimState -> Effect Unit
foreign import getSimulationFromWindow_ :: Effect (Ref SimState)

-- Animation
foreign import requestAnimationFrame_ :: Effect Unit -> Effect (Effect Unit)

-- Force creation (Spago-specific)
foreign import createSpagoCollision_ :: Number -> Number -> Number -> ForceHandle
foreign import createSpagoCharge_ :: Number -> Number -> Number -> Number -> ForceHandle
foreign import createSpagoChargeFiltered_ :: Number -> Number -> Number -> Number -> (SpagoSimNode -> Boolean) -> ForceHandle
foreign import createSpagoCenter_ :: Number -> Number -> Number -> ForceHandle
foreign import createSpagoLink_ :: Number -> Number -> Number -> ForceHandle
foreign import createSpagoClusterX_ :: Number -> (SpagoSimNode -> Boolean) -> ForceHandle
foreign import createSpagoClusterY_ :: Number -> (SpagoSimNode -> Boolean) -> ForceHandle
foreign import createSpagoRadial_ :: Number -> Number -> Number -> Number -> (SpagoSimNode -> Boolean) -> ForceHandle

-- Force initialization
foreign import initializeForce_ :: forall r. ForceHandle -> Array { | r } -> Effect ForceHandle
foreign import initializeLinkForce_ :: forall r l. ForceHandle -> Array { | r } -> Array { | l } -> Effect ForceHandle

-- Force application
foreign import applyForce_ :: ForceHandle -> Number -> Effect Unit

-- Position integration
foreign import integratePositions_ :: forall r. Array { | r } -> Number -> Effect Unit
foreign import initializeNodes_ :: forall r. Array { | r } -> Effect Unit

-- Alpha
foreign import decayAlpha_ :: Number -> Number -> Number -> Number -> Number

-- Swizzling
foreign import buildSwizzledLink_ :: forall node linkData.
  node -> node -> { source :: Int, target :: Int | linkData } -> SwizzledLink node linkData

-- Debug
foreign import logForceState_ :: forall r. String -> Array { | r } -> Number -> Effect Unit
foreign import setSimNodes_ :: Array SpagoSimNode -> Effect Unit

-- =============================================================================
-- Simulation Lifecycle
-- =============================================================================

-- | Create a new simulation and store it in window
createSimulation :: Effect (Ref SimState)
createSimulation = do
  log "[SimManager] Creating simulation"
  stateRef <- Ref.new
    { nodes: []
    , links: []
    , forces: Map.empty
    , alpha: 1.0
    , alphaMin: 0.001
    , alphaDecay: 0.0228
    , alphaTarget: 0.0
    , velocityDecay: 0.4
    , running: false
    , tickCallback: pure unit
    }
  setSimulationInWindow_ stateRef
  pure stateRef

-- | Set nodes and initialize them
setNodes :: Array SpagoSimNode -> Ref SimState -> Effect Unit
setNodes nodes stateRef = do
  log $ "[SimManager] Setting " <> show (Array.length nodes) <> " nodes"
  initializeNodes_ nodes
  setSimNodes_ nodes  -- Store for identity check in applyForce_
  Ref.modify_ (_ { nodes = nodes }) stateRef

  -- Re-initialize all forces with new nodes
  state <- Ref.read stateRef
  for_ (Map.toUnfoldable state.forces :: Array (Tuple String ForceHandle)) \(Tuple _ handle) -> do
    _ <- initializeForce_ handle nodes
    pure unit

-- | Set links
setLinks :: forall linkData. Array { source :: Int, target :: Int | linkData } -> Ref SimState -> Effect Unit
setLinks links stateRef = do
  log $ "[SimManager] Setting " <> show (Array.length links) <> " links"
  -- Store links (we need to cast to our expected type)
  Ref.modify_ (_ { links = unsafeCoerceLinks links }) stateRef
  where
  unsafeCoerceLinks :: forall r. Array { source :: Int, target :: Int | r } -> Array { source :: Int, target :: Int, linktype :: String }
  unsafeCoerceLinks = map \l -> { source: l.source, target: l.target, linktype: "M2M_Tree" }

-- | Start the simulation
start :: Ref SimState -> Effect Unit
start stateRef = do
  state <- Ref.read stateRef
  unless state.running do
    log "[SimManager] Starting"
    Ref.modify_ (_ { running = true }) stateRef
    runLoop stateRef

-- | Stop the simulation
stop :: Ref SimState -> Effect Unit
stop stateRef = do
  log "[SimManager] Stopping"
  Ref.modify_ (_ { running = false }) stateRef

-- | Reheat the simulation
reheat :: Ref SimState -> Effect Unit
reheat stateRef = do
  log "[SimManager] Reheating"
  Ref.modify_ (_ { alpha = 1.0 }) stateRef
  state <- Ref.read stateRef
  unless state.running do
    Ref.modify_ (_ { running = true }) stateRef
    runLoop stateRef

-- | Set tick callback
setTickCallback :: Effect Unit -> Ref SimState -> Effect Unit
setTickCallback callback stateRef = do
  Ref.modify_ (_ { tickCallback = callback }) stateRef

-- =============================================================================
-- Animation Loop
-- =============================================================================

runLoop :: Ref SimState -> Effect Unit
runLoop stateRef = do
  state <- Ref.read stateRef

  when state.running do
    -- Apply all forces
    for_ (Map.values state.forces) \handle -> do
      applyForce_ handle state.alpha

    -- Integrate positions (with fixed node support)
    integratePositions_ state.nodes state.velocityDecay

    -- Decay alpha
    let newAlpha = decayAlpha_ state.alpha state.alphaMin state.alphaDecay state.alphaTarget
    Ref.modify_ (_ { alpha = newAlpha }) stateRef

    -- Call tick callback (for DOM updates)
    state.tickCallback

    -- Continue or stop
    if newAlpha > 0.0
      then void $ requestAnimationFrame_ (runLoop stateRef)
      else do
        log "[SimManager] Cooled down"
        Ref.modify_ (_ { running = false }) stateRef

-- =============================================================================
-- Force Creation
-- =============================================================================

-- | Collision force with dynamic radius based on node.r
createCollision :: { padding :: Number, strength :: Number, iterations :: Number } -> ForceHandle
createCollision cfg = createSpagoCollision_ cfg.padding cfg.strength cfg.iterations

-- | Charge (many-body) force
createCharge :: { strength :: Number, theta :: Number, distanceMin :: Number, distanceMax :: Number } -> ForceHandle
createCharge cfg = createSpagoCharge_ cfg.strength cfg.theta cfg.distanceMin cfg.distanceMax

-- | Charge force with filter (only applies to matching nodes)
createChargeFiltered ::
  { strength :: Number, theta :: Number, distanceMin :: Number, distanceMax :: Number }
  -> (SpagoSimNode -> Boolean)
  -> ForceHandle
createChargeFiltered cfg predicate =
  createSpagoChargeFiltered_ cfg.strength cfg.theta cfg.distanceMin cfg.distanceMax predicate

-- | Center force
createCenter :: { x :: Number, y :: Number, strength :: Number } -> ForceHandle
createCenter cfg = createSpagoCenter_ cfg.x cfg.y cfg.strength

-- | Link force
createLink :: { distance :: Number, strength :: Number, iterations :: Number } -> ForceHandle
createLink cfg = createSpagoLink_ cfg.distance cfg.strength cfg.iterations

-- | Cluster X force (pull modules toward package X)
createClusterX :: Number -> (SpagoSimNode -> Boolean) -> ForceHandle
createClusterX strength predicate = createSpagoClusterX_ strength predicate

-- | Cluster Y force (pull modules toward package Y)
createClusterY :: Number -> (SpagoSimNode -> Boolean) -> ForceHandle
createClusterY strength predicate = createSpagoClusterY_ strength predicate

-- | Radial force
createRadial :: { radius :: Number, x :: Number, y :: Number, strength :: Number } -> (SpagoSimNode -> Boolean) -> ForceHandle
createRadial cfg predicate = createSpagoRadial_ cfg.radius cfg.x cfg.y cfg.strength predicate

-- =============================================================================
-- Force Management
-- =============================================================================

-- | Add a force to the simulation
addForce :: String -> ForceHandle -> Ref SimState -> Effect Unit
addForce name handle stateRef = do
  state <- Ref.read stateRef
  -- Initialize with current nodes
  _ <- initializeForce_ handle state.nodes
  Ref.modify_ (\s -> s { forces = Map.insert name handle s.forces }) stateRef
  log $ "[SimManager] Added force: " <> name

-- | Clear all forces
clearForces :: Ref SimState -> Effect Unit
clearForces stateRef = do
  log "[SimManager] Clearing all forces"
  Ref.modify_ (_ { forces = Map.empty }) stateRef

-- | Initialize a force with nodes
initializeForce :: ForceHandle -> Array SpagoSimNode -> Effect ForceHandle
initializeForce = initializeForce_

-- | Initialize a link force with nodes and links
initializeLinkForce :: forall linkData.
  ForceHandle
  -> Array SpagoSimNode
  -> Array { source :: Int, target :: Int | linkData }
  -> Effect ForceHandle
initializeLinkForce = initializeLinkForce_

-- =============================================================================
-- Swizzling
-- =============================================================================

-- | Swizzle links - replace IDs with node references
swizzleLinks :: forall linkData.
  Array SpagoSimNode
  -> Array { source :: Int, target :: Int | linkData }
  -> Array (SwizzledLink SpagoSimNode linkData)
swizzleLinks nodes rawLinks =
  let nodeMap = Map.fromFoldable $ map (\n -> Tuple n.id n) nodes
  in Array.mapMaybe (swizzle nodeMap) rawLinks
  where
  swizzle nodeMap raw = do
    src <- Map.lookup raw.source nodeMap
    tgt <- Map.lookup raw.target nodeMap
    pure $ buildSwizzledLink_ src tgt raw

-- =============================================================================
-- Debug
-- =============================================================================

-- | Log simulation state
logState :: String -> Ref SimState -> Effect Unit
logState label stateRef = do
  state <- Ref.read stateRef
  logForceState_ label state.nodes state.alpha
