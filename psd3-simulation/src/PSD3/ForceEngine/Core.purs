-- | Pure Force Engine Core
-- |
-- | This module provides the core force simulation loop.
-- | We use D3's force calculation algorithms but manage the simulation ourselves.
-- |
-- | Key principle: Forces are just functions that mutate vx/vy on nodes.
-- | We control when they run and how alpha decays.
module PSD3.ForceEngine.Core
  ( -- * Force Handles (opaque)
    ForceHandle
    -- * Force Creation
  , createManyBody
  , createCollide
  , createLink
  , createCenter
  , createForceX
  , createForceY
  , createRadial
    -- * Filtered/Dynamic Forces
  , createManyBodyFiltered
  , createRadialFiltered
  , createCollideDynamic
  , createForceXDynamic
  , createForceYDynamic
    -- * Optimized Grid Forces (no FFI callbacks)
  , createForceXGrid
  , createForceYGrid
  , createCollideGrid
    -- * Initialization
  , initializeNodes
  , initializeForce
  , initializeLinkForce
    -- * Simulation Step
  , applyForce
  , applyForces
  , integratePositions
  , decayAlpha
  , simulationTick
    -- * Animation
  , AnimationHandle
  , startAnimation
  , stopAnimation
    -- * Drag Behavior
  , attachDragWithReheat
  , attachGroupDragWithReheat
    -- * Debug
  , logNodes
  ) where

import Prelude

import Data.Traversable (for_)
import Effect (Effect)
import PSD3.ForceEngine.Types (ManyBodyConfig, CollideConfig, LinkConfig, CenterConfig, ForceXConfig, ForceYConfig, RadialConfig, ManyBodyFilteredConfig, RadialFilteredConfig, CollideDynamicConfig, ForceXDynamicConfig, ForceYDynamicConfig)
import Web.DOM.Element (Element)

-- =============================================================================
-- Foreign Imports
-- =============================================================================

-- Force handles are opaque JavaScript objects
foreign import data ForceHandle :: Type

-- Force creation
foreign import createManyBody_ :: ManyBodyConfig -> ForceHandle
foreign import createCollide_ :: CollideConfig -> ForceHandle
foreign import createLink_ :: LinkConfig -> ForceHandle
foreign import createCenter_ :: CenterConfig -> ForceHandle
foreign import createForceX_ :: ForceXConfig -> ForceHandle
foreign import createForceY_ :: ForceYConfig -> ForceHandle
foreign import createRadial_ :: RadialConfig -> ForceHandle

-- Filtered/Dynamic force creation
foreign import createManyBodyFiltered_ :: forall node. ManyBodyFilteredConfig node -> ForceHandle
foreign import createRadialFiltered_ :: forall node. RadialFilteredConfig node -> ForceHandle
foreign import createCollideDynamic_ :: forall node. CollideDynamicConfig node -> ForceHandle
foreign import createForceXDynamic_ :: forall node. ForceXDynamicConfig node -> ForceHandle
foreign import createForceYDynamic_ :: forall node. ForceYDynamicConfig node -> ForceHandle

-- Optimized Grid force creation (no PureScript callbacks - reads node properties directly)
foreign import createForceXGrid_ :: Number -> ForceHandle
foreign import createForceYGrid_ :: Number -> ForceHandle
foreign import createCollideGrid_ :: Number -> Number -> Int -> ForceHandle

-- Initialization
foreign import initializeNodes_ :: forall r. Array { | r } -> Effect Unit
foreign import initializeForce_ :: forall r. ForceHandle -> Array { | r } -> Effect ForceHandle
foreign import initializeLinkForce_ :: forall nodeRow linkRow. ForceHandle -> Array { | nodeRow } -> Array { | linkRow } -> Effect ForceHandle

-- Force application
foreign import applyForce_ :: ForceHandle -> Number -> Effect Unit

-- Position integration
foreign import integratePositions_ :: forall r. Array { | r } -> Number -> Effect Unit

-- Alpha decay (pure calculation)
foreign import decayAlpha_ :: Number -> Number -> Number -> Number -> Number

-- Animation
foreign import requestAnimationFrame_ :: (Number -> Effect Unit) -> Effect (Effect Unit)

-- Debug
foreign import logNodes_ :: forall r. String -> Array { | r } -> Effect Unit

-- =============================================================================
-- Force Creation (Pure)
-- =============================================================================

-- | Create a many-body (charge) force
createManyBody :: ManyBodyConfig -> ForceHandle
createManyBody = createManyBody_

-- | Create a collision force
createCollide :: CollideConfig -> ForceHandle
createCollide = createCollide_

-- | Create a link force
createLink :: LinkConfig -> ForceHandle
createLink = createLink_

-- | Create a centering force
createCenter :: CenterConfig -> ForceHandle
createCenter = createCenter_

-- | Create an X positioning force
createForceX :: ForceXConfig -> ForceHandle
createForceX = createForceX_

-- | Create a Y positioning force
createForceY :: ForceYConfig -> ForceHandle
createForceY = createForceY_

-- | Create a radial force
createRadial :: RadialConfig -> ForceHandle
createRadial = createRadial_

-- =============================================================================
-- Filtered/Dynamic Force Creation (Pure)
-- =============================================================================

-- | Create a many-body force that only applies to nodes matching a predicate
-- | Useful for applying charge only to certain node types (e.g., tree parents)
createManyBodyFiltered :: forall node. ManyBodyFilteredConfig node -> ForceHandle
createManyBodyFiltered = createManyBodyFiltered_

-- | Create a radial force that only applies to nodes matching a predicate
createRadialFiltered :: forall node. RadialFilteredConfig node -> ForceHandle
createRadialFiltered = createRadialFiltered_

-- | Create a collision force with dynamic radius per-node
-- | The radiusAccessor function is called for each node to determine collision radius
-- | Example: `{ radiusAccessor: \n -> n.r + 5.0, strength: 1.0, iterations: 1 }`
createCollideDynamic :: forall node. CollideDynamicConfig node -> ForceHandle
createCollideDynamic = createCollideDynamic_

-- | Create an X positioning force with dynamic target per-node
-- | The xAccessor function is called for each node to determine target X
-- | Useful for clustering (e.g., modules toward their parent package's X)
createForceXDynamic :: forall node. ForceXDynamicConfig node -> ForceHandle
createForceXDynamic = createForceXDynamic_

-- | Create a Y positioning force with dynamic target per-node
createForceYDynamic :: forall node. ForceYDynamicConfig node -> ForceHandle
createForceYDynamic = createForceYDynamic_

-- =============================================================================
-- Optimized Grid Forces (no FFI callbacks)
-- =============================================================================

-- | Create an X positioning force that reads node.gridX directly
-- | Much faster than createForceXDynamic because it avoids FFI callbacks.
-- | Nodes must have a `gridX :: Number` field.
createForceXGrid :: Number -> ForceHandle
createForceXGrid = createForceXGrid_

-- | Create a Y positioning force that reads node.gridY directly
-- | Much faster than createForceYDynamic because it avoids FFI callbacks.
-- | Nodes must have a `gridY :: Number` field.
createForceYGrid :: Number -> ForceHandle
createForceYGrid = createForceYGrid_

-- | Create a collision force that reads node.r directly
-- | Much faster than createCollideDynamic because it avoids FFI callbacks.
-- | Nodes must have an `r :: Number` field (radius).
-- | Parameters: padding, strength, iterations
createCollideGrid :: Number -> Number -> Int -> ForceHandle
createCollideGrid = createCollideGrid_

-- =============================================================================
-- Initialization (Effectful)
-- =============================================================================

-- | Initialize nodes with indices and default velocities
-- | This mutates the nodes array to add index, vx, vy if missing
initializeNodes :: forall r. Array { | r } -> Effect Unit
initializeNodes = initializeNodes_

-- | Initialize a force with nodes
-- | Must be called before applying the force
initializeForce :: forall r. ForceHandle -> Array { | r } -> Effect ForceHandle
initializeForce = initializeForce_

-- | Initialize a link force with nodes and links
-- | Link forces need both nodes and links
initializeLinkForce :: forall nodeRow linkRow. ForceHandle -> Array { | nodeRow } -> Array { | linkRow } -> Effect ForceHandle
initializeLinkForce = initializeLinkForce_

-- =============================================================================
-- Simulation Step
-- =============================================================================

-- | Apply a single force
-- | This mutates vx/vy on the nodes the force was initialized with
applyForce :: ForceHandle -> Number -> Effect Unit
applyForce = applyForce_

-- | Apply multiple forces in sequence
applyForces :: Array ForceHandle -> Number -> Effect Unit
applyForces forces alpha = for_ forces \f -> applyForce f alpha

-- | Integrate positions: apply velocity decay and update positions
-- | Call this once per tick after all forces have been applied
integratePositions :: forall r. Array { | r } -> Number -> Effect Unit
integratePositions = integratePositions_

-- | Calculate new alpha value (the "cooling" step)
-- | Returns 0 if alpha falls below alphaMin
decayAlpha :: Number -> Number -> Number -> Number -> Number
decayAlpha = decayAlpha_

-- | Complete simulation tick:
-- | 1. Apply all forces
-- | 2. Integrate positions
-- | 3. Return new alpha
simulationTick :: forall r.
  { forces :: Array ForceHandle
  , nodes :: Array { | r }
  , alpha :: Number
  , alphaMin :: Number
  , alphaDecay :: Number
  , alphaTarget :: Number
  , velocityDecay :: Number
  }
  -> Effect Number  -- Returns new alpha
simulationTick params = do
  -- Apply all forces
  applyForces params.forces params.alpha

  -- Integrate positions (velocity decay + position update)
  integratePositions params.nodes params.velocityDecay

  -- Return decayed alpha
  pure $ decayAlpha params.alpha params.alphaMin params.alphaDecay params.alphaTarget

-- =============================================================================
-- Animation Loop
-- =============================================================================

-- | Handle to a running animation (can be used to stop it)
type AnimationHandle = Effect Unit

-- | Start an animation loop
-- | The callback receives the current timestamp and should return whether to continue
startAnimation :: (Number -> Effect Boolean) -> Effect AnimationHandle
startAnimation onFrame = do
  -- We store the cancel function in a mutable ref
  cancelRef <- newCancelRef
  let
    loop timestamp = do
      continue <- onFrame timestamp
      when continue do
        cancel <- requestAnimationFrame_ loop
        setCancelRef cancelRef cancel
  -- Start the loop
  cancel <- requestAnimationFrame_ loop
  setCancelRef cancelRef cancel
  -- Return a function that cancels the animation
  pure $ getCancelRef cancelRef >>= identity

-- Foreign helpers for mutable cancel ref
foreign import newCancelRef :: Effect CancelRef
foreign import setCancelRef :: CancelRef -> Effect Unit -> Effect Unit
foreign import getCancelRef :: CancelRef -> Effect (Effect Unit)
foreign import data CancelRef :: Type

-- | Stop a running animation
stopAnimation :: AnimationHandle -> Effect Unit
stopAnimation cancel = cancel

-- =============================================================================
-- Drag Behavior
-- =============================================================================

-- | Attach simulation-aware drag behavior to node elements
-- |
-- | This sets up D3 drag handlers that:
-- | 1. Call the reheat callback on drag start
-- | 2. Update fx/fy (fixed position) during drag
-- | 3. Clear fx/fy on drag end (release node)
-- |
-- | The reheat callback should restart the simulation when dragging begins.
foreign import attachDragWithReheat_ :: Array Element -> Effect Unit -> Effect Unit

-- | Attach drag with a reheat callback
attachDragWithReheat :: Array Element -> Effect Unit -> Effect Unit
attachDragWithReheat = attachDragWithReheat_

-- | FFI for group drag (uses container for coordinate space)
foreign import attachGroupDragWithReheat_ :: Array Element -> String -> Effect Unit -> Effect Unit

-- | Attach drag to transformed group elements (like bubble packs)
-- |
-- | Unlike `attachDragWithReheat`, this version takes a container selector to
-- | get pointer coordinates in the correct coordinate space. Use this when
-- | dragging `<g>` elements that have `transform` attributes.
-- |
-- | Example:
-- | ```purescript
-- | -- For bubble packs inside a zoom group
-- | attachGroupDragWithReheat packElements "#zoom-group" (Sim.reheat sim)
-- | ```
attachGroupDragWithReheat :: Array Element -> String -> Effect Unit -> Effect Unit
attachGroupDragWithReheat = attachGroupDragWithReheat_

-- =============================================================================
-- Debug
-- =============================================================================

-- | Log node positions for debugging
logNodes :: forall r. String -> Array { | r } -> Effect Unit
logNodes = logNodes_
