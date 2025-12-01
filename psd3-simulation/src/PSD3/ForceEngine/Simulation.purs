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
  ( -- * Simulation State
    Simulation
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
  , getSwizzledLinks
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import PSD3.ForceEngine.Core as Core
import PSD3.ForceEngine.Types (ForceSpec(..), defaultSimParams)
import Unsafe.Coerce (unsafeCoerce)

-- =============================================================================
-- Types
-- =============================================================================

-- | A running simulation
-- | This is a mutable structure that holds the simulation state
type Simulation nodeRow linkRow =
  { nodes :: Ref (Array { x :: Number, y :: Number, vx :: Number, vy :: Number | nodeRow })
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
create :: forall nodeRow linkRow. SimConfig -> Effect (Simulation nodeRow linkRow)
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
setNodes :: forall nodeRow linkRow.
  Array { x :: Number, y :: Number, vx :: Number, vy :: Number | nodeRow }
  -> Simulation nodeRow linkRow
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
setLinks :: forall nodeRow linkRow.
  Array { source :: Int, target :: Int | linkRow }
  -> Simulation nodeRow linkRow
  -> Effect Unit
setLinks links sim = do
  Ref.write links sim.links
  -- TODO: Re-initialize link forces when we track force types

-- | Add a force to the simulation
addForce :: forall nodeRow linkRow.
  ForceSpec
  -> Simulation nodeRow linkRow
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
removeForce :: forall nodeRow linkRow.
  String
  -> Simulation nodeRow linkRow
  -> Effect Unit
removeForce name sim = do
  Ref.modify_ (Map.delete name) sim.forces

-- =============================================================================
-- Running
-- =============================================================================

-- | Start the simulation animation loop
start :: forall nodeRow linkRow.
  Simulation nodeRow linkRow
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
stop :: forall nodeRow linkRow.
  Simulation nodeRow linkRow
  -> Effect Unit
stop sim = do
  Ref.write false sim.running
  cancel <- Ref.read sim.cancelAnimation
  cancel

-- | Run a single tick of the simulation
-- | Returns the new alpha value
tick :: forall nodeRow linkRow.
  Simulation nodeRow linkRow
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
reheat :: forall nodeRow linkRow.
  Simulation nodeRow linkRow
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
onTick :: forall nodeRow linkRow.
  Effect Unit
  -> Simulation nodeRow linkRow
  -> Effect Unit
onTick callback sim = do
  Ref.write callback sim.tickCallback

-- =============================================================================
-- Query
-- =============================================================================

-- | Check if the simulation is running
isRunning :: forall nodeRow linkRow.
  Simulation nodeRow linkRow
  -> Effect Boolean
isRunning sim = Ref.read sim.running

-- | Get the current alpha value
getAlpha :: forall nodeRow linkRow.
  Simulation nodeRow linkRow
  -> Effect Number
getAlpha sim = Ref.read sim.alpha

-- | Get the current nodes (with updated positions)
getNodes :: forall nodeRow linkRow.
  Simulation nodeRow linkRow
  -> Effect (Array { x :: Number, y :: Number, vx :: Number, vy :: Number | nodeRow })
getNodes sim = Ref.read sim.nodes

-- | Get links with node references instead of indices ("swizzled")
-- |
-- | IMPORTANT: Only call this AFTER adding a Link force with `addForce (Link ...)`.
-- | The Link force initialization mutates link objects, replacing integer
-- | source/target indices with actual node object references.
-- |
-- | The returned links have the same shape as nodes for source/target,
-- | so you can read `link.source.x`, `link.source.y`, etc.
-- |
-- | Example:
-- | ```purescript
-- | -- After setup:
-- | Sim.addForce (Link "links" defaultLink { ... }) sim
-- |
-- | -- Get swizzled links for rendering:
-- | swizzled <- Sim.getSwizzledLinks sim
-- | linkSel <- appendData Line swizzled [x1 (_.source.x), ...]
-- | ```
getSwizzledLinks :: forall nodeRow linkRow.
  Simulation nodeRow linkRow
  -> Effect (Array { source :: { x :: Number, y :: Number, vx :: Number, vy :: Number | nodeRow }
                   , target :: { x :: Number, y :: Number, vx :: Number, vy :: Number | nodeRow }
                   | linkRow })
getSwizzledLinks sim = do
  links <- Ref.read sim.links
  -- D3's forceLink mutates links in place, replacing Int indices with node objects.
  -- At runtime, link.source IS a node object. We just need to tell PureScript's
  -- type system about this reality.
  pure (unsafeCoerce links)
