module D3.Viz.ForceNavigator.Forces where

import Prelude

import PSD3.Internal.Attributes.Instances (Label)
import D3.Viz.ForceNavigator.Model (NavigationSimNode, NodeType(..))
import PSD3.Internal.Simulation.Config as F
import PSD3.Internal.Simulation.Forces (createForce, createLinkForce, initialize)
import PSD3.Internal.Simulation.Types (Force, ForceType(..), RegularForceType(..), allNodes)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Number (infinity)
import Unsafe.Coerce (unsafeCoerce)

-- | Helper to get collision radius based on node type
-- | Can be customized per nodeType for different spacing
collisionRadius :: forall d. d -> Number
collisionRadius d = do
  let node = unsafeCoerce d :: NavigationSimNode
  case node.nodeType of
    _ -> 80.0  -- Use consistent collision radius for spacing

-- | Force Library - Central definition of all physics forces
-- |
-- | **Pattern:**
-- | Define all forces your visualization needs in one place. Each force has:
-- | - A unique label (used for enable/disable in forceStatuses)
-- | - A force type (ManyBody, Center, Collide, Link)
-- | - Configuration parameters (strength, distance, etc.)
-- |
-- | **Common forces:**
-- | - `ForceManyBody`: Charge force (positive = attract, negative = repel)
-- | - `ForceCenter`: Keeps nodes centered at a point
-- | - `ForceCollide`: Prevents nodes from overlapping
-- | - Link force: Connects nodes (created via `createLinkForce`)
-- |
-- | **All forces start enabled (On) by default** - see State.purs where
-- | `forceStatuses` is initialized from this library via `getStatusMap`
forceLibrary :: forall d. Map Label (Force d)
forceLibrary = initialize [
    -- Strong charge to spread nodes out
    createForce "charge" (RegularForce ForceManyBody) allNodes [
        F.strengthVal (-80.0)
      , F.thetaVal 0.9
      , F.distanceMinVal 1.0
      , F.distanceMaxVal infinity
      ]

    -- Center force to keep graph centered
  , createForce "center" (RegularForce ForceCenter) allNodes [
        F.strengthVal 0.5
      , F.xVal 0.0
      , F.yVal 0.0
      ]

    -- Collision force to prevent overlap
  , createForce "collision" (RegularForce ForceCollide) allNodes [
        F.strengthVal 1.0
      , F.radiusFn (\d _ -> collisionRadius d)
      ]

    -- Link force to connect parent-child nodes
  , createLinkForce Nothing [
        F.strengthVal 0.5
      , F.distanceVal 150.0
      ]
  ]
