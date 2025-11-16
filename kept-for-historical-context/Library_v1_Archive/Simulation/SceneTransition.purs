module PSD3.Simulation.SceneTransition where

import Prelude

import Effect (Effect)
import PSD3.Internal.Types (D3Selection_)
import PSD3.Simulation.Scene (TransitionSpec, encodeEnterBehavior, encodeExitBehavior, encodeUpdateBehavior)
import PSD3.Data.Node (SimulationNode)

-- ============================================================================
-- Scene Transition Execution
-- ============================================================================
-- |
-- | This module implements the bridge between declarative TransitionSpec
-- | configuration and D3's imperative transition API.
-- |
-- | Scene transitions coordinate three behaviors:
-- | 1. Enter: How new nodes appear (fade in, scale up, instant)
-- | 2. Exit: How old nodes disappear (fade out, scale down, instant)
-- | 3. Update: How existing nodes move to new positions (animated, instant)
-- |
-- | These are choreographed together with proper timing and simulation lifecycle.

-- | FFI-compatible transition spec with string-encoded behaviors
-- | This ensures we control exactly what strings are passed to JavaScript
type TransitionSpec_ =
  { duration :: Number
  , enterNodes :: String
  , exitNodes :: String
  , updateNodes :: String
  }

-- | Convert TransitionSpec with ADTs to FFI-compatible version with strings
encodeTransitionSpec :: TransitionSpec -> TransitionSpec_
encodeTransitionSpec spec =
  { duration: spec.duration
  , enterNodes: encodeEnterBehavior spec.enterNodes
  , exitNodes: encodeExitBehavior spec.exitNodes
  , updateNodes: encodeUpdateBehavior spec.updateNodes
  }

-- | Execute a complete scene transition
-- |
-- | This function coordinates the entire scene transition workflow:
-- | 1. Apply enter behavior to entering nodes
-- | 2. Apply exit behavior to exiting nodes
-- | 3. Apply update behavior to updating nodes (with new positions)
-- | 4. Call completion callback when all transitions finish
-- |
-- | Parameters:
-- | - spec: TransitionSpec defining duration and behaviors
-- | - nodeSelector: CSS selector for node elements (e.g., "g.nodes > g")
-- | - linkSelector: CSS selector for link elements (e.g., "g.links > line")
-- | - updatedNodes: Array of nodes with target positions set (fx/fy for pinned, x/y otherwise)
-- | - onComplete: Effect to run when transition completes
executeSceneTransition_
  :: forall d.
     TransitionSpec
  -> String                        -- Node selector
  -> String                        -- Link selector
  -> Array (SimulationNode d)      -- Nodes with target positions
  -> Effect Unit                   -- Completion callback
  -> Effect Unit
executeSceneTransition_ spec =
  executeSceneTransition_Impl (encodeTransitionSpec spec)

-- | Internal FFI function - uses string-encoded behaviors
foreign import executeSceneTransition_Impl
  :: forall d.
     TransitionSpec_
  -> String                        -- Node selector
  -> String                        -- Link selector
  -> Array (SimulationNode d)      -- Nodes with target positions
  -> Effect Unit                   -- Completion callback
  -> Effect Unit
