module PSD3.Simulation.SceneTransition where

import Prelude

import Effect (Effect)
import PSD3.Internal.Types (D3Selection_)
import PSD3.Simulation.Scene (TransitionSpec, EnterBehavior, ExitBehavior, UpdateBehavior)
import PSD3.Data.Node (D3_SimulationNode)

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
foreign import executeSceneTransition_
  :: forall d.
     TransitionSpec
  -> String                        -- Node selector
  -> String                        -- Link selector
  -> Array (D3_SimulationNode d)   -- Nodes with target positions
  -> Effect Unit                   -- Completion callback
  -> Effect Unit

-- | Execute a scene transition on specific selections (advanced usage)
-- |
-- | This variant gives you direct control over the selections, useful when
-- | you've already queried them or need to apply the transition to a subset.
-- |
-- | Note: This is lower-level API. Most users should use executeSceneTransition_.
foreign import executeSceneTransitionOnSelections_
  :: forall d.
     TransitionSpec
  -> D3Selection_                  -- Enter selection
  -> D3Selection_                  -- Update selection
  -> D3Selection_                  -- Exit selection
  -> Array (D3_SimulationNode d)   -- Nodes with target positions
  -> Effect Unit                   -- Completion callback
  -> Effect Unit
