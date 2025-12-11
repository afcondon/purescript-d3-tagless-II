-- | Scene Types
-- |
-- | Core type definitions for the scene orchestration system.
-- |
-- | A scene represents a visualization state with:
-- | - A target layout (where nodes should be)
-- | - Rules for initialization and finalization
-- | - A stable mode (physics simulation or static)
-- |
-- | The scene system supports smooth transitions between states
-- | using an interpolation engine for position animation.
module PSD3.Scene.Types
  ( -- * Core Types
    SceneConfig
  , TransitionConfig
  , TransitionState
  , EngineMode(..)
    -- * Position Types
  , PositionMap
  , Position
    -- * Rule Types
  , NodeRule
    -- * Defaults
  , defaultTransition
  ) where

import Prelude

import Foreign.Object (Object)
import PSD3.Transition.Tick as Tick

-- =============================================================================
-- Position Types
-- =============================================================================

-- | A position in 2D space
type Position = { x :: Number, y :: Number }

-- | Position map: node ID (as string) -> position
-- | Used for capturing current positions and specifying targets
type PositionMap = Object Position

-- =============================================================================
-- Rule Types
-- =============================================================================

-- | A rule that selects nodes and applies a transform.
-- |
-- | Rules are applied with first-match-wins semantics (like CSS cascade).
-- | If multiple rules match a node, only the first one applies.
-- |
-- | Example:
-- | ```purescript
-- | pinPackages :: NodeRule MyNode
-- | pinPackages =
-- |   { name: "pinPackages"
-- |   , select: \n -> n.nodeType == Package
-- |   , apply: \n -> n { fx = notNull n.x, fy = notNull n.y }
-- |   }
-- | ```
type NodeRule node =
  { name :: String                -- For debugging/logging
  , select :: node -> Boolean     -- Which nodes this rule applies to
  , apply :: node -> node         -- Transform to apply
  }

-- =============================================================================
-- Engine Mode
-- =============================================================================

-- | Engine mode determines what happens after a transition completes.
-- |
-- | - `Physics`: D3 force simulation runs, nodes settle via forces
-- | - `Static`: Nodes stay pinned at their final positions
data EngineMode
  = Physics   -- Force simulation active
  | Static    -- Nodes pinned, no simulation

derive instance eqEngineMode :: Eq EngineMode

instance showEngineMode :: Show EngineMode where
  show Physics = "Physics"
  show Static = "Static"

-- =============================================================================
-- Transition Configuration
-- =============================================================================

-- | Configuration for position transitions.
-- |
-- | Controls the duration and easing of the interpolation engine.
type TransitionConfig =
  { duration :: Number              -- Duration in milliseconds
  , easing :: Number -> Number      -- Easing function (0→1 input, 0→1 output)
  }

-- | Default transition: 2 seconds with ease-in-out-cubic
defaultTransition :: TransitionConfig
defaultTransition =
  { duration: 2000.0
  , easing: Tick.easeInOutCubic
  }

-- =============================================================================
-- Scene Configuration
-- =============================================================================

-- | Scene configuration with three-phase lifecycle.
-- |
-- | **Phase 1: Initialize** (`initRules`)
-- | Applied before transition starts. Use this to set up starting positions,
-- | e.g., moving tree nodes to the root for a "grow from root" animation.
-- |
-- | **Phase 2: Transition** (`layout`)
-- | The interpolation engine smoothly moves nodes from their current positions
-- | to the target positions computed by the layout function.
-- |
-- | **Phase 3: Finalize** (`finalRules`)
-- | Applied after transition completes. Use this to set up the stable state,
-- | e.g., unpinning nodes so forces can take over, or setting gridX/gridY.
-- |
-- | Example:
-- | ```purescript
-- | treeFormScene :: SceneConfig MyNode
-- | treeFormScene =
-- |   { name: "TreeForm"
-- |   , initRules: [ moveToRootRule ]
-- |   , layout: \nodes -> computeTreePositions nodes
-- |   , finalRules: \_ -> [ pinAtTreePositionsRule ]
-- |   , stableMode: Static
-- |   , transition: defaultTransition
-- |   }
-- | ```
type SceneConfig node =
  { name :: String

  -- Phase 1: Initialize (before transition)
  , initRules :: Array (NodeRule node)

  -- Phase 2: Transition target positions
  , layout :: Array node -> PositionMap

  -- Phase 3: Finalize (after transition)
  -- Takes all nodes as context for rules that need cross-node info
  , finalRules :: Array node -> Array (NodeRule node)

  -- Stable state configuration
  , stableMode :: EngineMode
  , transition :: TransitionConfig
  }

-- =============================================================================
-- Transition State
-- =============================================================================

-- | Runtime state during an active transition.
-- |
-- | Tracks the target scene, start/end positions, and current progress.
-- | The interpolation engine updates progress each tick until complete.
type TransitionState node =
  { targetScene :: SceneConfig node
  , startPositions :: PositionMap
  , targetPositions :: PositionMap
  , progress :: Tick.Progress        -- 0.0 to 1.0
  , elapsed :: Number                -- Milliseconds elapsed
  }
