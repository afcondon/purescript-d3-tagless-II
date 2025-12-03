-- | Scene Orchestration Module
-- |
-- | Declarative scene management with transition lifecycle.
-- |
-- | A scene defines:
-- | - Initial rules applied before transition
-- | - Target layout positions
-- | - Final rules applied after transition
-- | - Stable engine mode (Static or Physics)
-- |
-- | All types are parameterized by node type for flexibility.
module PSD3.Simulation.Scene
  ( -- Types
    NodeRule
  , SceneConfig
  , TransitionState
  , EngineMode(..)
  , ForceConfig
  , CSSConfig
  , PositionMap
  -- Functions
  , applyRules
  , applyRulesInPlace_
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect.Ref (Ref)
import Foreign.Object (Object)
import PSD3.Transition.Tick as Tick

-- =============================================================================
-- Types
-- =============================================================================

-- | Position map: node ID -> {x, y}
type PositionMap = Object { x :: Number, y :: Number }

-- | Force configuration for Physics engine
-- | Currently a placeholder - could specify force strengths, etc.
type ForceConfig =
  { -- Forces are already in the simulation, this just tracks which scene we're in
    -- In future: could specify force strengths, etc.
  }

-- | CSS transition configuration
type CSSConfig =
  { selector :: String      -- CSS selector for elements to transition
  , property :: String      -- CSS property (e.g., "opacity")
  , targetValue :: String   -- Target value (e.g., "0")
  , duration :: Number      -- Duration in ms
  }

-- | Engine mode determines what drives node positions
data EngineMode
  = Physics ForceConfig                    -- D3 simulation runs, forces apply
  | Static                                 -- Nodes stay where they are (pinned)

-- =============================================================================
-- Node Rules (D3-like selection + transform)
-- =============================================================================

-- | A rule that selects nodes and applies a transform
-- | Like CSS: selector { properties }
-- | Parameterized by node type for flexibility
type NodeRule node =
  { name :: String                         -- For debugging
  , select :: node -> Boolean              -- Which nodes this applies to
  , apply :: node -> node                  -- What to do to them
  }

-- | Apply rules to nodes (first matching rule wins) - creates new array
-- | Generic over node type
applyRules :: forall node. Array (NodeRule node) -> Array node -> Array node
applyRules rules nodes = map (applyFirstMatch rules) nodes
  where
  applyFirstMatch :: Array (NodeRule node) -> node -> node
  applyFirstMatch rs node =
    case Array.find (\r -> r.select node) rs of
      Just r -> r.apply node
      Nothing -> node

-- =============================================================================
-- Scene Configuration
-- =============================================================================

-- | Scene configuration with three-phase lifecycle:
-- | 1. Initialize: Rules applied before transition starts
-- | 2. Transition: DumbEngine interpolates to layout positions
-- | 3. Finalize: Rules applied after transition completes
-- | Parameterized by node type for flexibility
type SceneConfig node =
  { name :: String

  -- Phase 1: Initialize (before transition)
  , initRules :: Array (NodeRule node)

  -- Phase 2: Transition (DumbEngine targets)
  , layout :: Array node -> PositionMap

  -- Phase 3: Finalize (after transition)
  -- Takes all nodes as context for building rules that need cross-node info
  , finalRules :: Array node -> Array (NodeRule node)

  -- Stable state
  , stableMode :: EngineMode
  , cssTransition :: Maybe CSSConfig
  }

-- | Transition state while DumbEngine is running
-- | Parameterized by node type for flexibility
type TransitionState node =
  { targetScene :: SceneConfig node
  , startPositions :: PositionMap
  , targetPositions :: PositionMap
  , progress :: Tick.Progress
  }

-- =============================================================================
-- In-Place Mutation (FFI)
-- =============================================================================

-- | Apply rules in place with first-match-wins semantics (CSS-like cascade)
-- | For each node, find the first matching rule and apply it via Object.assign.
-- | Preserves object identity for D3 data binding.
-- |
-- | Note: Mutates in place, not wrapped in Effect.
-- | TODO: Consider proper ST-based approach for principled mutation tracking.
foreign import applyRulesInPlace_
  :: forall node
   . Array (NodeRule node)
  -> Ref (Array node)
  -> Unit
