-- | Scene Orchestration Module (Legacy)
-- |
-- | This module re-exports from the new PSD3.Scene modules for backwards compatibility.
-- |
-- | For new code, prefer importing from:
-- | - `PSD3.Scene` (umbrella module)
-- | - `PSD3.Scene.Types` (types)
-- | - `PSD3.Scene.Rules` (rule helpers)
-- | - `PSD3.Scene.Engine` (orchestration)
-- | - `PSD3.Scene.Transition` (position interpolation)
module PSD3.Simulation.Scene
  ( -- Types
    NodeRule
  , SceneConfig
  , TransitionState
  , CSSConfig
  , PositionMap
    -- Legacy types (kept for compatibility)
  , ForceConfig
    -- Functions
  , applyRules
    -- FFI
  , applyRulesInPlace_
    -- Re-export EngineMode from new module
  , module ReExport
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect.Ref (Ref)
import Foreign.Object (Object)
import PSD3.Scene.Types (EngineMode(..)) as ReExport
import PSD3.Scene.Types (EngineMode)
import PSD3.Transition.Tick as Tick

-- =============================================================================
-- Types (some re-exported, some kept for backwards compat)
-- =============================================================================

-- | Position map: node ID -> {x, y}
type PositionMap = Object { x :: Number, y :: Number }

-- | A rule that selects nodes and applies a transform
type NodeRule node =
  { name :: String
  , select :: node -> Boolean
  , apply :: node -> node
  }

-- | CSS transition configuration
type CSSConfig =
  { selector :: String
  , property :: String
  , targetValue :: String
  , duration :: Number
  }

-- =============================================================================
-- Legacy Types (kept for backwards compatibility)
-- =============================================================================

-- | Force configuration for Physics engine
-- | Currently a placeholder - could specify force strengths, etc.
-- | NOTE: New code should use `EngineMode = Physics` without ForceConfig
type ForceConfig =
  { -- Forces are already in the simulation, this just tracks which scene we're in
  }

-- | Scene configuration with three-phase lifecycle
-- | NOTE: This is the legacy version without TransitionConfig.
-- | New code should add `transition :: TransitionConfig` field.
type SceneConfig node =
  { name :: String
  , initRules :: Array (NodeRule node)
  , layout :: Array node -> PositionMap
  , finalRules :: Array node -> Array (NodeRule node)
  , stableMode :: EngineMode
  , cssTransition :: Maybe CSSConfig
  }

-- | Transition state while interpolation engine is running
-- | NOTE: This is the legacy version without elapsed time.
-- | New code should add `elapsed :: Number` field.
type TransitionState node =
  { targetScene :: SceneConfig node
  , startPositions :: PositionMap
  , targetPositions :: PositionMap
  , progress :: Tick.Progress
  }

-- =============================================================================
-- Functions
-- =============================================================================

-- | Apply rules to nodes (first matching rule wins) - creates new array
applyRules :: forall node. Array (NodeRule node) -> Array node -> Array node
applyRules rules nodes = map (applyFirstMatch rules) nodes
  where
  applyFirstMatch :: Array (NodeRule node) -> node -> node
  applyFirstMatch rs node =
    case Array.find (\r -> r.select node) rs of
      Just r -> r.apply node
      Nothing -> node

-- =============================================================================
-- In-Place Mutation (FFI)
-- =============================================================================

-- | Apply rules in place with first-match-wins semantics (CSS-like cascade)
-- | For each node, find the first matching rule and apply it via Object.assign.
-- | Preserves object identity for D3 data binding.
foreign import applyRulesInPlace_
  :: forall node
   . Array (NodeRule node)
  -> Ref (Array node)
  -> Unit
