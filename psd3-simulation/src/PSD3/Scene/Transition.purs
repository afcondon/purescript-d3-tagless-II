-- | Scene Transition Utilities
-- |
-- | Position interpolation and transition progress management.
-- |
-- | The interpolation engine smoothly moves nodes from start to target
-- | positions over a configurable duration with easing.
module PSD3.Scene.Transition
  ( -- * Position Capture
    capturePositions
  , capturePositionsBy
    -- * Progress Calculation
  , calculateProgress
  , isComplete
    -- * Position Lookup
  , lookupPosition
    -- * Re-exports
  , module Types
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import PSD3.Scene.Types (PositionMap, Position, TransitionConfig) as Types

-- =============================================================================
-- Position Capture
-- =============================================================================

-- | Capture current positions from an array of nodes.
-- |
-- | Uses the node's `id` field (converted to String) as the key.
-- | Requires nodes to have `id :: Int`, `x :: Number`, `y :: Number` fields.
capturePositions
  :: forall r
   . Array { id :: Int, x :: Number, y :: Number | r }
  -> Types.PositionMap
capturePositions nodes =
  Object.fromFoldable $ map toEntry nodes
  where
  toEntry node = Tuple (show node.id) { x: node.x, y: node.y }

-- | Capture positions with a custom ID extractor.
-- |
-- | Use when your node's ID field isn't `id :: Int`.
capturePositionsBy
  :: forall node
   . (node -> String)        -- ^ Extract ID as String
  -> (node -> Number)        -- ^ Extract X position
  -> (node -> Number)        -- ^ Extract Y position
  -> Array node
  -> Types.PositionMap
capturePositionsBy getId getX getY nodes =
  Object.fromFoldable $ map toEntry nodes
  where
  toEntry node = Tuple (getId node) { x: getX node, y: getY node }

-- =============================================================================
-- Progress Calculation
-- =============================================================================

-- | Calculate the eased progress given elapsed time.
-- |
-- | Returns a value from 0.0 to 1.0, transformed by the easing function.
-- |
-- | Example:
-- | ```purescript
-- | let progress = calculateProgress config 500.0
-- | -- If duration is 2000ms, raw progress is 0.25
-- | -- Eased progress depends on easing function
-- | ```
calculateProgress
  :: Types.TransitionConfig
  -> Number                  -- ^ Elapsed time in milliseconds
  -> Number                  -- ^ Eased progress (0.0 to 1.0)
calculateProgress config elapsed =
  let rawProgress = min 1.0 (elapsed / config.duration)
  in config.easing rawProgress

-- | Check if a transition is complete.
-- |
-- | Returns true when elapsed time >= duration.
isComplete
  :: Types.TransitionConfig
  -> Number                  -- ^ Elapsed time in milliseconds
  -> Boolean
isComplete config elapsed = elapsed >= config.duration

-- =============================================================================
-- Position Lookup
-- =============================================================================

-- | Look up a position by node ID.
lookupPosition
  :: String                  -- ^ Node ID
  -> Types.PositionMap
  -> Maybe Types.Position
lookupPosition = Object.lookup
