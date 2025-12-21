module Component.PatternTree where

import Prelude
import Data.Array as Array
import Data.String as String

-- | Pattern tree structure for visualizing TidalCycles-style patterns
-- |
-- | This is a simplified view of patterns that captures structure
-- | without the full semantics of TPat (speed, probability, etc.)
data PatternTree
  = Sequence (Array PatternTree)
  | Parallel (Array PatternTree)
  | Choice (Array PatternTree)
  | Sound String
  | Rest

instance Show PatternTree where
  show (Sound s) = "Sound(" <> s <> ")"
  show Rest = "Rest"
  show (Sequence children) = "Sequence[" <> String.joinWith ", " (map show children) <> "]"
  show (Parallel children) = "Parallel[" <> String.joinWith ", " (map show children) <> "]"
  show (Choice children) = "Choice[" <> String.joinWith ", " (map show children) <> "]"
