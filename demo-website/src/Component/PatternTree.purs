module Component.PatternTree where

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
