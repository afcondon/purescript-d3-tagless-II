-- | D3 Brush Types
-- |
-- | Types for brush selection and configuration.
module PSD3v2.Brush.Types
  ( BrushSelection(..)
  , BrushExtent
  , BrushConfig
  , BrushEvent
  , defaultBrushConfig
  , isInSelection
  , selectionWidth
  , selectionHeight
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)

-- | Brush selection - the rectangular region selected by the user
-- |
-- | For 2D brush: x0, y0 (top-left) to x1, y1 (bottom-right)
-- | For 1D brush: only x0/x1 or y0/y1 are meaningful
data BrushSelection
  = Selection2D { x0 :: Number, y0 :: Number, x1 :: Number, y1 :: Number }
  | SelectionX { x0 :: Number, x1 :: Number }
  | SelectionY { y0 :: Number, y1 :: Number }
  | NoSelection

derive instance Eq BrushSelection

instance Show BrushSelection where
  show (Selection2D s) = "Selection2D " <> show s
  show (SelectionX s) = "SelectionX " <> show s
  show (SelectionY s) = "SelectionY " <> show s
  show NoSelection = "NoSelection"

-- | Extent for brush (defines the brushable area)
type BrushExtent =
  { x0 :: Number
  , y0 :: Number
  , x1 :: Number
  , y1 :: Number
  }

-- | Brush event passed to handlers
type BrushEvent =
  { selection :: BrushSelection
  }

-- | Configuration for creating a brush
-- |
-- | - extent: The brushable area bounds
-- | - onStart: Called when brush gesture starts
-- | - onBrush: Called during brush gesture (as selection changes)
-- | - onEnd: Called when brush gesture ends
type BrushConfig =
  { extent :: BrushExtent
  , onStart :: Maybe (BrushEvent -> Effect Unit)
  , onBrush :: Maybe (BrushEvent -> Effect Unit)
  , onEnd :: Maybe (BrushEvent -> Effect Unit)
  }

-- | Default brush configuration
-- |
-- | Extent covers 0,0 to 100,100 with no handlers.
-- | Override as needed.
defaultBrushConfig :: BrushConfig
defaultBrushConfig =
  { extent: { x0: 0.0, y0: 0.0, x1: 100.0, y1: 100.0 }
  , onStart: Nothing
  , onBrush: Nothing
  , onEnd: Nothing
  }

-- | Check if a point is within a 2D brush selection
-- |
-- | Returns true if the point (x, y) is inside the selection bounds.
-- | Returns false for 1D selections or NoSelection.
isInSelection :: Number -> Number -> BrushSelection -> Boolean
isInSelection x y = case _ of
  Selection2D s ->
    x >= s.x0 && x <= s.x1 && y >= s.y0 && y <= s.y1
  SelectionX s ->
    x >= s.x0 && x <= s.x1
  SelectionY s ->
    y >= s.y0 && y <= s.y1
  NoSelection ->
    false

-- | Get the width of a brush selection
selectionWidth :: BrushSelection -> Number
selectionWidth = case _ of
  Selection2D s -> s.x1 - s.x0
  SelectionX s -> s.x1 - s.x0
  SelectionY _ -> 0.0
  NoSelection -> 0.0

-- | Get the height of a brush selection
selectionHeight :: BrushSelection -> Number
selectionHeight = case _ of
  Selection2D s -> s.y1 - s.y0
  SelectionX _ -> 0.0
  SelectionY s -> s.y1 - s.y0
  NoSelection -> 0.0
