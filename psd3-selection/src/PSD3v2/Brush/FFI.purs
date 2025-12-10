module PSD3v2.Brush.FFI
  ( attachBrush_
  , attachBrushX_
  , attachBrushY_
  , clearBrush_
  , moveBrush_
  , getBrushSelection_
  , BrushHandle
  , BrushExtent
  , BrushSelection2D
  , BrushSelectionX
  , BrushSelectionY
  ) where

import Prelude

import Data.Nullable (Nullable)
import Effect (Effect)
import Web.DOM.Element (Element)

-- | Extent for brush (defines the brushable area)
type BrushExtent =
  { x0 :: Number
  , y0 :: Number
  , x1 :: Number
  , y1 :: Number
  }

-- | 2D brush selection (for brush())
type BrushSelection2D =
  { x0 :: Number
  , y0 :: Number
  , x1 :: Number
  , y1 :: Number
  }

-- | 1D horizontal brush selection (for brushX())
type BrushSelectionX =
  { x0 :: Number
  , x1 :: Number
  }

-- | 1D vertical brush selection (for brushY())
type BrushSelectionY =
  { y0 :: Number
  , y1 :: Number
  }

-- | Opaque handle for controlling a brush programmatically
foreign import data BrushHandle :: Type

-- | Attach a 2D brush to an element
-- |
-- | Parameters:
-- | - element: DOM element (typically a <g>) to attach brush to
-- | - extent: Brushable area bounds
-- | - onStart: Handler called when brush gesture starts (receives selection or null)
-- | - onBrush: Handler called during brush gesture (receives selection or null)
-- | - onEnd: Handler called when brush gesture ends (receives selection or null)
-- |
-- | Returns a BrushHandle for programmatic control.
foreign import attachBrush_
  :: Element
  -> BrushExtent
  -> (Nullable BrushSelection2D -> Effect Unit)
  -> (Nullable BrushSelection2D -> Effect Unit)
  -> (Nullable BrushSelection2D -> Effect Unit)
  -> Effect BrushHandle

-- | Attach a 1D horizontal brush (brushX)
foreign import attachBrushX_
  :: Element
  -> BrushExtent
  -> (Nullable BrushSelectionX -> Effect Unit)
  -> (Nullable BrushSelectionX -> Effect Unit)
  -> (Nullable BrushSelectionX -> Effect Unit)
  -> Effect BrushHandle

-- | Attach a 1D vertical brush (brushY)
foreign import attachBrushY_
  :: Element
  -> BrushExtent
  -> (Nullable BrushSelectionY -> Effect Unit)
  -> (Nullable BrushSelectionY -> Effect Unit)
  -> (Nullable BrushSelectionY -> Effect Unit)
  -> Effect BrushHandle

-- | Clear the brush selection
foreign import clearBrush_ :: BrushHandle -> Effect Unit

-- | Move the brush selection programmatically
-- | Pass null to clear
foreign import moveBrush_
  :: forall selection
   . BrushHandle
  -> Nullable selection
  -> Effect Unit

-- | Get current brush selection
foreign import getBrushSelection_
  :: forall selection
   . BrushHandle
  -> Effect (Nullable selection)
