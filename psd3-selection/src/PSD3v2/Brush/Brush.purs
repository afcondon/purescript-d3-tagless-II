-- | D3 Brush API
-- |
-- | PureScript bindings for d3-brush, enabling interactive rectangular
-- | region selection in SVG visualizations.
-- |
-- | ## Basic Usage
-- |
-- | ```purescript
-- | import PSD3v2.Brush (attachBrush, BrushSelection(..))
-- |
-- | -- Attach a 2D brush to a <g> element
-- | brushHandle <- attachBrush brushGroup
-- |   { extent: { x0: 0.0, y0: 0.0, x1: width, y1: height }
-- |   , onStart: Nothing
-- |   , onBrush: Just \event -> case event.selection of
-- |       Selection2D s -> log $ "Brushing: " <> show s
-- |       _ -> pure unit
-- |   , onEnd: Just \event -> updateSelection event.selection
-- |   }
-- |
-- | -- Later, clear the brush programmatically
-- | clearBrush brushHandle
-- | ```
-- |
-- | ## Cross-filtering (SPLOM)
-- |
-- | ```purescript
-- | -- In each cell's brush handler
-- | onEnd: Just \event -> case event.selection of
-- |   Selection2D s -> do
-- |     -- Filter data points within selection
-- |     let filtered = filter (isInSelection s) allData
-- |     -- Update opacity of all points across all cells
-- |     updatePointOpacity filtered
-- |   NoSelection ->
-- |     -- Brush cleared, show all points
-- |     resetPointOpacity
-- | ```
module PSD3v2.Brush
  ( -- * Brush Attachment
    attachBrush
  , attachBrushX
  , attachBrushY
    -- * Programmatic Control
  , clearBrush
  , moveBrush
  , getBrushSelection
    -- * Types (re-exported)
  , module PSD3v2.Brush.Types
  , module ReExportHandle
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe, toNullable)
import Effect (Effect)
import PSD3v2.Brush.FFI (BrushHandle) as ReExportHandle
import PSD3v2.Brush.FFI (BrushHandle)
import PSD3v2.Brush.FFI as FFI
import PSD3v2.Brush.Types (BrushConfig, BrushEvent, BrushExtent, BrushSelection(..), defaultBrushConfig, isInSelection, selectionHeight, selectionWidth)
import Web.DOM.Element (Element)

-- | Attach a 2D brush to an element
-- |
-- | The brush allows users to select a rectangular region by clicking
-- | and dragging. Use this for scatterplots, heatmaps, and other 2D
-- | visualizations.
-- |
-- | The element should typically be a <g> group element within your SVG.
-- | The brush will create overlay, selection, and handle elements as children.
-- |
-- | Returns a BrushHandle for programmatic control (clear, move, get selection).
attachBrush :: Element -> BrushConfig -> Effect BrushHandle
attachBrush element config =
  FFI.attachBrush_
    element
    config.extent
    (makeHandler2D config.onStart)
    (makeHandler2D config.onBrush)
    (makeHandler2D config.onEnd)

-- | Attach a 1D horizontal brush (brushX)
-- |
-- | Constrains selection to the x-axis only. Useful for time series
-- | or 1D distributions.
attachBrushX :: Element -> BrushConfig -> Effect BrushHandle
attachBrushX element config =
  FFI.attachBrushX_
    element
    config.extent
    (makeHandlerX config.onStart)
    (makeHandlerX config.onBrush)
    (makeHandlerX config.onEnd)

-- | Attach a 1D vertical brush (brushY)
-- |
-- | Constrains selection to the y-axis only.
attachBrushY :: Element -> BrushConfig -> Effect BrushHandle
attachBrushY element config =
  FFI.attachBrushY_
    element
    config.extent
    (makeHandlerY config.onStart)
    (makeHandlerY config.onBrush)
    (makeHandlerY config.onEnd)

-- | Clear the brush selection programmatically
-- |
-- | Removes the visible brush selection without triggering end events.
clearBrush :: BrushHandle -> Effect Unit
clearBrush = FFI.clearBrush_

-- | Move the brush selection programmatically
-- |
-- | Sets the brush selection to the given bounds. Pass NoSelection to clear.
-- | Useful for syncing brushes across multiple views or setting initial selection.
moveBrush :: BrushHandle -> BrushSelection -> Effect Unit
moveBrush handle selection = case selection of
  Selection2D s -> FFI.moveBrush_ handle (toNullable (Just s))
  SelectionX s -> FFI.moveBrush_ handle (toNullable (Just s))
  SelectionY s -> FFI.moveBrush_ handle (toNullable (Just s))
  NoSelection -> FFI.moveBrush_ handle (toNullable (Nothing :: Maybe {}))

-- | Get the current brush selection
-- |
-- | Returns NoSelection if no brush is active.
getBrushSelection :: BrushHandle -> Effect BrushSelection
getBrushSelection handle = do
  nullable <- FFI.getBrushSelection_ handle
  pure $ fromNullable2D nullable

-- =============================================================================
-- Internal Helpers
-- =============================================================================

-- | Create a 2D selection handler that converts nullable JS to BrushEvent
makeHandler2D
  :: Maybe (BrushEvent -> Effect Unit)
  -> (Nullable FFI.BrushSelection2D -> Effect Unit)
makeHandler2D maybeHandler = case maybeHandler of
  Nothing -> \_ -> pure unit
  Just handler -> \nullable ->
    handler { selection: fromNullable2D nullable }

-- | Create an X selection handler
makeHandlerX
  :: Maybe (BrushEvent -> Effect Unit)
  -> (Nullable FFI.BrushSelectionX -> Effect Unit)
makeHandlerX maybeHandler = case maybeHandler of
  Nothing -> \_ -> pure unit
  Just handler -> \nullable ->
    handler { selection: fromNullableX nullable }

-- | Create a Y selection handler
makeHandlerY
  :: Maybe (BrushEvent -> Effect Unit)
  -> (Nullable FFI.BrushSelectionY -> Effect Unit)
makeHandlerY maybeHandler = case maybeHandler of
  Nothing -> \_ -> pure unit
  Just handler -> \nullable ->
    handler { selection: fromNullableY nullable }

-- | Convert nullable 2D selection to BrushSelection
fromNullable2D :: Nullable FFI.BrushSelection2D -> BrushSelection
fromNullable2D nullable = case toMaybe nullable of
  Nothing -> NoSelection
  Just s -> Selection2D s

-- | Convert nullable X selection to BrushSelection
fromNullableX :: Nullable FFI.BrushSelectionX -> BrushSelection
fromNullableX nullable = case toMaybe nullable of
  Nothing -> NoSelection
  Just s -> SelectionX s

-- | Convert nullable Y selection to BrushSelection
fromNullableY :: Nullable FFI.BrushSelectionY -> BrushSelection
fromNullableY nullable = case toMaybe nullable of
  Nothing -> NoSelection
  Just s -> SelectionY s
