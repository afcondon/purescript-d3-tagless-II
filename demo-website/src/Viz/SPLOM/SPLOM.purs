-- | SPLOM Visualization
-- |
-- | Brushable Scatterplot Matrix using PSD3 library APIs.
-- | Demonstrates:
-- | - PSD3.Scale for scales (linear, domain, range, ticks, invert, nice)
-- | - PSD3v2.Brush for brush interaction
-- | - PSD3v2 Tree API for declarative rendering
-- |
-- | This is a pure PureScript implementation with no local FFI.
-- | State is managed externally (e.g., by a Halogen component).
module D3.Viz.SPLOM.SPLOM
  ( -- Types
    SPLOMConfig
  , SPLOMState
  , DimensionScale
  , BrushBounds
  , Cell
  -- State management
  , initialState
  , defaultConfig
  -- Rendering
  , renderSPLOM
  , updatePointVisibility
  -- Brush interaction
  , attachBrushes
  , clearBrushSelection
  , isPointSelected
  -- Queries
  , getSelectedCount
  , getTotalCount
  ) where

import Prelude

import Data.Array (filter, length, mapWithIndex, mapMaybe, concat)
import Data.Array as Array
import Data.Foldable (minimum, maximum, for_)
import Data.Traversable (traverse)
import Data.Int (toNumber)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (floor) as Number
import Data.String.CodeUnits as StringCU
import Effect (Effect)
import Effect.Console as Console
import PSD3.Scale as Scale
import PSD3.Scale (ContinuousScale, applyScale, ticks, nice)
import PSD3v2.Attribute.Types (Attribute, AttributeName(..), AttributeValue(..), class_, cx, cy, fill, fillOpacity, height, id_, radius, stroke, strokeWidth, transform, viewBox, width, x, y, x1, y1, x2, y2, textAnchor, textContent, fontSize, fontWeight, dominantBaseline, strokeDasharray)
import PSD3v2.Attribute.Types as Attr
import PSD3v2.Brush (attachBrush, clearBrush, BrushHandle, BrushSelection, BrushConfig)
import PSD3v2.Capabilities.Selection (select, renderTree, clear)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2M)
import PSD3v2.Selection.Types (ElementType(..)) as ET
import PSD3v2.VizTree.Tree (Tree)
import PSD3v2.VizTree.Tree as T
import D3.Viz.SPLOM.Types (Penguin, NumericDimension, dimensionLabel, dimensionKey, getDimensionValue, speciesColor, allDimensions)
import Web.DOM.ParentNode (querySelector, QuerySelector(..))
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument (toParentNode)

-- | Configuration for SPLOM visualization
type SPLOMConfig =
  { cellSize :: Number
  , cellPadding :: Number
  , matrixPadding :: Number
  , pointRadius :: Number
  , tickCount :: Int
  , dimensions :: Array NumericDimension
  }

-- | Default configuration
defaultConfig :: SPLOMConfig
defaultConfig =
  { cellSize: 120.0
  , cellPadding: 12.0
  , matrixPadding: 50.0
  , pointRadius: 2.5
  , tickCount: 5
  , dimensions: allDimensions
  }

-- | Brush selection in data space
type BrushBounds =
  { dimX :: NumericDimension
  , dimY :: NumericDimension
  , xMin :: Number
  , xMax :: Number
  , yMin :: Number
  , yMax :: Number
  }

-- | Cell in the matrix
type Cell =
  { row :: Int
  , col :: Int
  , dimX :: NumericDimension
  , dimY :: NumericDimension
  }

-- | Scale for a dimension
type DimensionScale =
  { dimension :: NumericDimension
  , scale :: ContinuousScale
  }

-- | SPLOM state (managed externally, e.g., by Halogen component)
type SPLOMState =
  { penguins :: Array Penguin
  , validPenguins :: Array Penguin
  , config :: SPLOMConfig
  , scales :: Array DimensionScale
  , selection :: Maybe BrushBounds
  , brushHandles :: Array { cell :: Cell, handle :: BrushHandle }
  , activeBrushCell :: Maybe Cell
  , containerSelector :: String
  }

-- | Create initial state from penguins data
initialState :: String -> Array Penguin -> SPLOMState
initialState containerSelector penguins =
  let
    config = defaultConfig
    validPenguins = filterValidPenguins config.dimensions penguins
    scales = createScales validPenguins config.dimensions config.cellPadding config.cellSize
  in
    { penguins
    , validPenguins
    , config
    , scales
    , selection: Nothing
    , brushHandles: []
    , activeBrushCell: Nothing
    , containerSelector
    }

-- | Filter penguins to those with all dimension values
filterValidPenguins :: Array NumericDimension -> Array Penguin -> Array Penguin
filterValidPenguins dimensions penguins =
  filter (\p ->
    Array.all (\dim -> case getDimensionValue dim p of
      Just _ -> true
      Nothing -> false
    ) dimensions
  ) penguins

-- | Create scales for all dimensions
createScales :: Array Penguin -> Array NumericDimension -> Number -> Number -> Array DimensionScale
createScales penguins dimensions cellPadding cellSize =
  dimensions <#> \dim ->
    let
      values = mapMaybe (getDimensionValue dim) penguins
      minVal = fromMaybe 0.0 $ minimum values
      maxVal = fromMaybe 100.0 $ maximum values
      scale = Scale.linear
        # Scale.domain [minVal, maxVal]
        # Scale.range [cellPadding, cellSize - cellPadding]
        # nice
    in
      { dimension: dim, scale }

-- | Find scale for a dimension
findScale :: NumericDimension -> Array DimensionScale -> Maybe ContinuousScale
findScale dim scales =
  Array.find (\s -> s.dimension == dim) scales <#> _.scale

-- | Check if point is within selection
isPointSelected :: Maybe BrushBounds -> Penguin -> Boolean
isPointSelected Nothing _ = true
isPointSelected (Just bounds) penguin =
  case getDimensionValue bounds.dimX penguin, getDimensionValue bounds.dimY penguin of
    Just xVal, Just yVal ->
      xVal >= bounds.xMin && xVal <= bounds.xMax &&
      yVal >= bounds.yMin && yVal <= bounds.yMax
    _, _ -> false

-- | Generate cells for matrix
generateCells :: Array NumericDimension -> Array Cell
generateCells dimensions =
  concat $ mapWithIndex (\row dimY ->
    mapWithIndex (\col dimX ->
      { row, col, dimX, dimY }
    ) dimensions
  ) dimensions

-- | Get off-diagonal cells only
offDiagonalCells :: Array Cell -> Array Cell
offDiagonalCells = filter (\c -> c.row /= c.col)

-- | Get selected count
getSelectedCount :: SPLOMState -> Int
getSelectedCount state =
  length $ filter (isPointSelected state.selection) state.validPenguins

-- | Get total count
getTotalCount :: SPLOMState -> Int
getTotalCount state = length state.validPenguins

-- =============================================================================
-- Rendering
-- =============================================================================

-- | Render the SPLOM with current state (full re-render, clears container)
renderSPLOM :: SPLOMState -> Effect Unit
renderSPLOM state = runD3v2M $ renderSPLOMWithState state

-- | Update point visibility without full re-render
-- | This is much faster than full re-render and preserves brush elements
updatePointVisibility :: SPLOMState -> Effect Unit
updatePointVisibility state =
  case state.selection of
    Nothing ->
      -- No selection - show all points
      updatePointVisibility_ state.containerSelector Nothing state.config.pointRadius
    Just bounds ->
      -- Convert to FFI-friendly format with string dimension keys
      let ffiSelection =
            { dimX: dimensionKey bounds.dimX
            , dimY: dimensionKey bounds.dimY
            , xMin: bounds.xMin
            , xMax: bounds.xMax
            , yMin: bounds.yMin
            , yMax: bounds.yMax
            }
      in updatePointVisibility_ state.containerSelector (Just ffiSelection) state.config.pointRadius

-- | FFI-friendly selection bounds (uses string keys instead of ADT)
type FFIBrushBounds =
  { dimX :: String
  , dimY :: String
  , xMin :: Number
  , xMax :: Number
  , yMin :: Number
  , yMax :: Number
  }

-- | FFI for updating point visibility
foreign import updatePointVisibility_
  :: String              -- Container selector
  -> Maybe FFIBrushBounds -- Current selection bounds (with string dimension keys)
  -> Number              -- Point radius when selected
  -> Effect Unit

-- | Internal render function
renderSPLOMWithState :: SPLOMState -> D3v2M Unit
renderSPLOMWithState state = do
  -- Clear existing content first to avoid duplicate SVGs
  clear state.containerSelector

  container <- select state.containerSelector

  let n = length state.config.dimensions
  let totalSize = toNumber n * state.config.cellSize + state.config.matrixPadding * 2.0
  let cells = generateCells state.config.dimensions

  -- Build tree
  let tree = buildSPLOMTree state cells totalSize

  -- Render
  _ <- renderTree container tree
  pure unit

-- | Build the complete SPLOM tree
buildSPLOMTree :: SPLOMState -> Array Cell -> Number -> Tree Penguin
buildSPLOMTree state cells totalSize =
  T.named ET.SVG "svg"
    [ width totalSize
    , height totalSize
    , viewBox ("0 0 " <> show totalSize <> " " <> show totalSize)
    , class_ "splom-svg"
    , id_ "splom-matrix"
    ]
    `T.withChildren`
      [ -- Background
        T.elem ET.Rect
          [ width totalSize
          , height totalSize
          , fill "white"
          ]

      , -- All cells
        T.named ET.Group "cells" [] `T.withChildren`
          (cells <#> buildCell state)

      , -- Axis labels
        buildAxisLabels state
      ]

-- | Build a single cell
buildCell :: SPLOMState -> Cell -> Tree Penguin
buildCell state cell =
  let
    cellSize = state.config.cellSize
    isDiagonal = cell.row == cell.col
    mXScale = findScale cell.dimX state.scales
    mYScale = findScale cell.dimY state.scales

    -- Filter penguins valid for this cell
    cellPenguins = filter (\p ->
      case getDimensionValue cell.dimX p, getDimensionValue cell.dimY p of
        Just _, Just _ -> true
        _, _ -> false
    ) state.validPenguins

    cellId = "cell-" <> show cell.row <> "-" <> show cell.col
  in
    T.named ET.Group cellId
      [ id_ cellId
      , class_ $ if isDiagonal then "cell diagonal" else "cell off-diagonal"
      , transform ("translate(" <> show (state.config.matrixPadding + toNumber cell.col * cellSize) <> ","
                               <> show (state.config.matrixPadding + toNumber cell.row * cellSize) <> ")")
      ]
      `T.withChildren`
        ([ -- Cell frame
           T.elem ET.Rect
             [ class_ "cell-frame"
             , width cellSize
             , height cellSize
             , fill "none"
             , stroke "#aaa"
             , strokeWidth 0.5
             ]
         ] <> (if isDiagonal
               then buildDiagonalContent state cellPenguins cell mXScale
               else buildOffDiagonalContent state cellPenguins cell mXScale mYScale))

-- | Build diagonal cell content (label + scatter)
buildDiagonalContent :: SPLOMState -> Array Penguin -> Cell -> Maybe ContinuousScale -> Array (Tree Penguin)
buildDiagonalContent state penguins cell mScale =
  case mScale of
    Nothing -> []
    Just scale ->
      let
        cellSize = state.config.cellSize
        padding = state.config.cellPadding
        tickValues = ticks state.config.tickCount scale

        -- Get domain bounds from the scale
        -- We use the tick values to find min/max
        allTicks = ticks 10 scale
        minVal = fromMaybe 0.0 $ minimum allTicks
        maxVal = fromMaybe 100.0 $ maximum allTicks

        -- Inverted Y scale for SW-NE diagonal
        yScaleInverted = Scale.linear
          # Scale.domain [minVal, maxVal]
          # Scale.range [cellSize - padding, padding]

        label = dimensionLabel cell.dimX
        shortLabel = shortenLabel label

      in
        [ -- Grid lines (vertical)
          T.named ET.Group "grid-v" [] `T.withChildren`
            (tickValues <#> \tick ->
              T.elem ET.Line
                [ x1 (applyScale scale tick)
                , x2 (applyScale scale tick)
                , y1 padding
                , y2 (cellSize - padding)
                , stroke "#eee"
                , strokeWidth 0.5
                ])

        , -- Grid lines (horizontal)
          T.named ET.Group "grid-h" [] `T.withChildren`
            (tickValues <#> \tick ->
              T.elem ET.Line
                [ x1 padding
                , x2 (cellSize - padding)
                , y1 (applyScale scale tick)
                , y2 (applyScale scale tick)
                , stroke "#eee"
                , strokeWidth 0.5
                ])

        , -- Label
          T.elem ET.Text
            [ class_ "cell-label"
            , x (padding + 4.0)
            , y (padding + 12.0)
            , fontSize 11.0
            , fontWeight "bold"
            , fill "#333"
            , textContent shortLabel
            ]

        , -- Identity line (SW-NE)
          T.elem ET.Line
            [ class_ "identity-line"
            , x1 (applyScale scale minVal)
            , y1 (applyScale yScaleInverted minVal)
            , x2 (applyScale scale maxVal)
            , y2 (applyScale yScaleInverted maxVal)
            , stroke "#333"
            , strokeWidth 1.0
            , strokeDasharray "2,2"
            ]

        , -- Diagonal scatter points
          T.named ET.Group "diag-points" [] `T.withChildren`
            (mapMaybe (\p ->
              case getDimensionValue cell.dimX p of
                Just val -> Just $
                  T.elem ET.Circle
                    [ class_ "diag-point"
                    , cx (applyScale scale val)
                    , cy (applyScale yScaleInverted val)
                    , radius 1.5
                    , fill (speciesColor p.species)
                    , fillOpacity 0.7
                    ]
                Nothing -> Nothing
            ) penguins)
        ]

-- | Build off-diagonal cell content (scatter plot)
buildOffDiagonalContent :: SPLOMState -> Array Penguin -> Cell -> Maybe ContinuousScale -> Maybe ContinuousScale -> Array (Tree Penguin)
buildOffDiagonalContent state penguins cell mXScale mYScale =
  case mXScale, mYScale of
    Just xScale, Just yScale ->
      let
        cellSize = state.config.cellSize
        padding = state.config.cellPadding
        xTicks = ticks state.config.tickCount xScale
        yTicks = ticks state.config.tickCount yScale

      in
        [ -- Vertical grid lines
          T.named ET.Group "grid-v" [] `T.withChildren`
            (xTicks <#> \tick ->
              T.elem ET.Line
                [ x1 (applyScale xScale tick)
                , x2 (applyScale xScale tick)
                , y1 padding
                , y2 (cellSize - padding)
                , stroke "#eee"
                , strokeWidth 0.5
                ])

        , -- Horizontal grid lines
          T.named ET.Group "grid-h" [] `T.withChildren`
            (yTicks <#> \tick ->
              T.elem ET.Line
                [ x1 padding
                , x2 (cellSize - padding)
                , y1 (applyScale yScale tick)
                , y2 (applyScale yScale tick)
                , stroke "#eee"
                , strokeWidth 0.5
                ])

        , -- Scatter points
          T.named ET.Group "points" [] `T.withChildren`
            (mapMaybe (\p ->
              case getDimensionValue cell.dimX p, getDimensionValue cell.dimY p of
                Just xVal, Just yVal ->
                  let
                    selected = isPointSelected state.selection p
                    pointOpacity = if selected then 0.7 else 0.1
                    pointRadius = if selected then state.config.pointRadius else 1.5
                    -- Store all dimension values as data attributes for FFI access
                    billLen = fromMaybe 0.0 p.billLength
                    billDep = fromMaybe 0.0 p.billDepth
                    flipLen = fromMaybe 0.0 p.flipperLength
                    bodyMas = fromMaybe 0.0 p.bodyMass
                  in Just $
                    T.elem ET.Circle
                      [ class_ "point"
                      , cx (applyScale xScale xVal)
                      , cy (applyScale yScale yVal)
                      , radius pointRadius
                      , fill (speciesColor p.species)
                      , fillOpacity pointOpacity
                      , stroke "none"
                      , dataAttr "bill-length" billLen
                      , dataAttr "bill-depth" billDep
                      , dataAttr "flipper-length" flipLen
                      , dataAttr "body-mass" bodyMas
                      ]
                _, _ -> Nothing
            ) penguins)

        , -- Brush overlay (empty group for brush attachment)
          T.named ET.Group "brush" [ class_ "brush" ]
        ]
    _, _ -> []

-- | Build axis labels
buildAxisLabels :: SPLOMState -> Tree Penguin
buildAxisLabels state =
  let
    n = length state.scales
    cellSize = state.config.cellSize
    padding = state.config.matrixPadding
  in
    T.named ET.Group "axis-labels" []
      `T.withChildren`
        (concat
          [ -- Bottom axis labels
            mapWithIndex (\i dimScale ->
              let
                tickValues = ticks state.config.tickCount dimScale.scale
                xOffset = padding + toNumber i * cellSize
              in
                T.named ET.Group ("axis-bottom-" <> show i)
                  [ transform ("translate(" <> show xOffset <> "," <> show (padding + toNumber n * cellSize) <> ")") ]
                  `T.withChildren`
                    (tickValues <#> \tick ->
                      T.elem ET.Text
                        [ x (applyScale dimScale.scale tick)
                        , y 12.0
                        , textAnchor "middle"
                        , fontSize 8.0
                        , fill "#666"
                        , textContent (formatTick tick)
                        ])
            ) state.scales

          , -- Left axis labels
            mapWithIndex (\i dimScale ->
              let
                tickValues = ticks state.config.tickCount dimScale.scale
                yOffset = padding + toNumber i * cellSize
              in
                T.named ET.Group ("axis-left-" <> show i)
                  [ transform ("translate(" <> show padding <> "," <> show yOffset <> ")") ]
                  `T.withChildren`
                    (tickValues <#> \tick ->
                      T.elem ET.Text
                        [ x (-4.0)
                        , y (applyScale dimScale.scale tick)
                        , textAnchor "end"
                        , dominantBaseline "middle"
                        , fontSize 8.0
                        , fill "#666"
                        , textContent (formatTick tick)
                        ])
            ) state.scales
          ])

-- =============================================================================
-- Brush Interaction
-- =============================================================================

-- | Brush event callback type
type BrushCallback = BrushSelection -> Cell -> Effect Unit

-- | Attach brushes to all off-diagonal cells
-- | Returns the brush handles for storage in component state
attachBrushes
  :: SPLOMState
  -> BrushCallback      -- Called on brush move
  -> BrushCallback      -- Called on brush end
  -> Effect (Array { cell :: Cell, handle :: BrushHandle })
attachBrushes state onBrush onEnd = do
  let cells = offDiagonalCells $ generateCells state.config.dimensions
  let padding = state.config.cellPadding
  let cellSize = state.config.cellSize

  -- Attach brush to each off-diagonal cell
  handles <- Array.catMaybes <$> (traverse (attachBrushToCell padding cellSize onBrush onEnd) cells)
  pure handles

-- | Attach brush to a single cell
attachBrushToCell
  :: Number
  -> Number
  -> BrushCallback
  -> BrushCallback
  -> Cell
  -> Effect (Maybe { cell :: Cell, handle :: BrushHandle })
attachBrushToCell padding cellSize onBrush onEnd cell = do
  let cellSelector = "#splom-matrix #cell-" <> show cell.row <> "-" <> show cell.col <> " .brush"

  -- Find the brush group element
  doc <- window >>= document
  mElement <- querySelector (QuerySelector cellSelector) (toParentNode doc)

  case mElement of
    Nothing -> do
      Console.log $ "Brush element not found for cell " <> show cell.row <> "-" <> show cell.col
      pure Nothing
    Just element -> do
      -- Create brush config
      let brushConfig :: BrushConfig
          brushConfig =
            { extent:
                { x0: padding
                , y0: padding
                , x1: cellSize - padding
                , y1: cellSize - padding
                }
            , onStart: Nothing  -- Don't use onStart - causes event cascades when clearing other brushes
            , onBrush: Just \event -> onBrush event.selection cell
            , onEnd: Just \event -> onEnd event.selection cell
            }

      -- Attach brush
      handle <- attachBrush element brushConfig
      pure $ Just { cell, handle }

-- | Clear all brush selections
clearBrushSelection :: Array { cell :: Cell, handle :: BrushHandle } -> Effect Unit
clearBrushSelection handles =
  for_ handles \bh -> clearBrush bh.handle

-- =============================================================================
-- Helpers
-- =============================================================================

-- | Create a data-* attribute for storing values on DOM elements
dataAttr :: forall datum. String -> Number -> Attribute datum
dataAttr name value = Attr.StaticAttr (AttributeName ("data-" <> name)) (NumberValue value)

-- | Format tick value for display
formatTick :: Number -> String
formatTick value
  | value >= 1000.0 = show (Number.floor (value / 100.0) / 10.0) <> "k"
  | value == Number.floor value = show (fromMaybe 0 $ Int.fromNumber value)
  | otherwise = show (Number.floor (value * 10.0) / 10.0)

-- | Shorten dimension label
shortenLabel :: String -> String
shortenLabel label =
  replaceSubstring " (mm)" "" (replaceSubstring " (g)" "" label)

-- | Simple substring replacement
replaceSubstring :: String -> String -> String -> String
replaceSubstring search replacement str =
  go 0 str
  where
    searchLen = StringCU.length search
    go idx s
      | idx > StringCU.length s - searchLen = s
      | StringCU.take searchLen (StringCU.drop idx s) == search =
          StringCU.take idx s <> replacement <> StringCU.drop (idx + searchLen) s
      | otherwise = go (idx + 1) s
