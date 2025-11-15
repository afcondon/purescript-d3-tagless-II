module D3.Viz.TreeAPI.BarChartExample where

import Prelude

import Data.Array (length)
import Data.Foldable (maximum, minimum)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console
import PSD3v2.Attribute.Types (width, height, viewBox, class_, x, y, fill, stroke, strokeWidth, transform)
import PSD3v2.Axis.Axis (axisBottom, axisLeft, renderAxis, Scale)
import PSD3v2.Capabilities.Selection (select, renderTree)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2Selection_)
import PSD3v2.Selection.Types (ElementType(..), SEmpty)
import PSD3v2.VizTree.Tree (Tree, joinData)
import PSD3v2.VizTree.Tree as T
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (Element)

-- | Simple data point
type DataPoint = { x :: Number, y :: Number }

-- | Sample data
sampleData :: Array DataPoint
sampleData =
  [ { x: 1.0, y: 30.0 }
  , { x: 2.0, y: 80.0 }
  , { x: 3.0, y: 45.0 }
  , { x: 4.0, y: 60.0 }
  , { x: 5.0, y: 20.0 }
  , { x: 6.0, y: 90.0 }
  , { x: 7.0, y: 55.0 }
  ]

-- | Chart dimensions
type Dimensions =
  { width :: Number
  , height :: Number
  , marginTop :: Number
  , marginRight :: Number
  , marginBottom :: Number
  , marginLeft :: Number
  }

defaultDims :: Dimensions
defaultDims =
  { width: 600.0
  , height: 400.0
  , marginTop: 20.0
  , marginRight: 20.0
  , marginBottom: 40.0
  , marginLeft: 50.0
  }

innerWidth :: Dimensions -> Number
innerWidth dims = dims.width - dims.marginLeft - dims.marginRight

innerHeight :: Dimensions -> Number
innerHeight dims = dims.height - dims.marginTop - dims.marginBottom

-- | Bar chart using declarative tree API
barChart :: Effect Unit
barChart = runD3v2M do
  container <- select "#viz" :: _ (D3v2Selection_ SEmpty Element Unit)

  let dims = defaultDims
  let iWidth = innerWidth dims
  let iHeight = innerHeight dims

  -- Calculate data extents
  let yValues = map _.y sampleData
  let minY = 0.0  -- Start bars from zero
  let maxY = fromMaybe 100.0 $ maximum yValues

  -- Calculate bar width
  let numBars = length sampleData
  let barWidth = if numBars > 0 then (iWidth / (Int.toNumber numBars)) * 0.8 else 0.0

  -- Create scales for axes
  let xScale :: Scale
      xScale =
        { domain: { min: 1.0, max: Int.toNumber numBars }
        , range: { min: 0.0, max: iWidth }
        }

  let yScale :: Scale
      yScale =
        { domain: { min: minY, max: maxY }
        , range: { min: iHeight, max: 0.0 }
        }

  -- Create axes
  let xAxis = axisBottom xScale
  let yAxis = axisLeft yScale

  -- First, render the SVG container with axes (datum type: Unit)
  let axesTree :: Tree Unit
      axesTree =
        T.named SVG "svg"
          [ width dims.width
          , height dims.height
          , viewBox ("0 0 " <> show dims.width <> " " <> show dims.height)
          , class_ "bar-chart-tree"
          ]
          `T.withChild`
            (T.named Group "chartGroup"
              [ class_ "chart-content"
              , transform ("translate(" <> show dims.marginLeft <> "," <> show dims.marginTop <> ")")
              ]
              `T.withChildren`
                [ -- X axis
                  T.named Group "xAxis"
                    [ transform ("translate(0," <> show iHeight <> ")")
                    , class_ "x-axis"
                    ]
                    `T.withChild`
                      renderAxis xAxis
                , -- Y axis
                  T.named Group "yAxis"
                    [ class_ "y-axis"
                    ]
                    `T.withChild`
                      renderAxis yAxis
                ])

  -- Render axes first (underlaying)
  axesSelections <- renderTree container axesTree

  -- Then, render bars on top (datum type: DataPoint)
  -- We select the chartGroup from the first render
  chartGroupSel <- case Map.lookup "chartGroup" axesSelections of
    Just sel -> select ".chart-content" :: _ (D3v2Selection_ SEmpty Element Unit)
    Nothing -> select ".chart-content" :: _ (D3v2Selection_ SEmpty Element Unit)

  let barsTree :: Tree DataPoint
      barsTree =
        T.joinData "bars" "rect" sampleData $ \point ->
          let xPos = (point.x - 1.0) * (iWidth / (Int.toNumber numBars)) + ((iWidth / (Int.toNumber numBars)) - barWidth) / 2.0
              yPos = iHeight - ((point.y - minY) / (maxY - minY) * iHeight)
              barHeight = iHeight - yPos
          in T.elem Rect
            [ x xPos
            , y yPos
            , width barWidth
            , height barHeight
            , fill "#4a90e2"
            , stroke "#357abd"
            , strokeWidth 1.0
            , class_ "bar"
            ]

  -- Render bars into the chart group (overlaying)
  barsSelections <- renderTree chartGroupSel barsTree

  liftEffect do
    Console.log "=== Bar Chart (Tree API) ==="
    Console.log ""
    Console.log $ "Rendered " <> show (length sampleData) <> " bars with axes"

    case Map.lookup "svg" axesSelections of
      Just _ -> Console.log "✓ SVG created"
      Nothing -> Console.log "✗ Missing SVG"

    case Map.lookup "chartGroup" axesSelections of
      Just _ -> Console.log "✓ Chart group created"
      Nothing -> Console.log "✗ Missing chart group"

    case Map.lookup "xAxis" axesSelections of
      Just _ -> Console.log "✓ X axis created"
      Nothing -> Console.log "✗ Missing X axis"

    case Map.lookup "yAxis" axesSelections of
      Just _ -> Console.log "✓ Y axis created"
      Nothing -> Console.log "✗ Missing Y axis"

    case Map.lookup "bars" barsSelections of
      Just _ -> Console.log "✓ Bars collection created"
      Nothing -> Console.log "✗ Missing bars collection"

    Console.log ""
    Console.log "Expected: 7 blue bars of varying heights with X and Y axes"
