module D3.Viz.TreeAPI.AnscombesQuartet where

import Prelude

import Data.Array (length, index)
import Data.Foldable (sum)
import Data.Int (toNumber)
import Data.Maybe (fromMaybe)
import Data.Number (pow, sqrt)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console
import PSD3v2.Axis.Axis (axisBottom, axisLeft, renderAxis)
import PSD3v2.Capabilities.Selection (select, renderTree)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2Selection_)
import PSD3v2.Selection.Types (ElementType(..), SEmpty)
import PSD3.AST (Tree, joinData)
import PSD3.AST as T
import PSD3v3.Integration (v3Attr, v3AttrStr, v3AttrFn)
import PSD3v3.Expr (lit, str)
import Web.DOM.Element (Element)

-- | Anscombe's Quartet - Why visualization matters
-- |
-- | Four datasets with IDENTICAL statistics but DIFFERENT distributions
-- | This demonstrates why summary statistics alone are misleading
-- |
-- | Key insight: You MUST visualize data to understand it

type Point = { x :: Number, y :: Number }

type Dataset =
  { name :: String
  , data :: Array Point
  , stats :: Statistics
  }

type Statistics =
  { meanX :: Number
  , meanY :: Number
  , varianceX :: Number
  , varianceY :: Number
  , correlation :: Number
  }

-- Anscombe's original datasets
dataset1 :: Array Point
dataset1 =
  [ { x: 10.0, y: 8.04 }
  , { x: 8.0, y: 6.95 }
  , { x: 13.0, y: 7.58 }
  , { x: 9.0, y: 8.81 }
  , { x: 11.0, y: 8.33 }
  , { x: 14.0, y: 9.96 }
  , { x: 6.0, y: 7.24 }
  , { x: 4.0, y: 4.26 }
  , { x: 12.0, y: 10.84 }
  , { x: 7.0, y: 4.82 }
  , { x: 5.0, y: 5.68 }
  ]

dataset2 :: Array Point
dataset2 =
  [ { x: 10.0, y: 9.14 }
  , { x: 8.0, y: 8.14 }
  , { x: 13.0, y: 8.74 }
  , { x: 9.0, y: 8.77 }
  , { x: 11.0, y: 9.26 }
  , { x: 14.0, y: 8.10 }
  , { x: 6.0, y: 6.13 }
  , { x: 4.0, y: 3.10 }
  , { x: 12.0, y: 9.13 }
  , { x: 7.0, y: 7.26 }
  , { x: 5.0, y: 4.74 }
  ]

dataset3 :: Array Point
dataset3 =
  [ { x: 10.0, y: 7.46 }
  , { x: 8.0, y: 6.77 }
  , { x: 13.0, y: 12.74 }
  , { x: 9.0, y: 7.11 }
  , { x: 11.0, y: 7.81 }
  , { x: 14.0, y: 8.84 }
  , { x: 6.0, y: 6.08 }
  , { x: 4.0, y: 5.39 }
  , { x: 12.0, y: 8.15 }
  , { x: 7.0, y: 6.42 }
  , { x: 5.0, y: 5.73 }
  ]

dataset4 :: Array Point
dataset4 =
  [ { x: 8.0, y: 6.58 }
  , { x: 8.0, y: 5.76 }
  , { x: 8.0, y: 7.71 }
  , { x: 8.0, y: 8.84 }
  , { x: 8.0, y: 8.47 }
  , { x: 8.0, y: 7.04 }
  , { x: 8.0, y: 5.25 }
  , { x: 19.0, y: 12.50 }
  , { x: 8.0, y: 5.56 }
  , { x: 8.0, y: 7.91 }
  , { x: 8.0, y: 6.89 }
  ]

-- Calculate statistics for a dataset
calculateStats :: Array Point -> Statistics
calculateStats points =
  let n = toNumber (length points)
      xs = map _.x points
      ys = map _.y points

      meanX = sum xs / n
      meanY = sum ys / n

      varianceX = (sum (map (\x -> pow (x - meanX) 2.0) xs)) / (n - 1.0)
      varianceY = (sum (map (\y -> pow (y - meanY) 2.0) ys)) / (n - 1.0)

      -- Pearson correlation coefficient
      covariance = (sum (map (\p -> (p.x - meanX) * (p.y - meanY)) points)) / (n - 1.0)
      correlation = covariance / (sqrt varianceX * sqrt varianceY)

  in { meanX, meanY, varianceX, varianceY, correlation }

-- All four datasets with statistics
allDatasets :: Array Dataset
allDatasets =
  [ { name: "A", data: dataset1, stats: calculateStats dataset1 }
  , { name: "B", data: dataset2, stats: calculateStats dataset2 }
  , { name: "C", data: dataset3, stats: calculateStats dataset3 }
  , { name: "D", data: dataset4, stats: calculateStats dataset4 }
  ]

anscombesQuartet :: String -> Effect Unit
anscombesQuartet selector = runD3v2M do
  container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)

  -- Layout parameters
  -- Compact size for foundational example
  let plotSize = 140.0
      margin = { top: 20.0, right: 20.0, bottom: 40.0, left: 50.0 }
      plotWidth = plotSize - margin.left - margin.right
      plotHeight = plotSize - margin.top - margin.bottom
      gap = 20.0

      -- Shared scales across all plots
      xScale = { domain: { min: 0.0, max: 20.0 }, range: { min: 0.0, max: plotWidth } }
      yScale = { domain: { min: 0.0, max: 14.0 }, range: { min: plotHeight, max: 0.0 } }

      -- Scale helper functions
      scaleX :: Number -> Number
      scaleX value =
        let domainSpan = xScale.domain.max - xScale.domain.min
            rangeSpan = xScale.range.max - xScale.range.min
            normalized = (value - xScale.domain.min) / domainSpan
        in xScale.range.min + (normalized * rangeSpan)

      scaleY :: Number -> Number
      scaleY value =
        let domainSpan = yScale.domain.max - yScale.domain.min
            rangeSpan = yScale.range.max - yScale.range.min
            normalized = (value - yScale.domain.min) / domainSpan
        in yScale.range.min + (normalized * rangeSpan)

      -- Calculate position for each plot in a 2x2 grid
      getPlotX :: Int -> Number
      getPlotX idx = if idx `mod` 2 == 0 then 0.0 else plotSize + gap

      getPlotY :: Int -> Number
      getPlotY idx = if idx < 2 then 0.0 else plotSize + gap

  -- Build a single scatterplot with stats
  let buildPlot :: Int -> Dataset -> Tree Point
      buildPlot idx dataset =
        T.named Group ("plot-" <> show idx)
          [ v3AttrStr "transform" (str ("translate(" <> show (getPlotX idx) <> "," <> show (getPlotY idx) <> ")"))
          , v3AttrStr "class" (str "anscombe-plot")
          ]
          `T.withChildren`
            [ -- Title
              T.elem Text
                [ v3Attr "x" (lit (plotSize / 2.0))
                , v3Attr "y" (lit 10.0)
                , v3AttrStr "text-anchor" (str "middle")
                , v3AttrStr "class" (str "plot-title")
                , v3AttrStr "textContent" (str dataset.name)
                ]

            , -- Main plot area
              T.named Group "plot-area"
                [ v3AttrStr "transform" (str ("translate(" <> show margin.left <> "," <> show margin.top <> ")")) ]
                `T.withChildren`
                  [ -- X-axis
                    T.named Group "x-axis"
                      [ v3AttrStr "transform" (str ("translate(0," <> show plotHeight <> ")")) ]
                      `T.withChild` renderAxis (axisBottom xScale)

                  , -- Y-axis
                    T.named Group "y-axis"
                      []
                      `T.withChild` renderAxis (axisLeft yScale)

                  , -- Data points
                    joinData ("points-" <> show idx) "circle" dataset.data $ \d ->
                      T.elem Circle
                        [ v3Attr "cx" (lit (scaleX d.x))
                        , v3Attr "cy" (lit (scaleY d.y))
                        , v3Attr "r" (lit 3.0)
                        , v3AttrStr "fill" (str "steelblue")
                        ]
                  ]
            ]

  -- Build all four plots
  let svgWidth = (plotSize * 2.0) + gap
      svgHeight = (plotSize * 2.0) + gap

      tree :: Tree Point
      tree =
        T.named SVG "svg"
          [ v3Attr "width" (lit svgWidth)
          , v3Attr "height" (lit svgHeight)
          , v3AttrStr "viewBox" (str ("0 0 " <> show svgWidth <> " " <> show svgHeight))
          , v3AttrStr "id" (str "anscombes-quartet-svg")
          , v3AttrStr "class" (str "tree-api-example")
          ]
          `T.withChildren`
            [ buildPlot 0 (getDataset 0)
            , buildPlot 1 (getDataset 1)
            , buildPlot 2 (getDataset 2)
            , buildPlot 3 (getDataset 3)
            ]

      -- Safe dataset getter with fallback
      getDataset :: Int -> Dataset
      getDataset idx = fromMaybe (allDatasets `index` 0 # fromMaybe emptyDataset) (allDatasets `index` idx)

      emptyDataset :: Dataset
      emptyDataset = { name: "Empty", data: [], stats: { meanX: 0.0, meanY: 0.0, varianceX: 0.0, varianceY: 0.0, correlation: 0.0 } }

  -- Render the tree
  _ <- renderTree container tree

  liftEffect do
    Console.log "=== Anscombe's Quartet ==="
    Console.log ""
    Console.log "Four datasets with IDENTICAL statistics:"
    Console.log "  - Mean X ≈ 9.0"
    Console.log "  - Mean Y ≈ 7.5"
    Console.log "  - Variance X ≈ 11.0"
    Console.log "  - Variance Y ≈ 4.1"
    Console.log "  - Correlation ≈ 0.816"
    Console.log ""
    Console.log "But look at the plots - completely different distributions!"
    Console.log "  I: Linear relationship"
    Console.log "  II: Curved relationship"
    Console.log "  III: Linear with outlier"
    Console.log "  IV: Vertical line with outlier"
    Console.log ""
    Console.log "THIS IS WHY VISUALIZATION MATTERS."
    Console.log "Summary statistics alone don't tell the story."
    Console.log ""

-- Helper to format numbers to 2 decimal places
formatNum :: Number -> String
formatNum n =
  let rounded = (n * 100.0) / 100.0  -- Simple rounding
  in show rounded

