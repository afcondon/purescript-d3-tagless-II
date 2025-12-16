-- | Scatter Chart - Paradox Illustration
-- |
-- | Shows how grouped data can have opposite trends from combined data.
-- | Each color group trends upward, but overall trend is downward.
module D3.Viz.Simpsons.ScatterChart
  ( scatterChart
  , scatterChartStatic
  , scatterChartPoints
  , ScatterConfig
  , defaultConfig
  , IndexedPoint(..)
  , Point
  ) where

import Prelude

import D3.Viz.Simpsons.Types (green, purple, gray)
import Data.Array (mapWithIndex, uncons)
import Data.Maybe (Maybe(..))
import PSD3.Internal.Attribute (Attribute)
import PSD3.Expr.Friendly (num, text, attr, cx, cy, r, fill, stroke, strokeWidth, transform, path, textAnchor, fontSize, textContent, width, height, viewBox)
import PSD3.Axis.Axis (axisBottom, axisLeft, renderAxis, Scale)
import PSD3.Internal.Selection.Types (ElementType(..))
import PSD3.AST (Tree)
import PSD3.AST as T

-- =============================================================================
-- Configuration
-- =============================================================================

type ScatterConfig =
  { width :: Number
  , height :: Number
  , marginTop :: Number
  , marginRight :: Number
  , marginBottom :: Number
  , marginLeft :: Number
  }

defaultConfig :: ScatterConfig
defaultConfig =
  { width: 380.0
  , height: 260.0
  , marginTop: 20.0
  , marginRight: 10.0
  , marginBottom: 40.0
  , marginLeft: 40.0
  }

innerWidth :: ScatterConfig -> Number
innerWidth c = c.width - c.marginLeft - c.marginRight

innerHeight :: ScatterConfig -> Number
innerHeight c = c.height - c.marginTop - c.marginBottom

-- =============================================================================
-- Data Types
-- =============================================================================

type Point = { x :: Number, y :: Number, color :: String }

-- | Indexed point for data join
newtype IndexedPoint = IndexedPoint { index :: Int, point :: Point }

instance Eq IndexedPoint where
  eq (IndexedPoint a) (IndexedPoint b) = a.index == b.index

instance Ord IndexedPoint where
  compare (IndexedPoint a) (IndexedPoint b) = compare a.index b.index

-- | Sample data showing the paradox
-- | Green group: x=1-4, y=6-9 (positive trend within group)
-- | Purple group: x=8-11, y=1-4 (positive trend within group)
-- | Combined: negative trend from top-left to bottom-right
greenPoints :: Array Point
greenPoints =
  [ { x: 1.0, y: 6.0, color: green }
  , { x: 2.0, y: 7.0, color: green }
  , { x: 3.0, y: 8.0, color: green }
  , { x: 4.0, y: 9.0, color: green }
  ]

purplePoints :: Array Point
purplePoints =
  [ { x: 8.0, y: 1.0, color: purple }
  , { x: 9.0, y: 2.0, color: purple }
  , { x: 10.0, y: 3.0, color: purple }
  , { x: 11.0, y: 4.0, color: purple }
  ]

allPoints :: Array Point
allPoints = greenPoints <> purplePoints

-- =============================================================================
-- Scatter Chart Tree
-- =============================================================================

-- | Static portion of the scatter chart (axes and trend lines)
scatterChartStatic :: ScatterConfig -> Tree Unit
scatterChartStatic config =
  let
    iw = innerWidth config
    ih = innerHeight config

    xScale :: Number -> Number
    xScale v = v / 12.0 * iw

    yScale :: Number -> Number
    yScale v = ih - (v / 10.0 * ih)

    xAxisScale :: Scale
    xAxisScale = { domain: { min: 0.0, max: 12.0 }, range: { min: 0.0, max: iw } }

    yAxisScale :: Scale
    yAxisScale = { domain: { min: 0.0, max: 10.0 }, range: { min: ih, max: 0.0 } }

    greenTrendLine = [ { x: 0.0, y: 5.0 }, { x: 6.0, y: 11.0 } ]
    purpleTrendLine = [ { x: 7.0, y: 0.0 }, { x: 13.0, y: 6.0 } ]
    overallTrendLine = [ { x: 0.0, y: 8.2 }, { x: 13.0, y: 1.0 } ]
  in
    T.elem Group
      [ attr "class" $ text "chart-static" ]
      `T.withChildren`
        [ -- Y axis
          T.elem Group [ attr "class" $ text "y-axis" ]
            `T.withChildren`
              [ renderAxis (axisLeft yAxisScale)
              , T.elem Text
                  [ transform $ text ("translate(-28," <> show (ih / 2.0) <> ") rotate(-90)")
                  , textAnchor $ text "middle"
                  , fontSize $ num 12.0
                  , textContent $ text "y"
                  ]
              ]

        -- X axis
        , T.elem Group
            [ transform $ text ("translate(0," <> show ih <> ")")
            , attr "class" $ text "x-axis"
            ]
            `T.withChildren`
              [ renderAxis (axisBottom xAxisScale)
              , T.elem Text
                  [ transform $ text ("translate(" <> show (iw / 2.0) <> ",35)")
                  , textAnchor $ text "middle"
                  , fontSize $ num 12.0
                  , textContent $ text "x"
                  ]
              ]

        -- Green trend line
        , T.elem Path
            [ attr "class" $ text "trend-line green"
            , path $ text (pathFromPoints xScale yScale greenTrendLine)
            , stroke $ text green
            , strokeWidth $ num 1.5
            , fill $ text "none"
            ]

        -- Purple trend line
        , T.elem Path
            [ attr "class" $ text "trend-line purple"
            , path $ text (pathFromPoints xScale yScale purpleTrendLine)
            , stroke $ text purple
            , strokeWidth $ num 1.5
            , fill $ text "none"
            ]

        -- Overall trend line (dashed)
        , T.elem Path
            [ attr "class" $ text "trend-line overall"
            , path $ text (pathFromPoints xScale yScale overallTrendLine)
            , stroke $ text gray
            , strokeWidth $ num 1.5
            , attr "stroke-dasharray" $ text "4,3"
            , fill $ text "none"
            ]
        ]

-- | Data-bound portion of the scatter chart (points)
scatterChartPoints :: ScatterConfig -> Tree IndexedPoint
scatterChartPoints config =
  let
    iw = innerWidth config
    ih = innerHeight config

    xScale :: Number -> Number
    xScale v = v / 12.0 * iw

    yScale :: Number -> Number
    yScale v = ih - (v / 10.0 * ih)

    indexedPoints = mapWithIndex (\i p -> IndexedPoint { index: i, point: p }) allPoints
  in
    T.joinData "points" "circle" indexedPoints \(IndexedPoint { point }) ->
      T.elem Circle
        [ cx $ num (xScale point.x)
        , cy $ num (yScale point.y)
        , r $ num 6.0
        , fill $ text point.color
        , stroke $ text "white"
        , strokeWidth $ num 1.0
        ]

-- | Full scatter chart configuration for composing in App
scatterChart :: ScatterConfig -> { static :: Tree Unit, points :: Tree IndexedPoint, containerAttrs :: Array (Attribute Unit) }
scatterChart config =
  { static: scatterChartStatic config
  , points: scatterChartPoints config
  , containerAttrs:
      [ width $ num config.width
      , height $ num config.height
      , viewBox 0.0 0.0 config.width config.height
      , attr "class" $ text "scatter-chart"
      ]
  }

-- | Helper to build SVG path from points
pathFromPoints :: (Number -> Number) -> (Number -> Number) -> Array { x :: Number, y :: Number } -> String
pathFromPoints xScale yScale pts = go true pts
  where
    go :: Boolean -> Array { x :: Number, y :: Number } -> String
    go isFirst arr = case uncons arr of
      Nothing -> ""
      Just { head: p, tail: rest } ->
        let
          cmd = if isFirst then "M " else " L "
          px = xScale p.x
          py = yScale p.y
        in
          cmd <> show px <> " " <> show py <> go false rest
