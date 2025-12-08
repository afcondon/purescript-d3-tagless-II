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
import PSD3v2.Attribute.Types (Attribute(..), AttributeName(..), AttributeValue(..), class_, cx, cy, d, fill, fontSize, height, radius, stroke, strokeWidth, textAnchor, textContent, transform, viewBox, width)
import PSD3v2.Axis.Axis (axisBottom, axisLeft, renderAxis, Scale)
import PSD3v2.Selection.Types (ElementType(..))
import PSD3v2.VizTree.Tree (Tree)
import PSD3v2.VizTree.Tree as T

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
      [ class_ "chart-static" ]
      `T.withChildren`
        [ -- Y axis
          T.elem Group [ class_ "y-axis" ]
            `T.withChildren`
              [ renderAxis (axisLeft yAxisScale)
              , T.elem Text
                  [ transform ("translate(-28," <> show (ih / 2.0) <> ") rotate(-90)")
                  , textAnchor "middle"
                  , fontSize 12.0
                  , textContent "y"
                  ]
              ]

        -- X axis
        , T.elem Group
            [ transform ("translate(0," <> show ih <> ")")
            , class_ "x-axis"
            ]
            `T.withChildren`
              [ renderAxis (axisBottom xAxisScale)
              , T.elem Text
                  [ transform ("translate(" <> show (iw / 2.0) <> ",35)")
                  , textAnchor "middle"
                  , fontSize 12.0
                  , textContent "x"
                  ]
              ]

        -- Green trend line
        , T.elem Path
            [ class_ "trend-line green"
            , d (pathFromPoints xScale yScale greenTrendLine)
            , stroke green
            , strokeWidth 1.5
            , fill "none"
            ]

        -- Purple trend line
        , T.elem Path
            [ class_ "trend-line purple"
            , d (pathFromPoints xScale yScale purpleTrendLine)
            , stroke purple
            , strokeWidth 1.5
            , fill "none"
            ]

        -- Overall trend line (dashed)
        , T.elem Path
            [ class_ "trend-line overall"
            , d (pathFromPoints xScale yScale overallTrendLine)
            , stroke gray
            , strokeWidth 1.5
            , StaticAttr (AttributeName "stroke-dasharray") (StringValue "4,3")
            , fill "none"
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
        [ cx (xScale point.x)
        , cy (yScale point.y)
        , radius 6.0
        , fill point.color
        , stroke "white"
        , strokeWidth 1.0
        ]

-- | Full scatter chart configuration for composing in App
scatterChart :: ScatterConfig -> { static :: Tree Unit, points :: Tree IndexedPoint, containerAttrs :: Array (Attribute Unit) }
scatterChart config =
  { static: scatterChartStatic config
  , points: scatterChartPoints config
  , containerAttrs:
      [ width config.width
      , height config.height
      , viewBox ("0 0 " <> show config.width <> " " <> show config.height)
      , class_ "scatter-chart"
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
