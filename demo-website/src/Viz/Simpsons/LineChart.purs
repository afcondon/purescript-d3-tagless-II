-- | Line Chart - Interactive Admission Rates
-- |
-- | Shows how admission rates change with the proportion
-- | of applicants to the easy department.
-- | This chart updates reactively with slider changes.
module D3.Viz.Simpsons.LineChart
  ( lineChartStatic
  , lineChartPoints
  , LineConfig
  , defaultConfig
  , IndexedRatePoint(..)
  , RatePoint
  ) where

import Prelude

import D3.Viz.Simpsons.Types (Proportions, DerivedData, deriveData, rates, black, green, purple)
import Data.Array (mapWithIndex)
import PSD3.Expr.Friendly (num, text, attr, cx, cy, r, fill, stroke, strokeWidth, transform, path, textAnchor, fontSize, textContent)
import PSD3.Axis.Axis (axisBottom, axisLeft, renderAxis, Scale)
import PSD3.Internal.Selection.Types (ElementType(..))
import PSD3.AST (Tree)
import PSD3.AST as T
import Data.Maybe (Maybe(..))
import Data.Array (uncons)

-- =============================================================================
-- Configuration
-- =============================================================================

type LineConfig =
  { width :: Number
  , height :: Number
  , marginTop :: Number
  , marginRight :: Number
  , marginBottom :: Number
  , marginLeft :: Number
  }

defaultConfig :: LineConfig
defaultConfig =
  { width: 320.0
  , height: 300.0
  , marginTop: 40.0
  , marginRight: 20.0
  , marginBottom: 55.0
  , marginLeft: 45.0
  }

innerWidth :: LineConfig -> Number
innerWidth c = c.width - c.marginLeft - c.marginRight

innerHeight :: LineConfig -> Number
innerHeight c = c.height - c.marginTop - c.marginBottom

-- =============================================================================
-- Data Types
-- =============================================================================

-- | A point on the chart (position on rate line)
type RatePoint =
  { x :: Number  -- % applied to easy (0-100)
  , y :: Number  -- % admitted (0-100)
  , color :: String
  }

-- | Indexed point for data join
newtype IndexedRatePoint = IndexedRatePoint { index :: Int, point :: RatePoint }

instance Eq IndexedRatePoint where
  eq (IndexedRatePoint a) (IndexedRatePoint b) = a.index == b.index

instance Ord IndexedRatePoint where
  compare (IndexedRatePoint a) (IndexedRatePoint b) = compare a.index b.index

-- =============================================================================
-- Static Chart Components
-- =============================================================================

-- | Static portion: axes and rate lines (these don't change)
lineChartStatic :: LineConfig -> Tree Unit
lineChartStatic config =
  let
    iw = innerWidth config
    ih = innerHeight config

    xScale :: Number -> Number
    xScale v = v / 100.0 * iw

    yScale :: Number -> Number
    yScale v = ih - (v / 100.0 * ih)

    xAxisScale :: Scale
    xAxisScale = { domain: { min: 0.0, max: 100.0 }, range: { min: 0.0, max: iw } }

    yAxisScale :: Scale
    yAxisScale = { domain: { min: 0.0, max: 100.0 }, range: { min: ih, max: 0.0 } }

    -- Women's rate line: from (0, hard rate) to (100, easy rate)
    womenLine =
      [ { x: 0.0, y: rates.female.hard * 100.0 }
      , { x: 100.0, y: rates.female.easy * 100.0 }
      ]

    -- Men's rate line
    menLine =
      [ { x: 0.0, y: rates.male.hard * 100.0 }
      , { x: 100.0, y: rates.male.easy * 100.0 }
      ]
  in
    T.elem Group
      [ attr "class" $ text "line-chart-static" ]
      `T.withChildren`
        [ -- Y axis
          T.elem Group [ attr "class" $ text "y-axis" ]
            `T.withChildren`
              [ renderAxis (axisLeft yAxisScale)
              , T.elem Text
                  [ transform $ text ("translate(-35," <> show (ih / 2.0) <> ") rotate(-90)")
                  , textAnchor $ text "middle"
                  , fontSize $ num 12.0
                  , textContent $ text "% admitted"
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
                  [ transform $ text ("translate(" <> show (iw / 2.0) <> ",40)")
                  , textAnchor $ text "middle"
                  , fontSize $ num 12.0
                  , textContent $ text "% applied to easy department"
                  ]
              ]

        -- Women's rate line (green)
        , T.elem Path
            [ attr "class" $ text "rate-line women"
            , path $ text (pathFromPoints xScale yScale womenLine)
            , stroke $ text green
            , strokeWidth $ num 1.5
            , fill $ text "none"
            ]

        -- Men's rate line (purple)
        , T.elem Path
            [ attr "class" $ text "rate-line men"
            , path $ text (pathFromPoints xScale yScale menLine)
            , stroke $ text purple
            , strokeWidth $ num 1.5
            , fill $ text "none"
            ]
        ]

-- =============================================================================
-- Dynamic Chart Components
-- =============================================================================

-- | Dynamic portion: the current position markers (update with state)
lineChartPoints :: LineConfig -> Proportions -> Tree IndexedRatePoint
lineChartPoints config props =
  let
    iw = innerWidth config
    ih = innerHeight config

    xScale :: Number -> Number
    xScale v = v / 100.0 * iw

    yScale :: Number -> Number
    yScale v = ih - (v / 100.0 * ih)

    -- Calculate current admission rates
    derived = deriveData props

    -- Women's current position
    womenX = props.easyFemale * 100.0
    womenY = derived.combined.female * 100.0

    -- Men's current position
    menX = props.easyMale * 100.0
    menY = derived.combined.male * 100.0

    points =
      [ { x: womenX, y: womenY, color: green }
      , { x: menX, y: menY, color: purple }
      ]

    indexedPoints = mapWithIndex (\i p -> IndexedRatePoint { index: i, point: p }) points
  in
    T.joinData "rate-points" "g" indexedPoints \(IndexedRatePoint { point }) ->
      T.elem Group []
        `T.withChildren`
          [ -- Horizontal dashed line to y-axis
            T.elem Path
              [ attr "class" $ text "guide-line"
              , path $ text ("M 0 " <> show (yScale point.y) <> " L " <> show (xScale point.x) <> " " <> show (yScale point.y))
              , stroke $ text black
              , strokeWidth $ num 1.0
              , attr "stroke-dasharray" $ text "5,5"
              , fill $ text "none"
              ]
          -- The point itself
          , T.elem Circle
              [ cx $ num (xScale point.x)
              , cy $ num (yScale point.y)
              , r $ num 6.0
              , fill $ text point.color
              , stroke $ text "white"
              , strokeWidth $ num 1.0
              ]
          ]

-- =============================================================================
-- Helpers
-- =============================================================================

-- | Build SVG path from points
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
