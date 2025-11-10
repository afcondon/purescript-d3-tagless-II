module D3.Viz.RadialStackedBar where

-- | Radial Stacked Bar Chart
-- | Based on https://observablehq.com/@d3/radial-stacked-bar-chart/2
-- | Copyright 2019–2023 Observable, Inc.
-- | Created by Mike Bostock (@mbostock)
-- | ISC License

import Prelude

import PSD3.Internal.Attributes.Sugar (classed, d, dy, fill, fillOpacity, height, radius, strokeColor, strokeOpacity, strokeWidth, text, textAnchor, transform, viewBox, width, x, y)
import PSD3.Internal.Types (D3Selection_, Element(..), Selector)
import D3.Viz.Charts.Model (GroupedBarData)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach)
import Data.Array (filter, findIndex, index, length, mapWithIndex, nub, sortBy)
import Data.Foldable (foldl, maximum, traverse_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (pi, cos, sin)
import Data.Number.Format (toString)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Class (class MonadEffect)

-- Get all unique states
getStates :: Array GroupedBarData -> Array String
getStates = nub <<< map _.state

-- Get all unique age groups
getAges :: Array GroupedBarData -> Array String
getAges = nub <<< map _.age

-- Get data for a specific state, sorted by age group
getStateData :: String -> Array GroupedBarData -> Array GroupedBarData
getStateData state' data' =
  sortBy (\a b -> compare a.age b.age) $ filter (\d -> d.state == state') data'

-- Calculate cumulative sum for stacking
stackData :: Array Number -> Array { start :: Number, end :: Number, value :: Number }
stackData values =
  let folder acc v =
        case acc.last of
          Nothing -> { stack: [{ start: 0.0, end: v, value: v }], last: Just v }
          Just prev ->
            let next = prev + v
            in { stack: acc.stack <> [{ start: prev, end: next, value: v }], last: Just next }
      result = foldl folder { stack: [], last: Nothing } values
  in result.stack

-- Generate arc path (simplified arc for radial bars)
arcPath :: Number -> Number -> Number -> Number -> String
arcPath innerRadius outerRadius startAngle endAngle =
  let x0 = innerRadius * cos startAngle
      y0 = innerRadius * sin startAngle
      x1 = outerRadius * cos startAngle
      y1 = outerRadius * sin startAngle
      x2 = outerRadius * cos endAngle
      y2 = outerRadius * sin endAngle
      x3 = innerRadius * cos endAngle
      y3 = innerRadius * sin endAngle

      -- Use large arc flag if angle > π
      largeArc = if (endAngle - startAngle) > pi then "1" else "0"
  in "M " <> toString x0 <> " " <> toString y0 <>
     " L " <> toString x1 <> " " <> toString y1 <>
     " A " <> toString outerRadius <> " " <> toString outerRadius <> " 0 " <> largeArc <> " 1 " <> toString x2 <> " " <> toString y2 <>
     " L " <> toString x3 <> " " <> toString y3 <>
     " A " <> toString innerRadius <> " " <> toString innerRadius <> " 0 " <> largeArc <> " 0 " <> toString x0 <> " " <> toString y0 <>
     " Z"

-- Draw radial stacked bar chart
-- Matches Observable implementation with 928x928 dimensions and 180px inner radius
draw :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  Array GroupedBarData -> Selector (D3Selection_ Unit) -> m Unit
draw data' selector = do
  let dims = { width: 928.0, height: 928.0, innerRadius: 180.0, outerRadius: 400.0 }
  let centerX = dims.width / 2.0
  let centerY = dims.height / 2.0

  -- Get dimensions for scales
  let states = getStates data'
  let ages = getAges data'
  let stateCount = toNumber $ length states

  -- Find max total per state for scaling
  let getStateTotal state' =
        foldl (\acc d -> acc + d.population) 0.0 $ getStateData state' data'
  let maxTotal = fromMaybe 0.0 $ maximum $ map getStateTotal states

  -- Angular scale: divide 2π by number of states
  let anglePerState = (2.0 * pi) / stateCount
  let anglepadding = 0.02 * anglePerState

  -- Color scale using d3.schemeSpectral (Observable's choice)
  let ageColors =
        [ "#9e0142", "#d53e4f", "#f46d43", "#fdae61", "#fee08b"
        , "#e6f598", "#abdda4", "#66c2a5", "#3288bd"
        ]
  let getAgeColor age' =
        fromMaybe "#999999" $ do
          i <- findIndex (\a -> a == age') ages
          index ageColors i

  (root :: D3Selection_ Unit) <- attach selector
  svg <- appendTo root Svg [
      classed "radial-stacked-bar"
    , width dims.width
    , height dims.height
    , viewBox (negate centerX) (negate centerY) dims.width dims.height
    ]

  chartGroup <- appendTo svg Group [
      classed "chart"
    ]

  -- Draw each state's stacked bar
  let drawStateBar :: Int -> String -> m Unit
      drawStateBar stateIdx state' = do
        let stateData = getStateData state' data'
        let values = map _.population stateData
        let stacked = stackData values

        -- Calculate angles for this state
        let startAngle = toNumber stateIdx * anglePerState + anglepadding
        let endAngle = toNumber (stateIdx + 1) * anglePerState - anglepadding

        -- Draw each age segment
        let drawSegment :: Int -> { start :: Number, end :: Number, value :: Number } -> m Unit
            drawSegment ageIdx seg = case index stateData ageIdx of
              Nothing -> pure unit
              Just ageData -> do
                -- Scale radius by population
                let radiusScale v = dims.innerRadius + (v / maxTotal) * (dims.outerRadius - dims.innerRadius)
                let innerR = radiusScale seg.start
                let outerR = radiusScale seg.end
                let pathData = arcPath innerR outerR startAngle endAngle
                let color = getAgeColor ageData.age

                _ <- appendTo chartGroup Path [
                    d pathData
                  , fill color
                  , fillOpacity 0.8
                  , strokeColor "#ffffff"
                  , classed "segment"
                  ]
                pure unit

        _ <- traverse_ (\t -> drawSegment (fst t) (snd t)) $ mapWithIndex Tuple stacked
        pure unit

  _ <- traverse_ (\t -> drawStateBar (fst t) (snd t)) $ mapWithIndex Tuple states

  -- Draw circular grid lines (y-axis)
  yAxisGroup <- appendTo chartGroup Group [
      classed "y-axis"
    , textAnchor "middle"
    ]

  -- Add "Population" label at the top
  _ <- appendTo yAxisGroup Text [
      y (negate dims.outerRadius - 10.0)
      , dy (-16.0)  -- Approximately -1em
      , text "Population"
      , classed "axis-label"
    ]

  -- Draw circular grid lines at regular intervals
  let yTicks = [0.25 * maxTotal, 0.5 * maxTotal, 0.75 * maxTotal, maxTotal]
  let radiusScale v = dims.innerRadius + (v / maxTotal) * (dims.outerRadius - dims.innerRadius)

  let drawCircularGrid :: Number -> m Unit
      drawCircularGrid value = do
        let gridRadius = radiusScale value
        gridGroup <- appendTo yAxisGroup Group [
            fill "none"
          , classed "grid-line"
          ]

        -- Draw circle
        _ <- appendTo gridGroup Circle [
            radius gridRadius
          , strokeColor "#000000"
          , strokeOpacity 0.5
          , fill "none"
          ]

        -- Draw label with white outline (stroke) and black fill
        textBg <- appendTo gridGroup Text [
            y (negate gridRadius)
          , dy 5.6  -- Approximately 0.35em
          , text (formatSI value)
          , strokeColor "#ffffff"
          , strokeWidth 5.0
          , fill "none"
          , classed "grid-label-bg"
          ]

        textFg <- appendTo gridGroup Text [
            y (negate gridRadius)
          , dy 5.6  -- Approximately 0.35em
          , text (formatSI value)
          , fill "#000000"
          , classed "grid-label"
          ]

        pure unit

  _ <- traverse_ drawCircularGrid yTicks

  -- Draw color legend
  legendGroup <- appendTo chartGroup Group [
      classed "legend"
    ]

  let ageCount = length ages
  let drawLegendItem :: Int -> String -> m Unit
      drawLegendItem idx age' = do
        let yOffset = (toNumber ageCount / 2.0 - toNumber idx - 1.0) * 20.0
        itemGroup <- appendTo legendGroup Group [
            transform [ \_ -> "translate(-40," <> toString yOffset <> ")" ]
          , classed "legend-item"
          ]

        -- Color rectangle
        _ <- appendTo itemGroup Rect [
            width 18.0
          , height 18.0
          , fill (getAgeColor age')
          ]

        -- Age label
        _ <- appendTo itemGroup Text [
            x 24.0
          , y 9.0
          , dy 5.6  -- Approximately 0.35em
          , text age'
          , classed "legend-label"
          ]

        pure unit

  _ <- traverse_ (\t -> drawLegendItem (fst t) (snd t)) $ mapWithIndex Tuple ages

  -- Draw state labels around the perimeter
  let drawStateLabel :: Int -> String -> m Unit
      drawStateLabel stateIdx state' = do
        let angle = toNumber stateIdx * anglePerState + anglePerState / 2.0 - pi / 2.0
        let labelRadius = dims.outerRadius + 30.0
        let labelX = labelRadius * cos angle
        let labelY = labelRadius * sin angle
        let rotation = angle * 180.0 / pi

        _ <- appendTo chartGroup Text [
            x labelX
          , y labelY
          , text state'
          , textAnchor "middle"
          , transform [ \_ -> "rotate(" <> toString rotation <> " " <> toString labelX <> " " <> toString labelY <> ")" ]
          , classed "state-label"
          ]
        pure unit

  _ <- traverse_ (\t -> drawStateLabel (fst t) (snd t)) $ mapWithIndex Tuple states

  pure unit

-- Format large numbers with SI prefix (millions)
formatSI :: Number -> String
formatSI n =
  if n >= 1000000.0
    then toString (n / 1000000.0) <> "M"
    else toString n
