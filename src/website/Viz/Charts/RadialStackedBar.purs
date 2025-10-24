module D3.Viz.RadialStackedBar where

import Prelude

import D3.Attributes.Sugar (classed, d, fill, fillOpacity, height, strokeColor, text, textAnchor, transform, width, x, y)
import D3.Data.Types (D3Selection_, Element(..), Selector)
import D3.Viz.Charts.Model (GroupedBarData)
import D3Tagless.Capabilities (class SelectionM, appendTo, attach)
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
draw :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  Array GroupedBarData -> Selector D3Selection_ -> m Unit
draw data' selector = do
  let dims = { width: 900.0, height: 900.0, innerRadius: 100.0, outerRadius: 400.0 }
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

  -- Color scale for ages
  let ageColors =
        [ "#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00"
        , "#ffff33", "#a65628", "#f781bf", "#999999"
        ]
  let getAgeColor age' =
        fromMaybe "#999999" $ do
          i <- findIndex (\a -> a == age') ages
          index ageColors i

  (root :: D3Selection_) <- attach selector
  svg <- appendTo root Svg [
      classed "radial-stacked-bar"
    , width dims.width
    , height dims.height
    ]

  chartGroup <- appendTo svg Group [
      transform [ \_ -> "translate(" <> show centerX <> "," <> show centerY <> ")" ]
    , classed "chart"
    ]

  -- Draw each state's stacked bar
  let drawStateBar :: Int -> String -> m Unit
      drawStateBar stateIdx state' = do
        let stateData = getStateData state' data'
        let values = map _.population stateData
        let stacked = stackData values
        let stateTotal = getStateTotal state'

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
