module D3.Viz.GroupedBarChart where

import Prelude

import PSD3.Internal.Attributes.Sugar (classed, fill, height, strokeColor, text, textAnchor, transform, width, x, y)
import PSD3.Internal.Types (D3Selection_, Element(..), Selector)
import D3.Viz.Charts.Model (GroupedBarData)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach)
import Data.Array (filter, groupBy, nub, length, findIndex, index)
import Data.Foldable (traverse_, foldr, maximum)
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number.Format (toString)
import Effect.Class (class MonadEffect)

-- Group data by state
groupByState :: Array GroupedBarData -> Array (Array GroupedBarData)
groupByState data' =
  let states = nub $ map _.state data'
      getStateData state' = filter (\d -> d.state == state') data'
  in map getStateData states

-- Get all unique states
getStates :: Array GroupedBarData -> Array String
getStates = nub <<< map _.state

-- Get all unique age groups
getAges :: Array GroupedBarData -> Array String
getAges = nub <<< map _.age

-- Draw grouped bar chart
draw :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  Array GroupedBarData -> Selector D3Selection_ -> m Unit
draw data' selector = do
  let dims = { width: 900.0, height: 500.0, marginTop: 30.0, marginRight: 10.0, marginBottom: 30.0, marginLeft: 40.0 }
  let chartWidth = dims.width - dims.marginLeft - dims.marginRight
  let chartHeight = dims.height - dims.marginTop - dims.marginBottom

  -- Get dimensions for scales
  let states = getStates data'
  let ages = getAges data'
  let maxPop = fromMaybe 0.0 $ maximum $ map _.population data'

  -- Calculate scales
  let stateCount = toNumber $ length states
  let ageCount = toNumber $ length ages
  let stateWidth = chartWidth / stateCount
  let barWidth = (stateWidth * 0.9) / ageCount
  let barPadding = stateWidth * 0.1

  -- Helper to get state x position
  let getStateX state' =
        fromMaybe 0.0 $ map (\i -> toNumber i * stateWidth) $ findIndex (\s -> s == state') states

  -- Helper to get age x offset within state
  let getAgeOffset age' =
        fromMaybe 0.0 $ map (\i -> toNumber i * barWidth + barPadding / 2.0) $ findIndex (\a -> a == age') ages

  -- Color scale for ages (using spectral-like colors)
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
      classed "grouped-bar-chart"
    , width dims.width
    , height dims.height
    ]

  chartGroup <- appendTo svg Group [
      transform [ \_ -> "translate(" <> show dims.marginLeft <> "," <> show dims.marginTop <> ")" ]
    , classed "chart"
    ]

  -- Draw bars
  let drawBar :: GroupedBarData -> m Unit
      drawBar d = do
        let barHeight = (d.population / maxPop) * chartHeight
        let barX = getStateX d.state + getAgeOffset d.age
        let barY = chartHeight - barHeight
        _ <- appendTo chartGroup Rect [
            x barX
          , y barY
          , width barWidth
          , height barHeight
          , fill (getAgeColor d.age)
          , strokeColor "white"
          , classed "bar"
          ]
        pure unit

  _ <- traverse_ drawBar data'

  -- Draw x-axis (state labels)
  xAxis <- appendTo chartGroup Group [
      transform [ \_ -> "translate(0," <> show chartHeight <> ")" ]
    , classed "x-axis"
    ]

  let drawStateLabel :: String -> m Unit
      drawStateLabel state' = do
        let labelX = getStateX state' + stateWidth / 2.0
        _ <- appendTo xAxis Text [
            x labelX
          , y 20.0
          , text state'
          , textAnchor "middle"
          , classed "axis-label"
          ]
        pure unit

  _ <- traverse_ drawStateLabel states

  -- Draw y-axis
  yAxis <- appendTo chartGroup Group [
      classed "y-axis"
    ]

  -- Add a few y-axis ticks
  let yTicks = [0.0, maxPop / 4.0, maxPop / 2.0, (3.0 * maxPop) / 4.0, maxPop]
  let drawYTick :: Number -> m Unit
      drawYTick value = do
        let tickY = chartHeight - (value / maxPop) * chartHeight
        _ <- appendTo yAxis Text [
            x (-10.0)
          , y (tickY + 4.0)
          , text (formatSI value)
          , textAnchor "end"
          , classed "axis-label"
          ]
        pure unit

  _ <- traverse_ drawYTick yTicks

  pure unit

-- Format large numbers with SI prefix (millions)
formatSI :: Number -> String
formatSI n =
  if n >= 1000000.0
    then toString (n / 1000000.0) <> "M"
    else toString n
