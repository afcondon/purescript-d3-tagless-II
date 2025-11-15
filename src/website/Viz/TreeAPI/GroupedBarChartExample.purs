module D3.Viz.TreeAPI.GroupedBarChartExample where

import Prelude

import Data.Array (nub, filter, findIndex)
import Data.Array as Array
import Data.Foldable (maximum)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import D3.Viz.Charts.Model (GroupedBarData, groupedBarData)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console
import PSD3v2.Attribute.Types (width, height, viewBox, class_, x, y, fill, transform)
import PSD3v2.Axis.Axis (axisBottom, axisLeft, renderAxis, Scale)
import PSD3v2.Capabilities.Selection (select, renderTree)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2Selection_, reselectD3v2)
import PSD3v2.Selection.Types (ElementType(..), SEmpty)
import PSD3v2.VizTree.Tree as T
import Web.DOM.Element (Element)

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
  { width: 800.0
  , height: 500.0
  , marginTop: 20.0
  , marginRight: 20.0
  , marginBottom: 60.0
  , marginLeft: 60.0
  }

innerWidth :: Dimensions -> Number
innerWidth dims = dims.width - dims.marginLeft - dims.marginRight

innerHeight :: Dimensions -> Number
innerHeight dims = dims.height - dims.marginTop - dims.marginBottom

-- | Group data by state
type StateGroup = { state :: String, bars :: Array GroupedBarData }

groupByState :: Array GroupedBarData -> Array StateGroup
groupByState data' =
  let states = nub $ map _.state data'
      getStateData st = filter (\d -> d.state == st) data'
  in map (\st -> { state: st, bars: getStateData st }) states

-- | Get all unique age groups
getAges :: Array GroupedBarData -> Array String
getAges = nub <<< map _.age

-- | Color scale for age groups
colorForAge :: String -> String
colorForAge "<10" = "#e8384f"
colorForAge "10-19" = "#fd612c"
colorForAge "20-29" = "#fd8d3c"
colorForAge "30-39" = "#feb078"
colorForAge "40-49" = "#ffe0b3"
colorForAge "50-59" = "#d7eaf3"
colorForAge "60-69" = "#abd9e9"
colorForAge "70-79" = "#74add1"
colorForAge "≥80" = "#4575b4"
colorForAge _ = "#999"

-- | Grouped bar chart using Tree API
-- |
-- | This demonstrates:
-- | - Nested joins: states → bars within each state
-- | - Multiple data series with color encoding
-- | - Axes with proper scales
groupedBarChart :: Effect Unit
groupedBarChart = runD3v2M do
  container <- select "#viz" :: _ (D3v2Selection_ SEmpty Element Unit)

  let dims = defaultDims
  let iWidth = innerWidth dims
  let iHeight = innerHeight dims

  -- Group data by state
  let stateGroups = groupByState groupedBarData
  let ages = getAges groupedBarData
  let numStates = Array.length stateGroups
  let numAges = Array.length ages

  -- Calculate bar dimensions
  let groupWidth = iWidth / Int.toNumber numStates
  let barWidth = groupWidth / Int.toNumber numAges * 0.9

  -- Calculate scales
  let populationValues = map _.population groupedBarData
  let maxPop = fromMaybe 6000000.0 $ maximum populationValues

  -- X scale maps state index to position
  let xScale :: Scale
      xScale =
        { domain: { min: 0.0, max: Int.toNumber numStates }
        , range: { min: 0.0, max: iWidth }
        }

  -- Y scale maps population to height
  let yScale :: Scale
      yScale =
        { domain: { min: 0.0, max: maxPop }
        , range: { min: iHeight, max: 0.0 }
        }

  -- Create axes
  let xAxis = axisBottom xScale
  let yAxis = axisLeft yScale

  -- First tree: SVG container with axes
  let axesTree :: T.Tree Unit
      axesTree =
        T.named SVG "svg"
          [ width dims.width
          , height dims.height
          , viewBox ("0 0 " <> show dims.width <> " " <> show dims.height)
          , class_ "grouped-bar-chart"
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

  -- Render axes
  axesSelections <- renderTree container axesTree

  -- Second tree: Grouped bars
  -- Extract the chartGroup selection for the second render
  chartGroupSel <- liftEffect $ reselectD3v2 "chartGroup" axesSelections

  -- Use nestedJoin to create state groups → bars
  let barsTree :: T.Tree StateGroup
      barsTree =
        T.nestedJoin "stateGroups" "g" stateGroups (_.bars) $ \bar ->
          -- bar :: GroupedBarData
          -- Calculate position for this bar
          let
            -- Find which state this bar belongs to
            stateIdx = fromMaybe 0 $ findIndex (\g -> g.state == bar.state) stateGroups
            -- Find which age group this is
            ageIdx = fromMaybe 0 $ findIndex (\a -> a == bar.age) ages

            -- X position: state position + age offset
            xPos = Int.toNumber stateIdx * groupWidth + Int.toNumber ageIdx * barWidth

            -- Y position and height from population
            yPos = iHeight - ((bar.population / maxPop) * iHeight)
            barHeight = (bar.population / maxPop) * iHeight
          in
            T.elem Rect
              [ x xPos
              , y yPos
              , width barWidth
              , height barHeight
              , fill (colorForAge bar.age)
              , class_ "bar"
              ]

  -- Render bars
  barsSelections <- renderTree chartGroupSel barsTree

  liftEffect do
    Console.log "=== Grouped Bar Chart (Tree API) ==="
    Console.log ""
    Console.log $ "Rendered " <> show (Array.length groupedBarData) <> " bars in "
                  <> show numStates <> " groups"

    case Map.lookup "svg" axesSelections of
      Just _ -> Console.log "✓ SVG created"
      Nothing -> Console.log "✗ Missing SVG"

    case Map.lookup "xAxis" axesSelections of
      Just _ -> Console.log "✓ X axis created"
      Nothing -> Console.log "✗ Missing X axis"

    case Map.lookup "yAxis" axesSelections of
      Just _ -> Console.log "✓ Y axis created"
      Nothing -> Console.log "✗ Missing Y axis"

    case Map.lookup "stateGroups" barsSelections of
      Just _ -> Console.log $ "✓ State groups created (" <> show numStates <> ")"
      Nothing -> Console.log "✗ Missing state groups"

    Console.log ""
    Console.log $ "Expected: " <> show numStates <> " groups of " <> show numAges <> " bars each"
