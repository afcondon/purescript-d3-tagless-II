module D3.Viz.TreeAPI.GroupedBarChartExample where

import Prelude

import Data.Array (nub, filter, findIndex, mapMaybe)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (maximum)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Number
import Data.String (Pattern(..), split, trim)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import PSD3.Shared.Data (DataFile(..), loadDataFile, parseCSVRow)
import PSD3v2.Attribute.Types (width, height, viewBox, class_, x, y, fill, transform)
import PSD3v2.Axis.Axis (axisBottom, axisLeft, renderAxis, Scale)
import PSD3v2.Capabilities.Selection (select, renderTree)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2Selection_, reselectD3v2)
import PSD3v2.Selection.Types (ElementType(..), SEmpty)
import PSD3v2.VizTree.Tree as T
import Web.DOM.Element (Element)

-- | Grouped Bar Chart Data (State population by age group)
type GroupedBarData =
  { state :: String
  , age :: String
  , population :: Number
  }

-- | Parse CSV row into population data
-- | CSV format: State,Under 5 Years,5 to 13 Years,14 to 17 Years,18 to 24 Years,25 to 44 Years,45 to 64 Years,65 Years and Over
-- | Map to 9 age categories: <10, 10-19, 20-29, 30-39, 40-49, 50-59, 60-69, 70-79, ≥80
parsePopulationRow :: String -> Array GroupedBarData
parsePopulationRow line =
  case parseCSVRow line of
    [state, under5Str, age5to13Str, age14to17Str, age18to24Str, age25to44Str, age45to64Str, age65plusStr] -> do
      let under5 = fromMaybe 0.0 $ Number.fromString (trim under5Str)
          age5to13 = fromMaybe 0.0 $ Number.fromString (trim age5to13Str)
          age14to17 = fromMaybe 0.0 $ Number.fromString (trim age14to17Str)
          age18to24 = fromMaybe 0.0 $ Number.fromString (trim age18to24Str)
          age25to44 = fromMaybe 0.0 $ Number.fromString (trim age25to44Str)
          age45to64 = fromMaybe 0.0 $ Number.fromString (trim age45to64Str)
          age65plus = fromMaybe 0.0 $ Number.fromString (trim age65plusStr)
      [ { state: trim state, age: "<10", population: under5 + age5to13 * 0.5 }
      , { state: trim state, age: "10-19", population: age5to13 * 0.5 + age14to17 }
      , { state: trim state, age: "20-29", population: age18to24 + age25to44 * 0.25 }
      , { state: trim state, age: "30-39", population: age25to44 * 0.5 }
      , { state: trim state, age: "40-49", population: age25to44 * 0.25 }
      , { state: trim state, age: "50-59", population: age45to64 * 0.6 }
      , { state: trim state, age: "60-69", population: age45to64 * 0.4 }
      , { state: trim state, age: "70-79", population: age65plus * 0.6 }
      , { state: trim state, age: "≥80", population: age65plus * 0.4 }
      ]
    _ -> []

-- | Load population data from CSV
loadPopulationData :: Aff (Array GroupedBarData)
loadPopulationData = do
  result <- loadDataFile USPopulationStateAgeCSV
  case result of
    Left err -> do
      liftEffect $ Console.log $ "Failed to load population data: " <> err
      pure []
    Right body -> do
      let lines = split (Pattern "\n") body
      let dataLines = Array.drop 1 lines  -- Skip header
      let parsed = Array.concatMap parsePopulationRow dataLines
      liftEffect $ Console.log $ "Loaded " <> show (Array.length parsed) <> " population data points"
      pure parsed

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
-- | Draw grouped bar chart with loaded data
drawGroupedBarChart :: String -> Array GroupedBarData -> Effect Unit
drawGroupedBarChart selector populationData = runD3v2M do
  container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)

  let dims = defaultDims
  let iWidth = innerWidth dims
  let iHeight = innerHeight dims

  -- Group data by state
  let stateGroups = groupByState populationData
  let ages = getAges populationData
  let numStates = Array.length stateGroups
  let numAges = Array.length ages

  -- Calculate bar dimensions
  let groupWidth = iWidth / Int.toNumber numStates
  let barWidth = groupWidth / Int.toNumber numAges * 0.9

  -- Calculate scales
  let populationValues = map _.population populationData
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
    Console.log $ "Rendered " <> show (Array.length populationData) <> " bars in "
                  <> show numStates <> " groups"

    case Map.lookup "svg" axesSelections of
      Just _ -> Console.log "✓ SVG created"
      Nothing -> Console.log "✗ Missing SVG"

-- | Main entry point - loads data then renders
groupedBarChart :: String -> Effect Unit
groupedBarChart selector = launchAff_ do
  populationData <- loadPopulationData
  liftEffect $ drawGroupedBarChart selector populationData
