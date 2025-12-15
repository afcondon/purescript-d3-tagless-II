module D3.Viz.TreeAPI.RadialStackedBarExample where

import Prelude

import Data.Array (groupBy, length, sortBy)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Foldable (sum, maximum)
import Data.Function (on)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (pi, cos, sin)
import Data.Number as Number
import Data.String (Pattern(..), split, trim)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import PSD3.Shared.Data (DataFile(..), loadDataFile, parseCSVRow)
import PSD3v3.Integration (v3Attr, v3AttrStr)
import PSD3v3.Expr (lit, str)
import PSD3v2.Capabilities.Selection (select, renderTree)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2Selection_)
import PSD3v2.Selection.Types (ElementType(..), SEmpty)
import PSD3.AST (Tree)
import PSD3.AST as T
import Web.DOM.Element (Element)

-- | Radial Stacked Bar Chart - polar coordinates visualization
-- |
-- | Data: US population by state and age (all 50 states + DC)
-- | Visual: States arranged in a circle, age groups stacked radially
-- |
-- | Key concepts: Polar coordinates, arc paths, radial stacking, nestedJoin

type GroupedBarData =
  { state :: String
  , age :: String
  , population :: Number
  }

type AgeSegment =
  { age :: String
  , population :: Number
  , innerRadius :: Number
  , outerRadius :: Number
  }

type StateData =
  { state :: String
  , stateIndex :: Int
  , ageSegments :: Array AgeSegment
  , totalPopulation :: Number
  }

-- | Parse CSV row into population data
parsePopulationRow :: String -> Array GroupedBarData
parsePopulationRow line =
  case parseCSVRow line of
    [ state, under5Str, age5to13Str, age14to17Str, age18to24Str, age25to44Str, age45to64Str, age65plusStr ] -> do
      let
        under5 = fromMaybe 0.0 $ Number.fromString (trim under5Str)
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
      let dataLines = Array.drop 1 lines -- Skip header
      let parsed = Array.concatMap parsePopulationRow dataLines
      liftEffect $ Console.log $ "Loaded " <> show (Array.length parsed) <> " population data points"
      pure parsed

-- Age group colors (Spectral scheme)
ageColors :: Array String
ageColors =
  [ "#d53e4f" -- <10
  , "#f46d43" -- 10-19
  , "#fdae61" -- 20-29
  , "#fee08b" -- 30-39
  , "#e6f598" -- 40-49
  , "#abdda4" -- 50-59
  , "#66c2a5" -- 60-69
  , "#3288bd" -- 70-79
  , "#5e4fa2" -- ≥80
  ]

-- Get color for age group index
getAgeColor :: Int -> String
getAgeColor idx = fromMaybe "#999" $ Array.index ageColors idx

-- Get color for age group name
getAgeColorByName :: String -> String
getAgeColorByName age = case age of
  "<10" -> "#d53e4f"
  "10-19" -> "#f46d43"
  "20-29" -> "#fdae61"
  "30-39" -> "#fee08b"
  "40-49" -> "#e6f598"
  "50-59" -> "#abdda4"
  "60-69" -> "#66c2a5"
  "70-79" -> "#3288bd"
  "≥80" -> "#5e4fa2"
  _ -> "#999"

-- Format large numbers with SI prefix (millions)
formatSI :: Number -> String
formatSI n =
  if n >= 1000000.0 then show (n / 1000000.0) <> "M"
  else show n

-- Group data by state and calculate radial positions
prepareRadialData :: Array GroupedBarData -> Array StateData
prepareRadialData dataPoints =
  let
    grouped = groupBy (\a b -> a.state == b.state) $ sortBy (compare `on` _.state) dataPoints

    -- Find max total across ALL states for global scaling
    allTotals = map (\g -> sum $ map _.population $ NEA.toArray g) grouped
    maxTotal = fromMaybe 1.0 $ maximum allTotals

    innerRadius = 80.0
    outerRadius = 300.0
    radiusRange = outerRadius - innerRadius

  in
    Array.mapWithIndex (toStateData maxTotal innerRadius radiusRange) grouped
  where
  toStateData :: Number -> Number -> Number -> Int -> NEA.NonEmptyArray GroupedBarData -> StateData
  toStateData maxTotal innerR radiusRange stateIndex group =
    let
      first = NEA.head group
      points = NEA.toArray group
      total = sum $ map _.population points

      -- Calculate cumulative radii for stacking, scaled by global max
      segments = Array.foldl
        ( \acc item ->
            let
              prevRadius = case Array.last acc.result of
                Just prev -> prev.outerRadius
                Nothing -> innerR
              -- Scale by global max, not local total
              thickness = (item.population / maxTotal) * radiusRange
            in
              { result: Array.snoc acc.result
                  { age: item.age
                  , population: item.population
                  , innerRadius: prevRadius
                  , outerRadius: prevRadius + thickness
                  }
              }
        )
        { result: [] }
        points

    in
      { state: first.state
      , stateIndex
      , ageSegments: segments.result
      , totalPopulation: total
      }

-- Create an arc path in SVG
-- startAngle and endAngle in radians, innerRadius and outerRadius in pixels
arcPath :: Number -> Number -> Number -> Number -> String
arcPath startAngle endAngle innerRadius outerRadius =
  let -- Outer arc
    outerStartX = outerRadius * cos startAngle
    outerStartY = outerRadius * sin startAngle
    outerEndX = outerRadius * cos endAngle
    outerEndY = outerRadius * sin endAngle

    -- Inner arc
    innerEndX = innerRadius * cos endAngle
    innerEndY = innerRadius * sin endAngle
    innerStartX = innerRadius * cos startAngle
    innerStartY = innerRadius * sin startAngle

    largeArc = if (endAngle - startAngle) > pi then 1 else 0

  in
    "M" <> show outerStartX <> "," <> show outerStartY
      <> " A"
      <> show outerRadius
      <> ","
      <> show outerRadius
      <> " 0 "
      <> show largeArc
      <> ",1 "
      <> show outerEndX
      <> ","
      <> show outerEndY
      <> " L"
      <> show innerEndX
      <> ","
      <> show innerEndY
      <> " A"
      <> show innerRadius
      <> ","
      <> show innerRadius
      <> " 0 "
      <> show largeArc
      <> ",0 "
      <> show innerStartX
      <> ","
      <> show innerStartY
      <>
        " Z"

-- | Draw radial stacked bar chart with loaded data
drawRadialStackedBar :: String -> Array GroupedBarData -> Effect Unit
drawRadialStackedBar selector populationData = runD3v2M do
  container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)

  -- Prepare data
  let
    states = prepareRadialData populationData
    stateCount = toNumber $ length states
    anglePerState = (2.0 * pi) / stateCount

    -- Calculate max total for grid lines
    allTotals = map _.totalPopulation states
    maxTotal = fromMaybe 1.0 $ maximum allTotals

    -- Grid line values (25%, 50%, 75%, 100%)
    gridValues = [ 0.25 * maxTotal, 0.5 * maxTotal, 0.75 * maxTotal, maxTotal ]

    -- Age groups for legend
    ageGroups = [ "<10", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "≥80" ]

    -- Radii
    innerRadius = 80.0
    outerRadius = 300.0
    radiusRange = outerRadius - innerRadius

    -- Helper to scale value to radius
    radiusScale :: Number -> Number
    radiusScale v = innerRadius + (v / maxTotal) * radiusRange

    -- SVG dimensions
    svgSize = 700.0
    centerX = svgSize / 2.0
    centerY = svgSize / 2.0

    -- Helper to create grid line elements (circle + label with stroke outline)
    makeGridLine :: Number -> Array (Tree Unit)
    makeGridLine value =
      let
        gridRadius = radiusScale value
      in
        [ T.elem Circle
            [ v3Attr "r" (lit gridRadius)
            , v3AttrStr "stroke" (str "#000000")
            , v3Attr "stroke-opacity" (lit 0.5)
            , v3AttrStr "fill" (str "none")
            , v3AttrStr "class" (str "grid-circle")
            ]
        , T.elem Text
            [ v3Attr "y" (lit (-gridRadius))
            , v3Attr "dy" (lit 5.6)
            , v3AttrStr "text-content" (str (formatSI value))
            , v3AttrStr "stroke" (str "#ffffff")
            , v3Attr "stroke-width" (lit 5.0)
            , v3AttrStr "fill" (str "none")
            , v3AttrStr "class" (str "grid-label-bg")
            ]
        , T.elem Text
            [ v3Attr "y" (lit (-gridRadius))
            , v3Attr "dy" (lit 5.6)
            , v3AttrStr "text-content" (str (formatSI value))
            , v3AttrStr "fill" (str "#000000")
            , v3AttrStr "class" (str "grid-label")
            ]
        ]

    -- Helper to create legend items
    makeLegendItem :: Int -> String -> Tree Unit
    makeLegendItem idx age =
      let
        ageCount = length ageGroups
        yOffset = (toNumber ageCount / 2.0 - toNumber idx - 1.0) * 20.0
      in
        T.named Group ("legend-" <> age)
          [ v3AttrStr "transform" (str ("translate(-40," <> show yOffset <> ")"))
          , v3AttrStr "class" (str "legend-item")
          ]
          `T.withChildren`
            [ T.elem Rect
                [ v3Attr "width" (lit 18.0)
                , v3Attr "height" (lit 18.0)
                , v3AttrStr "fill" (str (getAgeColorByName age))
                ]
            , T.elem Text
                [ v3Attr "x" (lit 24.0)
                , v3Attr "y" (lit 9.0)
                , v3Attr "dy" (lit 5.6)
                , v3AttrStr "text-content" (str age)
                , v3AttrStr "class" (str "legend-label")
                ]
            ]

  -- Build tree using nestedJoin
  -- Note: The tree type is Unit because grid lines/legend are Unit, not StateData
  let
    tree :: Tree Unit
    tree =
      T.named SVG "svg"
        [ v3Attr "width" (lit svgSize)
        , v3Attr "height" (lit svgSize)
        , v3AttrStr "viewBox" (str ("0 0 " <> show svgSize <> " " <> show svgSize))
        , v3AttrStr "class" (str "radial-stacked-bar")
        ]
        `T.withChild`
          ( T.named Group "plot-area"
              [ v3AttrStr "transform" (str ("translate(" <> show centerX <> "," <> show centerY <> ")")) ]
              `T.withChildren`
                [ -- Circular grid lines
                  T.named Group "y-axis" [ v3AttrStr "text-anchor" (str "middle") ]
                    `T.withChildren`
                      ( [ T.elem Text
                            [ v3Attr "y" (lit (-(outerRadius + 10.0)))
                            , v3Attr "font-size" (lit 12.0)
                            , v3AttrStr "text-content" (str "Population")
                            , v3AttrStr "class" (str "axis-label")
                            ]
                        ] <> Array.concatMap makeGridLine gridValues
                      )
                , -- Color legend
                  T.named Group "legend" []
                    `T.withChildren` (Array.mapWithIndex makeLegendItem ageGroups)
                , -- Data arcs - manually build without nestedJoin to maintain Tree Unit type
                  T.named Group "data-arcs" []
                    `T.withChildren` (Array.concatMap makeStateArcs states)
                , -- State labels
                  T.named Group "state-labels" []
                    `T.withChildren` (map makeStateLabel states)
                ]
          )

    -- Helper to create arcs for a single state
    makeStateArcs :: StateData -> Array (Tree Unit)
    makeStateArcs stateData =
      Array.mapWithIndex
        ( \ageIdx segment ->
            let
              stateIdx = stateData.stateIndex
              startAngle = toNumber stateIdx * anglePerState - (pi / 2.0)
              endAngle = startAngle + anglePerState
            in
              T.elem Path
                [ v3AttrStr "d" (str (arcPath startAngle endAngle segment.innerRadius segment.outerRadius))
                , v3AttrStr "fill" (str (getAgeColor ageIdx))
                , v3AttrStr "class" (str "age-arc")
                ]
        )
        stateData.ageSegments

    -- Helper to create state label
    makeStateLabel :: StateData -> Tree Unit
    makeStateLabel stateData =
      let
        stateIdx = stateData.stateIndex
        startAngle = toNumber stateIdx * anglePerState - (pi / 2.0)
        labelAngle = startAngle + (anglePerState / 2.0)
        labelRadius = 320.0
        labelX = labelRadius * cos labelAngle
        labelY = labelRadius * sin labelAngle
        rotation = (labelAngle * 180.0 / pi) + 90.0
      in
        T.elem Text
          [ v3Attr "x" (lit labelX)
          , v3Attr "y" (lit labelY)
          , v3AttrStr "text-anchor" (str "middle")
          , v3Attr "font-size" (lit 10.0)
          , v3AttrStr "transform" (str ("rotate(" <> show rotation <> " " <> show labelX <> " " <> show labelY <> ")"))
          , v3AttrStr "text-content" (str stateData.state)
          , v3AttrStr "class" (str "state-label")
          ]

  -- Render
  _ <- renderTree container tree

  liftEffect do
    Console.log "=== Radial Stacked Bar Chart ==="
    Console.log ""
    Console.log ("States: " <> show (length states))
    Console.log "All 50 US states + DC in polar coordinates"
    Console.log ""

-- | Main entry point - loads data then renders
radialStackedBar :: String -> Effect Unit
radialStackedBar selector = launchAff_ do
  populationData <- loadPopulationData
  liftEffect $ drawRadialStackedBar selector populationData

