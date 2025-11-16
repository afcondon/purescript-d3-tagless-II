module D3.Viz.TreeAPI.MultiLineChartExample where

import Prelude

import Data.Array (findIndex, groupBy, length, mapMaybe, sortBy, uncons)
import Data.Array as Data.Array
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Foldable (maximum, foldl)
import Data.Function (on)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Number
import Data.String (Pattern(..), split, trim)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import PSD3.Shared.Data (DataFile(..), loadDataFile, parseCSVRow)
import PSD3v2.Attribute.Types (width, height, viewBox, class_, x, y, fill, transform, stroke, strokeWidth, d, textContent, textAnchor, fontSize)
import PSD3v2.Axis.Axis (axisBottom, axisLeft, renderAxis, Scale)
import PSD3v2.Capabilities.Selection (select, renderTree)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2Selection_)
import PSD3v2.Selection.Types (ElementType(..), SEmpty)
import PSD3v2.VizTree.Tree (Tree, joinData)
import PSD3v2.VizTree.Tree as T
import Web.DOM.Element (Element)

-- | Multi-line chart showing unemployment rates over time for US metro areas
-- |
-- | Data: BLS metro area unemployment data (42 cities, 2000-2013)
-- | Visual: Many light gray lines that highlight dark on hover
-- |
-- | Key concept: Hover interaction makes dense overlapping data readable

type DataPoint =
  { date :: String     -- Date string (YYYY-MM-DD)
  , division :: String -- Metro area name
  , rate :: Number     -- Unemployment rate (percentage)
  }

type Series =
  { division :: String
  , points :: Array DataPoint
  }

-- Parse CSV row into DataPoint using centralized CSV parser
parseDataPoint :: String -> Maybe DataPoint
parseDataPoint line =
  case parseCSVRow line of
    [division, date, rateStr] -> do
      rate <- Number.fromString (trim rateStr)
      pure { division: trim division, date: trim date, rate }
    _ -> Nothing

-- Load and parse CSV data using centralized loader
loadUnemploymentData :: Aff (Array DataPoint)
loadUnemploymentData = do
  result <- loadDataFile MetroUnemploymentCSV
  case result of
    Left err -> do
      liftEffect $ Console.log $ "Failed to load unemployment data: " <> err
      pure []
    Right body -> do
      let lines = split (Pattern "\n") body
      let dataLines = Data.Array.drop 1 lines  -- Skip header
      let parsed = mapMaybe parseDataPoint dataLines
      liftEffect $ Console.log $ "Loaded " <> show (length parsed) <> " unemployment data points"
      pure parsed

-- Group data by division
groupByDivision :: Array DataPoint -> Array Series
groupByDivision dataPoints =
  let grouped = groupBy (\a b -> a.division == b.division) $ sortBy (compare `on` _.division) dataPoints
      toSeries group =
        let first = NEA.head group
            points = NEA.toArray group
        in { division: first.division
           , points: sortBy (compare `on` _.date) points
           }
  in map toSeries grouped

-- Draw the multi-line chart with loaded data
drawMultiLineChart :: String -> Array DataPoint -> Effect Unit
drawMultiLineChart selector unemploymentData = runD3v2M do
  container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)

  -- Chart dimensions
  let margin = { top: 20.0, right: 20.0, bottom: 50.0, left: 60.0 }
      svgWidth = 928.0
      svgHeight = 600.0
      plotWidth = svgWidth - margin.left - margin.right
      plotHeight = svgHeight - margin.top - margin.bottom

      -- Get all unique dates for x-axis
      allDates = Data.Array.nub $ map _.date unemploymentData
      dateCount = toNumber $ length allDates

      -- Scales
      xScale :: Scale
      xScale = { domain: { min: 0.0, max: dateCount - 1.0 }, range: { min: 0.0, max: plotWidth } }

      maxRate = fromMaybe 20.0 $ maximum $ map _.rate unemploymentData
      yScale :: Scale
      yScale = { domain: { min: 0.0, max: maxRate }, range: { min: plotHeight, max: 0.0 } }

      -- Scale helpers
      scaleX :: String -> Number
      scaleX date =
        let idx = fromMaybe 0 $ findIndex (\d -> d == date) allDates
            value = toNumber idx
            domainSpan = xScale.domain.max - xScale.domain.min
            rangeSpan = xScale.range.max - xScale.range.min
            normalized = (value - xScale.domain.min) / domainSpan
        in xScale.range.min + (normalized * rangeSpan)

      scaleY :: Number -> Number
      scaleY rate =
        let domainSpan = yScale.domain.max - yScale.domain.min
            rangeSpan = yScale.range.max - yScale.range.min
            normalized = (rate - yScale.domain.min) / domainSpan
        in yScale.range.min + (normalized * rangeSpan)

      -- Build path string for a series
      linePath :: Array DataPoint -> String
      linePath points =
        case uncons points of
          Nothing -> ""
          Just { head: first, tail: rest } ->
            let moveCmd = "M" <> show (scaleX first.date) <> "," <> show (scaleY first.rate)
                lineSegments = map (\p -> "L" <> show (scaleX p.date) <> "," <> show (scaleY p.rate)) rest
            in moveCmd <> foldl (\acc seg -> acc <> seg) "" lineSegments

      -- Group data by division
      series = groupByDivision unemploymentData

  -- Build the tree
  let tree :: Tree Series
      tree =
        T.named SVG "svg"
          [ width svgWidth
          , height svgHeight
          , viewBox ("0 0 " <> show svgWidth <> " " <> show svgHeight)
          , class_ "multi-line-chart"  -- This class enables hover CSS
          ]
          `T.withChild`
            (T.named Group "plot-area"
              [ transform ("translate(" <> show margin.left <> "," <> show margin.top <> ")") ]
              `T.withChildren`
                [ -- X-axis
                  T.named Group "x-axis"
                    [ transform ("translate(0," <> show plotHeight <> ")")
                    , class_ "x-axis"
                    ]
                    `T.withChild` renderAxis (axisBottom xScale)

                , -- Y-axis
                  T.named Group "y-axis"
                    [ class_ "y-axis" ]
                    `T.withChild` renderAxis (axisLeft yScale)

                , -- Y-axis label
                  T.elem Text
                    [ transform "rotate(-90)"
                    , x (-(plotHeight / 2.0))
                    , y (-40.0)
                    , textAnchor "middle"
                    , fontSize 12.0
                    , textContent "Unemployment Rate (%)"
                    ]

                , -- Lines with hover interaction (CSS: .multi-line-chart .line:hover)
                  -- All lines are light gray (#ddd), dark gray (#333) on hover
                  joinData "lines" "path" series $ \s ->
                    T.elem Path
                      [ d (linePath s.points)
                      , fill "none"
                      , stroke "#ddd"  -- Light gray for all lines
                      , strokeWidth 1.5
                      , class_ "line"  -- CSS uses .multi-line-chart .line for hover
                      ]
                ])

  -- Render
  _ <- renderTree container tree

  liftEffect do
    Console.log "=== Multi-Line Chart ==="
    Console.log ""
    Console.log ("Metro areas: " <> show (length series))
    Console.log ("Data points: " <> show (length unemploymentData))
    Console.log ""

-- Main entry point - loads data then renders
multiLineChart :: String -> Effect Unit
multiLineChart selector = launchAff_ do
  unemploymentData <- loadUnemploymentData
  liftEffect $ drawMultiLineChart selector unemploymentData

