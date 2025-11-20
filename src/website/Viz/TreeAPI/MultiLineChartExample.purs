module D3.Viz.TreeAPI.MultiLineChartExample where

import Prelude

import Data.Array (findIndex, groupBy, length, mapMaybe, sortBy, uncons)
import Data.Array as Data.Array
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Foldable (maximum, foldl)
import Data.Function (on) as F
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
import PSD3v2.Capabilities.Selection (select, renderTree, on)
import PSD3v2.Behavior.Types (onMouseLeaveWithInfo, onMouseMoveWithInfo, MouseEventInfo)
import Data.Map as Map
import Web.DOM.Element as Element
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2Selection_)
import PSD3v2.Selection.Types (ElementType(..), SEmpty)
import PSD3v2.VizTree.Tree (Tree, joinData)
import PSD3v2.VizTree.Tree as T
import Web.DOM.Element (Element)
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument (body)
import Web.DOM.Node (appendChild)
import Web.DOM.Document (createElement)
import Web.HTML.HTMLDocument (toDocument)
import Web.DOM.Element (toNode, setAttribute, setId)
import Web.HTML.HTMLElement (toNode) as HTMLElement
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Data.Maybe (Maybe(..)) as Maybe

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
  let grouped = groupBy (\a b -> a.division == b.division) $ sortBy (compare `F.on` _.division) dataPoints
      toSeries group =
        let first = NEA.head group
            points = NEA.toArray group
        in { division: first.division
           , points: sortBy (compare `F.on` _.date) points
           }
  in map toSeries grouped

-- | Create or get the tooltip element
-- Returns the tooltip Element for manipulation
createTooltip :: Effect Element
createTooltip = do
  win <- window
  htmlDoc <- document win
  let doc = toDocument htmlDoc

  -- Create tooltip div
  tooltip <- createElement "div" doc
  setId "multiline-tooltip" tooltip

  -- Style the tooltip
  setAttribute "style"
    ( "position: absolute; "
    <> "background: rgba(0, 0, 0, 0.8); "
    <> "color: white; "
    <> "padding: 8px 12px; "
    <> "border-radius: 4px; "
    <> "font-size: 12px; "
    <> "pointer-events: none; "
    <> "opacity: 0; "
    <> "z-index: 1000; "
    <> "transition: opacity 0.15s;"
    ) tooltip

  -- Append to body
  maybeBody <- body htmlDoc
  case maybeBody of
    Just b -> void $ appendChild (toNode tooltip) (HTMLElement.toNode b)
    Nothing -> pure unit

  pure tooltip

-- | Show tooltip with content at position
showTooltip :: Element -> String -> Number -> Number -> Effect Unit
showTooltip tooltip content pageX pageY = do
  setAttribute "style"
    ( "position: absolute; "
    <> "background: rgba(0, 0, 0, 0.8); "
    <> "color: white; "
    <> "padding: 8px 12px; "
    <> "border-radius: 4px; "
    <> "font-size: 12px; "
    <> "pointer-events: none; "
    <> "opacity: 1; "
    <> "z-index: 1000; "
    <> "transition: opacity 0.15s; "
    <> "left: " <> show (pageX + 15.0) <> "px; "
    <> "top: " <> show (pageY - 10.0) <> "px;"
    ) tooltip
  setInnerHTML_ content tooltip

-- | Hide tooltip
hideTooltip :: Element -> Effect Unit
hideTooltip tooltip = do
  setAttribute "style"
    ( "position: absolute; "
    <> "background: rgba(0, 0, 0, 0.8); "
    <> "color: white; "
    <> "padding: 8px 12px; "
    <> "border-radius: 4px; "
    <> "font-size: 12px; "
    <> "pointer-events: none; "
    <> "opacity: 0; "
    <> "z-index: 1000; "
    <> "transition: opacity 0.15s;"
    ) tooltip

-- FFI for setting innerHTML
foreign import setInnerHTML_ :: String -> Element -> Effect Unit

-- FFI for setTimeout/clearTimeout
foreign import setTimeout_ :: Effect Unit -> Int -> Effect Int
foreign import clearTimeout_ :: Int -> Effect Unit

-- Draw the multi-line chart with loaded data
drawMultiLineChart :: String -> Array DataPoint -> Effect Unit
drawMultiLineChart selector unemploymentData = runD3v2M do
  container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)

  -- Create tooltip element and timeout ref for debounced hiding
  tooltip <- liftEffect createTooltip
  hideTimeoutRef <- liftEffect $ Ref.new (Maybe.Nothing :: Maybe.Maybe Int)

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

  -- Render and get selections
  selections <- renderTree container tree

  -- Helper to find nearest point by offsetX
  let findNearestPoint :: Series -> Number -> Maybe DataPoint
      findNearestPoint s mouseX = do
        -- Adjust for margin to get plot-relative x
        let plotX = mouseX - margin.left
        -- Sort points by date (x position)
        let sortedPoints = sortBy (compare `F.on` _.date) s.points
        -- Find nearest by comparing scaled x positions
        let withDistances = sortedPoints # map \p ->
              { point: p, dist: Number.abs (scaleX p.date - plotX) }
        case Data.Array.head (sortBy (compare `F.on` _.dist) withDistances) of
          Just { point } -> Just point
          Nothing -> Nothing

  -- Attach hover with tooltip to lines
  -- Visual highlighting is handled by CSS (.multi-line-chart .line:hover)
  -- Tooltip is handled by PureScript via onMouseMoveWithInfo/onMouseLeaveWithInfo
  -- We use a debounced hide to avoid flickering from tiny mouse movements
  case Map.lookup "lines" selections of
    Just linesSel -> do
      -- Use onMouseMoveWithInfo to continuously update tooltip as mouse moves along line
      _ <- on (onMouseMoveWithInfo \(info :: MouseEventInfo Series) -> do
        -- Cancel any pending hide timeout
        maybeTimeout <- Ref.read hideTimeoutRef
        case maybeTimeout of
          Maybe.Just timeoutId -> do
            clearTimeout_ timeoutId
            Ref.write Maybe.Nothing hideTimeoutRef
          Maybe.Nothing -> pure unit

        -- Find nearest point based on mouse x position
        case findNearestPoint info.datum info.offsetX of
          Maybe.Just point -> do
            let content = "<strong>" <> info.datum.division <> "</strong><br/>"
                       <> point.date <> ": " <> show point.rate <> "%"
            showTooltip tooltip content info.pageX info.pageY
          Maybe.Nothing -> pure unit
        ) linesSel

      -- Use onMouseLeaveWithInfo to hide tooltip with a small delay
      -- This provides a buffer so tiny mouse movements don't immediately cancel the tooltip
      _ <- on (onMouseLeaveWithInfo \(_ :: MouseEventInfo Series) -> do
        -- Set a timeout to hide after 150ms
        timeoutId <- setTimeout_ (hideTooltip tooltip) 150
        Ref.write (Maybe.Just timeoutId) hideTimeoutRef
        ) linesSel

      pure unit
    Nothing -> pure unit

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

