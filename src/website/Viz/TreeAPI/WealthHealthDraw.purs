module D3.Viz.TreeAPI.WealthHealthDraw where

import Prelude

import Data.Int (floor)
import Data.Maybe (Maybe(..))
import Data.Number (log, sqrt)
import Effect (Effect)
import PSD3v2.Attribute.Types (viewBox, width, height, class_, cx, cy, fill, fillOpacity, stroke, strokeOpacity, strokeWidth, x, x1, x2, y, y1, y2, textAnchor, fontSize, textContent, radius)
import PSD3v2.Capabilities.Selection (select, renderTree, on)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2Selection_)
import PSD3v2.Selection.Types (ElementType(..), SEmpty)
import PSD3v2.VizTree.Tree (Tree, joinData)
import PSD3v2.VizTree.Tree as T
import PSD3v2.Behavior.Types (onMouseEnterWithInfo, onMouseLeaveWithInfo, MouseEventInfo)
import PSD3v2.Tooltip (showTooltip, hideTooltip)
import Web.DOM.Element (Element)
import Data.Map as Map

-- | Type alias for a nation data point ready for visualization
type NationPoint =
  { name :: String
  , income :: Number
  , population :: Number
  , lifeExpectancy :: Number
  , region :: String  -- Region name for display
  , regionColor :: String  -- Color for the region
  }

-- | Configuration for visualization dimensions and margins
type VizConfig =
  { width :: Number
  , height :: Number
  , marginTop :: Number
  , marginRight :: Number
  , marginBottom :: Number
  , marginLeft :: Number
  }

defaultConfig :: VizConfig
defaultConfig =
  { width: 960.0
  , height: 560.0
  , marginTop: 20.0
  , marginRight: 20.0
  , marginBottom: 35.0
  , marginLeft: 40.0
  }

-- | Scale functions
scaleX :: VizConfig -> Number -> Number
scaleX config income =
  let
    -- Logarithmic scale for income ($200 to $100,000)
    minLog = 5.298317  -- log(200)
    maxLog = 11.512925 -- log(100000)
    logIncome = if income > 0.0 then log income else minLog
    normalized = (logIncome - minLog) / (maxLog - minLog)
    rangeWidth = config.width - config.marginLeft - config.marginRight
  in
    config.marginLeft + (normalized * rangeWidth)

scaleY :: VizConfig -> Number -> Number
scaleY config lifeExpectancy =
  let
    -- Linear scale for life expectancy (14 to 86 years)
    minLife = 14.0
    maxLife = 86.0
    normalized = (lifeExpectancy - minLife) / (maxLife - minLife)
    rangeHeight = config.height - config.marginTop - config.marginBottom
  in
    config.height - config.marginBottom - (normalized * rangeHeight)

scaleRadius :: Number -> Number
scaleRadius population =
  let
    -- Square root scale for population (area proportional to population)
    minRadius = 3.0
    maxRadius = 80.0
    maxPop = 5000000000.0  -- 5 billion
    normalized = population / maxPop
    scaled = sqrt normalized * maxRadius
  in
    max minRadius scaled

-- | Format income values for axis labels
formatIncome :: Number -> String
formatIncome value
  | value >= 1000.0 = "$" <> show (floor (value / 1000.0)) <> "k"
  | otherwise = "$" <> show (floor value)

-- | Tick values for axes
xTicks :: Array Number
xTicks = [200.0, 500.0, 1000.0, 2000.0, 5000.0, 10000.0, 20000.0, 50000.0, 100000.0]

yTicks :: Array Number
yTicks = [20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0]

-- | Build vertical grid line
verticalGridLine :: VizConfig -> Number -> Tree NationPoint
verticalGridLine config tickValue =
  T.elem Line
    [ x1 (0.5 + scaleX config tickValue)
    , x2 (0.5 + scaleX config tickValue)
    , y1 config.marginTop
    , y2 (config.height - config.marginBottom)
    ]

-- | Build horizontal grid line
horizontalGridLine :: VizConfig -> Number -> Tree NationPoint
horizontalGridLine config tickValue =
  T.elem Line
    [ y1 (0.5 + scaleY config tickValue)
    , y2 (0.5 + scaleY config tickValue)
    , x1 config.marginLeft
    , x2 (config.width - config.marginRight)
    ]

-- | Build x-axis tick mark and label
xAxisTick :: VizConfig -> Number -> Tree NationPoint
xAxisTick config tickValue =
  T.named Group "x-tick" []
    `T.withChildren`
      [ T.elem Line
          [ x1 (scaleX config tickValue)
          , x2 (scaleX config tickValue)
          , y1 (config.height - config.marginBottom)
          , y2 (config.height - config.marginBottom + 6.0)
          , stroke "#333"
          , strokeWidth 1.0
          ]
      , T.elem Text
          [ x (scaleX config tickValue)
          , y (config.height - config.marginBottom + 15.0)
          , textAnchor "middle"
          , fontSize 10.0
          , fill "#333"
          , textContent (formatIncome tickValue)
          ]
      ]

-- | Build y-axis tick mark and label
yAxisTick :: VizConfig -> Number -> Tree NationPoint
yAxisTick config tickValue =
  T.named Group "y-tick" []
    `T.withChildren`
      [ T.elem Line
          [ x1 (config.marginLeft - 6.0)
          , x2 config.marginLeft
          , y1 (scaleY config tickValue)
          , y2 (scaleY config tickValue)
          , stroke "#333"
          , strokeWidth 1.0
          ]
      , T.elem Text
          [ x (config.marginLeft - 10.0)
          , y (scaleY config tickValue + 3.0)
          , textAnchor "end"
          , fontSize 10.0
          , fill "#333"
          , textContent (show $ floor tickValue)
          ]
      ]

-- | Main draw function - renders visualization with data
drawWealthHealth :: String -> Array NationPoint -> Effect Unit
drawWealthHealth selector nations = runD3v2M do
  container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)

  let config = defaultConfig

  -- Build the visualization tree
  let tree :: Tree NationPoint
      tree =
        T.named SVG "svg"
          [ width config.width
          , height config.height
          , viewBox ("0 0 " <> show config.width <> " " <> show config.height)
          , class_ "wealth-health-viz"
          ]
          `T.withChildren`
            [ -- Grid lines
              T.named Group "grid"
                [ stroke "currentColor"
                , strokeOpacity 0.1
                ]
                `T.withChildren`
                  (map (verticalGridLine config) xTicks <>
                   map (horizontalGridLine config) yTicks)

            -- X-axis
            , T.named Group "x-axis" []
                `T.withChildren`
                  [ T.elem Line
                      [ x1 config.marginLeft
                      , y1 (config.height - config.marginBottom)
                      , x2 (config.width - config.marginRight)
                      , y2 (config.height - config.marginBottom)
                      , stroke "#333"
                      , strokeWidth 1.5
                      ]
                  ]

            -- X-axis ticks
            , T.named Group "x-ticks" []
                `T.withChildren` map (xAxisTick config) xTicks

            -- X-axis label
            , T.elem Text
                [ x (config.width / 2.0)
                , y (config.height - 5.0)
                , textAnchor "middle"
                , fontSize 14.0
                , fill "#333"
                , textContent "Income per Person (GDP/capita, PPP$ inflation-adjusted)"
                ]

            -- Y-axis
            , T.named Group "y-axis" []
                `T.withChildren`
                  [ T.elem Line
                      [ x1 config.marginLeft
                      , y1 config.marginTop
                      , x2 config.marginLeft
                      , y2 (config.height - config.marginBottom)
                      , stroke "#333"
                      , strokeWidth 1.5
                      ]
                  ]

            -- Y-axis ticks
            , T.named Group "y-ticks" []
                `T.withChildren` map (yAxisTick config) yTicks

            -- Y-axis label
            , T.elem Text
                [ x 15.0
                , y (config.height / 2.0)
                , textAnchor "middle"
                , fontSize 14.0
                , fill "#333"
                , textContent "Life Expectancy (years)"
                ]

            -- Nation circles with data join
            , joinData "nations" "circle" nations $ \nation ->
                T.elem Circle
                  [ cx (scaleX config nation.income)
                  , cy (scaleY config nation.lifeExpectancy)
                  , radius (scaleRadius nation.population)
                  , fill nation.regionColor
                  , fillOpacity 0.7
                  , stroke "#333"
                  , strokeWidth 0.5
                  , class_ "nation-circle"
                  ]
            ]

  -- Render the tree and get selections
  selections <- renderTree container tree

  -- Add tooltips to nation circles
  case Map.lookup "nations" selections of
    Just nationsSel -> do
      -- Show tooltip on hover
      _ <- on (onMouseEnterWithInfo \(info :: MouseEventInfo NationPoint) -> do
        let nation = info.datum
        let content = "<strong>" <> nation.name <> "</strong><br/>"
                   <> "Region: " <> nation.region <> "<br/>"
                   <> "Income: $" <> show (floor nation.income) <> "<br/>"
                   <> "Life Expectancy: " <> show (floor nation.lifeExpectancy) <> " years<br/>"
                   <> "Population: " <> formatPopulation nation.population
        showTooltip content info.pageX info.pageY
        ) nationsSel

      -- Hide tooltip on leave
      _ <- on (onMouseLeaveWithInfo \(_ :: MouseEventInfo NationPoint) -> hideTooltip) nationsSel

      pure unit
    Nothing -> pure unit

  pure unit

-- | Format population for display
formatPopulation :: Number -> String
formatPopulation n
  | n >= 1000000000.0 = show (floor (n / 1000000000.0)) <> "B"
  | n >= 1000000.0 = show (floor (n / 1000000.0)) <> "M"
  | n >= 1000.0 = show (floor (n / 1000.0)) <> "K"
  | otherwise = show (floor n)
