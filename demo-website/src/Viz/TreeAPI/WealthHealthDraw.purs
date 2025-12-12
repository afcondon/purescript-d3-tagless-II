module D3.Viz.TreeAPI.WealthHealthDraw where

import Prelude hiding (append)

import Data.Int (floor)
import Data.Number (log, sqrt)
import Effect (Effect)
import PSD3v2.Attribute.Types (Attribute)
import PSD3v2.Capabilities.Selection (select, renderTree, on, joinDataWithKey, append, setAttrs, remove, merge)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2Selection_)
import PSD3v2.Selection.Types (ElementType(..), SEmpty, JoinResult(..))
import PSD3v2.VizTree.Tree (Tree)
import PSD3v2.VizTree.Tree as T
import PSD3v2.Behavior.Types (onMouseEnterWithInfo, onMouseLeaveWithInfo, MouseEventInfo)
import PSD3v2.Tooltip (showTooltip, hideTooltip)
import PSD3v3.Integration (v3Attr, v3AttrStr, v3AttrFn, v3AttrFnStr)
import PSD3v3.Expr (lit, str)
import Web.DOM.Element (Element)

-- | Type alias for a nation data point ready for visualization
type NationPoint =
  { name :: String
  , income :: Number
  , population :: Number
  , lifeExpectancy :: Number
  , region :: String -- Region name for display
  , regionColor :: String -- Color for the region
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
    minLog = 5.298317 -- log(200)
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
    maxPop = 5000000000.0 -- 5 billion
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
xTicks = [ 200.0, 500.0, 1000.0, 2000.0, 5000.0, 10000.0, 20000.0, 50000.0, 100000.0 ]

yTicks :: Array Number
yTicks = [ 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0 ]

-- | Build vertical grid line
verticalGridLine :: VizConfig -> Number -> Tree NationPoint
verticalGridLine config tickValue =
  T.elem Line
    [ v3Attr "x1" (lit (0.5 + scaleX config tickValue))
    , v3Attr "x2" (lit (0.5 + scaleX config tickValue))
    , v3Attr "y1" (lit config.marginTop)
    , v3Attr "y2" (lit (config.height - config.marginBottom))
    ]

-- | Build horizontal grid line
horizontalGridLine :: VizConfig -> Number -> Tree NationPoint
horizontalGridLine config tickValue =
  T.elem Line
    [ v3Attr "y1" (lit (0.5 + scaleY config tickValue))
    , v3Attr "y2" (lit (0.5 + scaleY config tickValue))
    , v3Attr "x1" (lit config.marginLeft)
    , v3Attr "x2" (lit (config.width - config.marginRight))
    ]

-- | Build x-axis tick mark and label
xAxisTick :: VizConfig -> Number -> Tree NationPoint
xAxisTick config tickValue =
  T.named Group "x-tick" []
    `T.withChildren`
      [ T.elem Line
          [ v3Attr "x1" (lit (scaleX config tickValue))
          , v3Attr "x2" (lit (scaleX config tickValue))
          , v3Attr "y1" (lit (config.height - config.marginBottom))
          , v3Attr "y2" (lit (config.height - config.marginBottom + 6.0))
          , v3AttrStr "stroke" (str "#333")
          , v3Attr "stroke-width" (lit 1.0)
          ]
      , T.elem Text
          [ v3Attr "x" (lit (scaleX config tickValue))
          , v3Attr "y" (lit (config.height - config.marginBottom + 15.0))
          , v3AttrStr "text-anchor" (str "middle")
          , v3Attr "font-size" (lit 10.0)
          , v3AttrStr "fill" (str "#333")
          , v3AttrStr "textContent" (str (formatIncome tickValue))
          ]
      ]

-- | Build y-axis tick mark and label
yAxisTick :: VizConfig -> Number -> Tree NationPoint
yAxisTick config tickValue =
  T.named Group "y-tick" []
    `T.withChildren`
      [ T.elem Line
          [ v3Attr "x1" (lit (config.marginLeft - 6.0))
          , v3Attr "x2" (lit config.marginLeft)
          , v3Attr "y1" (lit (scaleY config tickValue))
          , v3Attr "y2" (lit (scaleY config tickValue))
          , v3AttrStr "stroke" (str "#333")
          , v3Attr "stroke-width" (lit 1.0)
          ]
      , T.elem Text
          [ v3Attr "x" (lit (config.marginLeft - 10.0))
          , v3Attr "y" (lit (scaleY config tickValue + 3.0))
          , v3AttrStr "text-anchor" (str "end")
          , v3Attr "font-size" (lit 10.0)
          , v3AttrStr "fill" (str "#333")
          , v3AttrStr "textContent" (str (show $ floor tickValue))
          ]
      ]

-- | Initialize the visualization - creates SVG structure and returns update function
initWealthHealth :: String -> Effect (Array NationPoint -> Effect Unit)
initWealthHealth selector = runD3v2M do
  container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)

  let config = defaultConfig

  -- Build the static visualization tree (without data-bound circles)
  let
    staticTree :: Tree NationPoint
    staticTree =
      T.named SVG "svg"
        [ v3Attr "width" (lit config.width)
        , v3Attr "height" (lit config.height)
        , v3AttrStr "viewBox" (str ("0 0 " <> show config.width <> " " <> show config.height))
        , v3AttrStr "class" (str "wealth-health-viz")
        ]
        `T.withChildren`
          [ -- Grid lines
            T.named Group "grid"
              [ v3AttrStr "stroke" (str "currentColor")
              , v3Attr "stroke-opacity" (lit 0.1)
              ]
              `T.withChildren`
                ( map (verticalGridLine config) xTicks <>
                    map (horizontalGridLine config) yTicks
                )

          -- X-axis
          , T.named Group "x-axis" []
              `T.withChildren`
                [ T.elem Line
                    [ v3Attr "x1" (lit config.marginLeft)
                    , v3Attr "y1" (lit (config.height - config.marginBottom))
                    , v3Attr "x2" (lit (config.width - config.marginRight))
                    , v3Attr "y2" (lit (config.height - config.marginBottom))
                    , v3AttrStr "stroke" (str "#333")
                    , v3Attr "stroke-width" (lit 1.5)
                    ]
                ]

          -- X-axis ticks
          , T.named Group "x-ticks" []
              `T.withChildren` map (xAxisTick config) xTicks

          -- X-axis label
          , T.elem Text
              [ v3Attr "x" (lit (config.width / 2.0))
              , v3Attr "y" (lit (config.height - 5.0))
              , v3AttrStr "text-anchor" (str "middle")
              , v3Attr "font-size" (lit 14.0)
              , v3AttrStr "fill" (str "#333")
              , v3AttrStr "textContent" (str "Income per Person (GDP/capita, PPP$ inflation-adjusted)")
              ]

          -- Y-axis
          , T.named Group "y-axis" []
              `T.withChildren`
                [ T.elem Line
                    [ v3Attr "x1" (lit config.marginLeft)
                    , v3Attr "y1" (lit config.marginTop)
                    , v3Attr "x2" (lit config.marginLeft)
                    , v3Attr "y2" (lit (config.height - config.marginBottom))
                    , v3AttrStr "stroke" (str "#333")
                    , v3Attr "stroke-width" (lit 1.5)
                    ]
                ]

          -- Y-axis ticks
          , T.named Group "y-ticks" []
              `T.withChildren` map (yAxisTick config) yTicks

          -- Y-axis label
          , T.elem Text
              [ v3Attr "x" (lit 15.0)
              , v3Attr "y" (lit (config.height / 2.0))
              , v3AttrStr "text-anchor" (str "middle")
              , v3Attr "font-size" (lit 14.0)
              , v3AttrStr "fill" (str "#333")
              , v3AttrStr "textContent" (str "Life Expectancy (years)")
              ]

          -- Empty group for nations (will be populated by update function)
          , T.named Group "nations-container" [ v3AttrStr "class" (str "nations") ] `T.withChildren` []
          ]

  -- Render the static structure
  _ <- renderTree container staticTree

  -- Return the update function
  pure $ \nations -> runD3v2M do
    -- Select the nations container
    nationsContainer <- select "#wealth-health-viz .nations" :: _ (D3v2Selection_ SEmpty Element Unit)

    -- Data join using Selection API with nation name as key
    JoinResult { enter, update: updateSel, exit } <- joinDataWithKey nations (_.name) "circle" nationsContainer

    -- Attributes for circles (using datum-only functions with explicit types)
    let
      circleAttrs :: Array (Attribute NationPoint)
      circleAttrs =
        [ v3AttrFn "cx" ((\n -> scaleX config n.income) :: NationPoint -> Number)
        , v3AttrFn "cy" ((\n -> scaleY config n.lifeExpectancy) :: NationPoint -> Number)
        , v3AttrFn "r" ((\n -> scaleRadius n.population) :: NationPoint -> Number)
        , v3AttrFnStr "fill" ((_.regionColor) :: NationPoint -> String)
        , v3Attr "fill-opacity" (lit 0.7)
        , v3AttrStr "stroke" (str "#333")
        , v3Attr "stroke-width" (lit 0.5)
        , v3AttrStr "class" (str "nation-circle")
        ]

    -- Handle enter: create new circles
    enterCircles <- append Circle circleAttrs enter

    -- Handle update: update existing circles
    _ <- setAttrs circleAttrs updateSel

    -- Handle exit: remove old circles
    remove exit

    -- Merge enter and update for tooltips
    merged <- merge enterCircles updateSel

    -- Add tooltips to all circles
    _ <- on
      ( onMouseEnterWithInfo \(info :: MouseEventInfo NationPoint) -> do
          let nation = info.datum
          let
            content = "<strong>" <> nation.name <> "</strong><br/>"
              <> "Region: "
              <> nation.region
              <> "<br/>"
              <> "Income: $"
              <> show (floor nation.income)
              <> "<br/>"
              <> "Life Expectancy: "
              <> show (floor nation.lifeExpectancy)
              <> " years<br/>"
              <> "Population: "
              <> formatPopulation nation.population
          showTooltip content info.clientX info.clientY
      )
      merged

    -- Hide tooltip on leave
    _ <- on (onMouseLeaveWithInfo \(_ :: MouseEventInfo NationPoint) -> hideTooltip) merged

    pure unit

-- | Format population for display
formatPopulation :: Number -> String
formatPopulation n
  | n >= 1000000000.0 = show (floor (n / 1000000000.0)) <> "B"
  | n >= 1000000.0 = show (floor (n / 1000000.0)) <> "M"
  | n >= 1000.0 = show (floor (n / 1000.0)) <> "K"
  | otherwise = show (floor n)
