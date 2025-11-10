module D3.Viz.WealthHealth.Draw where

import Prelude

import D3.Viz.WealthHealth.Unsafe (coerceDatumToKey, datum_)
import Data.Int (floor)
import Data.Number (log, sqrt)
import Data.Traversable (traverse)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach, openSelection, setAttributes, updateJoin)
import PSD3.Internal.Attributes.Sugar (classed, cx, cy, fill, fillOpacity, fontSize, height, radius, sortSelection, strokeColor, strokeOpacity, strokeWidth, text, textAnchor, viewBox, width, x, x1, x2, y, y1, y2)
import PSD3.Internal.Types (D3Selection_, Datum_, Element(..), Index_, Selector)

-- | Type alias for a nation data point ready for visualization
type NationPoint =
  { name :: String
  , income :: Number
  , population :: Number
  , lifeExpectancy :: Number
  , regionColor :: String
  , region :: String
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
  { width: 1000.0
  , height: 600.0
  , marginTop: 20.0
  , marginRight: 20.0
  , marginBottom: 35.0
  , marginLeft: 40.0
  }

-- | Scale functions (simple linear/log approximations)
-- | TODO: Use proper D3 scales via FFI for production
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
    minRadius = 3.0  -- Minimum radius for visibility
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

-- | Generate tick values for logarithmic income scale
-- | Similar to D3's x.ticks() for log scale
xTicks :: Array Number
xTicks = [200.0, 500.0, 1000.0, 2000.0, 5000.0, 10000.0, 20000.0, 50000.0, 100000.0]

-- | Generate tick values for linear life expectancy scale
-- | Similar to D3's y.ticks() for linear scale
yTicks :: Array Number
yTicks = [20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0]

-- | Convert Ordering to Int for D3 sort function
orderingToInt :: Ordering -> Int
orderingToInt LT = -1
orderingToInt EQ = 0
orderingToInt GT = 1

-- | Initialize the visualization structure (SVG, axes, etc.)
initializeVisualization :: forall m.
  SelectionM D3Selection_ m =>
  Selector (D3Selection_ Unit) ->
  m { svg :: D3Selection_ Unit, chartGroup :: D3Selection_ Unit }
initializeVisualization selector = do
  let config = defaultConfig

  root <- attach selector
  svg <- appendTo root Svg
    [ viewBox 0.0 0.0 config.width config.height
    , width config.width
    , height config.height
    , classed "wealth-health-viz"
    ]

  -- Create main chart group
  chartGroup <- appendTo svg Group [ classed "nations" ]

  -- TODO: Add axes and gridlines using PS<$>D3
  -- For now we'll just have the circles

  pure { svg, chartGroup }

-- | Update visualization with new data for a given year
updateVisualization :: forall m.
  SelectionM D3Selection_ m =>
  D3Selection_ Unit ->
  Array NationPoint ->
  m Unit
updateVisualization chartGroup nations = do
  let config = defaultConfig

  -- Calculate position and size for each nation
  let calculateAttrs :: Datum_ -> Index_ ->
        { x :: Number
        , y :: Number
        , r :: Number
        , color :: String
        }
      calculateAttrs d _ =
        let
          income = datum_.income d
          life = datum_.lifeExpectancy d
          pop = datum_.population d
          color = datum_.regionColor d
        in
          { x: scaleX config income
          , y: scaleY config life
          , r: scaleRadius pop
          , color
          }

  -- Use General Update Pattern for data binding
  enterSelection <- openSelection chartGroup "circle"
  updateSelections <- updateJoin enterSelection Circle nations coerceDatumToKey

  -- Exit: remove circles for nations that disappeared
  setAttributes updateSelections.exit
    [ classed "exit" ]
    -- TODO: Add exit transition

  -- Update: move existing circles to new positions
  setAttributes updateSelections.update
    [ cx \d i -> (calculateAttrs d i).x
    , cy \d i -> (calculateAttrs d i).y
    , radius \d i -> (calculateAttrs d i).r
    , fill \d i -> (calculateAttrs d i).color
    , classed "update"
    ]
    -- TODO: Add update transition

  -- Enter: create new circles
  newCircles <- appendTo updateSelections.enter Circle []
  setAttributes newCircles
    [ cx \d i -> (calculateAttrs d i).x
    , cy \d i -> (calculateAttrs d i).y
    , radius \d i -> (calculateAttrs d i).r
    , fill \d i -> (calculateAttrs d i).color
    , fillOpacity 0.7
    , strokeColor "#333"
    , strokeWidth 0.5
    , classed "nation-circle enter"
    ]

  pure unit

-- | Main draw function - creates structure once and returns update function
-- | Following the GUP pattern
draw :: forall m.
  SelectionM D3Selection_ m =>
  Selector (D3Selection_ Unit) ->
  m (Array NationPoint -> m (D3Selection_ NationPoint))
draw selector = do
  let config = defaultConfig

  root <- attach selector
  svg <- appendTo root Svg
    [ viewBox 0.0 0.0 config.width config.height
    , width config.width
    , height config.height
    , classed "wealth-health-viz"
    ]

  -- Add grid lines (matching Mike Bostock's approach)
  gridGroup <- appendTo svg Group
    [ strokeColor "currentColor"
    , strokeOpacity 0.1
    , classed "grid"
    ]

  -- Vertical grid lines at x ticks (create manually for each tick)
  let
    createVerticalLine tickValue = do
      line <- appendTo gridGroup Line []
      setAttributes line
        [ x1 (0.5 + scaleX config tickValue)
        , x2 (0.5 + scaleX config tickValue)
        , y1 config.marginTop
        , y2 (config.height - config.marginBottom)
        ]
      pure line

  _ <- traverse createVerticalLine xTicks

  -- Horizontal grid lines at y ticks (create manually for each tick)
  let
    createHorizontalLine tickValue = do
      line <- appendTo gridGroup Line []
      setAttributes line
        [ y1 (0.5 + scaleY config tickValue)
        , y2 (0.5 + scaleY config tickValue)
        , x1 config.marginLeft
        , x2 (config.width - config.marginRight)
        ]
      pure line

  _ <- traverse createHorizontalLine yTicks

  -- Add X-axis (bottom)
  xAxisLine <- appendTo svg Line
    [ x1 config.marginLeft
    , y1 (config.height - config.marginBottom)
    , x2 (config.width - config.marginRight)
    , y2 (config.height - config.marginBottom)
    , strokeColor "#333"
    , strokeWidth 1.5
    , classed "x-axis"
    ]

  -- Add X-axis tick marks and labels
  let
    createXTick tickValue = do
      -- Tick mark (short vertical line)
      tickMark <- appendTo svg Line []
      setAttributes tickMark
        [ x1 (scaleX config tickValue)
        , x2 (scaleX config tickValue)
        , y1 (config.height - config.marginBottom)
        , y2 (config.height - config.marginBottom + 6.0)
        , strokeColor "#333"
        , strokeWidth 1.0
        ]
      -- Tick label (formatted income value)
      tickLabel <- appendTo svg Text []
      setAttributes tickLabel
        [ x (scaleX config tickValue)
        , y (config.height - config.marginBottom + 15.0)
        , textAnchor "middle"
        , fontSize 10.0
        , fill "#333"
        , text $ formatIncome tickValue
        ]
      pure unit

  _ <- traverse createXTick xTicks

  -- Add X-axis label (Wealth)
  xAxisLabel <- appendTo svg Text
    [ x (config.width / 2.0)
    , y (config.height - 5.0)
    , textAnchor "middle"
    , fontSize 14.0
    , fill "#333"
    , classed "x-axis-label"
    , text "Wealth"
    ]

  -- Add Y-axis (left)
  yAxisLine <- appendTo svg Line
    [ x1 config.marginLeft
    , y1 config.marginTop
    , x2 config.marginLeft
    , y2 (config.height - config.marginBottom)
    , strokeColor "#333"
    , strokeWidth 1.5
    , classed "y-axis"
    ]

  -- Add Y-axis tick marks and labels
  let
    createYTick tickValue = do
      -- Tick mark (short horizontal line)
      tickMark <- appendTo svg Line []
      setAttributes tickMark
        [ x1 (config.marginLeft - 6.0)
        , x2 config.marginLeft
        , y1 (scaleY config tickValue)
        , y2 (scaleY config tickValue)
        , strokeColor "#333"
        , strokeWidth 1.0
        ]
      -- Tick label (life expectancy value)
      tickLabel <- appendTo svg Text []
      setAttributes tickLabel
        [ x (config.marginLeft - 10.0)
        , y (scaleY config tickValue + 3.0)
        , textAnchor "end"
        , fontSize 10.0
        , fill "#333"
        , text $ show $ floor tickValue
        ]
      pure unit

  _ <- traverse createYTick yTicks

  -- Add Y-axis label (Health) - rotated
  yAxisLabel <- appendTo svg Text
    [ x 15.0
    , y (config.height / 2.0)
    , textAnchor "middle"
    , fontSize 14.0
    , fill "#333"
    , classed "y-axis-label"
    , text "Health"
    ]

  chartGroup <- appendTo svg Group [ classed "nations" ]

  -- Return update function
  pure $ \nations -> do
    -- Calculate position and size for each nation
    let calculateAttrs :: Datum_ -> Index_ ->
          { x :: Number
          , y :: Number
          , r :: Number
          , color :: String
          }
        calculateAttrs d _ =
          let
            income = datum_.income d
            life = datum_.lifeExpectancy d
            pop = datum_.population d
            color = datum_.regionColor d
          in
            { x: scaleX config income
            , y: scaleY config life
            , r: scaleRadius pop
            , color
            }

    -- Use General Update Pattern for circles (we know this works)
    circleEnterSelection <- openSelection chartGroup "circle"
    circleUpdateSelections <- updateJoin circleEnterSelection Circle nations coerceDatumToKey

    -- Exit: remove circles for nations that disappeared
    setAttributes circleUpdateSelections.exit
      [ classed "exit" ]

    -- Update: move existing circles to new positions
    -- Sort by population (descending) so largest circles are drawn last
    setAttributes circleUpdateSelections.update
      [ sortSelection \a b ->
          let popA = datum_.population a
              popB = datum_.population b
          in orderingToInt $ compare popB popA  -- Descending order
      , cx \d i -> (calculateAttrs d i).x
      , cy \d i -> (calculateAttrs d i).y
      , radius \d i -> (calculateAttrs d i).r
      , fill \d i -> (calculateAttrs d i).color
      , classed "nation-circle update"
      ]

    -- Enter: create new circles
    newCircles <- appendTo circleUpdateSelections.enter Circle []
    setAttributes newCircles
      [ sortSelection \a b ->
          let popA = datum_.population a
              popB = datum_.population b
          in orderingToInt $ compare popB popA  -- Descending order
      , cx \d i -> (calculateAttrs d i).x
      , cy \d i -> (calculateAttrs d i).y
      , radius \d i -> (calculateAttrs d i).r
      , fill \d i -> (calculateAttrs d i).color
      , fillOpacity 0.7
      , strokeColor "#333"
      , strokeWidth 0.5
      , classed "nation-circle enter"
      ]

    -- Add title tooltips to entering circles (browser-native hover)
    let tooltipText d = datum_.name d <> "\n" <> datum_.region d
    newTitles <- appendTo newCircles Title []
    setAttributes newTitles
      [ text tooltipText ]

    -- Use General Update Pattern for labels (parallel join)
    labelEnterSelection <- openSelection chartGroup "text"
    labelUpdateSelections <- updateJoin labelEnterSelection Text nations coerceDatumToKey

    -- Exit: remove labels for nations that disappeared
    setAttributes labelUpdateSelections.exit
      [ classed "exit" ]

    -- Update: move existing labels to new positions
    setAttributes labelUpdateSelections.update
      [ x \d i -> (calculateAttrs d i).x
      , y \d i -> (calculateAttrs d i).y - (calculateAttrs d i).r - 5.0
      , classed "nation-label update"
      ]

    -- Enter: create new labels
    newLabels <- appendTo labelUpdateSelections.enter Text []
    setAttributes newLabels
      [ x \d i -> (calculateAttrs d i).x
      , y \d i -> (calculateAttrs d i).y - (calculateAttrs d i).r - 5.0
      , textAnchor "middle"
      , fontSize 11.0
      , fill "#333"
      , fillOpacity 0.0  -- Initially hidden
      , text datum_.name
      , classed "nation-label enter"
      ]

    pure newCircles
