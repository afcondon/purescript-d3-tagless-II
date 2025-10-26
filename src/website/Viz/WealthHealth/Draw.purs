module D3.Viz.WealthHealth.Draw where

import Prelude

import D3.Viz.WealthHealth.Unsafe (coerceDatumToKey, datum_)
import Data.Number (log, sqrt)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach, openSelection, setAttributes, updateJoin)
import PSD3.Internal.Attributes.Sugar (classed, cx, cy, fill, fillOpacity, height, radius, strokeColor, strokeWidth, viewBox, width)
import PSD3.Internal.Types (D3Selection_, Datum_, Element(..), Index_, Selector)

-- | Type alias for a nation data point ready for visualization
type NationPoint =
  { name :: String
  , income :: Number
  , population :: Number
  , lifeExpectancy :: Number
  , regionColor :: String
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
    maxPop = 5000000000.0  -- 5 billion
    normalized = population / maxPop
  in
    sqrt normalized * 40.0  -- Max radius of 40px

-- | Initialize the visualization structure (SVG, axes, etc.)
initializeVisualization :: forall m.
  SelectionM D3Selection_ m =>
  Selector D3Selection_ ->
  m { svg :: D3Selection_, chartGroup :: D3Selection_ }
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
  D3Selection_ ->
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
  Selector D3Selection_ ->
  m (Array NationPoint -> m D3Selection_)
draw selector = do
  let config = defaultConfig

  (root :: D3Selection_) <- attach selector
  svg <- appendTo root Svg
    [ viewBox 0.0 0.0 config.width config.height
    , width config.width
    , height config.height
    , classed "wealth-health-viz"
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

    -- Use General Update Pattern for data binding
    enterSelection <- openSelection chartGroup "circle"
    updateSelections <- updateJoin enterSelection Circle nations coerceDatumToKey

    -- Exit: remove circles for nations that disappeared
    setAttributes updateSelections.exit
      [ classed "exit" ]

    -- Update: move existing circles to new positions
    setAttributes updateSelections.update
      [ cx \d i -> (calculateAttrs d i).x
      , cy \d i -> (calculateAttrs d i).y
      , radius \d i -> (calculateAttrs d i).r
      , fill \d i -> (calculateAttrs d i).color
      , classed "update"
      ]

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

    pure newCircles
