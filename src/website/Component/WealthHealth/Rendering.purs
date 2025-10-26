module PSD3.WealthHealth.Rendering where

import Prelude

import Data.Foldable (for_)
import Effect (Effect)
import PSD3.WealthHealth.Data (getAllNationsAtYear)
import PSD3.WealthHealth.Types (NationPoint, WealthHealthModel, regionColor)

-- | Configuration for the visualization dimensions
type VizConfig =
  { width :: Number
  , height :: Number
  , marginTop :: Number
  , marginRight :: Number
  , marginBottom :: Number
  , marginLeft :: Number
  }

-- | Default visualization configuration
defaultConfig :: VizConfig
defaultConfig =
  { width: 1000.0
  , height: 600.0
  , marginTop: 20.0
  , marginRight: 20.0
  , marginBottom: 35.0
  , marginLeft: 40.0
  }

-- | Render the complete D3 visualization for a given year
-- | Takes hover callbacks for interactivity
renderVisualization :: String -> Int -> WealthHealthModel -> (String -> Effect Unit) -> (Unit -> Effect Unit) -> Effect Unit
renderVisualization containerId year model onHover onLeave = do
  let config = defaultConfig
  let nations = getAllNationsAtYear year model

  -- Clear any existing visualization
  clearContainer containerId

  -- Create SVG element
  svg <- createSVG containerId config.width config.height

  -- Create scales
  let xScale = createLogScale 200.0 100000.0 config.marginLeft (config.width - config.marginRight)
  let yScale = createLinearScale 14.0 86.0 (config.height - config.marginBottom) config.marginTop
  let rScale = createSqrtScale 0.0 5000000000.0 0.0 40.0

  -- Render axes
  renderXAxis svg xScale (config.height - config.marginBottom) config.marginLeft (config.width - config.marginRight)
  renderYAxis svg yScale config.marginTop (config.height - config.marginBottom) config.marginLeft

  -- Render axis labels
  renderXAxisLabel svg "Income per capita (dollars)" config.width (config.height - config.marginBottom)
  renderYAxisLabel svg "Life expectancy (years)" config.marginLeft config.height

  -- Render gridlines
  renderGridlines svg xScale yScale config

  -- Render nations as circles with hover callbacks
  for_ nations \nation -> do
    renderNation svg xScale yScale rScale nation onHover onLeave

-- | Render a single nation as a circle
renderNation :: SVGElement -> Scale -> Scale -> Scale -> NationPoint -> (String -> Effect Unit) -> (Unit -> Effect Unit) -> Effect Unit
renderNation svg xScale yScale rScale nation onHover onLeave = do
  let x = applyScale xScale nation.income
  let y = applyScale yScale nation.lifeExpectancy
  let r = applyScale rScale nation.population
  let color = regionColor nation.region

  addCircle svg x y r color nation.name onHover onLeave

-- | Update the visualization when the year changes (with transitions)
updateVisualization :: String -> Int -> WealthHealthModel -> (String -> Effect Unit) -> (Unit -> Effect Unit) -> Effect Unit
updateVisualization containerId year model onHover onLeave = do
  let nations = getAllNationsAtYear year model
  let config = defaultConfig

  -- Create scales (same as initial render)
  let xScale = createLogScale 200.0 100000.0 config.marginLeft (config.width - config.marginRight)
  let yScale = createLinearScale 14.0 86.0 (config.height - config.marginBottom) config.marginTop
  let rScale = createSqrtScale 0.0 5000000000.0 0.0 40.0

  -- Update circles with transitions
  updateCircles containerId nations xScale yScale rScale onHover onLeave

-- FFI imports for D3 operations

-- | Opaque types for D3 objects
foreign import data SVGElement :: Type
foreign import data Scale :: Type

-- | Clear the container
foreign import clearContainer :: String -> Effect Unit

-- | Create SVG element
foreign import createSVG :: String -> Number -> Number -> Effect SVGElement

-- | Create logarithmic scale
foreign import createLogScale :: Number -> Number -> Number -> Number -> Scale

-- | Create linear scale
foreign import createLinearScale :: Number -> Number -> Number -> Number -> Scale

-- | Create square root scale for circle radius
foreign import createSqrtScale :: Number -> Number -> Number -> Number -> Scale

-- | Apply a scale to a value
foreign import applyScale :: Scale -> Number -> Number

-- | Render X axis
foreign import renderXAxis :: SVGElement -> Scale -> Number -> Number -> Number -> Effect Unit

-- | Render Y axis
foreign import renderYAxis :: SVGElement -> Scale -> Number -> Number -> Number -> Effect Unit

-- | Render X axis label
foreign import renderXAxisLabel :: SVGElement -> String -> Number -> Number -> Effect Unit

-- | Render Y axis label
foreign import renderYAxisLabel :: SVGElement -> String -> Number -> Number -> Effect Unit

-- | Render gridlines
foreign import renderGridlines :: SVGElement -> Scale -> Scale -> VizConfig -> Effect Unit

-- | Add a circle to the SVG
foreign import addCircle :: SVGElement -> Number -> Number -> Number -> String -> String -> (String -> Effect Unit) -> (Unit -> Effect Unit) -> Effect Unit

-- | Update circles with transition
foreign import updateCircles :: String -> Array NationPoint -> Scale -> Scale -> Scale -> (String -> Effect Unit) -> (Unit -> Effect Unit) -> Effect Unit
