module D3.Viz.FpFtw.MapQuartet where

import Prelude

import PSD3.Internal.Attributes.Sugar (classed, cx, cy, fill, fontSize, height, radius, strokeColor, strokeWidth, text, textAnchor, transform, viewBox, width, x, y)
import PSD3.Internal.Axes (axisBottom_, axisLeft_, callAxis_)
import PSD3.Internal.Types (D3Selection_, Element(..), Selector)
import PSD3.Internal.Scales.Linear (applyScale_, createLinearScale_)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach)
import Data.Array (range)
import Data.Map (Map)
import Data.Map as Map
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Random (random)
import Effect (Effect)

-- | Data point for scatter plot
type DataPoint = {
    x :: Number
  , y :: Number
}

-- | Quartet of Map-based datasets
type MapQuartetData = {
    dataset1 :: Map Number Number
  , dataset2 :: Map Number Number
  , dataset3 :: Map Number Number
  , dataset4 :: Map Number Number
}

-- | Generate random y value with some noise around a base pattern
randomY :: Number -> Effect Number
randomY x = do
  noise <- random
  -- Linear relationship with noise: y ≈ 0.5*x + random scatter
  let baseY = 10.0 + 0.5 * x + (noise - 0.5) * 20.0
  pure baseY

-- | Generate a randomized Map with the given x values
generateRandomMap :: Array Number -> Effect (Map Number Number)
generateRandomMap xValues = do
  tuples <- traverse (\xVal -> do
    yVal <- randomY xVal
    pure (Tuple xVal yVal)
  ) xValues
  pure $ Map.fromFoldable tuples

-- | Generate a sparse set of random x values in the range [0, 200]
generateSparseKeys :: Int -> Effect (Array Number)
generateSparseKeys count = do
  -- Generate random indices between 0 and 200
  traverse (\_ -> do
    r <- random
    pure (r * 200.0)
  ) (range 1 count)

-- | Generate quartet data with different sparse Maps
-- | Each Map has only ~15 points out of a possible 200 x-values
generateMapQuartet :: Effect MapQuartetData
generateMapQuartet = do
  -- Each dataset has different randomly selected x values (keys)
  -- This demonstrates the power of Maps for sparse data!
  xValues1 <- generateSparseKeys 15
  xValues2 <- generateSparseKeys 15
  xValues3 <- generateSparseKeys 15
  xValues4 <- generateSparseKeys 15

  map1 <- generateRandomMap xValues1
  map2 <- generateRandomMap xValues2
  map3 <- generateRandomMap xValues3
  map4 <- generateRandomMap xValues4

  pure { dataset1: map1, dataset2: map2, dataset3: map3, dataset4: map4 }

-- | Draw quartet using Maps
drawMapQuartet :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  MapQuartetData -> Selector (D3Selection_ Unit) -> m Unit
drawMapQuartet quartet selector = do
  -- Overall dimensions for the quartet display
  let totalWidth = 900.0
  let totalHeight = 700.0
  let padding = 60.0

  -- Calculate dimensions for each subplot
  let plotWidth = (totalWidth - padding * 3.0) / 2.0
  let plotHeight = (totalHeight - padding * 3.0) / 2.0
  let margin = { top: 30.0, right: 20.0, bottom: 40.0, left: 50.0 }
  let iWidth = plotWidth - margin.left - margin.right
  let iHeight = plotHeight - margin.top - margin.bottom

  -- Use fixed scale domains for all four plots (for valid comparison)
  -- Wide x-axis range [0, 200] to show sparsity - only ~15 points in this space!
  let xDomain = [0.0, 200.0]
  let yDomain = [0.0, 120.0]

  (root :: D3Selection_ Unit) <- attach selector
  svg <- appendTo root Svg [
      viewBox 0.0 0.0 totalWidth totalHeight
    , classed "map-quartet"
    , width totalWidth
    , height totalHeight
    ]

  -- Helper function to draw a single subplot using a Map
  let drawSubplot :: String -> Map Number Number -> Number -> Number -> m Unit
      drawSubplot title dataMap xOffset yOffset = do
        -- Create group for this subplot
        subplotGroup <- appendTo svg Group [
            classed "subplot"
          , transform [ \_ -> "translate(" <> show (xOffset + margin.left) <> "," <> show (yOffset + margin.top) <> ")" ]
          ]

        -- Add title
        _ <- appendTo svg Text [
            x (xOffset + plotWidth / 2.0)
          , y (yOffset + 15.0)
          , text title
          , textAnchor "middle"
          , fontSize 16.0
          , classed "subplot-title"
          ]

        -- Create scales
        xScale <- liftEffect $ createLinearScale_ { domain: xDomain, range: [0.0, iWidth] }
        yScale <- liftEffect $ createLinearScale_ { domain: yDomain, range: [iHeight, 0.0] }

        -- Add axes
        xAxisGroup <- appendTo subplotGroup Group [
            classed "x-axis"
          , transform [ \_ -> "translate(0," <> show iHeight <> ")" ]
          ]
        yAxisGroup <- appendTo subplotGroup Group [ classed "y-axis" ]

        _ <- liftEffect $ callAxis_ xAxisGroup (axisBottom_ xScale)
        _ <- liftEffect $ callAxis_ yAxisGroup (axisLeft_ yScale)

        -- Convert Map to Array for visualization
        -- This showcases the Foldable capability - Maps can be folded to arrays!
        let dataPoints = Map.toUnfoldable dataMap :: Array (Tuple Number Number)

        -- Add data points as circles
        let addPoint :: Tuple Number Number -> m Unit
            addPoint (Tuple xVal yVal) = do
              let xPos = applyScale_ xScale xVal
              let yPos = applyScale_ yScale yVal
              _ <- appendTo subplotGroup Circle [
                  cx xPos
                , cy yPos
                , radius 4.0
                , fill "#9b59b6"
                , strokeColor "#8e44ad"
                , strokeWidth 1.5
                , classed "scatter-point"
                ]
              pure unit

        _ <- traverse addPoint dataPoints

        pure unit

  -- Draw all four subplots in a 2x2 grid
  _ <- drawSubplot "Map 1 (15/200)" quartet.dataset1 padding (padding)
  _ <- drawSubplot "Map 2 (15/200)" quartet.dataset2 (padding + plotWidth + padding) (padding)
  _ <- drawSubplot "Map 3 (15/200)" quartet.dataset3 padding (padding + plotHeight + padding)
  _ <- drawSubplot "Map 4 (15/200)" quartet.dataset4 (padding + plotWidth + padding) (padding + plotHeight + padding)

  pure unit
