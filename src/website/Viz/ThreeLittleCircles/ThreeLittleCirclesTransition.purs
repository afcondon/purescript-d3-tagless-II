module D3.Viz.ThreeLittleCirclesTransition where

-- | Transition demonstration with color mixing
-- | Shows three circles transitioning from green to RGB primary colors
-- | with overlapping positioning to demonstrate color mixing
-- | See Acknowledgements page for full credits

import Prelude

import D3.Viz.ThreeLittleCircles.Unsafe (coerceDatumToInt)
import Data.Time.Duration (Milliseconds(..))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach, setAttributes, simpleJoin)
import PSD3.Internal.Attributes.Sugar (classed, cx, cy, fill, fillOpacity, radius, to, transitionWithDuration, viewBox)
import PSD3.Internal.FFI (keyIsID_)
import PSD3.Internal.Types (D3Selection_, Datum_, Element(..), Index_, Selector, index_ToNumber)

-- | Draw three circles with initial green color and add transition button
drawThreeCirclesTransition :: forall m. MonadEffect m => SelectionM D3Selection_ m => Selector (D3Selection_ Unit) -> m { circles :: D3Selection_ Int, svg :: D3Selection_ Unit }
drawThreeCirclesTransition selector = do
  liftEffect $ log "=== drawThreeCirclesTransition: Starting ==="
  root        <- attach selector
  svg         <- appendTo root Svg [ viewBox (-10.0) (-10.0) 120.0 100.0, classed "d3svg three-circles-transition" ]
  circleGroup <- appendTo svg Group []

  -- Create three circles starting in green
  liftEffect $ log "=== Creating initial join with data: [0, 1, 2] ==="
  circles     <- simpleJoin circleGroup Circle [0, 1, 2] keyIsID_
  liftEffect $ log "=== Join complete, setting attributes ==="
  setAttributes circles
    [ fill "orange"  -- Changed to orange to make entering circles visible
    , cx (\(_ :: Int) i -> (index_ToNumber i) * 30.0 + 20.0)
    , cy 30.0
    , radius 15.0
    , fillOpacity 1.0
    ]
  liftEffect $ log "=== Initial circles created ==="

  pure { circles, svg }

-- | Transition circles to RGB primary colors with overlapping positions
transitionToColorMixing :: forall m. MonadEffect m => SelectionM D3Selection_ m => D3Selection_ Int -> m Unit
transitionToColorMixing circles = do
  -- Transition to primary colors (red, green, blue) with 50% opacity
  -- Position them to overlap and create secondary colors
  let transition = transitionWithDuration $ Milliseconds 1500.0

      -- Color based on datum
      colorFn :: Int -> Index_ -> String
      colorFn d _ = case d of
        0 -> "#ff0000"  -- Red
        1 -> "#00ff00"  -- Green
        _ -> "#0000ff"  -- Blue

      -- X position based on datum
      cxFn :: Int -> Index_ -> Number
      cxFn d _ = case d of
        0 -> 30.0  -- Red on left
        1 -> 60.0  -- Green on right
        _ -> 45.0  -- Blue in center

      -- Y position based on datum
      cyFn :: Int -> Index_ -> Number
      cyFn d _ = case d of
        0 -> 50.0  -- Red at bottom
        1 -> 50.0  -- Green at bottom
        _ -> 30.0  -- Blue at top

  setAttributes circles $
    transition `to`
      [ fill colorFn
      , fillOpacity 0.5  -- 50% opacity for color mixing
      , cx cxFn
      , cy cyFn
      , radius 20.0  -- Larger circles for better overlap
      ]

  pure unit
