module D3.Viz.ThreeLittleCirclesTransitionV2 where

-- | Three Little Circles with Transitions using PSD3v2
-- |
-- | Demonstrates:
-- | - Type-safe transitions with phantom types
-- | - Animated color, position, and size changes
-- | - Clean DSL for transition timing
-- |
-- | Based on Mike Bostock's "Three Little Circles" (https://bost.ocks.org/mike/circles/)
-- | See Acknowledgements page for full credits

import Prelude

import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import PSD3v2.Attribute.Types (Attribute, fill, radius, cx, cy, width, height, id_, viewBox, fillOpacity)
import PSD3v2.Capabilities.Selection (class SelectionM, select, appendChild, renderData)
import PSD3v2.Capabilities.Transition (class TransitionM, withTransition)
import PSD3v2.Interpreter.D3v2 (runD3v2M)
import PSD3v2.Selection.Types (ElementType(Circle, SVG))
import PSD3v2.Transition.Types (Easing(ElasticOut), transition, transitionWith)

-- | Draw three circles and then animate them
-- |
-- | This demonstrates:
-- | - Initial render with green circles
-- | - Transition to RGB primary colors (red, green, blue)
-- | - Animated position changes to create overlapping effect
-- | - Animated opacity for color mixing visualization
-- |
-- | The transitions show:
-- | 1. Color change: green → RGB primaries
-- | 2. Size change: radius 10 → 20
-- | 3. Position change: horizontal line → triangle formation
-- | 4. Opacity change: 1.0 → 0.5 for color mixing
drawThreeCirclesTransition :: forall m sel. SelectionM sel m => TransitionM sel m => String -> m Unit
drawThreeCirclesTransition selector = do
  -- Select the container element
  container <- select selector

  -- Create SVG element using the DSL
  svg <- appendChild SVG
    [ id_ "three-circles-transition-v2-svg"
    , width 400.0
    , height 200.0
    , viewBox "0 0 400 200"
    ]
    container

  -- Initial render: three green circles in a horizontal line
  circles <- renderData Circle [0, 1, 2] "circle" svg
    (Just enterAttrs)
    Nothing
    Nothing

  -- Transition to RGB primary colors with overlapping positions
  -- Duration: 1.5 seconds
  -- Easing: ElasticOut for bouncy effect
  -- Delay: 100ms to allow initial render to paint
  let transitionConfig = transitionWith
        { duration: Milliseconds 1500.0
        , delay: Just (Milliseconds 100.0)
        , easing: Just ElasticOut
        }

  withTransition transitionConfig circles
    [ fill colorFn
    , cx cxFn
    , cy cyFn
    , radius 30.0          -- Larger for better overlap
    , fillOpacity 0.5      -- Semi-transparent for color mixing
    ]

  pure unit
  where
    -- Initial attributes: green circles in horizontal line
    enterAttrs :: Int -> Array (Attribute Int)
    enterAttrs _ =
      [ fill "green"
      , cx (\d -> toNumber d * 100.0 + 100.0)  -- Evenly spaced: 100, 200, 300
      , cy 100.0                                 -- Middle of SVG
      , radius 15.0
      , fillOpacity 1.0
      ]

    -- Target colors: Red, Green, Blue
    colorFn :: Int -> String
    colorFn d = case d of
      0 -> "#ff0000"  -- Red
      1 -> "#00ff00"  -- Green
      _ -> "#0000ff"  -- Blue

    -- Target X positions: form triangle with overlap
    cxFn :: Int -> Number
    cxFn d = case d of
      0 -> 150.0  -- Red on left
      1 -> 250.0  -- Green on right
      _ -> 200.0  -- Blue in center

    -- Target Y positions: red/green at bottom, blue at top
    cyFn :: Int -> Number
    cyFn d = case d of
      0 -> 130.0  -- Red at bottom
      1 -> 130.0  -- Green at bottom
      _ -> 70.0   -- Blue at top

-- | Run the example with the D3v2 interpreter (for standalone testing)
main :: Effect Unit
main = runD3v2M (drawThreeCirclesTransition "#example-viz")
