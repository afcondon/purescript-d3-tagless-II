module D3.Viz.TreeAPI.ThreeLittleCirclesTransition where

-- | Three Little Circles with Transitions using Tree API
-- |
-- | Demonstrates:
-- | - Initial render with Tree API
-- | - Re-rendering with updated data
-- | - Transitions between states
-- |
-- | Based on Mike Bostock's "Three Little Circles" (https://bost.ocks.org/mike/circles/)

import Prelude

import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import PSD3v2.Attribute.Types (width, height, viewBox, id_, fill, cx, cy, radius, fillOpacity)
import PSD3v2.Capabilities.Selection (select, renderTree)
import PSD3v2.Capabilities.Transition (withTransition)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2Selection_)
import Partial.Unsafe (unsafePartial, unsafeCrashWith)
import PSD3v2.Selection.Types (ElementType(..), SEmpty)
import PSD3v2.Transition.Types (transitionWith, Easing(ElasticOut))
import PSD3v2.VizTree.Tree as T
import Web.DOM.Element (Element)

-- | Circle data type
type CircleData = Int

-- | Draw three circles and then animate them
threeLittleCirclesTransition :: Effect Unit
threeLittleCirclesTransition = runD3v2M do
  container <- select "#viz" :: _ (D3v2Selection_ SEmpty Element Unit)

  -- Initial tree: three green circles in a horizontal line
  let initialTree :: T.Tree CircleData
      initialTree =
        T.named SVG "svg"
          [ width 400.0
          , height 200.0
          , viewBox "0 0 400 200"
          , id_ "three-circles-transition-tree"
          ]
          `T.withChild`
            (T.joinData "circles" "circle" [0, 1, 2] $ \d ->
              T.elem Circle
                [ fill "green"
                , cx (toNumber d * 100.0 + 100.0)  -- Evenly spaced: 100, 200, 300
                , cy 100.0                          -- Middle of SVG
                , radius 15.0
                , fillOpacity 1.0
                ]
            )

  -- Render initial state
  selections <- renderTree container initialTree

  -- Extract the circles selection for transition
  -- The circles selection is already SBound from the joinData
  let circlesSel = case Map.lookup "circles" selections of
        Just sel -> sel
        Nothing -> unsafePartial $ unsafeCrashWith "circles selection not found"

  -- Transition to RGB primary colors with overlapping positions
  let transitionConfig = transitionWith
        { duration: Milliseconds 1500.0
        , delay: Just (Milliseconds 100.0)
        , easing: Just ElasticOut
        }

  withTransition transitionConfig circlesSel
    [ fill colorFn
    , cx cxFn
    , cy cyFn
    , radius 30.0          -- Larger for better overlap
    , fillOpacity 0.5      -- Semi-transparent for color mixing
    ]

  pure unit
  where
    -- Target colors: Red, Green, Blue
    colorFn :: CircleData -> String
    colorFn d = case d of
      0 -> "#ff0000"  -- Red
      1 -> "#00ff00"  -- Green
      _ -> "#0000ff"  -- Blue

    -- Target X positions: form triangle with overlap
    cxFn :: CircleData -> Number
    cxFn d = case d of
      0 -> 150.0  -- Red on left
      1 -> 250.0  -- Green on right
      _ -> 200.0  -- Blue in center

    -- Target Y positions: red/green at bottom, blue at top
    cyFn :: CircleData -> Number
    cyFn d = case d of
      0 -> 130.0  -- Red at bottom
      1 -> 130.0  -- Green at bottom
      _ -> 70.0   -- Blue at top
