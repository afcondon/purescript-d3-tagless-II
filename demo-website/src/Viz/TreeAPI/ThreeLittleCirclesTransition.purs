module D3.Viz.TreeAPI.ThreeLittleCirclesTransition where

-- | Three Little Circles with Transitions using Tree API
-- |
-- | Demonstrates:
-- | - Initial render with Tree API
-- | - Re-rendering with updated data
-- | - Transitions between states
-- | - Click handlers with onClick behavior
-- |
-- | Based on Mike Bostock's "Three Little Circles" (https://bost.ocks.org/mike/circles/)

import Prelude

import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref as Ref
import PSD3v2.Attribute.Types (width, height, viewBox, id_, fill, cx, cy, radius, fillOpacity)
import PSD3v2.Capabilities.Selection (renderTree, select)
import PSD3v2.Capabilities.Transition (withTransition)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2Selection_)
import PSD3v2.Selection.Types (ElementType(..), SBoundOwns, SEmpty)
import PSD3v2.Transition.Types (transitionWith)
import PSD3v2.VizTree.Tree as T
import Partial.Unsafe (unsafePartial, unsafeCrashWith)
import Web.DOM.Element (Element)

-- | Circle data type
type CircleData = Int

-- | State of the circles visualization
data CircleState = StateGreen | StateRGB

derive instance Eq CircleState

instance Show CircleState where
  show StateGreen = "StateGreen"
  show StateRGB = "StateRGB"

-- | Toggle between states
toggleState :: CircleState -> CircleState
toggleState StateGreen = StateRGB
toggleState StateRGB = StateRGB -- Stay in RGB state once shown

-- | Transition circles to a given state
transitionToState :: D3v2Selection_ SBoundOwns Element CircleData -> CircleState -> Effect Unit
transitionToState circlesSel state = runD3v2M do
  let
    transitionConfig = transitionWith
      { duration: Milliseconds 1000.0
      , delay: Nothing
      , staggerDelay: Nothing
      , easing: Nothing
      }

  case state of
    StateGreen ->
      -- Three green circles in a row
      withTransition transitionConfig circlesSel
        [ fill colorFnGreen
        , cx cxFnGreen
        , cy cyFnGreen
        , radius 40.0
        , fillOpacity 1.0 -- Fully opaque
        ]
    StateRGB ->
      withTransition transitionConfig circlesSel
        [ fill colorFnRGB
        , cx cxFnRGB
        , cy cyFnRGB
        , radius 60.0 -- Increased from 30 to 60 for proper overlap and color mixing
        , fillOpacity 0.5 -- Semi-transparent for color mixing
        ]

  pure unit
  where
  -- Green state: all circles green
  colorFnGreen :: CircleData -> String
  colorFnGreen _ = "#00ff00"

  cxFnGreen :: CircleData -> Number
  cxFnGreen d = 100.0 + toNumber d * 100.0 -- Spaced 100px apart

  cyFnGreen :: CircleData -> Number
  cyFnGreen _ = 125.0 -- All at same height

  -- RGB colors: Red, Green, Blue
  colorFnRGB :: CircleData -> String
  colorFnRGB d = case d of
    0 -> "#ff0000" -- Red
    1 -> "#00ff00" -- Green
    _ -> "#0000ff" -- Blue

  -- RGB X positions: form triangle with overlap
  cxFnRGB :: CircleData -> Number
  cxFnRGB d = case d of
    0 -> 150.0 -- Red on left
    1 -> 250.0 -- Green on right
    _ -> 200.0 -- Blue in center

  -- RGB Y positions: red/green at bottom, blue at top
  cyFnRGB :: CircleData -> Number
  cyFnRGB d = case d of
    0 -> 130.0 -- Red at bottom
    1 -> 130.0 -- Green at bottom
    _ -> 70.0 -- Blue at top

-- | Draw three circles with visible initial state
threeLittleCirclesTransition :: String -> Effect { stateRef :: Ref.Ref CircleState, circlesSel :: D3v2Selection_ SBoundOwns Element CircleData }
threeLittleCirclesTransition selector = do
  -- Create state ref - start with green circles
  stateRef <- Ref.new StateGreen

  runD3v2M do
    container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)

    -- Initial tree: three green circles in a row
    let
      initialTree :: T.Tree CircleData
      initialTree =
        T.named SVG "svg"
          [ width 400.0
          , height 250.0
          , viewBox "0 0 400 250"
          , id_ "three-circles-transition-tree"
          ]
          `T.withChild`
            ( T.joinData "circles" "circle" [ 0, 1, 2 ] $ \d ->
                T.elem Circle
                  [ fill "#00ff00" -- Start green
                  , cx (100.0 + toNumber d * 100.0) -- Three in a row
                  , cy 125.0
                  , radius 40.0
                  , fillOpacity 1.0
                  ]
            )

    -- Render initial state
    selections <- renderTree container initialTree

    -- Extract the circles selection for transitions
    let
      circlesSel = case Map.lookup "circles" selections of
        Just sel -> sel
        Nothing -> unsafePartial $ unsafeCrashWith "circles selection not found"

    -- Return the state ref and selection so external button can trigger transition
    pure { stateRef, circlesSel }

-- | Helper to create transition function for a button
-- | Call this after threeLittleCirclesTransition to get a function to attach to a button
createTransitionTrigger :: { stateRef :: Ref.Ref CircleState, circlesSel :: D3v2Selection_ SBoundOwns Element CircleData } -> Effect Unit
createTransitionTrigger { stateRef, circlesSel } = do
  currentState <- Ref.read stateRef
  let newState = toggleState currentState
  Ref.write newState stateRef
  log $ "Transitioning to: " <> show newState
  transitionToState circlesSel newState
