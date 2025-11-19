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
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref as Ref
import PSD3v2.Attribute.Types (width, height, viewBox, id_, fill, cx, cy, radius, fillOpacity)
import PSD3v2.Behavior.Types (onClick)
import PSD3v2.Capabilities.Selection (select, renderTree, on)
import PSD3v2.Capabilities.Transition (withTransition)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2Selection_)
import PSD3v2.Selection.Types (ElementType(..), SEmpty, SBoundOwns, SBoundInherits)
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
toggleState StateRGB = StateGreen

-- | Transition circles to a given state
transitionToState :: D3v2Selection_ SBoundOwns Element CircleData -> CircleState -> Effect Unit
transitionToState circlesSel state = runD3v2M do
  let transitionConfig = transitionWith
        { duration: Milliseconds 1000.0
        , delay: Nothing
        , easing: Nothing
        }

  case state of
    StateGreen ->
      withTransition transitionConfig circlesSel
        [ fill ((\_ -> "green") :: CircleData -> String)
        , cx ((\d -> toNumber d * 100.0 + 100.0) :: CircleData -> Number)  -- Evenly spaced: 100, 200, 300
        , cy ((\_ -> 100.0) :: CircleData -> Number)                        -- Middle
        , radius 15.0
        , fillOpacity 1.0
        ]
    StateRGB ->
      withTransition transitionConfig circlesSel
        [ fill colorFnRGB
        , cx cxFnRGB
        , cy cyFnRGB
        , radius 30.0          -- Larger for better overlap
        , fillOpacity 0.5      -- Semi-transparent for color mixing
        ]

  pure unit
  where
    -- RGB colors: Red, Green, Blue
    colorFnRGB :: CircleData -> String
    colorFnRGB d = case d of
      0 -> "#ff0000"  -- Red
      1 -> "#00ff00"  -- Green
      _ -> "#0000ff"  -- Blue

    -- RGB X positions: form triangle with overlap
    cxFnRGB :: CircleData -> Number
    cxFnRGB d = case d of
      0 -> 150.0  -- Red on left
      1 -> 250.0  -- Green on right
      _ -> 200.0  -- Blue in center

    -- RGB Y positions: red/green at bottom, blue at top
    cyFnRGB :: CircleData -> Number
    cyFnRGB d = case d of
      0 -> 130.0  -- Red at bottom
      1 -> 130.0  -- Green at bottom
      _ -> 70.0   -- Blue at top

-- | Draw three circles with toggle control circle
threeLittleCirclesTransition :: Effect Unit
threeLittleCirclesTransition = do
  -- Create state ref
  stateRef <- Ref.new StateGreen

  runD3v2M do
    container <- select "#viz" :: _ (D3v2Selection_ SEmpty Element Unit)

    -- Initial tree: three green circles + a control circle for toggling
    let initialTree :: T.Tree CircleData
        initialTree =
          T.named SVG "svg"
            [ width 400.0
            , height 250.0
            , viewBox "0 0 400 250"
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
            `T.withChild`
              -- Toggle button as a clickable circle
              T.named Circle "toggle-btn"
                [ cx 200.0
                , cy 220.0
                , radius 20.0
                , fill "#4CAF50"
                , fillOpacity 0.8
                ]

    -- Render initial state
    selections <- renderTree container initialTree

    -- Extract the circles selection for transitions
    let circlesSel = case Map.lookup "circles" selections of
          Just sel -> sel
          Nothing -> unsafePartial $ unsafeCrashWith "circles selection not found"

    -- Extract the toggle button selection
    let toggleBtnSel = case Map.lookup "toggle-btn" selections of
          Just sel -> sel
          Nothing -> unsafePartial $ unsafeCrashWith "toggle button not found"

    -- Attach click handler using the new onClick behavior
    liftEffect do
      _ <- runD3v2M $ on (onClick do
        currentState <- Ref.read stateRef
        let newState = toggleState currentState
        Ref.write newState stateRef
        log $ "Toggling to: " <> show newState
        transitionToState circlesSel newState
      ) toggleBtnSel
      pure unit

  pure unit
