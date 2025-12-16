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
import PSD3.Expr.Integration (v3Attr, v3AttrStr, v3AttrFn, v3AttrFnStr)
import PSD3.Expr.Expr (lit, str)
import PSD3.Internal.Capabilities.Selection (renderTree, select)
import PSD3.Internal.Capabilities.Transition (withTransition)
import PSD3.Interpreter.D3 (runD3v2M, D3v2Selection_)
import PSD3.Internal.Selection.Types (ElementType(..), SBoundOwns, SEmpty)
import PSD3.Internal.Transition.Types (transitionWith)
import PSD3.AST as T
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
        [ v3AttrFnStr "fill" colorFnGreen
        , v3AttrFn "cx" cxFnGreen
        , v3AttrFn "cy" cyFnGreen
        , v3Attr "r" (lit 40.0)
        , v3Attr "fill-opacity" (lit 1.0) -- Fully opaque
        ]
    StateRGB ->
      withTransition transitionConfig circlesSel
        [ v3AttrFnStr "fill" colorFnRGB
        , v3AttrFn "cx" cxFnRGB
        , v3AttrFn "cy" cyFnRGB
        , v3Attr "r" (lit 60.0) -- Increased from 30 to 60 for proper overlap and color mixing
        , v3Attr "fill-opacity" (lit 0.5) -- Semi-transparent for color mixing
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
          [ v3Attr "width" (lit 400.0)
          , v3Attr "height" (lit 250.0)
          , v3AttrStr "viewBox" (str "0 0 400 250")
          , v3AttrStr "id" (str "three-circles-transition-tree")
          ]
          `T.withChild`
            ( T.joinData "circles" "circle" [ 0, 1, 2 ] $ \d ->
                T.elem Circle
                  [ v3AttrStr "fill" (str "#00ff00") -- Start green
                  , v3Attr "cx" (lit (100.0 + toNumber d * 100.0)) -- Three in a row
                  , v3Attr "cy" (lit 125.0)
                  , v3Attr "r" (lit 40.0)
                  , v3Attr "fill-opacity" (lit 1.0)
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
