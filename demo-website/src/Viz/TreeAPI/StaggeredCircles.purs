module D3.Viz.TreeAPI.StaggeredCircles where

import Prelude

import Data.Array (range)
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Ref as Ref
import Partial.Unsafe (unsafePartial, unsafeCrashWith)
import PSD3v2.Attribute.Types (cx, cy, fill, radius, width, height, viewBox, id_, class_)
import PSD3v2.Capabilities.Selection (select, renderTree)
import PSD3v2.Capabilities.Transition (withTransition, withTransitionStaggered, staggerByIndex)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2Selection_)
import PSD3v2.Selection.Types (ElementType(..), SEmpty)
import PSD3v2.Transition.Types (transitionWith)
import PSD3v2.VizTree.Tree as T
import Web.DOM.Element (Element)

type CircleData = { index :: Int, x :: Number, y :: Number }

-- | Create a staggered animation demonstration
-- | Returns functions to trigger and reset the animation
staggeredCircles :: String -> Effect { trigger :: Effect Unit, reset :: Effect Unit }
staggeredCircles selector = do
  -- Create data: 10 circles in a row
  let
    numCircles = 10
    circleData = range 0 (numCircles - 1) <#> \i ->
      { index: i
      , x: 50.0 + (toNumber i) * 60.0
      , y: 100.0
      }

  -- Track animation state
  animatedRef <- Ref.new false

  -- Build the tree structure and render
  circlesSel <- runD3v2M do
    container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)

    let
      tree :: T.Tree CircleData
      tree =
        T.named SVG "staggered-svg"
          [ width 650.0
          , height 200.0
          , viewBox "0 0 650 200"
          , id_ "staggered-svg"
          , class_ "staggered-circles"
          ]
          `T.withChild`
            T.joinData "circles" "circle" circleData
              ( \d ->
                  T.elem Circle
                    [ cx d.x
                    , cy d.y
                    , radius 20.0
                    , fill "#4a90d9"
                    ]
              )

    -- Render and get selections map
    selections <- renderTree container tree

    -- Extract the circles selection
    let
      circlesSel = case Map.lookup "circles" selections of
        Just sel -> sel
        Nothing -> unsafePartial $ unsafeCrashWith "circles selection not found"

    pure circlesSel

  -- Trigger function: animate circles with stagger
  let
    trigger = do
      isAnimated <- Ref.read animatedRef
      if isAnimated then pure unit -- Already animated
      else do
        Ref.write true animatedRef
        runD3v2M do
          -- Transition with per-element staggered delays
          -- Each circle animates 100ms after the previous one
          let
            config = transitionWith
              { duration: Milliseconds 500.0
              , delay: Nothing
              , staggerDelay: Nothing -- Using withTransitionStaggered instead
              , easing: Nothing
              }
          withTransitionStaggered config (staggerByIndex 100.0) circlesSel
            [ cy 50.0 -- Move up
            , fill "#e74c3c" -- Change to red
            , radius 25.0 -- Grow slightly
            ]

    reset = do
      Ref.write false animatedRef
      runD3v2M do
        let
          config = transitionWith
            { duration: Milliseconds 300.0
            , delay: Nothing
            , staggerDelay: Nothing
            , easing: Nothing
            }
        withTransition config circlesSel
          [ cy 100.0
          , fill "#4a90d9"
          , radius 20.0
          ]

  pure { trigger, reset }
