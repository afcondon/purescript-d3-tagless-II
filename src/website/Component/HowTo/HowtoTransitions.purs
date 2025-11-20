module Component.HowTo.HowtoTransitions where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Website.Types (Route(..))
import PSD3.RoutingDSL (routeToPath)

type State = Unit

data Action = Initialize

component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ -> unit
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> pure unit

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "tutorial-page" ] ]
    [ renderHeader "Creating Animated Transitions"

    , HH.main_
        [ -- Intro
          HH.section
            [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ] ]
            [ HH.h1
                [ HP.classes [ HH.ClassName "tutorial-title" ] ]
                [ HH.text "Creating Animated Transitions" ]
            , HH.p_
                [ HH.text "PSD3 provides a TransitionM capability for smooth, animated attribute changes." ]
            ]

        -- Basic Transition
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Basic Transition" ]

            , HH.p_ [ HH.text "Animate attributes over a duration:" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """import Data.Time.Duration (Milliseconds(..))
import PSD3v2.Transition.Types (transition)
import PSD3v2.Capabilities.Transition (withTransition)

-- Animate circles to new state over 1 second
withTransition (transition (Milliseconds 1000.0)) circles
  [ fill "orange"
  , radius 25.0
  , cx 100.0
  ]""" ]
                ]
            ]

        -- Easing and Delay
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Easing & Delay" ]

            , HH.p_ [ HH.text "Add easing functions and delays:" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """import PSD3v2.Transition.Types (transitionWith, Easing(..))

let config = transitionWith
      { duration: Milliseconds 1500.0
      , delay: Just (Milliseconds 500.0)
      , easing: Just ElasticOut
      }

withTransition config circles [radius 30.0]""" ]
                ]

            , HH.p_ [ HH.text "Available easings include: Linear, Cubic, Quad, Sin, Exp, Circle, Elastic, Back, Bounce (each with In/Out/InOut variants)." ]
            ]

        -- Staggered Animations
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Staggered Animations" ]

            , HH.p_ [ HH.text "Animate elements sequentially with per-element delays:" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """import PSD3v2.Capabilities.Transition (withTransitionStaggered, staggerByIndex)

-- Each element delays 50ms more than the previous
withTransitionStaggered config (staggerByIndex 50.0) bars
  [ height \\d -> d.value * 10.0
  , fill "steelblue"
  ]""" ]
                ]

            , HH.p_ [ HH.text "Custom delay based on data:" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """-- Delay based on datum value
withTransitionStaggered config (\\d _ -> Milliseconds (d.priority * 100.0)) items
  [ opacity 1.0 ]""" ]
                ]
            ]

        -- Exit Transitions
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Exit Transitions" ]

            , HH.p_ [ HH.text "Animate elements before removal:" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """import PSD3v2.Capabilities.Transition (withTransitionExit)

-- Fade out and shrink before removing
withTransitionExit (transition (Milliseconds 500.0)) exitSelection
  [ opacity 0.0
  , radius 0.0
  ]""" ]
                ]
            ]

        -- Key Points
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Key Points" ]
            , HH.ul_
                [ HH.li_ [ HH.strong_ [ HH.text "Type-safe" ], HH.text " - Only bound selections can be transitioned" ]
                , HH.li_ [ HH.strong_ [ HH.text "Milliseconds" ], HH.text " - All durations use Data.Time.Duration" ]
                , HH.li_ [ HH.strong_ [ HH.text "staggerByIndex" ], HH.text " - Helper for simple sequential delays" ]
                , HH.li_ [ HH.strong_ [ HH.text "D3 easings" ], HH.text " - Full set of D3's easing functions" ]
                ]
            ]

        -- Real Example
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Real Example" ]
            , HH.p_ [ HH.text "See these files for transition usage:" ]
            , HH.ul_
                [ HH.li_
                    [ HH.code_ [ HH.text "src/lib/PSD3v2/Transition/Types.purs" ]
                    , HH.text " - All easing options"
                    ]
                , HH.li_
                    [ HH.code_ [ HH.text "src/lib/PSD3v2/Capabilities/Transition.purs" ]
                    , HH.text " - TransitionM type class"
                    ]
                ]
            ]
        ]
    ]

renderHeader :: forall w i. String -> HH.HTML w i
renderHeader title =
  HH.header
    [ HP.classes [ HH.ClassName "example-header" ] ]
    [ HH.div
        [ HP.classes [ HH.ClassName "example-header-left" ] ]
        [ HH.a
            [ HP.href $ "#" <> routeToPath Home
            , HP.classes [ HH.ClassName "example-logo-link" ]
            ]
            [ HH.img
                [ HP.src "assets/psd3-logo-color.svg"
                , HP.alt "PSD3 Logo"
                , HP.classes [ HH.ClassName "example-logo" ]
                ]
            ]
        , HH.a
            [ HP.href "#/howto"
            , HP.classes [ HH.ClassName "example-gallery-link" ]
            ]
            [ HH.text "How-to" ]
        , HH.div
            [ HP.classes [ HH.ClassName "example-title-container" ] ]
            [ HH.h1
                [ HP.classes [ HH.ClassName "example-title" ] ]
                [ HH.text title ]
            ]
        ]
    ]
