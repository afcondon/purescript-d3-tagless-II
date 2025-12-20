module Component.ParabolaAudioTest where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import D3.Viz.TreeAPI.ParabolaAudio as ParabolaAudio

-- | Component state
type State =
  { visualRendered :: Boolean
  , audioPlayed :: Boolean
  }

-- | Component actions
data Action
  = Initialize
  | PlayVisual
  | PlayAudio

-- | Test page component
component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

initialState :: forall i. i -> State
initialState _ =
  { visualRendered: false
  , audioPlayed: false
  }

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    pure unit

  PlayVisual -> do
    liftEffect $ ParabolaAudio.parabolaVisual
    H.modify_ _ { visualRendered = true }

  PlayAudio -> do
    liftEffect $ ParabolaAudio.parabolaAudio
    H.modify_ _ { audioPlayed = true }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.classes [ HH.ClassName "parabola-audio-test" ] ]
    [ HH.h1_ [ HH.text "🎵 Parabola Sonification Demo" ]

    , HH.section
        [ HP.classes [ HH.ClassName "intro-section" ] ]
        [ HH.p_
            [ HH.text "This demonstrates the Finally Tagless pattern: the "
            , HH.strong_ [ HH.text "same PureScript code" ]
            , HH.text " can be interpreted as:"
            ]
        , HH.ul_
            [ HH.li_ [ HH.text "Visual output (D3 → SVG circles)" ]
            , HH.li_ [ HH.text "Audio output (Web Audio → sound)" ]
            ]
        , HH.p_
            [ HH.text "The parabola data (y = x²) is mapped differently for each medium:" ]
        , HH.table
            [ HP.classes [ HH.ClassName "mapping-table" ] ]
            [ HH.thead_
                [ HH.tr_
                    [ HH.th_ [ HH.text "Data" ]
                    , HH.th_ [ HH.text "Visual (D3)" ]
                    , HH.th_ [ HH.text "Audio (Music)" ]
                    ]
                ]
            , HH.tbody_
                [ HH.tr_
                    [ HH.td_ [ HH.text "x value" ]
                    , HH.td_ [ HH.text "Horizontal position" ]
                    , HH.td_ [ HH.text "Time (when to play)" ]
                    ]
                , HH.tr_
                    [ HH.td_ [ HH.text "y value" ]
                    , HH.td_ [ HH.text "Vertical position" ]
                    , HH.td_ [ HH.text "Pitch (frequency)" ]
                    ]
                , HH.tr_
                    [ HH.td_ [ HH.text "y value" ]
                    , HH.td_ [ HH.text "Circle radius" ]
                    , HH.td_ [ HH.text "Duration & volume" ]
                    ]
                ]
            ]
        ]

    , HH.section
        [ HP.classes [ HH.ClassName "demo-section" ] ]
        [ HH.h2_ [ HH.text "1. Visual Interpretation" ]
        , HH.p_ [ HH.text "Click to render the parabola as SVG circles:" ]
        , HH.button
            [ HP.classes [ HH.ClassName "demo-button" ]
            , HE.onClick \_ -> PlayVisual
            , HP.disabled state.visualRendered
            ]
            [ HH.text if state.visualRendered
                then "✓ Rendered"
                else "🎨 Render Visual"
            ]
        , HH.div
            [ HP.id "parabola-visual-viz"
            , HP.classes [ HH.ClassName "viz-container" ]
            ]
            []
        ]

    , HH.section
        [ HP.classes [ HH.ClassName "demo-section" ] ]
        [ HH.h2_ [ HH.text "2. Audio Interpretation" ]
        , HH.p_ [ HH.text "Click to play the parabola as sound:" ]
        , HH.p
            [ HP.classes [ HH.ClassName "audio-hint" ] ]
            [ HH.em_ [ HH.text "🔊 Make sure your volume is at a comfortable level. You'll hear a melodic arc: high → low → high pitch, mirroring the parabola shape." ]
            ]
        , HH.button
            [ HP.classes [ HH.ClassName "demo-button", HH.ClassName "audio-button" ]
            , HE.onClick \_ -> PlayAudio
            ]
            [ HH.text if state.audioPlayed
                then "🔊 Playing..."
                else "🎵 Play Audio"
            ]
        ]

    , HH.section
        [ HP.classes [ HH.ClassName "code-section" ] ]
        [ HH.h2_ [ HH.text "The Polymorphic Code" ]
        , HH.p_ [ HH.text "This is the actual code that works with both interpreters:" ]
        , HH.pre_
            [ HH.code_
                [ HH.text """sonifyParabola :: forall sel m. SelectionM sel m => m Unit
sonifyParabola = do
  context <- select "audio"
  _ <- appendData Circle parabolaData
    [ time (\\i _ -> toNumber i * 0.4)
    , pitch (\\_ d -> 200.0 + (d.y * 3.0))
    , duration (\\_ d -> 0.3 + (d.y * 0.002))
    , volume (\\_ d -> 0.3 + (d.y * 0.003))
    , timbre (\\_ _ -> "sine")
    ] context
  pure unit

-- Run with D3:
runD3v2M sonifyParabola

-- Run with Music:
initMusicContext sonifyParabola"""
                ]
            ]
        ]

    , HH.section
        [ HP.classes [ HH.ClassName "next-steps-section" ] ]
        [ HH.h2_ [ HH.text "What This Proves" ]
        , HH.ul_
            [ HH.li_
                [ HH.strong_ [ HH.text "True polymorphism: " ]
                , HH.text "Same code, genuinely different outputs"
                ]
            , HH.li_
                [ HH.strong_ [ HH.text "Accessibility: " ]
                , HH.text "Data can be understood through hearing, not just sight"
                ]
            , HH.li_
                [ HH.strong_ [ HH.text "Extensibility: " ]
                , HH.text "New interpreters can be added without changing existing code"
                ]
            ]
        , HH.p_
            [ HH.text "Next: Try Anscombe's Quartet! Four statistically identical datasets that look different visually will also "
            , HH.em_ [ HH.text "sound" ]
            , HH.text " different."
            ]
        ]
    ]
