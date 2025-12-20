module Component.Tour.TourSonification where

import Prelude

import Component.Tour.AnscombeAudio as Audio
import Data.Array as Array
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Number.Format (fixed, toStringWith)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import PSD3.Music.Internal.FFI (AudioContext, createAudioContext)
import PSD3.Shared.TutorialNav as TutorialNav
import PSD3.Website.Types (Route(..))

-- | Tour page state
type State =
  { currentDataset :: Maybe String
  , isPlaying :: Boolean
  , audioContext :: Maybe AudioContext
  }

-- | Tour page actions
data Action
  = Initialize
  | ToggleDataset String
  | StopAll

-- | Tour page component
component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ ->
      { currentDataset: Nothing
      , isPlaying: false
      , audioContext: Nothing
      }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> pure unit

  ToggleDataset name -> do
    state <- H.get
    -- If this dataset is already playing, stop it
    if state.currentDataset == Just name then do
      liftEffect $ Audio.clearIntervalImpl unit
      H.modify_ (\s -> s { currentDataset = Nothing, isPlaying = false })
    else do
      -- Stop any currently playing dataset
      liftEffect $ Audio.clearIntervalImpl unit

      -- Create audio context if needed
      ctx <- case state.audioContext of
        Just c -> pure c
        Nothing -> do
          c <- liftEffect createAudioContext
          H.modify_ (\s -> s { audioContext = Just c })
          pure c

      -- Start playing this dataset
      case Audio.getDataset name of
        Nothing -> pure unit
        Just dataset -> do
          let sortedData = Array.sortBy (\a b -> compare a.x b.x) dataset
          -- Play immediately
          liftEffect $ Audio.playCycle ctx dataset
          -- Set up looping (cycle time = data length * 0.175 + 400ms pause)
          let cycleTimeMs = (toNumber (Array.length sortedData) * 175.0) + 400.0
          liftEffect $ Audio.setIntervalImpl (Audio.playCycle ctx dataset) cycleTimeMs
          H.modify_ (\s -> s { currentDataset = Just name, isPlaying = true })

  StopAll -> do
    liftEffect $ Audio.clearIntervalImpl unit
    H.modify_ (\s -> s { currentDataset = Nothing, isPlaying = false })

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.classes [ HH.ClassName "tutorial-page" ] ]
    [ TutorialNav.renderHeader TourSonification
    , HH.main_
        [ -- Page introduction
          HH.section
            [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ] ]
            [ HH.h1
                [ HP.classes [ HH.ClassName "tutorial-title" ] ]
                [ HH.text "Data Sonification: Hearing the Shape of Data" ]
            , HH.p_
                [ HH.text "The Finally Tagless pattern isn't just theoretical - it enables truly different interpretations of the same code. This page demonstrates "
                , HH.strong_ [ HH.text "data sonification" ]
                , HH.text ": the same PSD3 code that creates visual charts can be interpreted as "
                , HH.strong_ [ HH.text "sound" ]
                , HH.text "."
                ]
            , HH.p_
                [ HH.text "This has practical applications for accessibility (understanding data through hearing), multimodal analysis (using both sight and sound), and situations where visual attention is occupied." ]
            ]

        -- The Big Idea
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2_ [ HH.text "The Architecture" ]
            , HH.p_
                [ HH.text "PSD3 uses a Finally Tagless encoding where the same polymorphic code can have multiple interpreters:" ]
            , HH.div
                [ HP.classes [ HH.ClassName "code-example" ] ]
                [ HH.pre_
                    [ HH.code_
                        [ HH.text """-- This function is polymorphic over the interpreter!
sonifyData :: forall sel m. SelectionM sel m => Array Point -> m Unit

-- Run with D3 interpreter → SVG visualization
runD3v2M $ sonifyData points

-- Run with Music interpreter → Web Audio tones
initMusicContext $ sonifyData points"""
                        ]
                    ]
                ]
            , HH.p_
                [ HH.text "The interpreters share the same type class ("
                , HH.code_ [ HH.text "SelectionM" ]
                , HH.text ") but produce completely different outputs."
                ]
            ]

        -- Anscombe's Quartet: The Demo
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2_ [ HH.text "Anscombe's Quartet: Same Statistics, Different Sounds" ]
            , HH.p_
                [ HH.text "Anscombe's Quartet is a famous demonstration of why visualization matters. These four datasets have:" ]
            , HH.div
                [ HP.classes [ HH.ClassName "stats-box" ] ]
                [ HH.strong_ [ HH.text "Identical Statistics" ]
                , HH.ul_
                    [ HH.li_ [ HH.text "Same mean of x (9.0)" ]
                    , HH.li_ [ HH.text "Same mean of y (7.5)" ]
                    , HH.li_ [ HH.text "Same variance" ]
                    , HH.li_ [ HH.text "Same correlation (0.816)" ]
                    , HH.li_ [ HH.text "Same linear regression line" ]
                    ]
                ]
            , HH.p_
                [ HH.text "But when visualized, they look completely different. And when "
                , HH.strong_ [ HH.text "sonified" ]
                , HH.text ", they "
                , HH.em_ [ HH.text "sound" ]
                , HH.text " completely different!"
                ]

            -- Interactive Demo
            , HH.div
                [ HP.classes [ HH.ClassName "demo-section" ] ]
                [ HH.h3_ [ HH.text "Interactive Demo" ]
                , HH.p_
                    [ HH.text "The demo uses looping "
                    , HH.strong_ [ HH.text "arpeggiation" ]
                    , HH.text " - each dataset plays repeatedly with short pauses, so you can hear the 'shape' of the data aurally."
                    ]
                , HH.p_
                    [ HH.text "Key insight: Unlike vision (where you can scan back and forth), audio needs repetition to perceive patterns." ]
                , HH.div
                    [ HP.classes [ HH.ClassName "anscombe-status" ] ]
                    [ HH.text $ if state.isPlaying
                        then case state.currentDataset of
                          Just name -> "🎵 Arpeggiation Dataset " <> name <> " (looping)... Listen for the pattern!"
                          Nothing -> "🔊 Click any dataset to start looping arpeggiation"
                        else "🔊 Click any dataset to start looping arpeggiation"
                    ]
                , HH.div
                    [ HP.classes [ HH.ClassName "datasets-grid" ] ]
                    [ renderDatasetCard state "A" "Linear progression" "smooth ascending/descending melody" Audio.dataset1
                    , renderDatasetCard state "B" "Parabolic curve" "melodic arc with clear turning point" Audio.dataset2
                    , renderDatasetCard state "C" "Linear with outlier" "melody interrupted by jarring note" Audio.dataset3
                    , renderDatasetCard state "D" "Vertical line with outlier" "repeated note with one dramatic jump" Audio.dataset4
                    ]
                ]
            ]

        -- Audio Mappings
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2_ [ HH.text "Audio Mappings" ]
            , HH.p_ [ HH.text "Each data point (x, y) is mapped to sound parameters:" ]
            , HH.table
                [ HP.classes [ HH.ClassName "mapping-table" ] ]
                [ HH.thead_
                    [ HH.tr_
                        [ HH.th_ [ HH.text "Data Dimension" ]
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
                        , HH.td_ [ HH.text "Size/radius" ]
                        , HH.td_ [ HH.text "Volume (amplitude)" ]
                        ]
                    ]
                ]
            , HH.p_
                [ HH.text "The patterns you hear:" ]
            , HH.ul_
                [ HH.li_ [ HH.strong_ [ HH.text "Dataset A" ], HH.text ": Linear - smooth, consistent melody" ]
                , HH.li_ [ HH.strong_ [ HH.text "Dataset B" ], HH.text ": Parabolic - clear arc with turning point" ]
                , HH.li_ [ HH.strong_ [ HH.text "Dataset C" ], HH.text ": Linear with outlier - melody interrupted by jarring note" ]
                , HH.li_ [ HH.strong_ [ HH.text "Dataset D" ], HH.text ": Vertical with outlier - repeated pitch with one dramatic jump" ]
                ]
            ]

        -- What This Proves
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2_ [ HH.text "What This Proves" ]
            , HH.ul_
                [ HH.li_
                    [ HH.strong_ [ HH.text "True Polymorphism: " ]
                    , HH.text "Same code, genuinely different outputs (not just styling)"
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Accessibility: " ]
                    , HH.text "Data can be understood through hearing, not just sight"
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Extensibility: " ]
                    , HH.text "New interpreters can be added without changing existing code"
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Audio as a peer of SVG: " ]
                    , HH.code_ [ HH.text "audio" ]
                    , HH.text " is not just another interpretation - it's a completely different output medium"
                    ]
                ]
            ]

        -- Implementation
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2_ [ HH.text "Implementation: psd3-music Package" ]
            , HH.p_
                [ HH.text "The music interpreter is implemented as a separate package ("
                , HH.code_ [ HH.text "psd3-music" ]
                , HH.text ") that implements the same "
                , HH.code_ [ HH.text "SelectionM" ]
                , HH.text " type class as the D3 interpreter."
                ]
            , HH.h3_ [ HH.text "Music-Specific Attributes" ]
            , HH.div
                [ HP.classes [ HH.ClassName "code-example" ] ]
                [ HH.pre_
                    [ HH.code_
                        [ HH.text """-- Parallel to visual attributes (cx, cy, fill)
time     :: (Int -> datum -> Number) -> Attribute datum
pitch    :: (Int -> datum -> Number) -> Attribute datum
duration :: (Int -> datum -> Number) -> Attribute datum
volume   :: (Int -> datum -> Number) -> Attribute datum
timbre   :: (Int -> datum -> String) -> Attribute datum"""
                        ]
                    ]
                ]
            , HH.p_
                [ HH.text "These attributes use the same "
                , HH.code_ [ HH.text "Attribute" ]
                , HH.text " type as visual attributes, but map to Web Audio API parameters instead of SVG properties."
                ]
            ]

        -- Next Steps
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2_ [ HH.text "Future Directions" ]
            , HH.p_ [ HH.text "This proof-of-concept opens several possibilities:" ]
            , HH.ul_
                [ HH.li_ [ HH.text "Audio dashboards for monitoring system metrics" ]
                , HH.li_ [ HH.text "Accessibility tools for data analysis" ]
                , HH.li_ [ HH.text "Multimodal data exploration (vision + hearing)" ]
                , HH.li_ [ HH.text "Music composition DSL using the same patterns" ]
                , HH.li_ [ HH.text "CV output for controlling hardware synthesizers" ]
                ]
            ]
        ]
    ]

-- | Render a dataset card with table and play/stop button
renderDatasetCard :: forall w. State -> String -> String -> String -> Array Audio.Point -> HH.HTML w Action
renderDatasetCard state name title description dataset =
  let
    isPlaying = state.currentDataset == Just name
    sortedData = Array.sortBy (\a b -> compare a.x b.x) dataset
  in
    HH.div
      [ HP.classes
          [ HH.ClassName "dataset-card"
          , HH.ClassName $ if isPlaying then "playing" else ""
          ]
      ]
      [ HH.h3_ [ HH.text $ "Dataset " <> name ]
      , HH.p
          [ HP.classes [ HH.ClassName "description" ] ]
          [ HH.text $ title <> " - " <> description ]
      , HH.button
          [ HP.classes
              [ HH.ClassName $ "dataset-" <> name
              , HH.ClassName $ if isPlaying then "playing" else ""
              ]
          , HE.onClick \_ -> ToggleDataset name
          ]
          [ HH.text $ if isPlaying
              then "⏹️ Stop Dataset " <> name
              else "▶️ Start Dataset " <> name
          ]
      , HH.table_
          [ HH.thead_
              [ HH.tr_
                  [ HH.th_ [ HH.text "x" ]
                  , HH.th_ [ HH.text "y" ]
                  ]
              ]
          , HH.tbody_
              (map (\point ->
                HH.tr_
                  [ HH.td_ [ HH.text $ toStringWith (fixed 1) point.x ]
                  , HH.td_ [ HH.text $ toStringWith (fixed 2) point.y ]
                  ]
              ) sortedData)
          ]
      ]
