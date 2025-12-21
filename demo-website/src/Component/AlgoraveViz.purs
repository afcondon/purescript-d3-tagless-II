module Component.AlgoraveViz where

import Prelude

import Component.PatternTree (PatternTree(..))
import D3.Viz.PatternTreeViz (drawPatternForest, drawPatternForestRadial, drawPatternForestIsometric)
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PSD3.Shared.SiteNav as SiteNav

-- | Named pattern (a track in the set)
type Track =
  { name :: String
  , pattern :: PatternTree
  , active :: Boolean
  }

-- | Example algorave set with realistic patterns
-- | Mix of simple and complex mini-notation from real performances
exampleSet :: Array Track
exampleSet =
  [ { name: "kick"
    , pattern: Sequence [Sound "bd", Rest, Sound "bd", Rest]
    , active: true
    }
  , { name: "snare"
    , pattern: Sequence [Rest, Sound "sn", Rest, Parallel [Sound "sn", Sound "sn"]]
    , active: true
    }
  , { name: "hats"
    , pattern: Sequence
        [ Sound "hh", Sound "hh", Sound "oh", Sound "hh"
        , Sound "hh", Sound "cp", Sound "hh", Sound "oh"
        ]
    , active: true
    }
  , { name: "bass"
    , pattern: Choice
        [ Sequence [Sound "bass1", Sound "bass1", Rest, Sound "bass2"]
        , Sequence [Sound "bass3", Rest, Sound "bass1", Sound "bass1"]
        ]
    , active: true
    }
  , { name: "perc"
    , pattern: Sequence
        [ Parallel [Sound "rim", Sound "clap"]
        , Sound "rim"
        , Rest
        , Sound "cp"
        ]
    , active: false
    }
  , { name: "melody"
    , pattern: Sequence
        [ Sound "c4", Sound "e4"
        , Parallel [Sound "g4", Sound "b4"]
        , Sound "a4"
        ]
    , active: false
    }
  , { name: "texture"
    , pattern: Choice
        [ Sound "crackle"
        , Sequence [Sound "noise", Rest, Sound "noise"]
        , Parallel [Sound "pad1", Sound "pad2"]
        ]
    , active: false
    }
  ]

-- | Tree layout style
data LayoutStyle = LinearLayout | RadialLayout | IsometricLayout

derive instance eqLayoutStyle :: Eq LayoutStyle

-- | Component state
type State =
  { tracks :: Array Track
  , layout :: LayoutStyle
  }

-- | Component actions
data Action
  = Initialize
  | ToggleTrack Int
  | SetLayout LayoutStyle
  | RenderForest

-- | Component definition
component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ ->
      { tracks: exampleSet
      , layout: IsometricLayout
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

  ToggleTrack idx -> do
    H.modify_ \s -> s
      { tracks = updateAt idx (\t -> t { active = not t.active }) s.tracks
      }

  SetLayout layout -> do
    H.modify_ \s -> s { layout = layout }

  RenderForest -> do
    state <- H.get
    let activePatterns = Array.mapMaybe (\t -> if t.active then Just t.pattern else Nothing) state.tracks
    case state.layout of
      LinearLayout -> liftEffect $ drawPatternForest "#pattern-forest-viz" activePatterns
      RadialLayout -> liftEffect $ drawPatternForestRadial "#pattern-forest-viz" activePatterns
      IsometricLayout -> liftEffect $ drawPatternForestIsometric "#pattern-forest-viz" activePatterns

-- Helper to update array element
updateAt :: forall a. Int -> (a -> a) -> Array a -> Array a
updateAt idx f arr =
  case Array.index arr idx of
    Nothing -> arr
    Just elem -> fromMaybe arr $ Array.updateAt idx (f elem) arr

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.classes [ HH.ClassName "algorave-showcase" ] ]
    [ SiteNav.render
        { logoSize: SiteNav.Large
        , quadrant: SiteNav.NoQuadrant
        , prevNext: Nothing
        , pageTitle: Just "Algorave Pattern Visualizer"
        }

    -- Fullscreen visualization container
    , HH.div
        [ HP.classes [ HH.ClassName "fullscreen-container", HH.ClassName "algorave-viz-container" ] ]
        [ -- Main viz area
          HH.div
            [ HP.id "pattern-forest-viz"
            , HP.classes [ HH.ClassName "fullscreen-viz", HH.ClassName "svg-container" ]
            ]
            []

        -- Floating input panel (top-left)
        , HH.div
            [ HP.classes
                [ HH.ClassName "floating-panel"
                , HH.ClassName "floating-panel--top-left"
                , HH.ClassName "floating-panel--medium"
                , HH.ClassName "algorave-input-panel"
                ]
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "floating-panel__title" ] ]
                [ HH.text "Input Patterns" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-display" ] ]
                [ HH.code_ [ HH.text $ generateInputCode state.tracks ] ]
            ]

        -- Floating output panel (top-right)
        , HH.div
            [ HP.classes
                [ HH.ClassName "floating-panel"
                , HH.ClassName "floating-panel--top-right"
                , HH.ClassName "floating-panel--medium"
                , HH.ClassName "algorave-output-panel"
                ]
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "floating-panel__title" ] ]
                [ HH.text "Round-trip Output" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-display" ] ]
                [ HH.code_ [ HH.text $ generateTidalCode state.tracks ] ]
            ]

        -- Floating control panel (bottom-center)
        , HH.div
            [ HP.classes
                [ HH.ClassName "floating-panel"
                , HH.ClassName "floating-panel--bottom-center"
                , HH.ClassName "floating-panel--small"
                , HH.ClassName "algorave-control-panel"
                ]
            ]
            [ -- Layout toggle
              HH.div
                [ HP.classes [ HH.ClassName "control-group" ] ]
                [ HH.div
                    [ HP.classes [ HH.ClassName "button-row" ] ]
                    [ HH.button
                        [ HE.onClick \_ -> SetLayout LinearLayout
                        , HP.classes
                            [ HH.ClassName "control-button"
                            , HH.ClassName "control-button--secondary"
                            , HH.ClassName if state.layout == LinearLayout then "active" else ""
                            ]
                        ]
                        [ HH.text "Linear" ]
                    , HH.button
                        [ HE.onClick \_ -> SetLayout RadialLayout
                        , HP.classes
                            [ HH.ClassName "control-button"
                            , HH.ClassName "control-button--secondary"
                            , HH.ClassName if state.layout == RadialLayout then "active" else ""
                            ]
                        ]
                        [ HH.text "Radial" ]
                    , HH.button
                        [ HE.onClick \_ -> SetLayout IsometricLayout
                        , HP.classes
                            [ HH.ClassName "control-button"
                            , HH.ClassName "control-button--secondary"
                            , HH.ClassName if state.layout == IsometricLayout then "active" else ""
                            ]
                        ]
                        [ HH.text "Isometric" ]
                    ]
                ]
            -- Visualize button
            , HH.button
                [ HE.onClick \_ -> RenderForest
                , HP.classes [ HH.ClassName "control-button", HH.ClassName "control-button--primary" ]
                ]
                [ HH.text "▶ Visualize" ]
            ]
        ]
    ]

-- | Generate input code (all patterns as-is)
generateInputCode :: Array Track -> String
generateInputCode tracks =
  String.joinWith "\n" $
    Array.mapMaybe (\t ->
      Just $ t.name <> " $ sound \"" <> patternToMiniNotation t.pattern <> "\""
    ) tracks

-- | Generate Tidal mini-notation from pattern tree
patternToMiniNotation :: PatternTree -> String
patternToMiniNotation = case _ of
  Sound s -> s
  Rest -> "~"
  Sequence children ->
    String.joinWith " " (map patternToMiniNotation children)
  Parallel children ->
    "[" <> String.joinWith ", " (map patternToMiniNotation children) <> "]"
  Choice children ->
    String.joinWith " | " (map patternToMiniNotation children)

-- | Generate complete Tidal code for all active tracks
generateTidalCode :: Array Track -> String
generateTidalCode tracks =
  String.joinWith "\n" $
    Array.mapMaybe (\t ->
      if t.active
        then Just $ t.name <> " $ sound \"" <> patternToMiniNotation t.pattern <> "\""
        else Just $ "-- " <> t.name <> " $ sound \"" <> patternToMiniNotation t.pattern <> "\""
    ) tracks

