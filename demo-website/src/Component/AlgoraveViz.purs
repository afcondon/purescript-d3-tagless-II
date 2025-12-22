module Component.AlgoraveViz where

import Prelude

import Component.PatternTree (PatternTree(..), parseMiniNotation)
import D3.Viz.PatternTreeViz (drawPatternForest, drawPatternForestRadial, drawPatternForestIsometric, drawPatternForestSunburst)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Parsing (parseErrorMessage)
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
data LayoutStyle = LinearLayout | RadialLayout | IsometricLayout | SunburstLayout

derive instance eqLayoutStyle :: Eq LayoutStyle

-- | Component state
type State =
  { tracks :: Array Track
  , layout :: LayoutStyle
  , miniNotationInput :: String
  , parseError :: Maybe String
  }

-- | Component actions
data Action
  = Initialize
  | ToggleTrack Int
  | SetLayout LayoutStyle
  | RenderForest
  | ToggleTrackFromViz Int  -- Called from D3 viz click handler
  | UpdateMiniNotation String
  | ParseAndAddTrack
  | AddPresetPattern String String  -- name, pattern
  | ClearTracks

-- | Component definition
component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ ->
      { tracks: exampleSet
      , layout: SunburstLayout
      , miniNotationInput: "bd sn*2 [cp hh]"
      , parseError: Nothing
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

  ToggleTrackFromViz idx -> do
    -- Toggle track and re-render
    H.modify_ \s -> s
      { tracks = updateAt idx (\t -> t { active = not t.active }) s.tracks
      }
    handleAction RenderForest

  SetLayout layout -> do
    H.modify_ \s -> s { layout = layout }

  UpdateMiniNotation input -> do
    H.modify_ \s -> s { miniNotationInput = input, parseError = Nothing }

  ParseAndAddTrack -> do
    state <- H.get
    let input = state.miniNotationInput
    case parseMiniNotation input of
      Left err -> do
        liftEffect $ Console.log $ "Parse error: " <> parseErrorMessage err
        H.modify_ \s -> s { parseError = Just (parseErrorMessage err) }
      Right pattern -> do
        -- Add new track with parsed pattern
        liftEffect $ Console.log $ "Parsed pattern successfully: " <> show pattern
        let trackName = "parsed" <> show (Array.length state.tracks + 1)
        let newTrack = { name: trackName, pattern, active: true }
        H.modify_ \s -> s
          { tracks = Array.snoc s.tracks newTrack
          , miniNotationInput = ""
          , parseError = Nothing
          }
        liftEffect $ Console.log $ "Added track, now rendering..."
        handleAction RenderForest

  AddPresetPattern name patternStr -> do
    case parseMiniNotation patternStr of
      Left err -> do
        liftEffect $ Console.log $ "Preset parse error: " <> parseErrorMessage err
      Right pattern -> do
        liftEffect $ Console.log $ "Adding preset: " <> name <> " = " <> show pattern
        let newTrack = { name, pattern, active: true }
        H.modify_ \s -> s { tracks = Array.snoc s.tracks newTrack }
        handleAction RenderForest

  ClearTracks -> do
    H.modify_ \s -> s { tracks = [] }
    handleAction RenderForest

  RenderForest -> do
    state <- H.get
    let activePatterns = Array.mapMaybe (\t -> if t.active then Just t.pattern else Nothing) state.tracks
    -- For sunburst, show ALL tracks with track index for toggle callback
    let allNamedPatterns = Array.mapWithIndex (\idx t ->
          { name: t.name, pattern: t.pattern, trackIndex: idx, active: t.active }) state.tracks
    case state.layout of
      LinearLayout -> liftEffect $ drawPatternForest "#pattern-forest-viz" activePatterns
      RadialLayout -> liftEffect $ drawPatternForestRadial "#pattern-forest-viz" activePatterns
      IsometricLayout -> liftEffect $ drawPatternForestIsometric "#pattern-forest-viz" activePatterns
      SunburstLayout -> do
        -- Create emitter for click callbacks
        { emitter, listener } <- liftEffect HS.create
        _ <- H.subscribe (ToggleTrackFromViz <$> emitter)
        liftEffect $ drawPatternForestSunburst "#pattern-forest-viz" allNamedPatterns (HS.notify listener)

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
                , HH.ClassName "floating-panel--large"
                , HH.ClassName "algorave-input-panel"
                ]
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "floating-panel__title" ] ]
                [ HH.text "Mini-notation Parser" ]
            -- Mini-notation input
            , HH.div
                [ HP.classes [ HH.ClassName "control-group" ] ]
                [ HH.textarea
                    [ HP.value state.miniNotationInput
                    , HE.onValueInput UpdateMiniNotation
                    , HP.placeholder "Enter Tidal pattern: bd sn*2 [cp hh]"
                    , HP.rows 2
                    , HP.classes [ HH.ClassName "mini-notation-input" ]
                    ]
                , HH.div
                    [ HP.classes [ HH.ClassName "button-row" ] ]
                    [ HH.button
                        [ HE.onClick \_ -> ParseAndAddTrack
                        , HP.classes [ HH.ClassName "control-button", HH.ClassName "control-button--primary" ]
                        ]
                        [ HH.text "+ Add" ]
                    , HH.button
                        [ HE.onClick \_ -> ClearTracks
                        , HP.classes [ HH.ClassName "control-button", HH.ClassName "control-button--secondary" ]
                        ]
                        [ HH.text "Clear All" ]
                    ]
                -- Parse error display
                , case state.parseError of
                    Just err -> HH.div
                      [ HP.classes [ HH.ClassName "parse-error" ] ]
                      [ HH.text $ "Parse error: " <> err ]
                    Nothing -> HH.text ""
                ]
            -- Preset patterns
            , HH.h3
                [ HP.classes [ HH.ClassName "floating-panel__title" ] ]
                [ HH.text "Test Patterns" ]
            , HH.div
                [ HP.classes [ HH.ClassName "preset-grid" ] ]
                [ presetButton "fast" "808bd:05*4"
                , presetButton "slow" "bd/2 sn"
                , presetButton "euclid" "bd(3,8)"
                , presetButton "choice" "<bd sn cp hh>"
                , presetButton "group" "[tabla2:23 tabla2:09]"
                , presetButton "mixed" "bd*2 [sn cp] hh"
                , presetButton "rest" "bd ~ sn ~"
                , presetButton "poly" "{bd sn, cp cp cp}"
                , presetButton "prob" "bd? sn?0.5 cp"
                , presetButton "repeat" "bd!4"
                , presetButton "complex" "al_perc:00*2 [tabla2:23 tabla2:09]"
                , presetButton "nested" "[bd sn] [cp [hh oh]]"
                ]
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
                    , HH.button
                        [ HE.onClick \_ -> SetLayout SunburstLayout
                        , HP.classes
                            [ HH.ClassName "control-button"
                            , HH.ClassName "control-button--secondary"
                            , HH.ClassName if state.layout == SunburstLayout then "active" else ""
                            ]
                        ]
                        [ HH.text "Sunburst" ]
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

-- | Helper to create a preset pattern button
presetButton :: forall m. String -> String -> H.ComponentHTML Action () m
presetButton name pattern =
  HH.button
    [ HE.onClick \_ -> AddPresetPattern name pattern
    , HP.classes [ HH.ClassName "preset-button" ]
    , HP.title pattern
    ]
    [ HH.text name ]

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
  Fast n child ->
    patternToMiniNotation child <> "*" <> show n
  Slow n child ->
    patternToMiniNotation child <> "/" <> show n
  Euclidean n k child ->
    patternToMiniNotation child <> "(" <> show n <> "," <> show k <> ")"
  Degrade prob child ->
    patternToMiniNotation child <> "?" <> show prob
  Repeat n child ->
    patternToMiniNotation child <> "!" <> show n
  Elongate n child ->
    patternToMiniNotation child <> "@" <> show n

-- | Generate complete Tidal code for all active tracks
generateTidalCode :: Array Track -> String
generateTidalCode tracks =
  String.joinWith "\n" $
    Array.mapMaybe (\t ->
      if t.active
        then Just $ t.name <> " $ sound \"" <> patternToMiniNotation t.pattern <> "\""
        else Just $ "-- " <> t.name <> " $ sound \"" <> patternToMiniNotation t.pattern <> "\""
    ) tracks

