module Component.AlgoraveViz where

import Prelude

import Component.PatternTree (PatternTree(..), parseMiniNotation)
import D3.Viz.PatternTreeViz (TrackLayout(..), ZoomTransform, drawPatternForestMixed, identityZoom)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.Tuple (Tuple(..), uncurry)
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
  , layout :: TrackLayout  -- Per-track layout (click to toggle)
  }

-- | Example algorave set with realistic patterns
-- | Mix of simple and complex mini-notation from real performances
exampleSet :: Array Track
exampleSet =
  [ { name: "kick"
    , pattern: Sequence [Sound "bd", Rest, Sound "bd", Rest]
    , active: true
    , layout: SunburstLayout
    }
  , { name: "snare"
    , pattern: Sequence [Rest, Sound "sn", Rest, Parallel [Sound "sn", Sound "sn"]]
    , active: true
    , layout: SunburstLayout
    }
  , { name: "hats"
    , pattern: Sequence
        [ Sound "hh", Sound "hh", Sound "oh", Sound "hh"
        , Sound "hh", Sound "cp", Sound "hh", Sound "oh"
        ]
    , active: true
    , layout: SunburstLayout
    }
  , { name: "bass"
    , pattern: Choice
        [ Sequence [Sound "bass1", Sound "bass1", Rest, Sound "bass2"]
        , Sequence [Sound "bass3", Rest, Sound "bass1", Sound "bass1"]
        ]
    , active: true
    , layout: SunburstLayout
    }
  , { name: "perc"
    , pattern: Sequence
        [ Parallel [Sound "rim", Sound "clap"]
        , Sound "rim"
        , Rest
        , Sound "cp"
        ]
    , active: false
    , layout: SunburstLayout
    }
  , { name: "melody"
    , pattern: Sequence
        [ Sound "c4", Sound "e4"
        , Parallel [Sound "g4", Sound "b4"]
        , Sound "a4"
        ]
    , active: false
    , layout: SunburstLayout
    }
  , { name: "texture"
    , pattern: Choice
        [ Sound "crackle"
        , Sequence [Sound "noise", Rest, Sound "noise"]
        , Parallel [Sound "pad1", Sound "pad2"]
        ]
    , active: false
    , layout: SunburstLayout
    }
  ]

-- | Component state
type State =
  { tracks :: Array Track
  , miniNotationInput :: String
  , parseError :: Maybe String
  , zoomTransform :: ZoomTransform  -- Preserved zoom state
  , leftPanelOpen :: Boolean        -- Slide-out panel state
  }

-- | Path to a node in the pattern tree (list of child indices)
type NodePath = Array Int

-- | Component actions
data Action
  = Initialize
  | ToggleTrack Int
  | ToggleTrackLayout Int  -- Toggle between tree and sunburst for a track
  | RenderForest
  | ToggleTrackFromViz Int  -- Called from D3 viz click handler (toggles active)
  | ToggleLayoutFromViz Int -- Called from D3 viz click handler (toggles layout)
  | ToggleNodeTypeFromViz Int NodePath  -- Toggle seq↔par at path in track
  | AdjustEuclideanFromViz Int NodePath Int Int  -- trackIdx, path, deltaN, deltaK
  | ZoomChanged ZoomTransform  -- Called when user zooms/pans
  | UpdateMiniNotation String
  | ParseAndAddTrack
  | AddPresetPattern String String  -- name, pattern
  | ClearTracks
  | ToggleLeftPanel  -- Toggle slide-out panel

-- | Component definition
component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ ->
      { tracks: exampleSet
      , miniNotationInput: "bd sn*2 [cp hh]"
      , parseError: Nothing
      , zoomTransform: identityZoom
      , leftPanelOpen: true  -- Start with panel open
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

  ToggleTrackLayout idx -> do
    -- Toggle between tree and sunburst for a track
    H.modify_ \s -> s
      { tracks = updateAt idx (\t -> t { layout = toggleLayout t.layout }) s.tracks
      }
    handleAction RenderForest

  ToggleTrackFromViz idx -> do
    -- Toggle track active state and re-render
    H.modify_ \s -> s
      { tracks = updateAt idx (\t -> t { active = not t.active }) s.tracks
      }
    handleAction RenderForest

  ToggleLayoutFromViz idx -> do
    -- Toggle layout for a track and re-render
    H.modify_ \s -> s
      { tracks = updateAt idx (\t -> t { layout = toggleLayout t.layout }) s.tracks
      }
    handleAction RenderForest

  ToggleNodeTypeFromViz trackIdx nodePath -> do
    -- Toggle seq↔par at the given path in the track's pattern
    liftEffect $ Console.log $ "Toggle node type at track " <> show trackIdx <> " path " <> show nodePath
    H.modify_ \s -> s
      { tracks = updateAt trackIdx (\t -> t { pattern = toggleNodeType nodePath t.pattern }) s.tracks
      }
    handleAction RenderForest

  AdjustEuclideanFromViz trackIdx nodePath deltaN deltaK -> do
    -- Adjust euclidean n or k at the given path
    liftEffect $ Console.log $ "Adjust euclidean at track " <> show trackIdx <> " path " <> show nodePath <> " deltaN=" <> show deltaN <> " deltaK=" <> show deltaK
    H.modify_ \s -> s
      { tracks = updateAt trackIdx (\t -> t { pattern = adjustEuclidean nodePath deltaN deltaK t.pattern }) s.tracks
      }
    handleAction RenderForest

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
        let newTrack = { name: trackName, pattern, active: true, layout: SunburstLayout }
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
        let newTrack = { name, pattern, active: true, layout: SunburstLayout }
        H.modify_ \s -> s { tracks = Array.snoc s.tracks newTrack }
        handleAction RenderForest

  ClearTracks -> do
    H.modify_ \s -> s { tracks = [] }
    handleAction RenderForest

  ToggleLeftPanel -> do
    H.modify_ \s -> s { leftPanelOpen = not s.leftPanelOpen }

  RenderForest -> do
    state <- H.get
    -- Build track data with per-track layout info
    let allTracksWithLayout = Array.mapWithIndex (\idx t ->
          { name: t.name
          , pattern: t.pattern
          , trackIndex: idx
          , active: t.active
          , layout: t.layout
          }) state.tracks
    -- Create emitters for all callbacks
    { emitter: activeEmitter, listener: activeListener } <- liftEffect HS.create
    { emitter: layoutEmitter, listener: layoutListener } <- liftEffect HS.create
    { emitter: nodeTypeEmitter, listener: nodeTypeListener } <- liftEffect HS.create
    { emitter: euclidEmitter, listener: euclidListener } <- liftEffect HS.create
    { emitter: zoomEmitter, listener: zoomListener } <- liftEffect HS.create
    _ <- H.subscribe (ToggleTrackFromViz <$> activeEmitter)
    _ <- H.subscribe (ToggleLayoutFromViz <$> layoutEmitter)
    _ <- H.subscribe (uncurry ToggleNodeTypeFromViz <$> nodeTypeEmitter)
    _ <- H.subscribe ((\(Tuple trackIdx (Tuple path (Tuple dn dk))) -> AdjustEuclideanFromViz trackIdx path dn dk) <$> euclidEmitter)
    _ <- H.subscribe (ZoomChanged <$> zoomEmitter)
    liftEffect $ drawPatternForestMixed "#pattern-forest-viz" allTracksWithLayout
      (HS.notify activeListener)
      (HS.notify layoutListener)
      (\trackIdx path -> HS.notify nodeTypeListener (Tuple trackIdx path))
      (\trackIdx path dn dk -> HS.notify euclidListener (Tuple trackIdx (Tuple path (Tuple dn dk))))
      state.zoomTransform  -- Initial zoom state
      (HS.notify zoomListener)  -- Zoom change callback

  ZoomChanged transform -> do
    -- Just update state without re-rendering (the viz already shows the change)
    H.modify_ \s -> s { zoomTransform = transform }

-- | Toggle between tree and sunburst layout
toggleLayout :: TrackLayout -> TrackLayout
toggleLayout TreeLayout = SunburstLayout
toggleLayout SunburstLayout = TreeLayout

-- Helper to update array element
updateAt :: forall a. Int -> (a -> a) -> Array a -> Array a
updateAt idx f arr =
  case Array.index arr idx of
    Nothing -> arr
    Just elem -> fromMaybe arr $ Array.updateAt idx (f elem) arr

-- | Toggle node type (Sequence↔Parallel) at a path in the pattern tree
-- | Path is a list of child indices: [0, 2] means "first child, then third child"
toggleNodeType :: NodePath -> PatternTree -> PatternTree
toggleNodeType path tree = case Array.uncons path of
  -- Empty path: toggle this node
  Nothing -> case tree of
    Sequence children -> Parallel children
    Parallel children -> Sequence children
    -- Choice could toggle to Sequence? For now, leave as-is
    other -> other
  -- Non-empty path: recurse into children
  Just { head: idx, tail: rest } -> case tree of
    Sequence children ->
      Sequence $ updateAt idx (toggleNodeType rest) children
    Parallel children ->
      Parallel $ updateAt idx (toggleNodeType rest) children
    Choice children ->
      Choice $ updateAt idx (toggleNodeType rest) children
    Fast n child ->
      Fast n (if idx == 0 then toggleNodeType rest child else child)
    Slow n child ->
      Slow n (if idx == 0 then toggleNodeType rest child else child)
    Euclidean n k child ->
      Euclidean n k (if idx == 0 then toggleNodeType rest child else child)
    Degrade p child ->
      Degrade p (if idx == 0 then toggleNodeType rest child else child)
    Repeat n child ->
      Repeat n (if idx == 0 then toggleNodeType rest child else child)
    Elongate n child ->
      Elongate n (if idx == 0 then toggleNodeType rest child else child)
    -- Leaf nodes have no children to recurse into
    other -> other

-- | Adjust euclidean parameters (n hits, k divisions) at a path
-- | deltaN and deltaK are +1 or -1 (or 0 for no change)
adjustEuclidean :: NodePath -> Int -> Int -> PatternTree -> PatternTree
adjustEuclidean path deltaN deltaK tree = case Array.uncons path of
  -- Empty path: adjust this node if it's Euclidean
  Nothing -> case tree of
    Euclidean n k child ->
      let newN = max 1 (n + deltaN)  -- At least 1 hit
          newK = max newN (k + deltaK)  -- At least as many divisions as hits
      in Euclidean newN newK child
    other -> other
  -- Non-empty path: recurse into children
  Just { head: idx, tail: rest } -> case tree of
    Sequence children ->
      Sequence $ updateAt idx (adjustEuclidean rest deltaN deltaK) children
    Parallel children ->
      Parallel $ updateAt idx (adjustEuclidean rest deltaN deltaK) children
    Choice children ->
      Choice $ updateAt idx (adjustEuclidean rest deltaN deltaK) children
    Fast n child ->
      Fast n (if idx == 0 then adjustEuclidean rest deltaN deltaK child else child)
    Slow n child ->
      Slow n (if idx == 0 then adjustEuclidean rest deltaN deltaK child else child)
    Euclidean n k child ->
      Euclidean n k (if idx == 0 then adjustEuclidean rest deltaN deltaK child else child)
    Degrade p child ->
      Degrade p (if idx == 0 then adjustEuclidean rest deltaN deltaK child else child)
    Repeat n child ->
      Repeat n (if idx == 0 then adjustEuclidean rest deltaN deltaK child else child)
    Elongate n child ->
      Elongate n (if idx == 0 then adjustEuclidean rest deltaN deltaK child else child)
    other -> other

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

        -- Slide-out input panel (left side)
        , HH.div
            [ HP.classes $
                [ HH.ClassName "slide-panel"
                , HH.ClassName "slide-panel--left"
                ] <> if state.leftPanelOpen then [ HH.ClassName "slide-panel--open" ] else []
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "slide-panel__title" ] ]
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
                [ HP.classes [ HH.ClassName "slide-panel__subtitle" ] ]
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
            -- Usage hint
            , HH.p
                [ HP.classes [ HH.ClassName "hint-text" ] ]
                [ HH.text "Click seq/par nodes to toggle" ]
            ]

        -- Toggle tab for slide-out panel
        , HH.button
            [ HE.onClick \_ -> ToggleLeftPanel
            , HP.classes $
                [ HH.ClassName "slide-panel__toggle"
                , HH.ClassName "slide-panel__toggle--left"
                ] <> if state.leftPanelOpen then [ HH.ClassName "slide-panel__toggle--open" ] else []
            ]
            [ HH.text $ if state.leftPanelOpen then "◀" else "▶" ]

        -- Floating output panel (top-right) - keeping for now
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

