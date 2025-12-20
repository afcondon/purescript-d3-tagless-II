module Component.AlgoraveViz where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PSD3.Shared.SiteNav as SiteNav
import PSD3.Website.Types (Route(..))

-- | Pattern tree structure (minimal for now)
data PatternTree
  = Sequence (Array PatternTree)
  | Parallel (Array PatternTree)
  | Sound String
  | Rest

-- | Named pattern (a track in the set)
type Track =
  { name :: String
  , pattern :: PatternTree
  , active :: Boolean
  }

-- | Example Strudel set: "lo-fi birds" by keisi
-- | Source: https://strudel.cc/ (shared REPL example)
exampleSet :: Array Track
exampleSet =
  [ { name: "crackle"
    , pattern: Sound "crackle"
    , active: true
    }
  , { name: "chorus"
    , pattern: Sequence [Sound "Gm", Sound "Fm", Sound "Cm", Sound "G#m"]
    , active: true
    }
  , { name: "drums"
    , pattern: Sequence
        [ Sound "bd"
        , Rest
        , Sound "hh"
        , Sound "hh"
        , Rest
        , Sound "oh"
        , Sound "bd"
        , Parallel [Sound "rim", Sound "clap"]  -- [rim clap]
        ]
    , active: true
    }
  , { name: "lead"
    , pattern: Sequence
        [ Sound "5", Sound "3", Sound "4"
        , Sequence [Sound "7", Parallel [Sound "6", Sound "4"]]  -- <7 [6 4]>
        ]
    , active: true
    }
  , { name: "voice"
    , pattern: Sequence
        [ Sound "chill"
        , Sound "beats"
        , Parallel [Sound "birds", Sound "in_space"]  -- [birds in_space]
        ]
    , active: false
    }
  , { name: "crow"
    , pattern: Sound "crow"
    , active: false
    }
  , { name: "twitter"
    , pattern: Sequence
        [ Sound "0", Sound "1", Sound "2", Sound "3"
        , Sound "4", Sound "5", Sound "6"
        ]
    , active: false
    }
  ]

-- | Component state
type State =
  { tracks :: Array Track
  }

-- | Component actions
data Action
  = Initialize
  | ToggleTrack Int

-- | Component definition
component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ -> { tracks: exampleSet }
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

-- Helper to update array element
updateAt :: forall a. Int -> (a -> a) -> Array a -> Array a
updateAt idx f arr =
  case Array.index arr idx of
    Nothing -> arr
    Just elem -> fromMaybe arr $ Array.updateAt idx (f elem) arr

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.classes [ HH.ClassName "showcase-page" ] ]
    [ SiteNav.render
        { logoSize: SiteNav.Large
        , quadrant: SiteNav.NoQuadrant
        , prevNext: Nothing
        , pageTitle: Just "Algorave Visualizer"
        }

    , HH.main_
        [ HH.section
            [ HP.classes [ HH.ClassName "showcase-section" ] ]
            [ HH.h1_ [ HH.text "Algorave Set Visualization" ]
            , HH.p_
                [ HH.text "Visualizing the structure of live-coded music. Each tree represents a pattern, the forest represents the complete set." ]
            , HH.p
                [ HP.classes [ HH.ClassName "example-attribution" ] ]
                [ HH.text "Example: "
                , HH.strong_ [ HH.text "\"lo-fi birds\"" ]
                , HH.text " by keisi (from "
                , HH.a
                    [ HP.href "https://strudel.cc/"
                    , HP.target "_blank"
                    ]
                    [ HH.text "Strudel REPL" ]
                , HH.text ")"
                ]
            ]

        , HH.section
            [ HP.classes [ HH.ClassName "showcase-section" ] ]
            [ HH.h2_ [ HH.text "Pattern Forest" ]
            , HH.p_ [ HH.text "Click a track name to toggle it on/off (like commenting/uncommenting in the REPL). Active tracks shown with ✓" ]
            , HH.div
                [ HP.classes [ HH.ClassName "track-list" ] ]
                (Array.mapWithIndex renderTrack state.tracks)
            ]

        , HH.section
            [ HP.classes [ HH.ClassName "showcase-section" ] ]
            [ HH.h2_ [ HH.text "Mini-notation Output" ]
            , HH.p_ [ HH.text "Generated Tidal code from the active patterns:" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-output" ] ]
                [ HH.code_ [ HH.text $ generateTidalCode state.tracks ] ]
            ]
        ]
    ]

renderTrack :: forall w. Int -> Track -> HH.HTML w Action
renderTrack idx track =
  HH.div
    [ HP.classes
        [ HH.ClassName "track-item"
        , HH.ClassName $ if track.active then "active" else "inactive"
        ]
    ]
    [ HH.button
        [ HP.classes [ HH.ClassName "track-toggle" ]
        , HE.onClick \_ -> ToggleTrack idx
        ]
        [ HH.text $ (if track.active then "✓" else "○") <> " " <> track.name ]
    , HH.div
        [ HP.classes [ HH.ClassName "pattern-viz" ] ]
        [ renderPatternTree track.pattern ]
    ]

-- | Render a pattern tree (text for now, will use PSD3 tree layout later)
renderPatternTree :: forall w i. PatternTree -> HH.HTML w i
renderPatternTree = case _ of
  Sound s -> HH.span [ HP.classes [ HH.ClassName "sound-node" ] ] [ HH.text s ]
  Rest -> HH.span [ HP.classes [ HH.ClassName "rest-node" ] ] [ HH.text "~" ]
  Sequence children ->
    HH.div
      [ HP.classes [ HH.ClassName "sequence-node" ] ]
      [ HH.span [ HP.classes [ HH.ClassName "node-label" ] ] [ HH.text "seq:" ]
      , HH.div
          [ HP.classes [ HH.ClassName "node-children" ] ]
          (map renderPatternTree children)
      ]
  Parallel children ->
    HH.div
      [ HP.classes [ HH.ClassName "parallel-node" ] ]
      [ HH.span [ HP.classes [ HH.ClassName "node-label" ] ] [ HH.text "par:" ]
      , HH.div
          [ HP.classes [ HH.ClassName "node-children" ] ]
          (map renderPatternTree children)
      ]

-- | Generate Tidal mini-notation from pattern tree
patternToMiniNotation :: PatternTree -> String
patternToMiniNotation = case _ of
  Sound s -> s
  Rest -> "~"
  Sequence children ->
    String.joinWith " " (map patternToMiniNotation children)
  Parallel children ->
    "[" <> String.joinWith ", " (map patternToMiniNotation children) <> "]"

-- | Generate complete Tidal code for all active tracks
generateTidalCode :: Array Track -> String
generateTidalCode tracks =
  String.joinWith "\n" $
    Array.mapMaybe (\t ->
      if t.active
        then Just $ t.name <> " $ sound \"" <> patternToMiniNotation t.pattern <> "\""
        else Just $ "-- " <> t.name <> " $ sound \"" <> patternToMiniNotation t.pattern <> "\""
    ) tracks
