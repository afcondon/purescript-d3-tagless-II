module Component.HowTo.HowtoLoadingData where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.RoutingDSL (routeToPath)
import PSD3.Shared.SiteNav as SiteNav
import PSD3.Website.Types (Route(..))

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
    [ SiteNav.render
        { logoSize: SiteNav.Large
        , quadrant: SiteNav.QuadHowTo
        , prevNext: Nothing
        , pageTitle: Nothing
        }

    , HH.main_
        [ -- Intro
          HH.section
            [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ] ]
            [ HH.h1
                [ HP.classes [ HH.ClassName "tutorial-title" ] ]
                [ HH.text "Loading External Data" ]
            , HH.p_
                [ HH.text "PSD3 uses Affjax for async data loading with configurable strategies." ]
            ]

        -- Simple JSON Loading
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Simple JSON Loading" ]

            , HH.p_ [ HH.text "Load JSON with automatic error fallback:" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """import PSD3.Shared.DataLoader (simpleLoadJSON)

json <- H.liftAff $ simpleLoadJSON "data/les-miserables.json"
-- Returns fallback on error, never throws""" ]
                ]

            , HH.p_ [ HH.text "With explicit error handling:" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """import PSD3.Shared.DataLoader (loadJSON, defaultConfig, LoadError(..))

result <- H.liftAff $ loadJSON defaultConfig "data/graph.json"
case result of
  Left (NetworkError msg) -> log ("Network error: " <> msg)
  Left (ParseError msg) -> log ("Parse error: " <> msg)
  Right json -> processData json""" ]
                ]
            ]

        -- Load Strategies
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Load Strategies" ]

            , HH.p_ [ HH.text "Configure where to load data from:" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """import PSD3.Shared.DataLoader (loadJSON, LoadStrategy(..), LoadConfig)

-- Local only (bundled files)
let localConfig = { strategy: LocalOnly, baseUrl: Nothing }

-- Remote only (requires server)
let remoteConfig = { strategy: RemoteOnly, baseUrl: Just "http://localhost:8080" }

-- Try local first, fallback to remote
let hybridConfig = { strategy: LocalFirst, baseUrl: Just "http://localhost:8080" }

json <- H.liftAff $ loadJSON hybridConfig "data/graph.json\"""" ]
                ]
            ]

        -- CSV Loading
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "CSV Loading" ]

            , HH.p_ [ HH.text "Load CSV data as Foreign array:" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """import PSD3.Shared.DataLoader (simpleLoadCSV, loadCSV)
import Foreign (Foreign)

-- Simple loading with empty array fallback
rows <- H.liftAff $ simpleLoadCSV "data/population.csv"

-- With error handling
result <- H.liftAff $ loadCSV defaultConfig "data/timeseries.csv"
case result of
  Left err -> log (show err)
  Right rows -> processRows rows""" ]
                ]
            ]

        -- Halogen Pattern
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Halogen Pattern" ]

            , HH.p_ [ HH.text "Common pattern for loading in component initialization:" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """data State = { loading :: Boolean, graphData :: Maybe GraphData }

handleAction = case _ of
  Initialize -> do
    H.modify_ _ { loading = true }
    json <- H.liftAff $ simpleLoadJSON "data/graph.json"
    let graphData = decodeGraphData json
    H.modify_ _ { loading = false, graphData = Just graphData }

render state
  | state.loading = HH.div_ [ HH.text "Loading..." ]
  | otherwise = renderVisualization state.graphData""" ]
                ]
            ]

        -- Key Points
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Key Points" ]
            , HH.ul_
                [ HH.li_ [ HH.strong_ [ HH.text "Aff-based" ], HH.text " - Use H.liftAff to run in Halogen" ]
                , HH.li_ [ HH.strong_ [ HH.text "Strategies" ], HH.text " - LocalOnly, RemoteOnly, LocalFirst, RemoteFirst" ]
                , HH.li_ [ HH.strong_ [ HH.text "Error types" ], HH.text " - NetworkError, ParseError, FileNotFound, ConfigError" ]
                , HH.li_ [ HH.strong_ [ HH.text "simple* helpers" ], HH.text " - Return fallbacks instead of Either" ]
                ]
            ]

        -- Real Example
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Real Example" ]
            , HH.p_ [ HH.text "See data loading in action:" ]
            , HH.ul_
                [ HH.li_
                    [ HH.code_ [ HH.text "src/website/Shared/DataLoader.purs" ]
                    , HH.text " - Full DataLoader module"
                    ]
                , HH.li_
                    [ HH.code_ [ HH.text "src/website/Component/CodeExplorer/Data.purs" ]
                    , HH.text " - Loading Spago graph data"
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
