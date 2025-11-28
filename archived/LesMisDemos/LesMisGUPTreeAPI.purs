module Component.LesMisGUPTreeAPI where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import D3.Viz.LesMisGUPTreeAPI as LesMisGUPTreeAPI
import D3.Viz.LesMiserables.File (readGraphFromFileContents)
import Affjax.Web as AJAX
import Affjax.ResponseFormat as ResponseFormat

type State = { loaded :: Boolean }

data Action = Initialize

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState: \_ -> { loaded: false }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div [ HP.class_ (HH.ClassName "lesmis-gup-treeapi-page") ]
    [ HH.div [ HP.class_ (HH.ClassName "page-header") ]
        [ HH.h1_ [ HH.text "Les Misérables - TreeAPI + Simulation" ]
        , HH.p [ HP.class_ (HH.ClassName "page-description") ]
            [ HH.text "Testing renderTreeWithSimulation pattern - declarative tree structure with imperative simulation callback." ]
        ]

    , HH.div
        [ HP.id "lesmis-gup-treeapi"
        , HP.class_ (HH.ClassName "viz-container")
        ]
        []

    , HH.div [ HP.class_ (HH.ClassName "info-panel") ]
        [ HH.h3_ [ HH.text "What This Tests" ]
        , HH.ul_
            [ HH.li_ [ HH.text "renderTreeWithSimulation helper function" ]
            , HH.li_ [ HH.text "Declarative SVG structure via TreeAPI" ]
            , HH.li_ [ HH.text "Imperative simulation setup in callback" ]
            , HH.li_ [ HH.text "Force simulation with zoom/pan/drag" ]
            ]
        ]
    ]

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    log "LesMisGUPTreeAPI: Initializing"
    H.liftAff $ delay (Milliseconds 100.0)

    -- Load Les Misérables data
    lesMisResponse <- H.liftAff $ AJAX.get ResponseFormat.string "./data/miserables.json"
    case lesMisResponse of
      Left _ -> log "Failed to load Les Misérables data"
      Right response -> do
        let graph = readGraphFromFileContents (Right response)

        -- Call the main function which handles everything
        H.liftEffect $ LesMisGUPTreeAPI.main graph

        H.modify_ \s -> s { loaded = true }
        log "LesMisGUPTreeAPI: Initialized successfully"

    pure unit
