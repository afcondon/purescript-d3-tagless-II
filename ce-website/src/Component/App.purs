-- | Main application component
module Component.App where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Types (Scene(..))

-- | Component state
type State =
  { scene :: Scene
  , loading :: Boolean
  , error :: Maybe String
  }

-- | Actions
data Action
  = Initialize
  | SetScene Scene

-- | Main app component
component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState: \_ ->
        { scene: Orbit
        , loading: true
        , error: Nothing
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div [ HP.class_ (HH.ClassName "ce-container") ]
    [ -- Header
      HH.div [ HP.class_ (HH.ClassName "ce-header") ]
        [ HH.h1_ [ HH.text "Code Explorer" ]
        , HH.div [ HP.class_ (HH.ClassName "ce-controls") ]
            [ sceneButton Orbit "Orbit" state.scene
            , sceneButton Tree "Tree" state.scene
            , sceneButton BubblePack "Detail" state.scene
            ]
        ]
    -- Visualization area
    , HH.div [ HP.class_ (HH.ClassName "ce-viz"), HP.id "viz" ]
        [ if state.loading
            then HH.div [ HP.class_ (HH.ClassName "loading") ] [ HH.text "Loading data..." ]
            else case state.error of
              Just err -> HH.div [ HP.class_ (HH.ClassName "loading") ] [ HH.text err ]
              Nothing -> HH.text ""  -- SVG will be rendered by D3
        ]
    -- Info panel
    , HH.div [ HP.class_ (HH.ClassName "ce-info") ]
        [ HH.h3_ [ HH.text "Scene" ]
        , HH.p_ [ HH.text (sceneDescription state.scene) ]
        ]
    ]

sceneButton :: forall m. Scene -> String -> Scene -> H.ComponentHTML Action () m
sceneButton scene label current =
  HH.button
    [ HP.classes
        [ HH.ClassName "ce-btn"
        , HH.ClassName if scene == current then "active" else ""
        ]
    , HE.onClick \_ -> SetScene scene
    ]
    [ HH.text label ]

sceneDescription :: Scene -> String
sceneDescription = case _ of
  Orbit -> "Packages on outer ring, modules clustered around them"
  Tree -> "Dependency tree rooted at focal module"
  BubblePack -> "Module internals: types, classes, functions"

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    log "[CE] Initializing..."
    -- TODO: Load data and initialize visualization
    H.modify_ _ { loading = false }

  SetScene scene -> do
    log $ "[CE] Switching to scene: " <> show scene
    H.modify_ _ { scene = scene }
    -- TODO: Trigger scene transition
