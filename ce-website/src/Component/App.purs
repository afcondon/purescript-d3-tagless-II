-- | Main application component
module Component.App where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Ref (Ref)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.Loader (loadModel)
import Viz.Explorer as Explorer
import Types (Scene(..), TransitionPhase(..))

-- | Component state
type State =
  { scene :: Scene
  , transition :: TransitionPhase
  , loading :: Boolean
  , error :: Maybe String
  , explorerRef :: Maybe ExplorerRef
  }

-- | Opaque explorer ref type (forall-quantified row params)
foreign import data ExplorerRef :: Type

-- | Actions
data Action
  = Initialize
  | SetScene Scene

-- | Main app component
component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState: \_ ->
        { scene: Grid
        , transition: Stable
        , loading: true
        , error: Nothing
        , explorerRef: Nothing
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
    [ -- Header with build stamp
      HH.div [ HP.class_ (HH.ClassName "ce-header") ]
        [ HH.h1_ [ HH.text "Code Explorer" ]
        , HH.span [ HP.class_ (HH.ClassName "ce-build") ] [ HH.text "Build: 2024-12-01 14:45 (no ManyBody)" ]
        , HH.div [ HP.class_ (HH.ClassName "ce-controls") ]
            [ sceneButton Grid "Grid" state.scene
            , sceneButton Orbit "Orbit" state.scene
            , sceneButton Tree "Tree" state.scene
            , sceneButton BubblePack "Detail" state.scene
            ]
        ]
    -- Error display (if any)
    , case state.error of
        Just err -> HH.div [ HP.class_ (HH.ClassName "ce-error") ] [ HH.text err ]
        Nothing ->
          if state.loading
            then HH.div [ HP.class_ (HH.ClassName "ce-loading") ] [ HH.text "Loading data..." ]
            else HH.text ""  -- Don't render anything - D3 manages the viz
    -- Visualization area - D3 renders directly here, Halogen doesn't touch it
    , HH.div [ HP.class_ (HH.ClassName "ce-viz"), HP.id "viz" ] []
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
  Grid -> "Starting view: packages on grid, modules clustering"
  Orbit -> "Packages on outer ring, main module at center"
  Tree -> "Dependency tree rooted at main module"
  BubblePack -> "Module internals: types, classes, functions"

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    log "[CE] Loading data..."
    result <- liftAff loadModel
    case result of
      Left err -> do
        log $ "[CE] Error loading data: " <> err
        H.modify_ _ { loading = false, error = Just err }
      Right model -> do
        log $ "[CE] Loaded " <> show model.moduleCount <> " modules, " <> show model.packageCount <> " packages"
        -- Initialize the explorer
        explorerRef <- liftEffect $ Explorer.initExplorer model "#viz"
        H.modify_ _
          { loading = false
          , explorerRef = Just (toExplorerRef explorerRef)
          }

  SetScene scene -> do
    log $ "[CE] Switching to scene: " <> show scene
    state <- H.get
    case state.explorerRef of
      Nothing -> pure unit
      Just ref -> do
        liftEffect $ Explorer.transitionToScene scene (fromExplorerRef ref)
        H.modify_ _ { scene = scene }

-- | FFI helpers to convert between concrete Ref and opaque ExplorerRef
foreign import toExplorerRef :: forall a. Ref a -> ExplorerRef
foreign import fromExplorerRef :: forall a. ExplorerRef -> Ref a
