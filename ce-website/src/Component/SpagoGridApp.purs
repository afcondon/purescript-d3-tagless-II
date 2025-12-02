-- | Halogen wrapper for Code Explorer
-- | Provides UI controls for scene transitions
module Component.SpagoGridApp where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Ref as Ref
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Effect (Effect)
import Engine.Explorer as Explorer
import Viz.FlareRadialTree as FlareRadialTree

-- | FFI to show the flare container
foreign import showFlareContainer :: Effect Unit

-- | Component state (minimal - explorer state is in global ref)
type State = { initialized :: Boolean }

-- | Actions
data Action
  = Initialize
  | GoToGrid
  | GoToTree1
  | GoToTree2
  | GoToTree3
  | GoToTree4
  | ShowFlareTree
  | ShowDependencyTree
  | ShowVerticalTree

-- | Main app component
component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState: \_ -> { initialized: false }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div
    [ HP.class_ (HH.ClassName "explorer-app")
    , HP.style "width: 100%; height: 100%; position: relative;"
    ]
    [ -- Control panel
      HH.div
        [ HP.style "position: absolute; top: 10px; left: 10px; z-index: 100; display: flex; gap: 10px; align-items: center;"
        ]
        [ HH.h1
            [ HP.style "margin: 0; font-size: 18px; color: #333;" ]
            [ HH.text "Code Explorer" ]
        , HH.button
            [ HP.style buttonStyle
            , HE.onClick \_ -> GoToGrid
            ]
            [ HH.text "Grid" ]
        , HH.button
            [ HP.style buttonStyle
            , HE.onClick \_ -> GoToTree1
            ]
            [ HH.text "Tree1" ]
        , HH.button
            [ HP.style buttonStyle
            , HE.onClick \_ -> GoToTree2
            ]
            [ HH.text "Tree2" ]
        , HH.button
            [ HP.style buttonStyle
            , HE.onClick \_ -> GoToTree3
            ]
            [ HH.text "Tree3" ]
        , HH.button
            [ HP.style tree4ButtonStyle
            , HE.onClick \_ -> GoToTree4
            ]
            [ HH.text "Tree4" ]
        , HH.button
            [ HP.style flareButtonStyle
            , HE.onClick \_ -> ShowFlareTree
            ]
            [ HH.text "Flare Test" ]
        , HH.button
            [ HP.style depTreeButtonStyle
            , HE.onClick \_ -> ShowDependencyTree
            ]
            [ HH.text "Dep Tree" ]
        , HH.button
            [ HP.style vertTreeButtonStyle
            , HE.onClick \_ -> ShowVerticalTree
            ]
            [ HH.text "Vertical" ]
        ]
    , -- Visualization container
      HH.div [ HP.id "viz", HP.style "width: 100%; height: 100%;" ] []
    , -- Flare tree test container (overlaid)
      HH.div
        [ HP.id "flare-tree-container"
        , HP.style "position: absolute; top: 60px; right: 10px; width: 620px; height: 620px; background: white; border: 2px solid #999; border-radius: 8px; display: none; z-index: 200;"
        ]
        []
    ]
  where
  buttonStyle = "padding: 8px 16px; border: none; border-radius: 4px; background: #4a90d9; color: white; cursor: pointer; font-size: 14px;"
  tree4ButtonStyle = "padding: 8px 16px; border: none; border-radius: 4px; background: #9b4ad9; color: white; cursor: pointer; font-size: 14px;"
  flareButtonStyle = "padding: 8px 16px; border: none; border-radius: 4px; background: #d94a90; color: white; cursor: pointer; font-size: 14px;"
  depTreeButtonStyle = "padding: 8px 16px; border: none; border-radius: 4px; background: #4ad990; color: white; cursor: pointer; font-size: 14px;"
  vertTreeButtonStyle = "padding: 8px 16px; border: none; border-radius: 4px; background: #d9904a; color: white; cursor: pointer; font-size: 14px;"

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    log "[Explorer] Initializing with new Scene Engine..."
    liftEffect $ Explorer.initExplorer "#viz"
    H.modify_ _ { initialized = true }

  GoToGrid -> goToScene "Grid"
  GoToTree1 -> goToScene "Tree1"
  GoToTree2 -> goToScene "Tree2"
  GoToTree3 -> goToScene "Tree3"
  GoToTree4 -> goToScene "Tree4"
  ShowFlareTree -> do
    log "[Explorer] Showing Flare radial tree test..."
    liftEffect $ showFlareContainer
    liftEffect $ FlareRadialTree.loadAndRenderFlareTree "#flare-tree-container"

  ShowDependencyTree -> do
    log "[Explorer] Showing dependency tree test..."
    liftEffect $ showFlareContainer
    liftEffect $ FlareRadialTree.loadAndRenderDependencyTree "#flare-tree-container"

  ShowVerticalTree -> do
    log "[Explorer] Showing vertical tree for debugging..."
    liftEffect $ showFlareContainer
    liftEffect $ FlareRadialTree.loadAndRenderDependencyTreeVertical "#flare-tree-container"

-- | Helper to transition to a scene by name
goToScene :: forall output m. MonadAff m => String -> H.HalogenM State Action () output m Unit
goToScene sceneName = do
  log $ "[Explorer] " <> sceneName <> " button clicked"
  liftEffect do
    mRef <- Ref.read Explorer.globalStateRef
    case mRef of
      Nothing -> log "[Explorer] Not ready yet"
      Just ref -> Explorer.goToScene sceneName ref
