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
import Engine.Explorer as Explorer

-- | Component state (minimal - explorer state is in global ref)
type State = { initialized :: Boolean }

-- | Actions
data Action
  = Initialize
  | FormGrid    -- Transition to Grid layout (Static)
  | GridRun     -- Run physics in Grid layout
  | FormOrbit   -- Form orbit ring (Static)
  | OrbitRun    -- Run physics in Orbit layout
  | FormTree    -- Form tree positions (Static)
  | TreeRun     -- Run force-directed tree

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
            [ HP.style formButtonStyle
            , HE.onClick \_ -> FormGrid
            ]
            [ HH.text "Form Grid" ]
        , HH.button
            [ HP.style runButtonStyle
            , HE.onClick \_ -> GridRun
            ]
            [ HH.text "Grid Run" ]
        , HH.button
            [ HP.style formButtonStyle
            , HE.onClick \_ -> FormOrbit
            ]
            [ HH.text "Form Orbit" ]
        , HH.button
            [ HP.style runButtonStyle
            , HE.onClick \_ -> OrbitRun
            ]
            [ HH.text "Orbit Run" ]
        , HH.button
            [ HP.style formButtonStyle
            , HE.onClick \_ -> FormTree
            ]
            [ HH.text "Form Tree" ]
        , HH.button
            [ HP.style runButtonStyle
            , HE.onClick \_ -> TreeRun
            ]
            [ HH.text "Tree Run" ]
        ]
    , -- Visualization container
      HH.div [ HP.id "viz", HP.style "width: 100%; height: 100%;" ] []
    ]
  where
  formButtonStyle = "padding: 8px 16px; border: none; border-radius: 4px; background: #4a90d9; color: white; cursor: pointer; font-size: 14px;"
  runButtonStyle = "padding: 8px 16px; border: none; border-radius: 4px; background: #9b4ad9; color: white; cursor: pointer; font-size: 14px;"

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    log "[Explorer] Initializing with new Scene Engine..."
    liftEffect $ Explorer.initExplorer "#viz"
    H.modify_ _ { initialized = true }

  FormGrid -> goToScene "GridForm"
  GridRun -> goToScene "GridRun"
  FormOrbit -> goToScene "OrbitForm"
  OrbitRun -> goToScene "OrbitRun"
  FormTree -> goToScene "TreeForm"
  TreeRun -> goToScene "TreeRun"

-- | Helper to transition to a scene by name
goToScene :: forall output m. MonadAff m => String -> H.HalogenM State Action () output m Unit
goToScene sceneName = do
  log $ "[Explorer] " <> sceneName <> " button clicked"
  liftEffect do
    mRef <- Ref.read Explorer.globalStateRef
    case mRef of
      Nothing -> log "[Explorer] Not ready yet"
      Just ref -> Explorer.goToScene sceneName ref
