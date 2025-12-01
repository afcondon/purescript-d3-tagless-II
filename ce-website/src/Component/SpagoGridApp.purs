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
import Types (Scene(..))
import Viz.SpagoGridTest (initSpagoGridTest, transitionToScene, globalExplorerRef)

-- | Component state (minimal - explorer state is in global ref)
type State = { initialized :: Boolean }

-- | Actions
data Action
  = Initialize
  | GoToGrid
  | GoToTree

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
            , HE.onClick \_ -> GoToTree
            ]
            [ HH.text "Tree" ]
        ]
    , -- Visualization container
      HH.div [ HP.id "viz", HP.style "width: 100%; height: 100%;" ] []
    ]
  where
  buttonStyle = "padding: 8px 16px; border: none; border-radius: 4px; background: #4a90d9; color: white; cursor: pointer; font-size: 14px;"

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    log "[Explorer] Initializing..."
    liftEffect $ initSpagoGridTest "#viz"
    H.modify_ _ { initialized = true }

  GoToGrid -> do
    log "[Explorer] Grid button clicked"
    liftEffect do
      mRef <- Ref.read globalExplorerRef
      case mRef of
        Nothing -> log "[Explorer] Not ready yet"
        Just ref -> transitionToScene Grid ref

  GoToTree -> do
    log "[Explorer] Tree button clicked"
    liftEffect do
      mRef <- Ref.read globalExplorerRef
      case mRef of
        Nothing -> log "[Explorer] Not ready yet"
        Just ref -> transitionToScene Tree ref
