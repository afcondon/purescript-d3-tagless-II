-- | Halogen wrapper for Code Explorer
-- | Provides minimal container for the visualization
module Component.SpagoGridApp where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Engine.Explorer as Explorer

-- | Component state
type State =
  { initialized :: Boolean
  , error :: Maybe String
  }

-- | Actions
data Action
  = Initialize

-- | Main app component
component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState: \_ ->
        { initialized: false
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
  HH.div
    [ HP.class_ (HH.ClassName "explorer-app") ]
    [ -- Narrative Panel is created by Engine.NarrativePanel (top-left)
      -- It provides contextual description, color key, and back button
      -- The panel updates dynamically based on view state

      -- Error message (bottom-right to avoid collision with narrative panel)
      case state.error of
        Just err -> HH.div
          [ HP.classes [ HH.ClassName "floating-panel", HH.ClassName "floating-panel--bottom-right", HH.ClassName "error-message" ] ]
          [ HH.text err ]
        Nothing -> HH.text ""

    -- Visualization container
    , HH.div [ HP.id "viz" ] []
    ]

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    log "[Explorer] Initializing with new Scene Engine..."
    -- Initialize the explorer (loads data via legacy endpoints)
    liftEffect $ Explorer.initExplorer "#viz"
    H.modify_ _ { initialized = true }
