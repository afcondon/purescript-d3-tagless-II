-- | Minimal Halogen wrapper around SpagoGridTest
-- | Tests if Halogen itself causes the slowdown
module Component.SpagoGridApp where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Viz.SpagoGridTest (initSpagoGridTest)

-- | Component state
type State = { initialized :: Boolean }

-- | Actions
data Action = Initialize

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
    [ HP.class_ (HH.ClassName "spago-grid-app")
    , HP.style "width: 100%; height: 100%;"
    ]
    [ HH.h1 [ HP.style "position: absolute; top: 10px; left: 10px; z-index: 100; color: white;" ]
        [ HH.text "Spago Grid Test (Halogen)" ]
    , HH.div [ HP.id "viz", HP.style "width: 100%; height: 100%;" ] []
    ]

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    log "[SpagoGridApp] Initializing via Halogen..."
    liftEffect $ initSpagoGridTest "#viz"
    H.modify_ _ { initialized = true }
    log "[SpagoGridApp] Initialized"
