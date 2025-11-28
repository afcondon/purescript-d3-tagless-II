module Component.Tour.TourWealthHealth where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Shared.SiteNav as SiteNav
import Effect.Aff (Milliseconds(..), delay)
import Component.WealthHealth.WealthHealth as WealthHealth
import Type.Proxy (Proxy(..))

-- | Tour page state
type State = Unit

-- | Tour page actions
data Action = Initialize

-- | Slots for child components
type Slots = ( wealthHealth :: forall q. H.Slot q Void Unit )

_wealthHealth :: Proxy "wealthHealth"
_wealthHealth = Proxy

-- | Tour page component
component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ -> unit
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action Slots o m Unit
handleAction = case _ of
  Initialize -> do
    -- Small delay to ensure DOM is ready
    H.liftAff $ delay (Milliseconds 100.0)
    pure unit

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "showcase-page", HH.ClassName "wealth-health-showcase" ] ]
    [ -- Site Navigation
      SiteNav.render
        { logoSize: SiteNav.Normal
        , quadrant: SiteNav.NoQuadrant
        , prevNext: Nothing
        , pageTitle: Just "Wealth & Health of Nations"
        }
    -- The WealthHealth component (full-screen visualization)
    , HH.slot_ _wealthHealth unit WealthHealth.component unit
    ]
