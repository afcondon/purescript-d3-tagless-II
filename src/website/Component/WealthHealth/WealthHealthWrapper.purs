module PSD3.WealthHealth.WealthHealthWrapper where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.WealthHealth.WealthHealth as WealthHealth
import Type.Proxy (Proxy(..))

-- | Wrapper component for the Wealth & Health visualization
-- | Provides a full-page layout container
component :: forall q i o m. MonadAff m => MonadEffect m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ -> unit
  , render
  , eval: H.mkEval H.defaultEval
  }

type Slots =
  ( wealthHealth :: forall q. H.Slot q Void Unit
  )

_wealthHealth = Proxy :: Proxy "wealthHealth"

render :: forall m. MonadAff m => MonadEffect m => Unit -> H.ComponentHTML Void Slots m
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "wealth-health-wrapper" ] ]
    [ HH.slot_ _wealthHealth unit WealthHealth.component unit
    ]
