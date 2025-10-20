module PSD3.SpagoWrapper where

import Prelude

import PSD3.Spago as SpagoComponent
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Effect.Aff.Class (class MonadAff)
import Type.Proxy (Proxy(..))

type Slots =
  ( spagoComponent :: forall q. H.Slot q Void Unit
  )

_spagoComponent = Proxy :: Proxy "spagoComponent"

-- | Spago Explorer page
component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval H.defaultEval
  }

render :: forall state m. MonadAff m => state -> H.ComponentHTML Unit Slots m
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "fullscreen-page-wrapper", HH.ClassName "spago-page-wrapper" ] ]
    [ -- Full-screen Spago application
      HH.slot_ _spagoComponent unit SpagoComponent.component unit
    ]
