module V2.Pages.Home where

import Prelude

import V2.Pages.HomeForceLayout as HomeForceLayout
import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import Effect.Aff.Class (class MonadAff)

type Slots =
  ( forceLayout :: forall q. H.Slot q Void Unit
  )

_forceLayout = Proxy :: Proxy "forceLayout"

-- | Home page component - renders fullscreen force layout
component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ -> unit
  , render
  , eval: H.mkEval H.defaultEval
  }

render :: forall m. MonadAff m => Unit -> H.ComponentHTML Unit Slots m
render _ =
  -- Render the fullscreen force layout version
  HH.slot_ _forceLayout unit HomeForceLayout.component unit
