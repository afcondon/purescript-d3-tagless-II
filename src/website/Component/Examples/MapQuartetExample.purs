module PSD3.Examples.MapQuartet where

import Prelude
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.RoutingDSL (routeToPath)
import PSD3.Website.Types (Route(..))

component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ -> unit
  , render
  , eval: H.mkEval H.defaultEval
  }

render :: forall m. Unit -> H.ComponentHTML Unit () m
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "example-page" ] ]
    [ HH.header_
        [ HH.a [ HP.href $ "#" <> routeToPath ExamplesGallery ] [ HH.text "← Examples Gallery" ]
        , HH.h1_ [ HH.text "Map Quartet Example" ]
        , HH.p_ [ HH.text "Example coming soon" ]
        ]
    ]
