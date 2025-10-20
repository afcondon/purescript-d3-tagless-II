module PSD3.HomeForceLayout where

import Prelude

import PSD3.ForceNavigator as NavigationComponent
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Effect.Aff.Class (class MonadAff)
import Type.Proxy (Proxy(..))

type Slots =
  ( navigationViz :: forall q. H.Slot q Void Unit
  )

_navigationViz = Proxy :: Proxy "navigationViz"

-- | Fullscreen force layout home page
component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ -> unit
  , render
  , eval: H.mkEval H.defaultEval
  }

render :: forall m. MonadAff m => Unit -> H.ComponentHTML Unit Slots m
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "fullscreen-page-wrapper", HH.ClassName "home-force-layout" ] ]
    [ -- Full-screen container with force layout
      HH.div
        [ HP.classes [ HH.ClassName "fullscreen-container" ] ]
        [ -- Main visualization fills viewport - D3 will render into .svg-container
          HH.div
            [ HP.classes [ HH.ClassName "fullscreen-viz", HH.ClassName "home-force-layout__viz", HH.ClassName "svg-container" ]
            ]
            [ -- Render the navigation visualization component
              HH.slot_ _navigationViz unit NavigationComponent.component unit
            ]

        , -- Floating toolbar (top-left) - minimal for now
          HH.div
            [ HP.classes [ HH.ClassName "floating-panel", HH.ClassName "floating-panel--top-left", HH.ClassName "home-force-layout__toolbar" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "home-force-layout__title" ] ]
                [ HH.text "PureScript Tagless D3" ]
            , HH.p
                [ HP.classes [ HH.ClassName "home-force-layout__subtitle" ] ]
                [ HH.text "Type-safe, composable data visualization" ]
            ]
        ]
    ]
