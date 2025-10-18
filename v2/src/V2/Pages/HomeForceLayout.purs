module V2.Pages.HomeForceLayout where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Effect.Aff.Class (class MonadAff)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)

type Slots :: forall k. Row k
type Slots = ()

type State = {
  initialized :: Boolean
}

data Action = Initialize

-- | Fullscreen force layout home page
component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ -> { initialized: false }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

render :: forall m. State -> H.ComponentHTML Action Slots m
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "fullscreen-page-wrapper", HH.ClassName "home-force-layout" ] ]
    [ -- Full-screen container with force layout
      HH.div
        [ HP.classes [ HH.ClassName "fullscreen-container" ] ]
        [ -- Main visualization fills viewport
          HH.div
            [ HP.classes [ HH.ClassName "fullscreen-viz", HH.ClassName "home-force-layout__viz" ]
            , HP.id "home-force-viz"
            ]
            [ -- Force layout will be rendered here via D3
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

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action Slots o m Unit
handleAction = case _ of
  Initialize -> do
    -- Get window dimensions and initialize empty force layout
    dims <- liftEffect getWindowDimensions
    _ <- liftEffect $ initializeEmptyForceLayout "#home-force-viz" dims.width dims.height
    H.modify_ _ { initialized = true }

-- | FFI: Get current window dimensions
foreign import getWindowDimensions :: Effect { width :: Number, height :: Number }

-- | FFI: Initialize an empty D3 force simulation
foreign import initializeEmptyForceLayout :: String -> Number -> Number -> Effect Unit
