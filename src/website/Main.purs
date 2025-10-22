module PSD3.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Debug (spy)
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import PSD3.About as About
import PSD3.ExampleDetail as ExampleDetail
import PSD3.Gallery as Gallery
import PSD3.Home as Home
import PSD3.Interpreters as Interpreters
import PSD3.Navigation as Navigation
import PSD3.RoutingDSL (routing, routeToPath)
import PSD3.SpagoWrapper as Spago
import PSD3.Types (Route(..), ExampleId)
import Routing.Hash (matches, setHash)
import Type.Proxy (Proxy(..))

-- | Main application state
type State = {
  currentRoute :: Route
}

-- | Main application actions
data Action
  = Initialize
  | Navigate Route
  | HandleGalleryOutput ExampleId
  | RouteChanged (Maybe Route)

-- | Child component slots
type Slots =
  ( navigation :: forall q. H.Slot q Void Unit
  , home :: forall q. H.Slot q Void Unit
  , gallery :: forall q. H.Slot q ExampleId Unit
  , exampleDetail :: forall q. H.Slot q Void Unit
  , spago :: forall q. H.Slot q Void Unit
  , interpreters :: forall q. H.Slot q Void Unit
  , about :: forall q. H.Slot q Void Unit
  )

_navigation = Proxy :: Proxy "navigation"
_home = Proxy :: Proxy "home"
_gallery = Proxy :: Proxy "gallery"
_exampleDetail = Proxy :: Proxy "exampleDetail"
_spago = Proxy :: Proxy "spago"
_interpreters = Proxy :: Proxy "interpreters"
_about = Proxy :: Proxy "about"

-- | Main application component
component :: forall q i. H.Component q i Void Aff
component = H.mkComponent
  { initialState: \_ -> { currentRoute: Home } -- note, it really doesn't matter what's initalized here as Initialize reads the route from the hash
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

render :: State -> H.ComponentHTML Action Slots Aff
render state =
  HH.div
    [ HP.classes [ HH.ClassName "app" ] ]
    [ -- Navigation (hidden on example detail pages for fullscreen experience)
      if shouldHideNavigation state.currentRoute
        then HH.text ""
        else HH.slot_ _navigation unit Navigation.component state.currentRoute

    , -- Main content area
      HH.main
        [ HP.classes [ HH.ClassName "app__main" ] ]
        [ renderPage state.currentRoute ]
    ]

-- | Hide navigation on example detail pages for immersive fullscreen
shouldHideNavigation :: Route -> Boolean
shouldHideNavigation Gallery = false
shouldHideNavigation _ = true

-- | Render the current page based on route
renderPage :: Route -> H.ComponentHTML Action Slots Aff
renderPage route = case spy "Route is" route of
  Home ->
    HH.slot_ _home unit Home.component unit

  Gallery ->
    HH.slot _gallery unit Gallery.component unit HandleGalleryOutput

  Example exampleId ->
    HH.slot_ _exampleDetail unit ExampleDetail.component exampleId

  Spago ->
    HH.slot_ _spago unit Spago.component unit

  Interpreters ->
    HH.slot_ _interpreters unit Interpreters.component unit

  About ->
    HH.slot_ _about unit About.component unit

  NotFound ->
    HH.div
      [ HP.classes [ HH.ClassName "not-found" ] ]
      [ HH.h1_ [ HH.text "404 - Page Not Found" ]
      , HH.p_ [ HH.text "The page you're looking for doesn't exist." ]
      , HH.a
          [ HP.href $ "#" <> routeToPath Home ]
          [ HH.text "Go Home" ]
      ]

handleAction :: Action -> H.HalogenM State Action Slots Void Aff Unit
handleAction = case _ of
  Initialize -> do
    -- Subscribe to route changes using purescript-routing
    -- This handles both initial route and hash changes (back/forward buttons)
    _ <- H.subscribe $ HS.makeEmitter \push -> do
      matches routing \_ newRoute -> push (RouteChanged (Just newRoute))
    pure unit

  Navigate route -> do
    -- Navigation is now handled by Routing.Hash.setHash
    -- which will trigger the matches subscription above
    H.liftEffect $ setHash (routeToPath route)

  HandleGalleryOutput exampleId -> do
    -- When gallery emits an example ID, navigate to that example
    handleAction $ Navigate (Example exampleId)

  RouteChanged maybeRoute -> do
    -- When route changes (initial load, back/forward, or manual navigation)
    case maybeRoute of
      Just route -> H.modify_ _ { currentRoute = route }
      Nothing -> H.modify_ _ { currentRoute = NotFound } -- Fallback if route doesn't match

-- | Entry point
main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body
