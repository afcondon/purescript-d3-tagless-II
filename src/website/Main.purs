module PSD3.Main where

import Prelude

import PSD3.Types (Route(..), ExampleId)
import PSD3.Router (parseRoute, routeToHash)
import PSD3.Navigation as Navigation
import PSD3.Gallery as Gallery
import PSD3.Home as Home
import PSD3.ExampleDetail as ExampleDetail
import PSD3.SpagoWrapper as Spago
import PSD3.Interpreters as Interpreters
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Web.HTML (window)
import Web.HTML.Window (location, toEventTarget)
import Web.HTML.Location (hash, setHash)
import Web.Event.EventTarget (eventListener, addEventListener)
import Web.Event.Event (EventType(..))
import Data.Maybe (Maybe(..))
import Type.Proxy (Proxy(..))
import Halogen.Subscription as HS

-- | Main application state
type State = {
  currentRoute :: Route
}

-- | Main application actions
data Action
  = Initialize
  | Navigate Route
  | HandleGalleryOutput ExampleId
  | HashChanged String

-- | Child component slots
type Slots =
  ( navigation :: forall q. H.Slot q Void Unit
  , home :: forall q. H.Slot q Void Unit
  , gallery :: forall q. H.Slot q ExampleId Unit
  , exampleDetail :: forall q. H.Slot q Void Unit
  , spago :: forall q. H.Slot q Void Unit
  , interpreters :: forall q. H.Slot q Void Unit
  )

_navigation = Proxy :: Proxy "navigation"
_home = Proxy :: Proxy "home"
_gallery = Proxy :: Proxy "gallery"
_exampleDetail = Proxy :: Proxy "exampleDetail"
_spago = Proxy :: Proxy "spago"
_interpreters = Proxy :: Proxy "interpreters"

-- | Main application component
component :: forall q i. H.Component q i Void Aff
component = H.mkComponent
  { initialState: \_ -> { currentRoute: Home }
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
shouldHideNavigation (Example _) = true
shouldHideNavigation Spago = true
shouldHideNavigation Home = true
shouldHideNavigation _ = false

-- | Render the current page based on route
renderPage :: Route -> H.ComponentHTML Action Slots Aff
renderPage route = case route of
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

  NotFound ->
    HH.div
      [ HP.classes [ HH.ClassName "not-found" ] ]
      [ HH.h1_ [ HH.text "404 - Page Not Found" ]
      , HH.p_ [ HH.text "The page you're looking for doesn't exist." ]
      , HH.a
          [ HP.href $ routeToHash Home ]
          [ HH.text "Go Home" ]
      ]

handleAction :: Action -> H.HalogenM State Action Slots Void Aff Unit
handleAction = case _ of
  Initialize -> do
    -- Read initial route from URL hash
    currentHash <- H.liftEffect $ window >>= location >>= hash
    let route = parseRoute currentHash
    H.modify_ _ { currentRoute = route }

    -- Subscribe to hash changes
    _ <- H.subscribe do
      HS.makeEmitter \push -> do
        win <- window
        let target = toEventTarget win
        listener <- eventListener \_ -> do
          newHash <- window >>= location >>= hash
          push (HashChanged newHash)
        addEventListener (EventType "hashchange") listener false target
        pure mempty
    pure unit

  Navigate route -> do
    -- Update component state
    H.modify_ _ { currentRoute = route }
    -- Update browser URL hash
    let newHash = routeToHash route
    H.liftEffect do
      win <- window
      loc <- location win
      setHash newHash loc

  HandleGalleryOutput exampleId -> do
    -- When gallery emits an example ID, navigate to that example
    handleAction $ Navigate (Example exampleId)

  HashChanged newHash -> do
    -- When browser hash changes (back button, etc), update route
    let route = parseRoute newHash
    H.modify_ _ { currentRoute = route }

-- | Entry point
main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body
