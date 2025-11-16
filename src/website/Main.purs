module PSD3.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Debug (spy)
import Effect (Effect)
import Effect.Aff (Aff)
import Web.HTML (window)
import Web.HTML.Location as Web.HTML.Location
import Web.HTML.Window as Web.HTML.Window
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
-- Core pages
import PSD3.Home as Home
import PSD3.Tutorial.GettingStarted as GettingStarted
import PSD3.Wizard.Wizard as Wizard
import PSD3.HowTo.HowtoIndex as HowtoIndex
import PSD3.Reference.Reference as Reference
import PSD3.Acknowledgements as Acknowledgements

-- Tour pages
import Component.Tour.TourFoundations as TourFoundations
import Component.Tour.TourProfessional as TourProfessional
import Component.Tour.TourFlow as TourFlow
import Component.Tour.TourHierarchies as TourHierarchies
import Component.Tour.TourMotion as TourMotion
import Component.Tour.TourInterpreters as TourInterpreters
import Component.Tour.TourFPFTW as TourFPFTW
import Component.Tour.TourShowcase as TourShowcase

-- Tree API Examples
import Component.ExamplesGallery as ExamplesGallery
import Component.Example as Example
import Component.TreeAPI as TreeAPI
import Component.AnimatedTreeCluster as AnimatedTreeCluster
import Component.LesMisGUPTree as LesMisGUPTree
import Component.ModuleGraph as ModuleGraph
import Component.MermaidTreeDemo as MermaidTreeDemo

-- Routing
import PSD3.RoutingDSL (routing, routeToPath)
import PSD3.Website.Types (Route(..))
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
  | RouteChanged (Maybe Route)

-- | Child component slots
type Slots =
  ( home :: forall q. H.Slot q Void Unit
  , gettingStarted :: forall q. H.Slot q Void Unit
  , wizard :: forall q. H.Slot q Void Unit
  , howtoIndex :: forall q. H.Slot q Void Unit
  , reference :: forall q. H.Slot q Void Unit
  , tourFoundations :: forall q. H.Slot q Void Unit
  , tourProfessional :: forall q. H.Slot q Void Unit
  , tourFlow :: forall q. H.Slot q Void Unit
  , tourHierarchies :: forall q. H.Slot q Void Unit
  , tourMotion :: forall q. H.Slot q Void Unit
  , tourInterpreters :: forall q. H.Slot q Void Unit
  , tourFPFTW :: forall q. H.Slot q Void Unit
  , tourShowcase :: forall q. H.Slot q Void Unit
  , gallery :: forall q. H.Slot q Void Unit
  , example :: forall q. H.Slot q Void Unit
  , treeAPI :: forall q. H.Slot q Void Unit
  , animatedTreeCluster :: forall q. H.Slot q Void Unit
  , lesMisGUPTree :: forall q. H.Slot q Void Unit
  , moduleGraph :: forall q. H.Slot q Void Unit
  , mermaidTreeDemo :: forall q. H.Slot q Void Unit
  , acknowledgements :: forall q. H.Slot q Void Unit
  )

_home = Proxy :: Proxy "home"
_gettingStarted = Proxy :: Proxy "gettingStarted"
_wizard = Proxy :: Proxy "wizard"
_howtoIndex = Proxy :: Proxy "howtoIndex"
_reference = Proxy :: Proxy "reference"
_tourFoundations = Proxy :: Proxy "tourFoundations"
_tourProfessional = Proxy :: Proxy "tourProfessional"
_tourFlow = Proxy :: Proxy "tourFlow"
_tourHierarchies = Proxy :: Proxy "tourHierarchies"
_tourMotion = Proxy :: Proxy "tourMotion"
_tourInterpreters = Proxy :: Proxy "tourInterpreters"
_tourFPFTW = Proxy :: Proxy "tourFPFTW"
_tourShowcase = Proxy :: Proxy "tourShowcase"
_gallery = Proxy :: Proxy "gallery"
_example = Proxy :: Proxy "example"
_treeAPI = Proxy :: Proxy "treeAPI"
_animatedTreeCluster = Proxy :: Proxy "animatedTreeCluster"
_lesMisGUPTree = Proxy :: Proxy "lesMisGUPTree"
_moduleGraph = Proxy :: Proxy "moduleGraph"
_mermaidTreeDemo = Proxy :: Proxy "mermaidTreeDemo"
_acknowledgements = Proxy :: Proxy "acknowledgements"

-- | Main application component
component :: forall q i. H.Component q i Void Aff
component = H.mkComponent
  { initialState: \_ -> { currentRoute: Home } -- note, it really doesn't matter what's initialized here as Initialize reads the route from the hash
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
    [ -- Main content area (no separate navigation component)
      HH.main
        [ HP.classes [ HH.ClassName "app__main" ] ]
        [ renderPage state.currentRoute ]
    ]

-- | Render the current page based on route
renderPage :: Route -> H.ComponentHTML Action Slots Aff
renderPage route = case spy "Route is" route of
  Home ->
    HH.slot_ _home unit Home.component unit

  GettingStarted ->
    HH.slot_ _gettingStarted unit GettingStarted.component unit

  Wizard ->
    HH.slot_ _wizard unit Wizard.component unit

  HowtoIndex ->
    HH.slot_ _howtoIndex unit HowtoIndex.component unit

  Reference ->
    HH.slot_ _reference unit Reference.component Reference

  ReferenceModule moduleName ->
    HH.slot_ _reference unit Reference.component (ReferenceModule moduleName)

  -- Tour Pages
  TourFoundations ->
    HH.slot_ _tourFoundations unit TourFoundations.component unit

  TourProfessional ->
    HH.slot_ _tourProfessional unit TourProfessional.component unit

  TourFlow ->
    HH.slot_ _tourFlow unit TourFlow.component unit

  TourHierarchies ->
    HH.slot_ _tourHierarchies unit TourHierarchies.component unit

  TourMotion ->
    HH.slot_ _tourMotion unit TourMotion.component unit

  TourInterpreters ->
    HH.slot_ _tourInterpreters unit TourInterpreters.component unit

  TourFPFTW ->
    HH.slot_ _tourFPFTW unit TourFPFTW.component unit

  TourShowcase ->
    HH.slot_ _tourShowcase unit TourShowcase.component unit

  -- Tree API Examples
  Gallery ->
    HH.slot_ _gallery unit ExamplesGallery.component unit

  Example exampleId ->
    HH.slot_ _example unit Example.component exampleId

  TreeAPI ->
    HH.slot_ _treeAPI unit TreeAPI.component unit

  AnimatedTreeCluster ->
    HH.slot_ _animatedTreeCluster unit AnimatedTreeCluster.component unit

  LesMisGUPTree ->
    HH.slot_ _lesMisGUPTree unit LesMisGUPTree.component unit

  ModuleGraph ->
    HH.slot_ _moduleGraph unit ModuleGraph.component unit

  MermaidTreeDemo ->
    HH.slot_ _mermaidTreeDemo unit MermaidTreeDemo.component unit

  Acknowledgements ->
    HH.slot_ _acknowledgements unit Acknowledgements.component unit

  NotFound ->
    HH.div
      [ HP.classes [ HH.ClassName "not-found" ] ]
      [ HH.h1_ [ HH.text "404 - Page Not Found" ]
      , HH.p_ [ HH.text "The page you're looking for doesn't exist." ]
      , HH.a
          [ HP.href $ "#" <> routeToPath Home ]
          [ HH.text "Go to Home" ]
      ]

handleAction :: Action -> H.HalogenM State Action Slots Void Aff Unit
handleAction = case _ of
  Initialize -> do
    -- Check if we're at root and redirect to /home for proper history
    currentHash <- H.liftEffect $ do
      w <- window
      loc <- Web.HTML.Window.location w
      Web.HTML.Location.hash loc
    when (currentHash == "" || currentHash == "#" || currentHash == "#/") $ do
      H.liftEffect $ setHash (routeToPath Home)

    -- Subscribe to route changes using purescript-routing
    -- This handles both initial route and hash changes (back/forward buttons)
    _ <- H.subscribe $ HS.makeEmitter \push -> do
      matches routing \_ newRoute -> do
        push (RouteChanged (Just newRoute))
    pure unit

  Navigate route -> do
    -- Navigation is now handled by Routing.Hash.setHash
    -- which will trigger the matches subscription above
    H.liftEffect $ setHash (routeToPath route)

  RouteChanged maybeRoute -> do
    -- When route changes (initial load, back/forward, or manual navigation)
    case maybeRoute of
      Just route -> do
        H.modify_ _ { currentRoute = route }
      Nothing -> H.modify_ _ { currentRoute = NotFound } -- Fallback if route doesn't match

-- | Entry point
main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body
