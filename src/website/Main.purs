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
import PSD3.Home as Home
import PSD3.Tutorial.GettingStarted as GettingStarted
import PSD3.Wizard.Wizard as Wizard
import PSD3.HowTo.HowtoIndex as HowtoIndex
import PSD3.Reference.Reference as Reference
import PSD3.Understanding.About as About
import PSD3.Understanding.Concepts as Concepts
import PSD3.Understanding.Patterns as Patterns
import PSD3.Understanding.Tutorial as Tutorial
import PSD3.Understanding.SimpleCharts as SimpleCharts
import PSD3.Understanding.ChordDiagram as ChordDiagram
import PSD3.Understanding.BubbleChart as BubbleChart
import PSD3.Understanding.SankeyDiagram as SankeyDiagram
import PSD3.Understanding.Hierarchies as Hierarchies
import PSD3.Understanding.Interpreters as Interpreters
import PSD3.CodeExplorer.CodeExplorationPage as CodeExplorationPage
import PSD3.RoutingDSL (routing, routeToPath)
import PSD3.CodeExplorer.CodeExplorerWrapper as CodeExplorer
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
  , about :: forall q. H.Slot q Void Unit
  , concepts :: forall q. H.Slot q Void Unit
  , patterns :: forall q. H.Slot q Void Unit
  , tutorial :: forall q. H.Slot q Void Unit
  , simpleCharts :: forall q. H.Slot q Void Unit
  , chordDiagram :: forall q. H.Slot q Void Unit
  , bubbleChart :: forall q. H.Slot q Void Unit
  , sankeyDiagram :: forall q. H.Slot q Void Unit
  , hierarchies :: forall q. H.Slot q Void Unit
  , interpreters :: forall q. H.Slot q Void Unit
  , codeExplorer :: forall q. H.Slot q Void Unit
  , codeExploration :: forall q. H.Slot q Void Unit
  )

_home = Proxy :: Proxy "home"
_gettingStarted = Proxy :: Proxy "gettingStarted"
_wizard = Proxy :: Proxy "wizard"
_howtoIndex = Proxy :: Proxy "howtoIndex"
_reference = Proxy :: Proxy "reference"
_about = Proxy :: Proxy "about"
_concepts = Proxy :: Proxy "concepts"
_patterns = Proxy :: Proxy "patterns"
_tutorial = Proxy :: Proxy "tutorial"
_simpleCharts = Proxy :: Proxy "simpleCharts"
_chordDiagram = Proxy :: Proxy "chordDiagram"
_bubbleChart = Proxy :: Proxy "bubbleChart"
_sankeyDiagram = Proxy :: Proxy "sankeyDiagram"
_hierarchies = Proxy :: Proxy "hierarchies"
_interpreters = Proxy :: Proxy "interpreters"
_codeExplorer = Proxy :: Proxy "codeExplorer"
_codeExploration = Proxy :: Proxy "codeExploration"

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

  About ->
    HH.slot_ _about unit About.component unit

  UnderstandingConcepts ->
    HH.slot_ _concepts unit Concepts.component unit

  UnderstandingPatterns ->
    HH.slot_ _patterns unit Patterns.component unit

  UnderstandingPhilosophy ->
    HH.slot_ _about unit About.component unit

  Tutorial ->
    HH.slot_ _tutorial unit Tutorial.component unit

  SimpleCharts ->
    HH.slot_ _simpleCharts unit SimpleCharts.component unit

  ChordDiagram ->
    HH.slot_ _chordDiagram unit ChordDiagram.component unit

  BubbleChart ->
    HH.slot_ _bubbleChart unit BubbleChart.component unit

  SankeyDiagram ->
    HH.slot_ _sankeyDiagram unit SankeyDiagram.component unit

  Hierarchies ->
    HH.slot_ _hierarchies unit Hierarchies.component unit

  Interpreters ->
    HH.slot_ _interpreters unit Interpreters.component unit

  CodeExplorer ->
    HH.slot_ _codeExplorer unit CodeExplorer.component unit

  Explore snippetId ->
    HH.slot_ _codeExploration unit CodeExplorationPage.component snippetId

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
