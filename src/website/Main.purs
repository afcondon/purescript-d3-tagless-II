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
import PSD3.Understanding.UnderstandingIndex as UnderstandingIndex
import PSD3.Understanding.UnderstandingFinallyTagless as UnderstandingFinallyTagless
import PSD3.Understanding.UnderstandingSelectionM as UnderstandingSelectionM
import PSD3.Understanding.UnderstandingCapabilities as UnderstandingCapabilities
import PSD3.Understanding.UnderstandingTypeSystem as UnderstandingTypeSystem
import PSD3.Component.PSD3v2Examples as PSD3v2Examples
import PSD3.Understanding.UnderstandingDatumPattern as UnderstandingDatumPattern
import PSD3.Understanding.UnderstandingGrammar as UnderstandingGrammar
import PSD3.Understanding.SimpleCharts1 as SimpleCharts1
import PSD3.Understanding.SimpleCharts2 as SimpleCharts2
import PSD3.Understanding.DataFlowViz as DataFlowViz
import PSD3.Understanding.Hierarchies as Hierarchies
import PSD3.Understanding.IsometricCurveExperiment as IsometricCurveExperiment
import PSD3.Understanding.Interpreters as Interpreters
import PSD3.Component.MermaidDiagrams as MermaidDiagrams
import PSD3.ForceNavigator as ForceNavigator
import PSD3.Understanding.Movement as Movement
import PSD3.CodeExplorer.CodeExplorationPage as CodeExplorationPage
import PSD3.RoutingDSL (routing, routeToPath)
import PSD3.CodeExplorer.CodeExplorerWrapper as CodeExplorer
import PSD3.WealthHealth.WealthHealthWrapper as WealthHealth
-- import PSD3.CodeAtlas.CodeAtlasWrapper as CodeAtlas  -- Archived
import PSD3.FpFtw as FpFtw
import PSD3.ExamplesGallery as ExamplesGallery
import PSD3.Component.Example as Example
import PSD3.Acknowledgements as Acknowledgements
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
  , understandingIndex :: forall q. H.Slot q Void Unit
  , understandingFinallyTagless :: forall q. H.Slot q Void Unit
  , understandingSelectionM :: forall q. H.Slot q Void Unit
  , understandingCapabilities :: forall q. H.Slot q Void Unit
  , understandingTypeSystem :: forall q. H.Slot q Void Unit
  , understandingDatumPattern :: forall q. H.Slot q Void Unit
  , understandingGrammar :: forall q. H.Slot q Void Unit
  , simpleCharts1 :: forall q. H.Slot q Void Unit
  , simpleCharts2 :: forall q. H.Slot q Void Unit
  , dataFlowViz :: forall q. H.Slot q Void Unit
  , movement :: forall q. H.Slot q Void Unit
  , hierarchies :: forall q. H.Slot q Void Unit
  , isometricCurveExperiment :: forall q. H.Slot q Void Unit
  , interpreters :: forall q. H.Slot q Void Unit
  , mermaidDiagrams :: forall q. H.Slot q Void Unit
  , forceNavigator :: forall q. H.Slot q Void Unit
  , codeExplorer :: forall q. H.Slot q Void Unit
  , codeExploration :: forall q. H.Slot q Void String
  , wealthHealth :: forall q. H.Slot q Void Unit
  , codeAtlas :: forall q. H.Slot q Void Unit
  , fpFtw :: forall q. H.Slot q Void Unit
  , examplesGallery :: forall q. H.Slot q Void Unit
  , psd3v2Examples :: forall q. H.Slot q Void Unit
  , example :: forall q. H.Slot q Void String
  , acknowledgements :: forall q. H.Slot q Void Unit
  )

_home = Proxy :: Proxy "home"
_gettingStarted = Proxy :: Proxy "gettingStarted"
_wizard = Proxy :: Proxy "wizard"
_howtoIndex = Proxy :: Proxy "howtoIndex"
_reference = Proxy :: Proxy "reference"
_about = Proxy :: Proxy "about"
_concepts = Proxy :: Proxy "concepts"
_patterns = Proxy :: Proxy "patterns"
_understandingIndex = Proxy :: Proxy "understandingIndex"
_understandingFinallyTagless = Proxy :: Proxy "understandingFinallyTagless"
_understandingSelectionM = Proxy :: Proxy "understandingSelectionM"
_understandingCapabilities = Proxy :: Proxy "understandingCapabilities"
_understandingTypeSystem = Proxy :: Proxy "understandingTypeSystem"
_understandingDatumPattern = Proxy :: Proxy "understandingDatumPattern"
_understandingGrammar = Proxy :: Proxy "understandingGrammar"
_simpleCharts1 = Proxy :: Proxy "simpleCharts1"
_simpleCharts2 = Proxy :: Proxy "simpleCharts2"
_dataFlowViz = Proxy :: Proxy "dataFlowViz"
_movement = Proxy :: Proxy "movement"
_hierarchies = Proxy :: Proxy "hierarchies"
_isometricCurveExperiment = Proxy :: Proxy "isometricCurveExperiment"
_interpreters = Proxy :: Proxy "interpreters"
_mermaidDiagrams = Proxy :: Proxy "mermaidDiagrams"
_forceNavigator = Proxy :: Proxy "forceNavigator"
_codeExplorer = Proxy :: Proxy "codeExplorer"
_codeExploration = Proxy :: Proxy "codeExploration"
_wealthHealth = Proxy :: Proxy "wealthHealth"
-- _codeAtlas = Proxy :: Proxy "codeAtlas"  -- Archived
_fpFtw = Proxy :: Proxy "fpFtw"
_examplesGallery = Proxy :: Proxy "examplesGallery"
_psd3v2Examples = Proxy :: Proxy "psd3v2Examples"
_example = Proxy :: Proxy "example"
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

  About ->
    HH.slot_ _about unit About.component unit

  UnderstandingIndex ->
    HH.slot_ _understandingIndex unit UnderstandingIndex.component unit

  UnderstandingFinallyTagless ->
    HH.slot_ _understandingFinallyTagless unit UnderstandingFinallyTagless.component unit

  UnderstandingSelectionM ->
    HH.slot_ _understandingSelectionM unit UnderstandingSelectionM.component unit

  UnderstandingCapabilities ->
    HH.slot_ _understandingCapabilities unit UnderstandingCapabilities.component unit

  UnderstandingTypeSystem ->
    HH.slot_ _understandingTypeSystem unit UnderstandingTypeSystem.component unit

  UnderstandingDatumPattern ->
    HH.slot_ _understandingDatumPattern unit UnderstandingDatumPattern.component unit

  UnderstandingGrammar ->
    HH.slot_ _understandingGrammar unit UnderstandingGrammar.component unit

  UnderstandingConcepts ->
    HH.slot_ _concepts unit Concepts.component unit

  UnderstandingPatterns ->
    HH.slot_ _patterns unit Patterns.component unit

  UnderstandingPhilosophy ->
    HH.slot_ _about unit About.component unit

  SimpleCharts1 ->
    HH.slot_ _simpleCharts1 unit SimpleCharts1.component unit

  SimpleCharts2 ->
    HH.slot_ _simpleCharts2 unit SimpleCharts2.component unit

  DataFlowViz ->
    HH.slot_ _dataFlowViz unit DataFlowViz.component unit

  Movement ->
    HH.slot_ _movement unit Movement.component unit

  Hierarchies ->
    HH.slot_ _hierarchies unit Hierarchies.component unit

  IsometricCurveExperiment ->
    HH.slot_ _isometricCurveExperiment unit IsometricCurveExperiment.component unit

  Interpreters ->
    HH.slot_ _interpreters unit Interpreters.component unit

  MermaidDiagrams ->
    HH.slot_ _mermaidDiagrams unit MermaidDiagrams.component unit

  ForceNavigator ->
    HH.slot_ _forceNavigator unit ForceNavigator.component unit

  CodeExplorer ->
    HH.slot_ _codeExplorer unit CodeExplorer.component unit

  Explore snippetId ->
    HH.slot_ _codeExploration snippetId CodeExplorationPage.component snippetId

  WealthHealth ->
    HH.slot_ _wealthHealth unit WealthHealth.component unit

  CodeAtlas ->
    HH.div_ [ HH.text "CodeAtlas archived - under reconstruction" ]

  FpFtw ->
    HH.slot_ _fpFtw unit FpFtw.component unit

  ExamplesGallery ->
    HH.slot_ _examplesGallery unit ExamplesGallery.component unit

  PSD3v2Examples ->
    HH.slot_ _psd3v2Examples unit PSD3v2Examples.component unit

  Example exampleId ->
    renderExamplePage exampleId

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

-- | Render individual example pages based on exampleId
renderExamplePage :: String -> H.ComponentHTML Action Slots Aff
renderExamplePage exampleId =
  HH.slot_ _example exampleId Example.component exampleId

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
