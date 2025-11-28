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
import Component.HowTo.HowtoTransitions as HowtoTransitions
import Component.HowTo.HowtoForceGraphs as HowtoForceGraphs
import Component.HowTo.HowtoHierarchical as HowtoHierarchical
import Component.HowTo.HowtoEvents as HowtoEvents
import Component.HowTo.HowtoTreeAPI as HowtoTreeAPI
import Component.HowTo.HowtoLoadingData as HowtoLoadingData
import Component.HowTo.HowtoAxesScales as HowtoAxesScales
import Component.HowTo.HowtoTooltips as HowtoTooltips
import Component.HowTo.HowtoDebugging as HowtoDebugging
import Component.HowTo.HowtoPerformance as HowtoPerformance
import Component.HowTo.HowtoTreeExplorer as HowtoTreeExplorer
import PSD3.Understanding as Understanding
import Component.Understanding.UnderstandingGrammar as UnderstandingGrammar
import Component.Understanding.UnderstandingAttributes as UnderstandingAttributes
import Component.Understanding.UnderstandingSelections as UnderstandingSelections
import Component.Understanding.UnderstandingTreeAPI as UnderstandingTreeAPI
import Component.Understanding.UnderstandingScenes as UnderstandingScenes
import PSD3.Reference.Reference as Reference
import PSD3.Acknowledgements as Acknowledgements

-- Tour pages
import Component.Tour.TourIndex as TourIndex
import Component.Tour.TourFoundations as TourFoundations
import Component.Tour.TourProfessional as TourProfessional
import Component.Tour.TourFlow as TourFlow
import Component.Tour.TourHierarchies as TourHierarchies
import Component.Tour.TourMotion as TourMotion
import Component.Tour.TourWealthHealth as TourWealthHealth
import Component.Tour.TourInterpreters as TourInterpreters
import Component.Tour.TourFPFTW as TourFPFTW
import Component.Tour.TourGraphAlgorithms as TourGraphAlgorithms
import Component.Tour.TourShowcase as TourShowcase

-- Showcase
import Component.Showcase.ShowcaseIndex as ShowcaseIndex

-- Tree API Examples
import Component.ExamplesGallery as ExamplesGallery
import Component.Example as Example
import Component.TreeAPI as TreeAPI
import Component.AnimatedTreeCluster as AnimatedTreeCluster
import Component.GeneralUpdatePattern as GeneralUpdatePattern
import Component.MermaidTreeDemo as MermaidTreeDemo
import Component.SceneJoinDemo as SceneJoinDemo
import Component.ForceConfigPOC as ForceConfigPOC
import Component.ForceConfigV2Test as ForceConfigV2Test
import Component.SimpleForceGraph as SimpleForceGraph
import Component.CodeExplorerV3 as CodeExplorerV3

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
  , howtoTransitions :: forall q. H.Slot q Void Unit
  , howtoForceGraphs :: forall q. H.Slot q Void Unit
  , howtoHierarchical :: forall q. H.Slot q Void Unit
  , howtoEvents :: forall q. H.Slot q Void Unit
  , howtoTreeAPI :: forall q. H.Slot q Void Unit
  , howtoLoadingData :: forall q. H.Slot q Void Unit
  , howtoAxesScales :: forall q. H.Slot q Void Unit
  , howtoTooltips :: forall q. H.Slot q Void Unit
  , howtoDebugging :: forall q. H.Slot q Void Unit
  , howtoPerformance :: forall q. H.Slot q Void Unit
  , howtoTreeExplorer :: forall q. H.Slot q Void Unit
  , understanding :: forall q. H.Slot q Void Unit
  , understandingGrammar :: forall q. H.Slot q Void Unit
  , understandingAttributes :: forall q. H.Slot q Void Unit
  , understandingSelections :: forall q. H.Slot q Void Unit
  , understandingTreeAPI :: forall q. H.Slot q Void Unit
  , understandingScenes :: forall q. H.Slot q Void Unit
  , reference :: forall q. H.Slot q Void Unit
  , tourIndex :: forall q. H.Slot q Void Unit
  , tourFoundations :: forall q. H.Slot q Void Unit
  , tourProfessional :: forall q. H.Slot q Void Unit
  , tourFlow :: forall q. H.Slot q Void Unit
  , tourHierarchies :: forall q. H.Slot q Void Unit
  , tourMotion :: forall q. H.Slot q Void Unit
  , tourWealthHealth :: forall q. H.Slot q Void Unit
  , tourInterpreters :: forall q. H.Slot q Void Unit
  , tourFPFTW :: forall q. H.Slot q Void Unit
  , tourGraphAlgorithms :: forall q. H.Slot q Void Unit
  , tourShowcase :: forall q. H.Slot q Void Unit
  , showcase :: forall q. H.Slot q Void Unit
  , gallery :: forall q. H.Slot q Void Unit
  , example :: forall q. H.Slot q Void Unit
  , treeAPI :: forall q. H.Slot q Void Unit
  , animatedTreeCluster :: forall q. H.Slot q Void Unit
  , generalUpdatePattern :: forall q. H.Slot q Void Unit
  , mermaidTreeDemo :: forall q. H.Slot q Void Unit
  , sceneJoinDemo :: forall q. H.Slot q Void Unit
  , forceConfigPOC :: forall q. H.Slot q Void Unit
  , forceConfigV2Test :: forall q. H.Slot q Void Unit
  , simpleForceGraph :: forall q. H.Slot q Void Unit
  , codeExplorerV3 :: forall q. H.Slot q Void Unit
  , acknowledgements :: forall q. H.Slot q Void Unit
  )

_home = Proxy :: Proxy "home"
_gettingStarted = Proxy :: Proxy "gettingStarted"
_wizard = Proxy :: Proxy "wizard"
_howtoIndex = Proxy :: Proxy "howtoIndex"
_howtoTransitions = Proxy :: Proxy "howtoTransitions"
_howtoForceGraphs = Proxy :: Proxy "howtoForceGraphs"
_howtoHierarchical = Proxy :: Proxy "howtoHierarchical"
_howtoEvents = Proxy :: Proxy "howtoEvents"
_howtoTreeAPI = Proxy :: Proxy "howtoTreeAPI"
_howtoLoadingData = Proxy :: Proxy "howtoLoadingData"
_howtoAxesScales = Proxy :: Proxy "howtoAxesScales"
_howtoTooltips = Proxy :: Proxy "howtoTooltips"
_howtoDebugging = Proxy :: Proxy "howtoDebugging"
_howtoPerformance = Proxy :: Proxy "howtoPerformance"
_howtoTreeExplorer = Proxy :: Proxy "howtoTreeExplorer"
_understanding = Proxy :: Proxy "understanding"
_understandingGrammar = Proxy :: Proxy "understandingGrammar"
_understandingAttributes = Proxy :: Proxy "understandingAttributes"
_understandingSelections = Proxy :: Proxy "understandingSelections"
_understandingTreeAPI = Proxy :: Proxy "understandingTreeAPI"
_understandingScenes = Proxy :: Proxy "understandingScenes"
_reference = Proxy :: Proxy "reference"
_tourIndex = Proxy :: Proxy "tourIndex"
_tourFoundations = Proxy :: Proxy "tourFoundations"
_tourProfessional = Proxy :: Proxy "tourProfessional"
_tourFlow = Proxy :: Proxy "tourFlow"
_tourHierarchies = Proxy :: Proxy "tourHierarchies"
_tourMotion = Proxy :: Proxy "tourMotion"
_tourWealthHealth = Proxy :: Proxy "tourWealthHealth"
_tourInterpreters = Proxy :: Proxy "tourInterpreters"
_tourFPFTW = Proxy :: Proxy "tourFPFTW"
_tourGraphAlgorithms = Proxy :: Proxy "tourGraphAlgorithms"
_tourShowcase = Proxy :: Proxy "tourShowcase"
_showcase = Proxy :: Proxy "showcase"
_gallery = Proxy :: Proxy "gallery"
_example = Proxy :: Proxy "example"
_treeAPI = Proxy :: Proxy "treeAPI"
_animatedTreeCluster = Proxy :: Proxy "animatedTreeCluster"
_generalUpdatePattern = Proxy :: Proxy "generalUpdatePattern"
_mermaidTreeDemo = Proxy :: Proxy "mermaidTreeDemo"
_sceneJoinDemo = Proxy :: Proxy "sceneJoinDemo"
_forceConfigPOC = Proxy :: Proxy "forceConfigPOC"
_forceConfigV2Test = Proxy :: Proxy "forceConfigV2Test"
_simpleForceGraph = Proxy :: Proxy "simpleForceGraph"
_codeExplorerV3 = Proxy :: Proxy "codeExplorerV3"
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

  -- How-to sub-pages
  HowtoTransitions ->
    HH.slot_ _howtoTransitions unit HowtoTransitions.component unit

  HowtoForceGraphs ->
    HH.slot_ _howtoForceGraphs unit HowtoForceGraphs.component unit

  HowtoHierarchical ->
    HH.slot_ _howtoHierarchical unit HowtoHierarchical.component unit

  HowtoEvents ->
    HH.slot_ _howtoEvents unit HowtoEvents.component unit

  HowtoTreeAPI ->
    HH.slot_ _howtoTreeAPI unit HowtoTreeAPI.component unit

  HowtoLoadingData ->
    HH.slot_ _howtoLoadingData unit HowtoLoadingData.component unit

  HowtoAxesScales ->
    HH.slot_ _howtoAxesScales unit HowtoAxesScales.component unit

  HowtoTooltips ->
    HH.slot_ _howtoTooltips unit HowtoTooltips.component unit

  HowtoDebugging ->
    HH.slot_ _howtoDebugging unit HowtoDebugging.component unit

  HowtoPerformance ->
    HH.slot_ _howtoPerformance unit HowtoPerformance.component unit

  HowtoTreeExplorer ->
    HH.slot_ _howtoTreeExplorer unit HowtoTreeExplorer.component unit

  Understanding ->
    HH.slot_ _understanding unit Understanding.component unit

  -- Understanding sub-pages
  UnderstandingGrammar ->
    HH.slot_ _understandingGrammar unit UnderstandingGrammar.component unit

  UnderstandingAttributes ->
    HH.slot_ _understandingAttributes unit UnderstandingAttributes.component unit

  UnderstandingSelections ->
    HH.slot_ _understandingSelections unit UnderstandingSelections.component unit

  UnderstandingTreeAPI ->
    HH.slot_ _understandingTreeAPI unit UnderstandingTreeAPI.component unit

  UnderstandingScenes ->
    HH.slot_ _understandingScenes unit UnderstandingScenes.component unit

  Reference ->
    HH.slot_ _reference unit Reference.component unit

  ReferenceModule _ ->
    -- ReferenceModule routes redirect to the main Reference page
    HH.slot_ _reference unit Reference.component unit

  -- Tour Pages
  TourIndex ->
    HH.slot_ _tourIndex unit TourIndex.component unit

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

  TourWealthHealth ->
    HH.slot_ _tourWealthHealth unit TourWealthHealth.component unit

  TourInterpreters ->
    HH.slot_ _tourInterpreters unit TourInterpreters.component unit

  TourFPFTW ->
    HH.slot_ _tourFPFTW unit TourFPFTW.component unit

  TourGraphAlgorithms ->
    HH.slot_ _tourGraphAlgorithms unit TourGraphAlgorithms.component unit

  TourLesMisGUP ->
    -- Archived, redirect to CodeExplorerV3
    HH.slot_ _codeExplorerV3 unit CodeExplorerV3.component unit

  TourShowcase ->
    HH.slot_ _tourShowcase unit TourShowcase.component unit

  -- Showcase
  Showcase ->
    HH.slot_ _showcase unit ShowcaseIndex.component unit

  -- Tree API Examples
  Gallery ->
    HH.slot_ _gallery unit ExamplesGallery.component unit

  Example exampleId ->
    HH.slot_ _example unit Example.component exampleId

  TreeAPI ->
    HH.slot_ _treeAPI unit TreeAPI.component unit

  AnimatedTreeCluster ->
    HH.slot_ _animatedTreeCluster unit AnimatedTreeCluster.component unit

  GeneralUpdatePattern ->
    HH.slot_ _generalUpdatePattern unit GeneralUpdatePattern.component unit

  LesMisGUPTree ->
    -- Archived, redirect to V3
    HH.slot_ _codeExplorerV3 unit CodeExplorerV3.component unit

  LesMisGUP ->
    -- Archived, redirect to V3
    HH.slot_ _codeExplorerV3 unit CodeExplorerV3.component unit

  LesMisGUPSimple ->
    -- Archived, redirect to V3
    HH.slot_ _codeExplorerV3 unit CodeExplorerV3.component unit

  LesMisGUPTreeAPI ->
    -- Archived, redirect to V3
    HH.slot_ _codeExplorerV3 unit CodeExplorerV3.component unit

  LesMisGUPClean ->
    -- Archived, redirect to V3
    HH.slot_ _codeExplorerV3 unit CodeExplorerV3.component unit

  LesMisQueryDemo ->
    -- Archived, redirect to V3
    HH.slot_ _codeExplorerV3 unit CodeExplorerV3.component unit

  ModuleGraph ->
    -- Archived, redirect to V3
    HH.slot_ _codeExplorerV3 unit CodeExplorerV3.component unit

  MermaidTreeDemo ->
    HH.slot_ _mermaidTreeDemo unit MermaidTreeDemo.component unit

  SceneJoinDemo ->
    HH.slot_ _sceneJoinDemo unit SceneJoinDemo.component unit

  ForceConfigPOC ->
    HH.slot_ _forceConfigPOC unit ForceConfigPOC.component unit

  ForceConfigV2Test ->
    HH.slot_ _forceConfigV2Test unit ForceConfigV2Test.component unit

  SimpleForceGraph ->
    HH.slot_ _simpleForceGraph unit SimpleForceGraph.component unit

  CodeExplorer ->
    -- V1 archived, redirect to V3
    HH.slot_ _codeExplorerV3 unit CodeExplorerV3.component unit

  CodeExplorerV2 ->
    -- V2 archived, redirect to V3
    HH.slot_ _codeExplorerV3 unit CodeExplorerV3.component unit

  CodeExplorerV3 ->
    HH.slot_ _codeExplorerV3 unit CodeExplorerV3.component unit

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
    let _ = spy "RouteChanged" maybeRoute
    case maybeRoute of
      Just route -> do
        let _ = spy "Updating currentRoute to" route
        H.modify_ _ { currentRoute = route }
      Nothing -> H.modify_ _ { currentRoute = NotFound } -- Fallback if route doesn't match

-- | Entry point
main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body
