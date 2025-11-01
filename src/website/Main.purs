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
import PSD3.Understanding.SimpleCharts1 as SimpleCharts1
import PSD3.Understanding.SimpleCharts2 as SimpleCharts2
import PSD3.Understanding.DataFlowViz as DataFlowViz
import PSD3.Understanding.Hierarchies as Hierarchies
import PSD3.Understanding.Interpreters as Interpreters
import PSD3.Understanding.Movement as Movement
import PSD3.CodeExplorer.CodeExplorationPage as CodeExplorationPage
import PSD3.RoutingDSL (routing, routeToPath)
import PSD3.CodeExplorer.CodeExplorerWrapper as CodeExplorer
import PSD3.WealthHealth.WealthHealthWrapper as WealthHealth
import PSD3.CodeAtlas.CodeAtlasWrapper as CodeAtlas
import PSD3.FpFtw as FpFtw
import PSD3.ExamplesGallery as ExamplesGallery
import PSD3.Examples.BarChart as BarChartExample
import PSD3.Examples.ThreeLittleCircles as ThreeLittleCirclesExample
import PSD3.Examples.LineChart as LineChartExample
import PSD3.Examples.ScatterPlot as ScatterPlotExample
import PSD3.Examples.GroupedBarChart as GroupedBarChartExample
import PSD3.Examples.MultiLineChart as MultiLineChartExample
import PSD3.Examples.BubbleChart as BubbleChartExample
import PSD3.Examples.ChordDiagram as ChordDiagramExample
-- import PSD3.Examples.VerticalTree as VerticalTreeExample
-- import PSD3.Examples.HorizontalTree as HorizontalTreeExample
-- import PSD3.Examples.RadialTree as RadialTreeExample
-- import PSD3.Examples.Treemap as TreemapExample
-- import PSD3.Examples.Icicle as IcicleExample
-- import PSD3.Examples.LesMisForce as LesMisForceExample
-- import PSD3.Examples.TopologicalSort as TopologicalSortExample
-- import PSD3.Examples.SankeyDiagram as SankeyDiagramExample
-- import PSD3.Examples.MapQuartet as MapQuartetExample
import PSD3.Examples.ThreeCirclesTransition as ThreeCirclesTransitionExample
import PSD3.Examples.GeneralUpdatePattern as GeneralUpdatePatternExample
-- import PSD3.Examples.AnimatedRadialTree as AnimatedRadialTreeExample
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
  , simpleCharts1 :: forall q. H.Slot q Void Unit
  , simpleCharts2 :: forall q. H.Slot q Void Unit
  , dataFlowViz :: forall q. H.Slot q Void Unit
  , movement :: forall q. H.Slot q Void Unit
  , hierarchies :: forall q. H.Slot q Void Unit
  , interpreters :: forall q. H.Slot q Void Unit
  , codeExplorer :: forall q. H.Slot q Void Unit
  , codeExploration :: forall q. H.Slot q Void String
  , wealthHealth :: forall q. H.Slot q Void Unit
  , codeAtlas :: forall q. H.Slot q Void Unit
  , fpFtw :: forall q. H.Slot q Void Unit
  , examplesGallery :: forall q. H.Slot q Void Unit
  , barChartExample :: forall q. H.Slot q Void Unit
  , threeLittleCirclesExample :: forall q. H.Slot q Void Unit
  , lineChartExample :: forall q. H.Slot q Void Unit
  , scatterPlotExample :: forall q. H.Slot q Void Unit
  , groupedBarChartExample :: forall q. H.Slot q Void Unit
  , multiLineChartExample :: forall q. H.Slot q Void Unit
  , bubbleChartExample :: forall q. H.Slot q Void Unit
  , chordDiagramExample :: forall q. H.Slot q Void Unit
  -- , verticalTreeExample :: forall q. H.Slot q Void Unit
  -- , horizontalTreeExample :: forall q. H.Slot q Void Unit
  -- , radialTreeExample :: forall q. H.Slot q Void Unit
  -- , treemapExample :: forall q. H.Slot q Void Unit
  -- , icicleExample :: forall q. H.Slot q Void Unit
  -- , lesMisForceExample :: forall q. H.Slot q Void Unit
  -- , topologicalSortExample :: forall q. H.Slot q Void Unit
  -- , sankeyDiagramExample :: forall q. H.Slot q Void Unit
  -- , mapQuartetExample :: forall q. H.Slot q Void Unit
  , threeCirclesTransitionExample :: forall q. H.Slot q Void Unit
  , generalUpdatePatternExample :: forall q. H.Slot q Void Unit
  -- , animatedRadialTreeExample :: forall q. H.Slot q Void Unit
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
_simpleCharts1 = Proxy :: Proxy "simpleCharts1"
_simpleCharts2 = Proxy :: Proxy "simpleCharts2"
_dataFlowViz = Proxy :: Proxy "dataFlowViz"
_movement = Proxy :: Proxy "movement"
_hierarchies = Proxy :: Proxy "hierarchies"
_interpreters = Proxy :: Proxy "interpreters"
_codeExplorer = Proxy :: Proxy "codeExplorer"
_codeExploration = Proxy :: Proxy "codeExploration"
_wealthHealth = Proxy :: Proxy "wealthHealth"
_codeAtlas = Proxy :: Proxy "codeAtlas"
_fpFtw = Proxy :: Proxy "fpFtw"
_examplesGallery = Proxy :: Proxy "examplesGallery"
_barChartExample = Proxy :: Proxy "barChartExample"
_threeLittleCirclesExample = Proxy :: Proxy "threeLittleCirclesExample"
_lineChartExample = Proxy :: Proxy "lineChartExample"
_scatterPlotExample = Proxy :: Proxy "scatterPlotExample"
_groupedBarChartExample = Proxy :: Proxy "groupedBarChartExample"
_multiLineChartExample = Proxy :: Proxy "multiLineChartExample"
_bubbleChartExample = Proxy :: Proxy "bubbleChartExample"
_chordDiagramExample = Proxy :: Proxy "chordDiagramExample"
-- _verticalTreeExample = Proxy :: Proxy "verticalTreeExample"
-- _horizontalTreeExample = Proxy :: Proxy "horizontalTreeExample"
-- _radialTreeExample = Proxy :: Proxy "radialTreeExample"
-- _treemapExample = Proxy :: Proxy "treemapExample"
-- _icicleExample = Proxy :: Proxy "icicleExample"
-- _lesMisForceExample = Proxy :: Proxy "lesMisForceExample"
-- _topologicalSortExample = Proxy :: Proxy "topologicalSortExample"
-- _sankeyDiagramExample = Proxy :: Proxy "sankeyDiagramExample"
-- _mapQuartetExample = Proxy :: Proxy "mapQuartetExample"
_threeCirclesTransitionExample = Proxy :: Proxy "threeCirclesTransitionExample"
_generalUpdatePatternExample = Proxy :: Proxy "generalUpdatePatternExample"
-- _animatedRadialTreeExample = Proxy :: Proxy "animatedRadialTreeExample"
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

  Interpreters ->
    HH.slot_ _interpreters unit Interpreters.component unit

  CodeExplorer ->
    HH.slot_ _codeExplorer unit CodeExplorer.component unit

  Explore snippetId ->
    HH.slot_ _codeExploration snippetId CodeExplorationPage.component snippetId

  WealthHealth ->
    HH.slot_ _wealthHealth unit WealthHealth.component unit

  CodeAtlas ->
    HH.slot_ _codeAtlas unit CodeAtlas.component unit

  FpFtw ->
    HH.slot_ _fpFtw unit FpFtw.component unit

  ExamplesGallery ->
    HH.slot_ _examplesGallery unit ExamplesGallery.component unit

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
renderExamplePage exampleId = case exampleId of
  "three-little-circles" ->
    HH.slot_ _threeLittleCirclesExample unit ThreeLittleCirclesExample.component unit
  "bar-chart" ->
    HH.slot_ _barChartExample unit BarChartExample.component unit
  "line-chart" ->
    HH.slot_ _lineChartExample unit LineChartExample.component unit
  "scatter-plot" ->
    HH.slot_ _scatterPlotExample unit ScatterPlotExample.component unit
  "grouped-bar-chart" ->
    HH.slot_ _groupedBarChartExample unit GroupedBarChartExample.component unit
  "multi-line-chart" ->
    HH.slot_ _multiLineChartExample unit MultiLineChartExample.component unit
  "bubble-chart" ->
    HH.slot_ _bubbleChartExample unit BubbleChartExample.component unit
  "chord-diagram" ->
    HH.slot_ _chordDiagramExample unit ChordDiagramExample.component unit
  "three-circles-transition" ->
    HH.slot_ _threeCirclesTransitionExample unit ThreeCirclesTransitionExample.component unit
  "general-update-pattern" ->
    HH.slot_ _generalUpdatePatternExample unit GeneralUpdatePatternExample.component unit
  -- TODO: Fix data loading for these examples
  -- "vertical-tree" -> HH.slot_ _verticalTreeExample unit VerticalTreeExample.component unit
  -- "horizontal-tree" -> HH.slot_ _horizontalTreeExample unit HorizontalTreeExample.component unit
  -- "radial-tree" -> HH.slot_ _radialTreeExample unit RadialTreeExample.component unit
  -- "treemap" -> HH.slot_ _treemapExample unit TreemapExample.component unit
  -- "icicle" -> HH.slot_ _icicleExample unit IcicleExample.component unit
  -- "lesmis-force" -> HH.slot_ _lesMisForceExample unit LesMisForceExample.component unit
  -- "topological-sort" -> HH.slot_ _topologicalSortExample unit TopologicalSortExample.component unit
  -- "sankey-diagram" -> HH.slot_ _sankeyDiagramExample unit SankeyDiagramExample.component unit
  -- "map-quartet" -> HH.slot_ _mapQuartetExample unit MapQuartetExample.component unit
  -- "animated-radial-tree" -> HH.slot_ _animatedRadialTreeExample unit AnimatedRadialTreeExample.component unit
  _ ->
    HH.div
      [ HP.classes [ HH.ClassName "not-found" ] ]
      [ HH.h1_ [ HH.text "Example Not Found" ]
      , HH.p_ [ HH.text $ "The example '" <> exampleId <> "' doesn't exist yet." ]
      , HH.a
          [ HP.href $ "#" <> routeToPath ExamplesGallery ]
          [ HH.text "← Back to Examples Gallery" ]
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
