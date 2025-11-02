module PSD3.Component.Example where

import Prelude

import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web as AJAX
import D3.Viz.AnimatedRadialTree as AnimatedRadialTree
import D3.Viz.BarChart as BarChart
import D3.Viz.BubbleChart as BubbleChart
import D3.Viz.Charts.Model as ChartModel
import D3.Viz.GroupedBarChart as GroupedBarChart
import D3.Viz.LineChart as LineChart
import D3.Viz.MultiLineChart as MultiLineChart
import D3.Viz.ScatterPlot as ScatterPlot
import D3.Viz.FpFtw.MapQuartet as MapQuartet
import D3.Viz.FpFtw.TopologicalSort as TopologicalSort
import D3.Viz.Hierarchies as Hierarchies
import D3.Viz.LesMiserables as LesMis
import D3.Viz.LesMiserables.File (readGraphFromFileContents)
import D3.Viz.Sankey.Model (energyData)
import D3.Viz.SankeyDiagram as Sankey
import D3.Viz.ThreeLittleCircles as ThreeLittleCircles
import D3.Viz.Tree.HorizontalTree as HorizontalTree
import D3.Viz.Tree.RadialTree as RadialTree
import D3.Viz.Tree.VerticalTree as VerticalTree
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Data.Tree (TreeType(..))
import PSD3.Internal.FFI (linksForceName_)
import PSD3.Internal.Hierarchical (getTreeViaAJAX)
import PSD3.Internal.Sankey.Types (SankeyLayoutState_, initialSankeyLayoutState_)
import PSD3.Internal.Simulation.Config as F
import PSD3.Internal.Simulation.Forces (createForce, createLinkForce, initialize)
import PSD3.Internal.Simulation.Types (D3SimulationState_, ForceType(..), RegularForceType(..), allNodes, initialSimulationState)
import PSD3.Interpreter.D3 (eval_D3M, runWithD3_Sankey, runWithD3_Simulation)
import PSD3.RoutingDSL (routeToPath)
import PSD3.Website.Types (Route(..))

-- | Component state varies by example type
type State =
  { exampleId :: String
  , simulation :: D3SimulationState_
  , sankeyLayout :: SankeyLayoutState_
  }

data Action = Initialize

forces = {
    manyBodyNeg: createForce "many body negative" (RegularForce ForceManyBody) allNodes [ F.strength (-40.0) ]
  , collision:   createForce "collision"          (RegularForce ForceCollide)  allNodes [ F.radius 4.0 ]
  , center:      createForce "center"             (RegularForce ForceCenter)   allNodes [ F.x 0.0, F.y 0.0, F.strength 1.0 ]
  , links:       createLinkForce Nothing []
}

component :: forall q o m. MonadAff m => H.Component q String o m
component = H.mkComponent
  { initialState: \exampleId ->
      { exampleId
      , simulation: initialSimulationState (initialize [ forces.manyBodyNeg, forces.collision, forces.center, forces.links ])
      , sankeyLayout: initialSankeyLayoutState_
      }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    state <- H.get
    log $ "Example: Initializing " <> state.exampleId
    case state.exampleId of
      "three-little-circles" -> do
        _ <- H.liftEffect $ eval_D3M $ ThreeLittleCircles.drawThreeCircles "#example-viz"
        pure unit

      "bar-chart" -> do
        _ <- H.liftEffect $ eval_D3M $ BarChart.draw ChartModel.monthlySales "#example-viz"
        pure unit

      "line-chart" -> do
        _ <- H.liftEffect $ eval_D3M $ LineChart.draw ChartModel.sineWaveData "#example-viz"
        pure unit

      "scatter-plot" -> do
        _ <- H.liftEffect $ eval_D3M $ ScatterPlot.drawQuartet ChartModel.anscombesQuartet "#example-viz"
        pure unit

      "grouped-bar-chart" -> do
        _ <- H.liftEffect $ eval_D3M $ GroupedBarChart.draw ChartModel.groupedBarData "#example-viz"
        pure unit

      "multi-line-chart" -> do
        _ <- H.liftEffect $ eval_D3M $ MultiLineChart.draw ChartModel.multiLineData "#example-viz"
        pure unit

      "bubble-chart" -> do
        result <- H.liftAff $ getTreeViaAJAX "./data/flare-2.json"
        case result of
          Left err -> log "BubbleChart: Failed to load data"
          Right treeData -> do
            _ <- H.liftEffect $ eval_D3M $ BubbleChart.draw treeData "#example-viz"
            pure unit

      "vertical-tree" -> do
        result <- H.liftAff $ getTreeViaAJAX "./data/flare-2.json"
        case result of
          Left err -> log "VerticalTree: Failed to load data"
          Right treeData -> do
            _ <- H.liftEffect $ eval_D3M $ VerticalTree.drawVerticalTree TidyTree treeData "#example-viz"
            pure unit

      "horizontal-tree" -> do
        result <- H.liftAff $ getTreeViaAJAX "./data/flare-2.json"
        case result of
          Left err -> log "HorizontalTree: Failed to load data"
          Right treeData -> do
            _ <- H.liftEffect $ eval_D3M $ HorizontalTree.drawHorizontalTree TidyTree treeData "#example-viz"
            pure unit

      "radial-tree" -> do
        result <- H.liftAff $ getTreeViaAJAX "./data/flare-2.json"
        case result of
          Left err -> log "RadialTree: Failed to load data"
          Right treeData -> do
            _ <- H.liftEffect $ eval_D3M $ RadialTree.drawRadialTree TidyTree treeData "#example-viz"
            pure unit

      "animated-radial-tree" -> do
        result <- H.liftAff $ getTreeViaAJAX "./data/flare-2.json"
        case result of
          Left err -> log "AnimatedRadialTree: Failed to load data"
          Right treeData -> do
            _ <- H.liftEffect $ eval_D3M $ AnimatedRadialTree.drawAnimatedRadialTree TidyTree treeData "#example-viz"
            pure unit

      "treemap" -> do
        result <- H.liftAff $ getTreeViaAJAX "./data/flare-2.json"
        case result of
          Left err -> log "Treemap: Failed to load data"
          Right treeData -> do
            _ <- H.liftAff $ Hierarchies.drawTreemap treeData "#example-viz"
            pure unit

      "icicle" -> do
        result <- H.liftAff $ getTreeViaAJAX "./data/flare-2.json"
        case result of
          Left err -> log "Icicle: Failed to load data"
          Right treeData -> do
            _ <- H.liftAff $ Hierarchies.drawIcicle treeData "#example-viz"
            pure unit

      "lesmis-force" -> do
        response <- H.liftAff $ AJAX.get ResponseFormat.string "./data/miserables.json"
        let graph = readGraphFromFileContents response
        let forcesArray = [ forces.manyBodyNeg, forces.collision, forces.center, forces.links ]
            activeForces = Set.fromFoldable ["many body negative", "collision", "center", linksForceName_]
        runWithD3_Simulation do
          LesMis.drawSimplified forcesArray activeForces graph "#example-viz"
        pure unit

      "topological-sort" -> do
        _ <- H.liftEffect $ eval_D3M $ TopologicalSort.drawTopologicalSort TopologicalSort.buildPipelineTasks "#example-viz"
        pure unit

      "sankey-diagram" -> do
        runWithD3_Sankey do
          Sankey.draw energyData "#example-viz"
        pure unit

      "map-quartet" -> do
        quartet <- H.liftEffect MapQuartet.generateMapQuartet
        _ <- H.liftEffect $ eval_D3M $ MapQuartet.drawMapQuartet quartet "#example-viz"
        pure unit

      _ -> log $ "Example: Unknown example ID: " <> state.exampleId

-- | Example metadata
type ExampleMeta =
  { id :: String
  , name :: String
  , description :: String
  , category :: String
  }

-- | Lookup example metadata by ID
getExampleMeta :: String -> Maybe ExampleMeta
getExampleMeta id = case id of
  "three-little-circles" -> Just
    { id, name: "Three Little Circles", description: "The simplest possible D3 example - three circles", category: "Simple Charts" }
  "bar-chart" -> Just
    { id, name: "Bar Chart", description: "Basic bar chart with axis and labels", category: "Simple Charts" }
  "line-chart" -> Just
    { id, name: "Line Chart", description: "Line chart with time series data", category: "Simple Charts" }
  "scatter-plot" -> Just
    { id, name: "Scatter Plot", description: "Scatter plot with Anscombe's Quartet", category: "Simple Charts" }
  "grouped-bar-chart" -> Just
    { id, name: "Grouped Bar Chart", description: "Bar chart with grouped categories", category: "Simple Charts" }
  "multi-line-chart" -> Just
    { id, name: "Multi-Line Chart", description: "Multiple line series on one chart", category: "Simple Charts" }
  "bubble-chart" -> Just
    { id, name: "Bubble Chart", description: "Scatter plot with sized bubbles representing a third dimension of data", category: "Simple Charts" }
  "vertical-tree" -> Just
    { id, name: "Vertical Tree", description: "Top-down hierarchical tree layout", category: "Hierarchies" }
  "horizontal-tree" -> Just
    { id, name: "Horizontal Tree", description: "Left-to-right hierarchical tree layout", category: "Hierarchies" }
  "radial-tree" -> Just
    { id, name: "Radial Tree", description: "Circular hierarchical tree layout", category: "Hierarchies" }
  "animated-radial-tree" -> Just
    { id, name: "Animated Radial Tree", description: "Transitions between tree layouts with smooth animations", category: "Transitions" }
  "treemap" -> Just
    { id, name: "Treemap", description: "Space-filling hierarchical visualization using nested rectangles", category: "Hierarchies" }
  "icicle" -> Just
    { id, name: "Icicle Chart", description: "Hierarchical icicle/partition layout visualization", category: "Hierarchies" }
  "lesmis-force" -> Just
    { id, name: "Les Misérables Network", description: "Character co-occurrence force-directed graph with physics simulation", category: "Force-Directed" }
  "topological-sort" -> Just
    { id, name: "Topological Sort Layers", description: "Force layout with layer constraints for dependency graphs", category: "Force-Directed" }
  "sankey-diagram" -> Just
    { id, name: "Sankey Diagram", description: "Energy flow visualization showing sources, transformations, and destinations", category: "Data Flow" }
  "map-quartet" -> Just
    { id, name: "Map Quartet", description: "Four scatter plots demonstrating sparse data with PureScript Maps (15 points out of 200 possible x-values)", category: "Rich Data Structures" }
  _ -> Nothing

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  case getExampleMeta state.exampleId of
    Nothing ->
      HH.div
        [ HP.classes [ HH.ClassName "example-page" ] ]
        [ HH.header
            [ HP.classes [ HH.ClassName "example-header" ] ]
            [ HH.a
                [ HP.href $ "#" <> routeToPath ExamplesGallery ]
                [ HH.text "← Examples Gallery" ]
            , HH.h1_ [ HH.text "Example Not Found" ]
            , HH.p_ [ HH.text $ "No example found with ID: " <> state.exampleId ]
            ]
        ]

    Just meta ->
      HH.div
        [ HP.classes [ HH.ClassName "example-page" ] ]
        [ HH.header
            [ HP.classes [ HH.ClassName "example-header" ] ]
            [ HH.a
                [ HP.href $ "#" <> routeToPath ExamplesGallery ]
                [ HH.text "← Examples Gallery" ]
            , HH.h1_ [ HH.text meta.name ]
            , HH.p_ [ HH.text meta.description ]
            ]
        , HH.section
            [ HP.classes [ HH.ClassName "example-viz-section" ] ]
            [ HH.h2_ [ HH.text "Visualization" ]
            , HH.div
                [ HP.id "example-viz"
                , HP.classes [ HH.ClassName "example-viz" ]
                ]
                []
            ]
        ]
