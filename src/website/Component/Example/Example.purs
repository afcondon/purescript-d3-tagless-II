module PSD3.Component.Example where

import Prelude

import Control.Monad (void)

import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web as AJAX
import D3.Viz.AnimatedRadialTree as AnimatedRadialTree
import D3.Viz.BarChart as BarChart
import D3.Viz.BubbleChart as BubbleChart
import D3.Viz.Charts.Model as ChartModel
import D3.Viz.ChordDiagram as ChordDiagram
import D3.Viz.GroupedBarChart as GroupedBarChart
import D3.Viz.GUP as GUP
import D3.Viz.LineChart as LineChart
import D3.Viz.MultiLineChart as MultiLineChart
import D3.Viz.Parabola as Parabola
import D3.Viz.RadialStackedBar as RadialStackedBar
import D3.Viz.ScatterPlot as ScatterPlot
import D3.Viz.FpFtw.MapQuartet as MapQuartet
import D3.Viz.FpFtw.TopologicalSort as TopologicalSort
import D3.Viz.Hierarchies as Hierarchies
import D3.Viz.TreeViz as TreeViz
import D3.Viz.TreemapViz as TreemapViz
import D3.Viz.PackViz as PackViz
import D3.Viz.ClusterViz as ClusterViz
import D3.Viz.PartitionViz as PartitionViz
import D3.Viz.SunburstViz as SunburstViz
import D3.Viz.LesMiserables as LesMis
import D3.Viz.LesMiserablesGUP as LesMisGUP
import D3.Viz.LesMiserablesGUP.Model (LesMisRawModel)
import PSD3.Internal.Types as PSD3Types
import PSD3.Internal.FFI as PSD3FFI
import PSD3.Data.Node (D3_SimulationNode(..))
import PSD3.Capabilities.Selection as PSD3Selection
import PSD3.Capabilities.Simulation as PSD3Simulation
import Halogen.HTML.Events as HE
import D3.Viz.LesMiserables.File (readGraphFromFileContents)
import D3.Viz.SankeyDiagram as Sankey
import D3.Viz.ThreeLittleCircles as ThreeLittleCircles
import D3.Viz.ThreeLittleCirclesTransition as CirclesTransition
import D3.Viz.ThreeLittleDimensions as ThreeLittleDimensions
import D3.Viz.Tree.HorizontalTree as HorizontalTree
import D3.Viz.Tree.RadialTree as RadialTree
import D3.Viz.Tree.VerticalTree as VerticalTree
import D3.Viz.WealthHealth.Draw as WealthHealth
import Data.Array as Data.Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Data.Tree (TreeType(..))
import PSD3.Internal.FFI (linksForceName_)
import PSD3.Internal.Hierarchical (getTreeViaAJAX, readJSON_)
import PSD3.Internal.Sankey.Types (SankeyLayoutState_, initialSankeyLayoutState_)
import PSD3.Internal.Simulation.Config as F
import PSD3.Internal.Simulation.Forces (createForce, createLinkForce, initialize)
import PSD3.Internal.Simulation.Types (D3SimulationState_, ForceType(..), RegularForceType(..), allNodes, initialSimulationState)
import PSD3.Interpreter.D3 (eval_D3M, runWithD3_Sankey, runWithD3_Simulation)
import PSD3.RoutingDSL (routeToPath)
import PSD3.Website.Types (Route(..))
import Utility (getWindowWidthHeight)

-- | Component state varies by example type
type State =
  { exampleId :: String
  , simulation :: D3SimulationState_
  , sankeyLayout :: SankeyLayoutState_
  -- LesMisGUP-specific state for interactive controls
  , lesMisData :: Maybe LesMisRawModel
  , lesMisVisibleGroups :: Set.Set Int
  , lesMisActiveForces :: Set.Set String
  , lesMisNodesPinned :: Boolean  -- Track if nodes are pinned to grid
  }

data Action
  = Initialize
  | ToggleGroup Int           -- Toggle visibility of a character group
  | ToggleForce String        -- Toggle a force on/off
  | MoveToGrid                -- Transition nodes to grid layout
  | ReleaseFromGrid           -- Unpin nodes, return to force layout

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
      -- LesMisGUP initial state - all groups visible, all forces active, nodes not pinned
      , lesMisData: Nothing
      , lesMisVisibleGroups: Set.fromFoldable [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
      , lesMisActiveForces: Set.fromFoldable ["many body negative", "collision", "center", linksForceName_]
      , lesMisNodesPinned: false
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

      "tree-purescript" -> do
        result <- H.liftAff $ AJAX.get ResponseFormat.string "./data/flare.json"
        case result of
          Left err -> log "Tree (PureScript): Failed to load data"
          Right response -> do
            let blessed = readJSON_ response.body
            _ <- H.liftEffect $ eval_D3M $ TreeViz.draw blessed "#example-viz"
            pure unit

      "treemap" -> do
        result <- H.liftAff $ AJAX.get ResponseFormat.string "./data/flare.json"
        case result of
          Left err -> log "Treemap: Failed to load data"
          Right response -> do
            let blessed = readJSON_ response.body
            _ <- H.liftEffect $ eval_D3M $ TreemapViz.draw blessed "#example-viz"
            pure unit

      "pack-purescript" -> do
        result <- H.liftAff $ AJAX.get ResponseFormat.string "./data/flare.json"
        case result of
          Left err -> log "Pack (PureScript): Failed to load data"
          Right response -> do
            let blessed = readJSON_ response.body
            _ <- H.liftEffect $ eval_D3M $ PackViz.draw blessed "#example-viz"
            pure unit

      "cluster-purescript" -> do
        result <- H.liftAff $ AJAX.get ResponseFormat.string "./data/flare.json"
        case result of
          Left err -> log "Cluster (PureScript): Failed to load data"
          Right response -> do
            let blessed = readJSON_ response.body
            _ <- H.liftEffect $ eval_D3M $ ClusterViz.draw blessed "#example-viz"
            pure unit

      "icicle" -> do
        result <- H.liftAff $ AJAX.get ResponseFormat.string "./data/flare.json"
        case result of
          Left err -> log "Partition (Icicle): Failed to load data"
          Right response -> do
            let blessed = readJSON_ response.body
            _ <- H.liftEffect $ eval_D3M $ PartitionViz.draw blessed "#example-viz"
            pure unit

      "sunburst-purescript" -> do
        result <- H.liftAff $ AJAX.get ResponseFormat.string "./data/flare.json"
        case result of
          Left err -> log "Sunburst (PureScript): Failed to load data"
          Right response -> do
            let blessed = readJSON_ response.body
            _ <- H.liftEffect $ eval_D3M $ SunburstViz.draw blessed "#example-viz"
            pure unit

      "lesmis-force" -> do
        response <- H.liftAff $ AJAX.get ResponseFormat.string "./data/miserables.json"
        let graph = readGraphFromFileContents response
        let forcesArray = [ forces.manyBodyNeg, forces.collision, forces.center, forces.links ]
            activeForces = Set.fromFoldable ["many body negative", "collision", "center", linksForceName_]
        runWithD3_Simulation do
          LesMis.drawSimplified forcesArray activeForces graph "#example-viz"
        pure unit

      "lesmisgup" -> do
        response <- H.liftAff $ AJAX.get ResponseFormat.string "./data/miserables.json"
        let graph = readGraphFromFileContents response
        let forcesArray = [ forces.manyBodyNeg, forces.collision, forces.center, forces.links ]
            activeForces = Set.fromFoldable ["many body negative", "collision", "center", linksForceName_]

        -- Draw initial visualization
        runWithD3_Simulation do
          void $ LesMisGUP.drawSimplified forcesArray activeForces graph "#example-viz"

        -- Store model data (selections will be queried from DOM when needed)
        H.modify_ _ { lesMisData = Just graph }
        pure unit

      "topological-sort" -> do
        _ <- H.liftEffect $ eval_D3M $ TopologicalSort.drawTopologicalSort TopologicalSort.buildPipelineTasks "#example-viz"
        pure unit

      "sankey-diagram" -> do
        result <- H.liftAff $ AJAX.get ResponseFormat.string "./data/energy.csv"
        case result of
          Left err -> log "Sankey: Failed to load data"
          Right response -> do
            _ <- H.liftEffect $ eval_D3M $ Sankey.draw response.body "#example-viz"
            pure unit

      "map-quartet" -> do
        quartet <- H.liftEffect MapQuartet.generateMapQuartet
        _ <- H.liftEffect $ eval_D3M $ MapQuartet.drawMapQuartet quartet "#example-viz"
        pure unit

      "chord-diagram" -> do
        _ <- H.liftEffect $ eval_D3M $ ChordDiagram.draw ChordDiagram.exampleMatrix ChordDiagram.exampleLabels "#example-viz"
        pure unit

      "general-update-pattern" -> do
        updateFn <- H.liftEffect $ eval_D3M $ GUP.exGeneralUpdatePattern "#example-viz"
        _ <- H.liftEffect $ eval_D3M $ updateFn ['A', 'B', 'C', 'D', 'E']
        pure unit

      "three-circles-transition" -> do
        _ <- H.liftEffect $ eval_D3M $ CirclesTransition.drawThreeCirclesTransition "#example-viz"
        pure unit

      "parabola" -> do
        let parabolaData = [310, 474, 613, 726, 814, 877, 914, 926, 914, 877, 814, 726, 613, 474, 310]
        _ <- H.liftEffect $ eval_D3M $ Parabola.drawWithData parabolaData "#example-viz"
        pure unit

      "radial-stacked-bar" -> do
        _ <- H.liftEffect $ eval_D3M $ RadialStackedBar.draw ChartModel.groupedBarData "#example-viz"
        pure unit

      "nested-data" -> do
        _ <- H.liftEffect $ eval_D3M $ ThreeLittleDimensions.drawThreeDimensions "#example-viz"
        pure unit

      "working-with-sets" -> do
        _ <- H.liftEffect $ eval_D3M $ ThreeLittleDimensions.drawThreeDimensionsSets "#example-viz"
        pure unit

      "wealth-health" -> do
        -- TODO: This will need data loading and animation setup
        log "Wealth & Health example not yet implemented"
        pure unit

      _ -> log $ "Example: Unknown example ID: " <> state.exampleId

  ToggleGroup groupId -> do
    state <- H.get
    -- Toggle the group in/out of visible set
    let newVisibleGroups = if Set.member groupId state.lesMisVisibleGroups
                           then Set.delete groupId state.lesMisVisibleGroups
                           else Set.insert groupId state.lesMisVisibleGroups
    H.modify_ _ { lesMisVisibleGroups = newVisibleGroups }

    -- Call updateSimulation with DECLARATIVE API + re-heat
    case state.lesMisData of
      Just model -> do
        runWithD3_Simulation do
          -- Get selections from DOM (they were created by drawSimplified)
          root <- PSD3Selection.attach "#example-viz"
          nodesGroup <- PSD3Selection.selectUnder root ".zoom-group > .node"
          linksGroup <- PSD3Selection.selectUnder root ".zoom-group > .link"

          -- Stop simulation to re-heat it
          PSD3Simulation.stop

          -- DECLARATIVE API: Pass full datasets + predicate
          -- Library handles filtering AND automatic link filtering
          LesMisGUP.updateSimulation
            { nodes: Just nodesGroup, links: Just linksGroup }
            { allNodes: model.nodes                        -- FULL dataset!
            , allLinks: model.links                        -- FULL dataset!
            , nodeFilter: \(D3SimNode node) ->             -- Predicate (single source of truth)
                Set.member node.group newVisibleGroups
            , activeForces: state.lesMisActiveForces
            }

          -- Restart simulation (re-heats with full alpha)
          PSD3Simulation.start
      _ -> log "ToggleGroup: No model data available"

  ToggleForce forceLabel -> do
    state <- H.get
    -- Toggle the force in/out of active set
    let newActiveForces = if Set.member forceLabel state.lesMisActiveForces
                          then Set.delete forceLabel state.lesMisActiveForces
                          else Set.insert forceLabel state.lesMisActiveForces
    H.modify_ _ { lesMisActiveForces = newActiveForces }

    -- Call updateSimulation with DECLARATIVE API + re-heat
    case state.lesMisData of
      Just model -> do
        runWithD3_Simulation do
          -- Get selections from DOM (they were created by drawSimplified)
          root <- PSD3Selection.attach "#example-viz"
          nodesGroup <- PSD3Selection.selectUnder root ".zoom-group > .node"
          linksGroup <- PSD3Selection.selectUnder root ".zoom-group > .link"

          -- Stop simulation to re-heat it
          PSD3Simulation.stop

          -- DECLARATIVE API: Pass full datasets + predicate
          -- Library handles filtering AND automatic link filtering
          LesMisGUP.updateSimulation
            { nodes: Just nodesGroup, links: Just linksGroup }
            { allNodes: model.nodes                        -- FULL dataset!
            , allLinks: model.links                        -- FULL dataset!
            , nodeFilter: \(D3SimNode node) ->             -- Predicate (single source of truth)
                Set.member node.group state.lesMisVisibleGroups
            , activeForces: newActiveForces
            }

          -- Restart simulation (re-heats with full alpha)
          PSD3Simulation.start
      _ -> log "ToggleForce: No model data available"

  MoveToGrid -> do
    state <- H.get
    case state.lesMisData of
      Just model -> do
        (Tuple w h) <- liftEffect getWindowWidthHeight

        runWithD3_Simulation do
          -- Get selections from DOM
          root <- PSD3Selection.attach "#example-viz"
          nodesGroup <- PSD3Selection.selectUnder root ".zoom-group > .node"
          linksGroup <- PSD3Selection.selectUnder root ".zoom-group > .link"

          -- Stop simulation during transition (so tick doesn't fight the animation)
          PSD3Simulation.stop

          -- Calculate grid positions (60px spacing, centered on window)
          let gridNodes = LesMisGUP.nodesToGridLayout model.nodes 60.0 w

          -- Start D3 transition (animates nodes smoothly to grid over 1500ms)
          -- Key: simulation is STOPPED so tick function doesn't interfere
          liftEffect $ LesMisGUP.transitionNodesToGridPositions_
            ".lesmis"                    -- SVG class
            ".zoom-group > .node circle" -- Node selector
            ".zoom-group > .link line"   -- Link selector
            gridNodes
            (log "Grid transition complete - nodes now at grid positions")

          -- Update simulation data with pinned nodes (but DON'T start simulation yet!)
          -- This sets fx/fy in the internal simulation data so nodes are "ready to be pinned"
          -- But since simulation is stopped, tick function won't override the transition
          LesMisGUP.updateSimulation
            { nodes: Just nodesGroup, links: Just linksGroup }
            { allNodes: gridNodes                      -- Nodes with fx/fy set
            , allLinks: model.links
            , nodeFilter: \(D3SimNode node) ->         -- Still apply group filter
                Set.member node.group state.lesMisVisibleGroups
            , activeForces: state.lesMisActiveForces
            }

          -- DON'T restart simulation yet - let the D3 transition complete first!
          -- After transition finishes (1500ms), nodes will be visually at grid positions
          -- Mark nodes as pinned in state
          H.modify_ _ { lesMisNodesPinned = true }
      _ -> log "MoveToGrid: No model data available"

  ReleaseFromGrid -> do
    state <- H.get
    case state.lesMisData of
      Just model -> do
        runWithD3_Simulation do
          -- Get selections from DOM
          root <- PSD3Selection.attach "#example-viz"
          nodesGroup <- PSD3Selection.selectUnder root ".zoom-group > .node"
          linksGroup <- PSD3Selection.selectUnder root ".zoom-group > .link"

          -- Unpin all nodes (set fx/fy to null)
          let unpinnedNodes = LesMisGUP.unpinAllNodes model.nodes

          -- Update simulation with unpinned nodes
          LesMisGUP.updateSimulation
            { nodes: Just nodesGroup, links: Just linksGroup }
            { allNodes: unpinnedNodes                  -- Nodes with fx/fy = null
            , allLinks: model.links
            , nodeFilter: \(D3SimNode node) ->
                Set.member node.group state.lesMisVisibleGroups
            , activeForces: state.lesMisActiveForces
            }

          -- Restart simulation with full alpha (re-heat)
          PSD3Simulation.stop
          PSD3Simulation.start

          -- Mark nodes as unpinned in state
          H.modify_ _ { lesMisNodesPinned = false }
      _ -> log "ReleaseFromGrid: No model data available"

-- | Example metadata
type ExampleMeta =
  { id :: String
  , name :: String
  , description :: String
  , category :: String
  }

-- | All example IDs in order
allExampleIds :: Array String
allExampleIds =
  [ "three-little-circles"
  , "bar-chart"
  , "line-chart"
  , "scatter-plot"
  , "grouped-bar-chart"
  , "multi-line-chart"
  , "radial-stacked-bar"
  , "parabola"
  , "bubble-chart"
  , "vertical-tree"
  , "horizontal-tree"
  , "radial-tree"
  , "animated-radial-tree"
  , "tree-purescript"
  , "treemap"
  , "pack-purescript"
  , "cluster-purescript"
  , "icicle"
  , "sunburst-purescript"
  , "lesmis-force"
  , "lesmisgup"
  , "topological-sort"
  , "chord-diagram"
  , "sankey-diagram"
  , "map-quartet"
  , "nested-data"
  , "working-with-sets"
  , "wealth-health"
  , "general-update-pattern"
  , "three-circles-transition"
  ]

-- | Get next example ID
getNextExampleId :: String -> Maybe String
getNextExampleId currentId =
  case Data.Array.findIndex (\id -> id == currentId) allExampleIds of
    Nothing -> Nothing
    Just idx -> allExampleIds Data.Array.!! (idx + 1)

-- | Get previous example ID
getPrevExampleId :: String -> Maybe String
getPrevExampleId currentId =
  case Data.Array.findIndex (\id -> id == currentId) allExampleIds of
    Nothing -> Nothing
    Just idx -> if idx > 0 then allExampleIds Data.Array.!! (idx - 1) else Nothing

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
  "radial-stacked-bar" -> Just
    { id, name: "Radial Stacked Bar Chart", description: "Population by age and state in radial form", category: "Simple Charts" }
  "parabola" -> Just
    { id, name: "Parabola", description: "Colored circles in parabolic formation", category: "Simple Charts" }
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
  "tree-purescript" -> Just
    { id, name: "Tree (Pure PureScript)", description: "Node-link tree layout using pure PureScript implementation (Reingold-Tilford algorithm)", category: "Hierarchies" }
  "treemap" -> Just
    { id, name: "Treemap (Pure PureScript)", description: "Space-filling hierarchical visualization using squarified tiling algorithm in pure PureScript", category: "Hierarchies" }
  "pack-purescript" -> Just
    { id, name: "Pack (Pure PureScript)", description: "Circle packing layout with iterative relaxation algorithm in pure PureScript", category: "Hierarchies" }
  "cluster-purescript" -> Just
    { id, name: "Cluster (Pure PureScript)", description: "Dendrogram with equal leaf depth using pure PureScript implementation", category: "Hierarchies" }
  "icicle" -> Just
    { id, name: "Partition/Icicle (Pure PureScript)", description: "Rectangular partition layout (icicle chart) with equal layer height in pure PureScript", category: "Hierarchies" }
  "sunburst-purescript" -> Just
    { id, name: "Sunburst (Pure PureScript)", description: "Radial partition layout with nested rings in pure PureScript", category: "Hierarchies" }
  "lesmis-force" -> Just
    { id, name: "Les Misérables Network", description: "Character co-occurrence force-directed graph with physics simulation", category: "Force-Directed" }
  "lesmisgup" -> Just
    { id, name: "Les Misérables GUP", description: "Les Misérables network with General Update Pattern - filter groups dynamically", category: "Force-Directed" }
  "topological-sort" -> Just
    { id, name: "Topological Sort Layers", description: "Force layout with layer constraints for dependency graphs", category: "Force-Directed" }
  "sankey-diagram" -> Just
    { id, name: "Sankey Diagram", description: "Energy flow visualization showing sources, transformations, and destinations", category: "Data Flow" }
  "map-quartet" -> Just
    { id, name: "Map Quartet", description: "Four scatter plots demonstrating sparse data with PureScript Maps (15 points out of 200 possible x-values)", category: "Rich Data Structures" }
  "nested-data" -> Just
    { id, name: "Nested Data", description: "Nested selections with 2D arrays", category: "Rich Data Structures" }
  "working-with-sets" -> Just
    { id, name: "Working with Sets", description: "Nested selections with Set data structures", category: "Rich Data Structures" }
  "wealth-health" -> Just
    { id, name: "Wealth & Health of Nations", description: "Animated scatterplot across time", category: "Animations" }
  "chord-diagram" -> Just
    { id, name: "Chord Diagram", description: "Circular flow diagram showing relationships between programming concepts", category: "Data Flow" }
  "general-update-pattern" -> Just
    { id, name: "General Update Pattern", description: "Enter, update, exit pattern visualization with animated letters", category: "Transitions" }
  "three-circles-transition" -> Just
    { id, name: "Color Mixing Transition", description: "RGB color mixing with smooth transitions demonstrating additive color", category: "Transitions" }
  _ -> Nothing

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  case getExampleMeta state.exampleId of
    Nothing ->
      HH.div
        [ HP.classes [ HH.ClassName "example-page" ] ]
        [ renderHeader state.exampleId Nothing
        , HH.main
            [ HP.classes [ HH.ClassName "example-content" ] ]
            [ HH.h1_ [ HH.text "Example Not Found" ]
            , HH.p_ [ HH.text $ "No example found with ID: " <> state.exampleId ]
            ]
        ]

    Just meta ->
      HH.div
        [ HP.classes [ HH.ClassName "example-page" ] ]
        [ renderHeader state.exampleId (Just meta)
        , HH.main
            [ HP.classes [ HH.ClassName "example-content" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "example-viz-panel" ] ]
                [ renderLesMisControls state  -- Controls for lesmisgup
                , HH.div
                    [ HP.id "example-viz"
                    , HP.classes [ HH.ClassName "example-viz" ]
                    ]
                    []
                ]
            , HH.div
                [ HP.classes [ HH.ClassName "example-code-panel" ] ]
                [ HH.h2
                    [ HP.classes [ HH.ClassName "code-panel-title" ] ]
                    [ HH.text "Source Code" ]
                , HH.pre
                    [ HP.classes [ HH.ClassName "code-block" ] ]
                    [ HH.code
                        [ HP.classes [ HH.ClassName "language-haskell" ] ]
                        [ HH.text $ getExampleCode state.exampleId ]
                    ]
                ]
            ]
        ]

-- | Render interactive controls for LesMisGUP example
renderLesMisControls :: forall m. State -> H.ComponentHTML Action () m
renderLesMisControls state =
  if state.exampleId == "lesmisgup"
  then
    HH.div
      [ HP.classes [ HH.ClassName "lesmis-controls" ] ]
      [ HH.div
          [ HP.classes [ HH.ClassName "control-section" ] ]
          [ HH.h3_ [ HH.text "Character Groups" ]
          , HH.div
              [ HP.classes [ HH.ClassName "button-group" ] ]
              (Data.Array.range 0 10 <#> \groupId ->
                let isActive = Set.member groupId state.lesMisVisibleGroups
                in HH.button
                  [ HP.classes [ HH.ClassName if isActive then "active" else "inactive" ]
                  , HE.onClick \_ -> ToggleGroup groupId
                  ]
                  [ HH.text $ "Group " <> show groupId ]
              )
          ]
      , HH.div
          [ HP.classes [ HH.ClassName "control-section" ] ]
          [ HH.h3_ [ HH.text "Forces" ]
          , HH.div
              [ HP.classes [ HH.ClassName "button-group" ] ]
              [ renderForceButton "many body negative" state.lesMisActiveForces
              , renderForceButton "collision" state.lesMisActiveForces
              , renderForceButton "center" state.lesMisActiveForces
              , renderForceButton linksForceName_ state.lesMisActiveForces
              ]
          ]
      , HH.div
          [ HP.classes [ HH.ClassName "control-section" ] ]
          [ HH.h3_ [ HH.text "Layout" ]
          , HH.div
              [ HP.classes [ HH.ClassName "button-group" ] ]
              [ if state.lesMisNodesPinned
                then
                  HH.button
                    [ HP.classes [ HH.ClassName "layout-button" ]
                    , HE.onClick \_ -> ReleaseFromGrid
                    ]
                    [ HH.text "Release from Grid" ]
                else
                  HH.button
                    [ HP.classes [ HH.ClassName "layout-button" ]
                    , HE.onClick \_ -> MoveToGrid
                    ]
                    [ HH.text "Move to Grid" ]
              ]
          ]
      ]
  else
    HH.text ""  -- No controls for other examples

renderForceButton :: forall m. String -> Set.Set String -> H.ComponentHTML Action () m
renderForceButton forceLabel activeForces =
  let isActive = Set.member forceLabel activeForces
  in HH.button
    [ HP.classes [ HH.ClassName if isActive then "active" else "inactive" ]
    , HE.onClick \_ -> ToggleForce forceLabel
    ]
    [ HH.text forceLabel ]

-- | Render the header with logo, navigation, and prev/next buttons
renderHeader :: forall w i. String -> Maybe ExampleMeta -> HH.HTML w i
renderHeader currentId maybeMeta =
  HH.header
    [ HP.classes [ HH.ClassName "example-header" ] ]
    [ HH.div
        [ HP.classes [ HH.ClassName "example-header-left" ] ]
        [ HH.a
            [ HP.href $ "#" <> routeToPath Home
            , HP.classes [ HH.ClassName "example-logo-link" ]
            ]
            [ HH.img
                [ HP.src "assets/psd3-logo-color.svg"
                , HP.alt "PSD3 Logo"
                , HP.classes [ HH.ClassName "example-logo" ]
                ]
            ]
        , HH.a
            [ HP.href $ "#" <> routeToPath ExamplesGallery
            , HP.classes [ HH.ClassName "example-gallery-link" ]
            ]
            [ HH.text "Examples" ]
        , case maybeMeta of
            Just meta ->
              HH.div
                [ HP.classes [ HH.ClassName "example-title-container" ] ]
                [ HH.h1
                    [ HP.classes [ HH.ClassName "example-title" ] ]
                    [ HH.text meta.name ]
                ]
            Nothing -> HH.text ""
        ]
    , HH.div
        [ HP.classes [ HH.ClassName "example-header-right" ] ]
        [ case getPrevExampleId currentId of
            Nothing ->
              HH.span
                [ HP.classes [ HH.ClassName "example-nav-button", HH.ClassName "disabled" ] ]
                [ HH.text "← Previous" ]
            Just prevId ->
              HH.a
                [ HP.href $ "#" <> routeToPath (Example prevId)
                , HP.classes [ HH.ClassName "example-nav-button" ]
                ]
                [ HH.text "← Previous" ]
        , case getNextExampleId currentId of
            Nothing ->
              HH.span
                [ HP.classes [ HH.ClassName "example-nav-button", HH.ClassName "disabled" ] ]
                [ HH.text "Next →" ]
            Just nextId ->
              HH.a
                [ HP.href $ "#" <> routeToPath (Example nextId)
                , HP.classes [ HH.ClassName "example-nav-button" ]
                ]
                [ HH.text "Next →" ]
        ]
    ]

-- | Get source code for an example (placeholder for now)
getExampleCode :: String -> String
getExampleCode exampleId =
  "-- Source code for " <> exampleId <> "\n" <>
  "-- This will be populated with actual code snippets\n" <>
  "\n" <>
  "draw :: forall m.\n" <>
  "  Bind m =>\n" <>
  "  MonadEffect m =>\n" <>
  "  SelectionM D3Selection_ m =>\n" <>
  "  Selector D3Selection_ -> m Unit\n" <>
  "draw selector = do\n" <>
  "  svg <- attach selector >>= appendTo _ Svg []\n" <>
  "  -- visualization code here\n" <>
  "  pure unit"
