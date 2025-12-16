module Component.Example where

import Prelude

import CodeSnippets (getSnippet)
import Control.Monad.Rec.Class (forever)
import Data.Array (catMaybes)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as SCU
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff)
import PSD3.Shared.DataLoader (simpleLoadText)
import Effect.Class (liftEffect)
import Effect.Random (random)
import PSD3.Internal.Selection.Operations (clear) as Ops
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.PrismJS as Prism
import PSD3.RoutingDSL (routeToPath)
import PSD3.Shared.SiteNav as SiteNav
import PSD3.Shared.Utilities (syntaxHighlightedCode)
import PSD3.Website.Types (Route(..))

-- Import TreeAPI examples
import D3.Viz.TreeAPI.ThreeLittleCircles as ThreeLittleCircles
import D3.Viz.TreeAPI.ThreeLittleCirclesTransition as ThreeLittleCirclesTransition
import D3.Viz.TreeAPI.SimpleTreeExample as SimpleTreeExample
import D3.Viz.TreeAPI.NestedElementsExample as NestedElementsExample
import D3.Viz.TreeAPI.BarChartExample as BarChartExample
import D3.Viz.TreeAPI.ScatterPlotExample as ScatterPlotExample
import D3.Viz.TreeAPI.LineChartExample as LineChartExample
import D3.Viz.TreeAPI.TreeViz as TreeViz
import D3.Viz.TreeAPI.ThreeLittleDimensionsExample as ThreeLittleDimensionsExample
import D3.Viz.TreeAPI.GroupedBarChartExample as GroupedBarChartExample
import D3.Viz.TreeAPI.V3ParabolaDemo as V3ParabolaDemo
import D3.Viz.TreeAPI.V3TransitionDemo as V3TransitionDemo
import D3.Viz.TreeAPI.V3GUPDemo as V3GUPDemo
-- Additional hierarchy visualizations
import D3.Viz.TreeAPI.RadialTreeViz as RadialTreeViz
import D3.Viz.TreeAPI.HorizontalTreeViz as HorizontalTreeViz
import D3.Viz.TreeAPI.ClusterViz as ClusterViz
import D3.Viz.TreeAPI.TreemapViz as TreemapViz
import D3.Viz.TreeAPI.SunburstViz as SunburstViz
import D3.Viz.TreeAPI.PackViz as PackViz
import D3.Viz.TreeAPI.PartitionViz as PartitionViz
-- Relational diagrams
import D3.Viz.TreeAPI.SankeyDiagram as SankeyDiagram
import D3.Viz.TreeAPI.ChordDiagram as ChordDiagram
import D3.Viz.TreeAPI.EdgeBundleViz as EdgeBundleViz
-- More charts
import D3.Viz.TreeAPI.MultiLineChartExample as MultiLineChartExample
import D3.Viz.TreeAPI.RadialStackedBarExample as RadialStackedBarExample
import D3.Viz.TreeAPI.AnscombesQuartet as AnscombesQuartet
-- Force-directed
import D3.Viz.SimpleForceGraph as SimpleForceGraph

-- | Component state
type State =
  { exampleId :: String
  , gupFiber :: Maybe H.ForkId
  }

data Action
  = Initialize
  | Receive String

-- | Example page component
component :: forall q o m. MonadAff m => H.Component q String o m
component = H.mkComponent
  { initialState: \exampleId -> { exampleId, gupFiber: Nothing }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      , receive = Just <<< Receive
      }
  }

-- | Clear the viz container before rendering new content
clearVizContainer :: forall o m. MonadAff m => H.HalogenM State Action () o m Unit
clearVizContainer = Ops.clear "#viz"

-- | Kill the GUP fiber if running
killGupFiber :: forall o m. MonadAff m => H.HalogenM State Action () o m Unit
killGupFiber = do
  state <- H.get
  case state.gupFiber of
    Just fid -> H.kill fid
    Nothing -> pure unit
  H.modify_ _ { gupFiber = Nothing }

-- | Generate random letters for GUP demo
-- | Returns a sorted subset of the alphabet with no duplicates
-- | Each letter has ~40% chance of being selected
getRandomLetters :: Effect String
getRandomLetters = do
  let
    alphabet = SCU.toCharArray "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    coinToss :: Char -> Effect (Maybe Char)
    coinToss c = do
      n <- random
      pure $ if n > 0.6 then Just c else Nothing

  choices <- sequence $ coinToss <$> alphabet
  pure $ SCU.fromCharArray $ catMaybes choices

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    -- Kill any existing GUP fiber first
    killGupFiber
    -- Clear any existing visualization
    clearVizContainer
    state <- H.get
    case state.exampleId of
      "three-little-circles" ->
        liftEffect ThreeLittleCircles.threeLittleCircles
      "three-circles-transition" ->
        liftEffect $ void $ ThreeLittleCirclesTransition.threeLittleCirclesTransition "#viz"
      "simple-tree" ->
        liftEffect SimpleTreeExample.testSimpleTree
      "nested-elements" ->
        liftEffect NestedElementsExample.testNestedElements
      "bar-chart" ->
        liftEffect BarChartExample.barChart
      "scatter-plot" ->
        liftEffect ScatterPlotExample.scatterPlot
      "line-chart" ->
        liftEffect LineChartExample.lineChart
      "grouped-bar-chart" ->
        liftEffect $ GroupedBarChartExample.groupedBarChart "#viz"
      "simple-hierarchy" ->
        liftEffect $ TreeViz.treeViz "#viz"
      "three-little-dimensions" ->
        liftEffect ThreeLittleDimensionsExample.threeLittleDimensions
      "v3-parabola" ->
        liftEffect V3ParabolaDemo.v3ParabolaDemo
      "v3-transition" ->
        liftEffect V3TransitionDemo.v3TransitionDemo
      "v3-gup" -> do
        -- Initial render
        liftEffect V3GUPDemo.v3GUPDemo
        -- Fork the update loop
        forkId <- H.fork $ forever do
          H.liftAff $ delay (Milliseconds 2500.0)
          letters <- liftEffect getRandomLetters
          liftEffect $ V3GUPDemo.updateWithLetters letters
        H.modify_ _ { gupFiber = Just forkId }
      -- Additional charts
      "multi-line-chart" ->
        liftEffect $ MultiLineChartExample.multiLineChart "#viz"
      "radial-stacked-bar" ->
        liftEffect $ RadialStackedBarExample.radialStackedBar "#viz"
      "anscombes-quartet" ->
        liftEffect $ AnscombesQuartet.anscombesQuartet "#viz"
      -- Hierarchies
      "radial-tree" ->
        liftEffect $ RadialTreeViz.radialTreeViz "#viz"
      "horizontal-tree" ->
        liftEffect $ HorizontalTreeViz.horizontalTreeViz "#viz"
      "cluster" ->
        liftEffect $ ClusterViz.clusterViz "#viz"
      "treemap" ->
        liftEffect $ TreemapViz.treemapViz "#viz"
      "sunburst" ->
        liftEffect $ SunburstViz.sunburstViz "#viz"
      "pack" ->
        liftEffect $ PackViz.packViz "#viz"
      "partition" ->
        liftEffect $ PartitionViz.partitionViz "#viz"
      -- Relational
      "sankey" -> do
        csvText <- H.liftAff $ simpleLoadText "./data/energy.csv"
        liftEffect $ SankeyDiagram.startSankey csvText "#viz"
      "chord" ->
        liftEffect $ ChordDiagram.startChord "#viz"
      "edge-bundle" ->
        liftEffect $ EdgeBundleViz.edgeBundleViz "#viz"
      -- Force-directed
      "simple-force" ->
        liftEffect $ void $ SimpleForceGraph.simpleForceGraph "#viz"
      _ -> pure unit
    -- Highlight code blocks with Prism
    liftEffect Prism.highlightAll

  Receive newExampleId -> do
    state <- H.get
    -- Only re-initialize if the example ID has changed
    when (state.exampleId /= newExampleId) do
      -- Kill GUP fiber when navigating away
      killGupFiber
      H.modify_ _ { exampleId = newExampleId }
      handleAction Initialize

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
  -- Basic examples
  "three-little-circles" -> Just
    { id, name: "Three Little Circles", description: "Data join basics - bind data to DOM elements", category: "Basic Examples" }
  "three-circles-transition" -> Just
    { id, name: "Circles Transition", description: "Animated transitions with easing and delays", category: "Basic Examples" }
  "simple-tree" -> Just
    { id, name: "Simple Tree", description: "Basic nesting with named selections", category: "Basic Examples" }
  "nested-elements" -> Just
    { id, name: "Nested Elements", description: "Multi-level nesting (Group → Circle + Text)", category: "Basic Examples" }
  "three-little-dimensions" -> Just
    { id, name: "Three Little Dimensions", description: "Nested data joins (2D array → table)", category: "Basic Examples" }
  -- Charts
  "bar-chart" -> Just
    { id, name: "Bar Chart", description: "Data-driven bars with scaling", category: "Chart Examples" }
  "scatter-plot" -> Just
    { id, name: "Scatter Plot", description: "Points positioned by data coordinates", category: "Chart Examples" }
  "line-chart" -> Just
    { id, name: "Line Chart", description: "Path element generated from data", category: "Chart Examples" }
  "grouped-bar-chart" -> Just
    { id, name: "Grouped Bar Chart", description: "Nested joins with multiple series", category: "Chart Examples" }
  "multi-line-chart" -> Just
    { id, name: "Multi-Line Chart", description: "Multiple data series with path generator", category: "Chart Examples" }
  "radial-stacked-bar" -> Just
    { id, name: "Radial Stacked Bar", description: "Stacked bars in polar coordinates", category: "Chart Examples" }
  "anscombes-quartet" -> Just
    { id, name: "Anscombe's Quartet", description: "Four datasets with same statistics, different shapes", category: "Chart Examples" }
  "wealth-health" -> Just
    { id, name: "Wealth & Health", description: "Gapminder-style bubble chart", category: "Chart Examples" }
  -- Hierarchies
  "simple-hierarchy" -> Just
    { id, name: "Tree Layout", description: "Vertical tree with node-link diagram", category: "Hierarchies" }
  "radial-tree" -> Just
    { id, name: "Radial Tree", description: "Tree layout in polar coordinates", category: "Hierarchies" }
  "horizontal-tree" -> Just
    { id, name: "Horizontal Tree", description: "Tree layout oriented left-to-right", category: "Hierarchies" }
  "cluster" -> Just
    { id, name: "Cluster Dendrogram", description: "Leaf nodes aligned at same depth", category: "Hierarchies" }
  "treemap" -> Just
    { id, name: "Treemap", description: "Nested rectangles sized by value", category: "Hierarchies" }
  "sunburst" -> Just
    { id, name: "Sunburst", description: "Radial partition diagram", category: "Hierarchies" }
  "pack" -> Just
    { id, name: "Circle Packing", description: "Nested circles sized by value", category: "Hierarchies" }
  "partition" -> Just
    { id, name: "Icicle / Partition", description: "Rectangular partition diagram", category: "Hierarchies" }
  -- Relational
  "sankey" -> Just
    { id, name: "Sankey Diagram", description: "Flow diagram with weighted links", category: "Relational" }
  "chord" -> Just
    { id, name: "Chord Diagram", description: "Circular flow between groups", category: "Relational" }
  "edge-bundle" -> Just
    { id, name: "Edge Bundling", description: "Hierarchical edge bundling", category: "Relational" }
  -- Force
  "simple-force" -> Just
    { id, name: "Simple Force Graph", description: "Basic force-directed layout", category: "Force-Directed" }
  "les-mis" -> Just
    { id, name: "Les Misérables Network", description: "Character co-occurrence network", category: "Force-Directed" }
  -- v3 DSL
  "v3-parabola" -> Just
    { id, name: "v3 Parabola", description: "Finally Tagless DSL - polymorphic expressions", category: "v3 DSL Examples" }
  "v3-transition" -> Just
    { id, name: "v3 Transition", description: "v3 expressions as animated transition targets", category: "v3 DSL Examples" }
  "v3-gup" -> Just
    { id, name: "v3 GUP", description: "General Update Pattern with staggered transitions", category: "v3 DSL Examples" }
  _ -> Nothing

-- | Get next example ID in the list
getNextExampleId :: String -> Maybe String
getNextExampleId currentId = case currentId of
  "three-little-circles" -> Just "three-circles-transition"
  "three-circles-transition" -> Just "simple-tree"
  "simple-tree" -> Just "nested-elements"
  "nested-elements" -> Just "three-little-dimensions"
  "three-little-dimensions" -> Just "bar-chart"
  "bar-chart" -> Just "scatter-plot"
  "scatter-plot" -> Just "line-chart"
  "line-chart" -> Just "grouped-bar-chart"
  "grouped-bar-chart" -> Just "simple-hierarchy"
  "simple-hierarchy" -> Just "v3-parabola"
  "v3-parabola" -> Just "v3-transition"
  "v3-transition" -> Just "v3-gup"
  "v3-gup" -> Nothing
  _ -> Nothing

-- | Get previous example ID in the list
getPrevExampleId :: String -> Maybe String
getPrevExampleId currentId = case currentId of
  "three-little-circles" -> Nothing
  "three-circles-transition" -> Just "three-little-circles"
  "simple-tree" -> Just "three-circles-transition"
  "nested-elements" -> Just "simple-tree"
  "three-little-dimensions" -> Just "nested-elements"
  "bar-chart" -> Just "three-little-dimensions"
  "scatter-plot" -> Just "bar-chart"
  "line-chart" -> Just "scatter-plot"
  "grouped-bar-chart" -> Just "line-chart"
  "simple-hierarchy" -> Just "grouped-bar-chart"
  "v3-parabola" -> Just "simple-hierarchy"
  "v3-transition" -> Just "v3-parabola"
  "v3-gup" -> Just "v3-transition"
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
            , HH.p_
                [ HH.a
                    [ HP.href $ "#" <> routeToPath Examples ]
                    [ HH.text "Return to Examples" ]
                ]
            ]
        ]

    Just meta ->
      HH.div
        [ HP.classes [ HH.ClassName "example-page" ] ]
        [ renderHeader state.exampleId (Just meta)
        , HH.main
            [ HP.classes [ HH.ClassName "example-content" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "example-description" ] ]
                [ HH.h1
                    [ HP.classes [ HH.ClassName "example-title" ] ]
                    [ HH.text meta.name ]
                , HH.p
                    [ HP.classes [ HH.ClassName "example-subtitle" ] ]
                    [ HH.text meta.description ]
                , HH.p
                    [ HP.classes [ HH.ClassName "example-category-badge" ] ]
                    [ HH.text meta.category ]
                ]
            , HH.div
                [ HP.classes [ HH.ClassName "example-viz-panel" ] ]
                [ HH.div
                    [ HP.id "viz"
                    , HP.classes [ HH.ClassName "example-viz" ]
                    ]
                    []
                ]
            , HH.div
                [ HP.classes [ HH.ClassName "example-code-panel" ] ]
                [ HH.h2
                    [ HP.classes [ HH.ClassName "code-panel-title" ] ]
                    [ HH.text "Source Code" ]
                , HH.p
                    [ HP.classes [ HH.ClassName "code-note" ] ]
                    [ HH.text "View the source for this example in the "
                    , HH.a
                        [ HP.href $ getGithubLink meta.id
                        , HP.target "_blank"
                        , HP.rel "noopener noreferrer"
                        ]
                        [ HH.text "GitHub repository" ]
                    ]
                , HH.div
                    [ HP.classes [ HH.ClassName "code-block" ] ]
                    (syntaxHighlightedCode $ getExampleCodeSummary meta.id)
                ]
            ]
        ]

-- | Render the header with navigation
renderHeader :: forall w i. String -> Maybe ExampleMeta -> HH.HTML w i
renderHeader currentId maybeMeta =
  SiteNav.render
    { logoSize: SiteNav.Large
    , quadrant: SiteNav.NoQuadrant
    , prevNext: Just
        { prev: Example <$> getPrevExampleId currentId
        , next: Example <$> getNextExampleId currentId
        }
    , pageTitle: (\m -> m.name) <$> maybeMeta
    }

-- | Get GitHub link for example source code
getGithubLink :: String -> String
getGithubLink exampleId =
  "https://github.com/afcondon/purescript-d3-tagless/tree/main/src/website/Viz/TreeAPI/" <> getModuleName exampleId <> ".purs"

-- | Get module name from example ID
getModuleName :: String -> String
getModuleName = case _ of
  "three-little-circles" -> "ThreeLittleCircles"
  "three-circles-transition" -> "ThreeLittleCirclesTransition"
  "simple-tree" -> "SimpleTreeExample"
  "nested-elements" -> "NestedElementsExample"
  "bar-chart" -> "BarChartExample"
  "scatter-plot" -> "ScatterPlotExample"
  "line-chart" -> "LineChartExample"
  "grouped-bar-chart" -> "GroupedBarChartExample"
  "simple-hierarchy" -> "SimpleHierarchyExample"
  "three-little-dimensions" -> "ThreeLittleDimensionsExample"
  _ -> "Unknown"

-- | Get example code summary from extracted snippets
getExampleCodeSummary :: String -> String
getExampleCodeSummary = case _ of
  -- Basic examples
  "three-little-circles" -> getSnippet "threeLittleCircles"
  "three-circles-transition" -> getSnippet "threeLittleCirclesTransition"
  "simple-tree" -> getSnippet "simpleTree"
  "nested-elements" -> getSnippet "nestedElements"
  "three-little-dimensions" -> getSnippet "threeLittleDimensions"
  -- Charts
  "bar-chart" -> getSnippet "barChart"
  "scatter-plot" -> getSnippet "scatterPlot"
  "line-chart" -> getSnippet "lineChart"
  "grouped-bar-chart" -> getSnippet "groupedBarChart"
  "multi-line-chart" -> getSnippet "multiLineChart"
  "radial-stacked-bar" -> getSnippet "radialStackedBar"
  "anscombes-quartet" -> getSnippet "anscombesQuartet"
  "wealth-health" -> getSnippet "wealthHealth"
  -- Hierarchies
  "simple-hierarchy" -> getSnippet "treeViz"
  "radial-tree" -> getSnippet "radialTree"
  "horizontal-tree" -> getSnippet "horizontalTree"
  "cluster" -> getSnippet "cluster"
  "treemap" -> getSnippet "treemap"
  "sunburst" -> getSnippet "sunburst"
  "pack" -> getSnippet "pack"
  "partition" -> getSnippet "partition"
  -- Relational
  "sankey" -> getSnippet "sankey"
  "chord" -> getSnippet "chord"
  "edge-bundle" -> getSnippet "edgeBundle"
  -- Force
  "simple-force" -> getSnippet "simpleForce"
  "les-mis" -> getSnippet "lesMis"
  -- v3 DSL
  "v3-parabola" -> getSnippet "v3Parabola"
  "v3-transition" -> getSnippet "v3Transition"
  "v3-gup" -> getSnippet "v3GUP"
  _ -> "-- See GitHub for full source code"
