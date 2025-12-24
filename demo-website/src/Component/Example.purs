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
  , emmet :: String
  }

-- | Lookup example metadata by ID
getExampleMeta :: String -> Maybe ExampleMeta
getExampleMeta id = case id of
  -- Basic examples
  "three-little-circles" -> Just
    { id, name: "Three Little Circles", description: "Data join basics - bind data to DOM elements", category: "Basic Examples"
    , emmet: "svg[viewBox='0 0 400 200']>j(CircleData)>c[cx:x,cy:y,r:r,fill:color]"
    }
  "three-circles-transition" -> Just
    { id, name: "Circles Transition", description: "Animated transitions with easing and delays", category: "Basic Examples"
    , emmet: "svg[viewBox='0 0 400 200']>j(CircleData)>c[cx:x,cy:y,r:r,fill:color]"
    }
  "simple-tree" -> Just
    { id, name: "Simple Tree", description: "Basic nesting with named selections", category: "Basic Examples"
    , emmet: "svg>g[id=tree-container]>g.node*3>(c[r=20]+t[dy=5])"
    }
  "nested-elements" -> Just
    { id, name: "Nested Elements", description: "Multi-level nesting (Group → Circle + Text)", category: "Basic Examples"
    , emmet: "svg>g>(c[cx=100,cy=100,r=50]+t[x=100,y=110])"
    }
  "three-little-dimensions" -> Just
    { id, name: "Three Little Dimensions", description: "Nested data joins (2D array → table)", category: "Basic Examples"
    , emmet: "svg>j(Row)>g.row[transform:translateY]>j(Cell)>r[x:x,y:y,width=20,height=20]"
    }
  -- Charts
  "bar-chart" -> Just
    { id, name: "Bar Chart", description: "Data-driven bars with scaling", category: "Chart Examples"
    , emmet: "svg[viewBox='0 0 600 400']>g[transform='translate(50,20)']>j(DataPoint)>r[x:scaledX,y:scaledY,width=barWidth,height:scaledHeight,fill=steelblue]"
    }
  "scatter-plot" -> Just
    { id, name: "Scatter Plot", description: "Points positioned by data coordinates", category: "Chart Examples"
    , emmet: "svg[viewBox='0 0 600 400']>g[transform='translate(50,20)']>j(Point)>c[cx:scaledX,cy:scaledY,r=5,fill=steelblue,opacity=0.6]"
    }
  "line-chart" -> Just
    { id, name: "Line Chart", description: "Path element generated from data", category: "Chart Examples"
    , emmet: "svg[viewBox='0 0 600 400']>g[transform='translate(50,20)']>p.line[d:pathData,stroke=steelblue,stroke-width=2,fill=none]"
    }
  "grouped-bar-chart" -> Just
    { id, name: "Grouped Bar Chart", description: "Nested joins with multiple series", category: "Chart Examples"
    , emmet: "svg>g[transform='translate(50,20)']>j(Category)>g.category[transform:translateX]>j(Series)>r[x:x,y:y,width=barWidth,height:height,fill:seriesColor]"
    }
  "multi-line-chart" -> Just
    { id, name: "Multi-Line Chart", description: "Multiple data series with path generator", category: "Chart Examples"
    , emmet: "svg>g[transform='translate(50,20)']>j(Series)>p.line[d:pathData,stroke:seriesColor,stroke-width=2,fill=none]"
    }
  "radial-stacked-bar" -> Just
    { id, name: "Radial Stacked Bar", description: "Stacked bars in polar coordinates", category: "Chart Examples"
    , emmet: "svg>g[transform='translate(300,200)']>j(DataPoint)>p.arc[d:arcPath,fill:color]"
    }
  "anscombes-quartet" -> Just
    { id, name: "Anscombe's Quartet", description: "Four datasets with same statistics, different shapes", category: "Chart Examples"
    , emmet: "svg>g.quartet*4>g[transform:translateQuadrant]>j(Point)>c[cx:scaledX,cy:scaledY,r=3]"
    }
  "wealth-health" -> Just
    { id, name: "Wealth & Health", description: "Gapminder-style bubble chart", category: "Chart Examples"
    , emmet: "svg>g[transform='translate(60,20)']>j(Country)>c[cx:wealthScale,cy:healthScale,r:populationScale,fill:regionColor,opacity=0.7]"
    }
  -- Hierarchies
  "simple-hierarchy" -> Just
    { id, name: "Tree Layout", description: "Vertical tree with node-link diagram", category: "Hierarchies"
    , emmet: "svg>g[transform='translate(50,50)']>j(HierNode)>(g.node[transform:translate]>(c[r=5,fill=steelblue]+t[dy=20])+p.link[d:linkPath,stroke=#ccc])"
    }
  "radial-tree" -> Just
    { id, name: "Radial Tree", description: "Tree layout in polar coordinates", category: "Hierarchies"
    , emmet: "svg>g[transform='translate(300,300)']>j(HierNode)>(g.node[transform:rotate]>c[r=4]+p.link[d:radialPath])"
    }
  "horizontal-tree" -> Just
    { id, name: "Horizontal Tree", description: "Tree layout oriented left-to-right", category: "Hierarchies"
    , emmet: "svg>g[transform='translate(50,50)']>j(HierNode)>(g.node[transform:translate]>c[r=5]+p.link[d:horizontalPath])"
    }
  "cluster" -> Just
    { id, name: "Cluster Dendrogram", description: "Leaf nodes aligned at same depth", category: "Hierarchies"
    , emmet: "svg>g[transform='translate(50,50)']>j(HierNode)>(g.leaf[transform:translate]>c[r=3]+p.link[d:linkPath])"
    }
  "treemap" -> Just
    { id, name: "Treemap", description: "Nested rectangles sized by value", category: "Hierarchies"
    , emmet: "svg>g>j(HierNode)>r[x:x0,y:y0,width:width,height:height,fill:color,stroke=#fff]"
    }
  "sunburst" -> Just
    { id, name: "Sunburst", description: "Radial partition diagram", category: "Hierarchies"
    , emmet: "svg>g[transform='translate(400,300)']>j(HierNode)>p.arc[d:arcPath,fill:color]"
    }
  "pack" -> Just
    { id, name: "Circle Packing", description: "Nested circles sized by value", category: "Hierarchies"
    , emmet: "svg>g[transform='translate(300,300)']>j(HierNode)>c[cx:x,cy:y,r:r,fill:color,stroke=#fff]"
    }
  "partition" -> Just
    { id, name: "Icicle / Partition", description: "Rectangular partition diagram", category: "Hierarchies"
    , emmet: "svg>g>j(HierNode)>r[x:x,y:y,width:width,height:height,fill:color]"
    }
  -- Relational
  "sankey" -> Just
    { id, name: "Sankey Diagram", description: "Flow diagram with weighted links", category: "Relational"
    , emmet: "svg>g[transform='translate(20,20)']>(j(SankeyNode)>r.node[x:x,y:y,width:width,height:height,fill=steelblue]+j(SankeyLink)>p.link[d:linkPath,stroke:color,stroke-width:width,fill=none,opacity=0.5])"
    }
  "chord" -> Just
    { id, name: "Chord Diagram", description: "Circular flow between groups", category: "Relational"
    , emmet: "svg>g[transform='translate(300,300)']>(j(ChordGroup)>p.arc[d:groupPath,fill:color]+j(Chord)>p.ribbon[d:chordPath,fill:color,opacity=0.7])"
    }
  "edge-bundle" -> Just
    { id, name: "Edge Bundling", description: "Hierarchical edge bundling", category: "Relational"
    , emmet: "svg>g[transform='translate(400,400)']>(j(Node)>c.node[cx:x,cy:y,r=3,fill=steelblue]+j(Link)>p.link[d:bundledPath,stroke=#ccc,fill=none,opacity=0.3])"
    }
  -- Force
  "simple-force" -> Just
    { id, name: "Simple Force Graph", description: "Basic force-directed layout", category: "Force-Directed"
    , emmet: "svg>g>(j(Link)>l.link[x1:source.x,y1:source.y,x2:target.x,y2:target.y,stroke=#999]+j(Node)>c.node[cx:x,cy:y,r=5,fill=steelblue])"
    }
  "les-mis" -> Just
    { id, name: "Les Misérables Network", description: "Character co-occurrence network", category: "Force-Directed"
    , emmet: "svg[viewBox='0 0 800 600']>g>(j(Link)>l.link[x1:source.x,y1:source.y,x2:target.x,y2:target.y,stroke=#999,stroke-width:value]+j(Node)>g.node[transform:translate]>c[r:degree,fill:groupColor])"
    }
  -- v3 DSL
  "v3-parabola" -> Just
    { id, name: "v3 Parabola", description: "Finally Tagless DSL - polymorphic expressions", category: "v3 DSL Examples"
    , emmet: "svg>g[transform='translate(50,250)']>j(Point)>c[cx:x,cy:y,r=3,fill=purple]"
    }
  "v3-transition" -> Just
    { id, name: "v3 Transition", description: "v3 expressions as animated transition targets", category: "v3 DSL Examples"
    , emmet: "svg>g>j(DataPoint)>c[cx:x,cy:y,r:radius,fill:color]"
    }
  "v3-gup" -> Just
    { id, name: "v3 GUP", description: "General Update Pattern with staggered transitions", category: "v3 DSL Examples"
    , emmet: "svg[viewBox='0 0 600 200']>u(Letter)>t.letter[x:x,y=100,font-size=48,fill=steelblue]"
    }
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
                [ -- Emmet Syntax Section
                  HH.div
                    [ HP.classes [ HH.ClassName "emmet-section" ] ]
                    [ HH.h3
                        [ HP.classes [ HH.ClassName "section-title" ] ]
                        [ HH.text "Emmet Syntax" ]
                    , HH.pre
                        [ HP.classes [ HH.ClassName "emmet-code" ] ]
                        [ HH.code
                            [ HP.classes [ HH.ClassName "language-emmet" ] ]
                            [ HH.text meta.emmet ]
                        ]
                    , HH.p
                        [ HP.classes [ HH.ClassName "emmet-note" ] ]
                        [ HH.text "Copy this expression to use in "
                        , HH.a
                            [ HP.href "#/simple-tree-builder"
                            , HP.classes [ HH.ClassName "builder-link" ]
                            ]
                            [ HH.text "Simple Tree Builder" ]
                        ]
                    ]
                -- PureScript Code Section
                , HH.h3
                    [ HP.classes [ HH.ClassName "section-title" ] ]
                    [ HH.text "PureScript Code" ]
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
