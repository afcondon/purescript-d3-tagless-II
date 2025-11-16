module Component.Example where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.RoutingDSL (routeToPath)
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
import D3.Viz.TreeAPI.LesMisTreeExample as LesMisTreeExample

-- | Component state
type State =
  { exampleId :: String
  }

data Action
  = Initialize

-- | Example page component
component :: forall q o m. MonadAff m => H.Component q String o m
component = H.mkComponent
  { initialState: \exampleId -> { exampleId }
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
    case state.exampleId of
      "three-little-circles" ->
        liftEffect ThreeLittleCircles.threeLittleCircles
      "three-circles-transition" ->
        liftEffect ThreeLittleCirclesTransition.threeLittleCirclesTransition
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
      "lesmis-force" ->
        liftEffect LesMisTreeExample.testLesMisTree
      _ -> pure unit

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
    { id, name: "Three Little Circles", description: "Data join basics - bind data to DOM elements", category: "Basic Examples" }
  "three-circles-transition" -> Just
    { id, name: "Circles Transition", description: "Animated transitions with easing and delays", category: "Basic Examples" }
  "simple-tree" -> Just
    { id, name: "Simple Tree", description: "Basic nesting with named selections", category: "Basic Examples" }
  "nested-elements" -> Just
    { id, name: "Nested Elements", description: "Multi-level nesting (Group → Circle + Text)", category: "Basic Examples" }
  "bar-chart" -> Just
    { id, name: "Bar Chart", description: "Data-driven bars with scaling", category: "Chart Examples" }
  "scatter-plot" -> Just
    { id, name: "Scatter Plot", description: "Points positioned by data coordinates", category: "Chart Examples" }
  "line-chart" -> Just
    { id, name: "Line Chart", description: "Path element generated from data", category: "Chart Examples" }
  "grouped-bar-chart" -> Just
    { id, name: "Grouped Bar Chart", description: "Nested joins with multiple series", category: "Chart Examples" }
  "simple-hierarchy" -> Just
    { id, name: "Tree Layout", description: "Hierarchical node-link diagram (pure PureScript)", category: "Hierarchies & Simulations" }
  "three-little-dimensions" -> Just
    { id, name: "Three Little Dimensions", description: "Nested data joins (2D array → table)", category: "Basic Examples" }
  "lesmis-force" -> Just
    { id, name: "Les Misérables Network", description: "Force-directed graph with simulation", category: "Hierarchies & Simulations" }
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
  "simple-hierarchy" -> Just "lesmis-force"
  "lesmis-force" -> Nothing
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
  "lesmis-force" -> Just "simple-hierarchy"
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
                    [ HP.href $ "#" <> routeToPath Gallery ]
                    [ HH.text "Return to Examples Gallery" ]
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
                , HH.pre
                    [ HP.classes [ HH.ClassName "code-block" ] ]
                    [ HH.code_
                        [ HH.text $ getExampleCodeSummary meta.id ]
                    ]
                ]
            ]
        ]

-- | Render the header with navigation
renderHeader :: forall w i. String -> Maybe ExampleMeta -> HH.HTML w i
renderHeader currentId maybeMeta =
  HH.header
    [ HP.classes [ HH.ClassName "example-header" ] ]
    [ HH.div
        [ HP.classes [ HH.ClassName "example-header-content" ] ]
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
        , HH.nav
            [ HP.classes [ HH.ClassName "example-nav" ] ]
            [ HH.a
                [ HP.href $ "#" <> routeToPath Gallery
                , HP.classes [ HH.ClassName "example-nav-link" ]
                ]
                [ HH.text "← Gallery" ]
            , case getPrevExampleId currentId of
                Nothing ->
                  HH.span
                    [ HP.classes [ HH.ClassName "example-nav-link", HH.ClassName "disabled" ] ]
                    [ HH.text "← Prev" ]
                Just prevId ->
                  HH.a
                    [ HP.href $ "#" <> routeToPath (Example prevId)
                    , HP.classes [ HH.ClassName "example-nav-link" ]
                    ]
                    [ HH.text "← Prev" ]
            , case getNextExampleId currentId of
                Nothing ->
                  HH.span
                    [ HP.classes [ HH.ClassName "example-nav-link", HH.ClassName "disabled" ] ]
                    [ HH.text "Next →" ]
                Just nextId ->
                  HH.a
                    [ HP.href $ "#" <> routeToPath (Example nextId)
                    , HP.classes [ HH.ClassName "example-nav-link" ]
                    ]
                    [ HH.text "Next →" ]
            ]
        ]
    ]

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
  "lesmis-force" -> "LesMisTreeExample"
  _ -> "Unknown"

-- | Get example code summary (placeholder showing key concepts)
getExampleCodeSummary :: String -> String
getExampleCodeSummary = case _ of
  "three-little-circles" -> """
-- Three Little Circles: Basic data join
circleData = [
  { x: 100.0, y: 100.0, r: 20.0, color: "red" },
  { x: 200.0, y: 100.0, r: 20.0, color: "green" },
  { x: 300.0, y: 100.0, r: 20.0, color: "blue" }
]

tree = named SVG "svg" [...] `withChild`
  (joinData "circles" "circle" circleData $ \\d ->
    elem Circle
      [ cx d.x, cy d.y, radius d.r, fill d.color ])

selections <- renderTree container tree
"""

  "bar-chart" -> """
-- Bar Chart: Data-driven rectangles with scaling
dataPoints = [
  { name: "Jan", value: 30.0 },
  { name: "Feb", value: 50.0 },
  ...
]

let yScale val = iHeight - (val * iHeight / maxValue)
    barWidth = iWidth / toNumber (length dataPoints)

tree = named SVG "svg" [...] `withChild`
  (joinData "bars" "rect" dataPoints $ \\point ->
    elem Rect
      [ x (\\i -> toNumber i * barWidth)
      , y (yScale point.value)
      , width barWidth
      , height (\\_ i -> iHeight - yScale (unsafeIndex dataPoints i).value)
      , fill "#4a90e2"
      ])
"""

  "nested-elements" -> """
-- Nested Elements: Multi-level structure
tree = named SVG "svg" [...] `withChild`
  (named Group "mainGroup" [...] `withChildren`
    [ named Group "node1" [...] `withChildren`
        [ named Circle "circle1" [cx 100.0, cy 100.0, radius 30.0]
        , named Text "label1" [x 100.0, y 150.0, textContent "Node 1"]
        ]
    , named Group "node2" [...] `withChildren`
        [ named Circle "circle2" [cx 250.0, cy 100.0, radius 30.0]
        , named Text "label2" [x 250.0, y 150.0, textContent "Node 2"]
        ]
    ])
"""

  "grouped-bar-chart" -> """
-- Grouped Bar Chart: Nested joins for multiple series
let stateGroups = groupByState data
    ages = getAges data

barsTree = nestedJoin "stateGroups" "g" stateGroups (_.bars) $ \\bar ->
  let xPos = stateIdx * groupWidth + ageIdx * barWidth
      yPos = yScale bar.population
  in elem Rect
      [ x xPos, y yPos
      , width barWidth
      , height (iHeight - yPos)
      , fill (colorForAge bar.age)
      ]
"""

  "simple-hierarchy" -> """
-- Tree Layout: Pure PureScript hierarchy with links and nodes
let positioned = tree config hierarchyData
    nodes = Array.fromFoldable positioned
    links = makeLinks positioned

tree = named SVG "svg" [...] `withChildren`
  [ named Group "linksGroup" [...] `withChild`
      (joinData "links" "path" links $ \\link ->
        elem Path [d (linkPath link), ...])
  , named Group "nodesGroup" [...] `withChild`
      (joinData "nodes" "g" nodes $ \\node ->
        named Group ("node-" <> node.name) [...]
          `withChildren`
            [ elem Circle [cx node.x, cy node.y, ...]
            , elem Text [textContent node.name, ...]
            ])
  ]
"""

  "three-little-dimensions" -> """
-- Three Little Dimensions: Nested data (2D array → table)
let matrixData = [[1,2,3],[4,5,6],[7,8,9]]

tree = named Table "table" [...] `withChild`
  (nestedJoin "rows" "tr" matrixData identity $ \\cellValue ->
    elem Td [textContent (show cellValue)])

-- nestedJoin decomposes:
-- - Outer type: Array Int (a row)
-- - Inner type: Int (a cell value)
-- - Creates: table rows → table cells
"""

  _ -> "-- See GitHub for full source code"
