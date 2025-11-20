module Component.Example where

import Prelude

import CodeSnippets (getSnippet)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.RoutingDSL (routeToPath)
import PSD3.Website.Types (Route(..))

-- SNIPPET: threeLittleCircles src/website/Viz/TreeAPI/ThreeLittleCircles.purs 25-62
-- SNIPPET: threeLittleCirclesTransition src/website/Viz/TreeAPI/ThreeLittleCirclesTransition.purs 112-150
-- SNIPPET: simpleTree src/website/Viz/TreeAPI/SimpleTreeExample.purs 28-45
-- SNIPPET: nestedElements src/website/Viz/TreeAPI/NestedElementsExample.purs 25-105
-- SNIPPET: barChart src/website/Viz/TreeAPI/BarChartExample.purs 65-157
-- SNIPPET: scatterPlot src/website/Viz/TreeAPI/ScatterPlotExample.purs 64-113
-- SNIPPET: lineChart src/website/Viz/TreeAPI/LineChartExample.purs 78-121
-- SNIPPET: groupedBarChart src/website/Viz/TreeAPI/GroupedBarChartExample.purs 134-240
-- SNIPPET: treeViz src/website/Viz/TreeAPI/TreeViz.purs 54-141
-- SNIPPET: threeLittleDimensions src/website/Viz/TreeAPI/ThreeLittleDimensionsExample.purs 27-54
-- SNIPPET: lesMisTree src/website/Viz/TreeAPI/LesMisTreeExample.purs 104-268

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

-- | Get example code summary from extracted snippets
getExampleCodeSummary :: String -> String
getExampleCodeSummary = case _ of
  "three-little-circles" -> getSnippet "threeLittleCircles"
  "three-circles-transition" -> getSnippet "threeLittleCirclesTransition"
  "simple-tree" -> getSnippet "simpleTree"
  "nested-elements" -> getSnippet "nestedElements"
  "bar-chart" -> getSnippet "barChart"
  "scatter-plot" -> getSnippet "scatterPlot"
  "line-chart" -> getSnippet "lineChart"
  "grouped-bar-chart" -> getSnippet "groupedBarChart"
  "simple-hierarchy" -> getSnippet "treeViz"
  "three-little-dimensions" -> getSnippet "threeLittleDimensions"
  "lesmis-force" -> getSnippet "lesMisTree"
  _ -> "-- See GitHub for full source code"
