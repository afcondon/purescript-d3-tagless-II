module Component.TreeAPI where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

import D3.Viz.TreeAPI.ThreeLittleCircles as ThreeLittleCircles
import D3.Viz.TreeAPI.ThreeLittleCirclesTransition as ThreeLittleCirclesTransition
import D3.Viz.TreeAPI.SimpleTreeExample as SimpleTreeExample
import D3.Viz.TreeAPI.NestedElementsExample as NestedElementsExample
import D3.Viz.TreeAPI.BarChartExample as BarChartExample
import D3.Viz.TreeAPI.ScatterPlotExample as ScatterPlotExample
import D3.Viz.TreeAPI.LineChartExample as LineChartExample
import D3.Viz.TreeAPI.SimpleHierarchyExample as SimpleHierarchyExample
import D3.Viz.TreeAPI.ThreeLittleDimensionsExample as ThreeLittleDimensionsExample
import D3.Viz.TreeAPI.GroupedBarChartExample as GroupedBarChartExample
import D3.Viz.TreeAPI.LesMisTreeExample as LesMisTreeExample

-- | State for the TreeAPI examples component
type State =
  { currentExample :: Example
  }

data Example
  = ThreeCircles
  | ThreeCirclesTransition
  | SimpleTree
  | NestedElements
  | BarChart
  | ScatterPlot
  | LineChart
  | GroupedBarChart
  | SimpleHierarchy
  | ThreeDimensions
  | LesMisForce

derive instance eqExample :: Eq Example

-- | Actions for the component
data Action
  = Initialize
  | SelectExample Example

-- | The TreeAPI examples component
component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState: \_ -> { currentExample: ThreeCircles }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div [ HP.class_ (HH.ClassName "tree-api-examples") ]
    [ HH.h1_ [ HH.text "Declarative Tree API Examples" ]

    , HH.div [ HP.class_ (HH.ClassName "description") ]
        [ HH.p_
            [ HH.text "This page demonstrates the new declarative tree API for PSD3v2. "
            , HH.text "Instead of imperative "
            , HH.code_ [ HH.text "appendChild" ]
            , HH.text " chains, you define the entire DOM structure as a tree, "
            , HH.text "then render it in one step."
            ]
        , HH.p_
            [ HH.strong_ [ HH.text "Key features:" ]
            ]
        , HH.ul_
            [ HH.li_ [ HH.text "Tree structure is immediately visible" ]
            , HH.li_ [ HH.text "Data joins with template functions" ]
            , HH.li_ [ HH.text "Named selections via Map lookup" ]
            , HH.li_ [ HH.text "Type-safe, polymorphic attributes" ]
            ]
        ]

    , HH.div [ HP.class_ (HH.ClassName "example-selector") ]
        [ HH.h2_ [ HH.text "Basic Examples:" ]
        , HH.div [ HP.class_ (HH.ClassName "buttons") ]
            [ exampleButton state ThreeCircles "Three Little Circles" "Data join basics"
            , exampleButton state ThreeCirclesTransition "Circles Transition" "Animated transitions"
            , exampleButton state SimpleTree "Simple Tree" "Basic nesting"
            , exampleButton state NestedElements "Nested Elements" "Multi-level (Group → Circle + Text)"
            , exampleButton state ThreeDimensions "Three Little Dimensions" "Nested data joins (2D array → table)"
            ]
        , HH.h2_ [ HH.text "Chart Examples:" ]
        , HH.div [ HP.class_ (HH.ClassName "buttons") ]
            [ exampleButton state BarChart "Bar Chart" "Data-driven bars with scaling"
            , exampleButton state ScatterPlot "Scatter Plot" "Points positioned by data"
            , exampleButton state LineChart "Line Chart" "Path element from data"
            , exampleButton state GroupedBarChart "Grouped Bar Chart" "Nested joins with multiple series"
            ]
        , HH.h2_ [ HH.text "Hierarchy & Simulation Examples:" ]
        , HH.div [ HP.class_ (HH.ClassName "buttons") ]
            [ exampleButton state SimpleHierarchy "Tree Layout" "Hierarchical node-link diagram"
            , exampleButton state LesMisForce "Les Misérables" "Force-directed graph with simulation"
            ]
        ]

    , HH.div [ HP.class_ (HH.ClassName "visualization-container") ]
        [ HH.h2_ [ HH.text $ exampleTitle state.currentExample ]
        , HH.div
            [ HP.id "viz"
            , HP.class_ (HH.ClassName "viz-area")
            ]
            []
        , HH.div [ HP.class_ (HH.ClassName "code-example") ]
            [ HH.h3_ [ HH.text "Code:" ]
            , HH.pre_
                [ HH.code_ [ HH.text $ exampleCode state.currentExample ]
                ]
            ]
        ]
    ]

exampleButton :: forall m. State -> Example -> String -> String -> H.ComponentHTML Action () m
exampleButton state ex label description =
  HH.button
    [ HP.class_ (HH.ClassName $ if state.currentExample == ex then "selected" else "")
    , HE.onClick \_ -> SelectExample ex
    ]
    [ HH.div [ HP.class_ (HH.ClassName "label") ] [ HH.text label ]
    , HH.div [ HP.class_ (HH.ClassName "description") ] [ HH.text description ]
    ]

exampleTitle :: Example -> String
exampleTitle ThreeCircles = "Three Little Circles (Data Join)"
exampleTitle ThreeCirclesTransition = "Three Little Circles (Transition)"
exampleTitle SimpleTree = "Simple Tree (Basic Structure)"
exampleTitle NestedElements = "Nested Elements (Multi-Level)"
exampleTitle BarChart = "Bar Chart"
exampleTitle ScatterPlot = "Scatter Plot"
exampleTitle LineChart = "Line Chart"
exampleTitle GroupedBarChart = "Grouped Bar Chart (Nested Joins)"
exampleTitle SimpleHierarchy = "Tree Layout (Hierarchy)"
exampleTitle ThreeDimensions = "Three Little Dimensions (Nested Joins)"
exampleTitle LesMisForce = "Les Misérables (Force Simulation)"

exampleCode :: Example -> String
exampleCode ThreeCircles = """
tree = named SVG "svg" [...] `withChild`
  (joinData "circles" "circle" circleData $ \d ->
    elem Circle
      [ cx d.x
      , cy d.y
      , radius d.r
      , fill d.color
      ])

selections <- renderTree container tree
"""

exampleCode ThreeCirclesTransition = """
-- Initial render: green circles in horizontal line
initialTree = named SVG "svg" [...] `withChild`
  (joinData "circles" "circle" [0, 1, 2] $ \\d ->
    elem Circle [fill "green", cx (d * 100 + 100), ...])

selections <- renderTree container initialTree

-- Extract circles selection
circlesSel <- liftEffect $ reselectD3v2 "circles" selections

-- Transition to RGB colors with overlapping positions
let transitionConfig = transitionWith
      { duration: Milliseconds 1500.0
      , delay: Just (Milliseconds 100.0)
      , easing: Just ElasticOut
      }

withTransition transitionConfig circlesSel
  [ fill colorFn
  , cx cxFn
  , cy cyFn
  , radius 30.0
  , fillOpacity 0.5
  ]
"""

exampleCode SimpleTree = """
tree = named SVG "svg" [...] `withChild`
  (named Group "container" [...] `withChildren`
    [ named Circle "circle" [...]
    , named Text "text" [...]
    ])

selections <- renderTree container tree
-- Access: Map.lookup "circle" selections
"""

exampleCode NestedElements = """
tree = named SVG "svg" [...] `withChild`
  (named Group "mainGroup" [...] `withChildren`
    [ named Group "node1" [...] `withChildren`
        [ named Circle "circle1" [...]
        , named Text "label1" [...]
        ]
    , named Group "node2" [...] `withChildren`
        [ named Circle "circle2" [...]
        , named Text "label2" [...]
        ]
    ])

selections <- renderTree container tree
"""

exampleCode BarChart = """
tree = named SVG "svg" [...] `withChild`
  (named Group "chartGroup" [...] `withChild`
    (joinData "bars" "rect" dataPoints $ \\point ->
      T.elem Rect
        [ x (xScale point.x)
        , y (yScale point.y)
        , width barWidth
        , height barHeight
        , fill "#4a90e2"
        ]))

selections <- renderTree container tree
"""

exampleCode ScatterPlot = """
tree = named SVG "svg" [...] `withChild`
  (named Group "chartGroup" [...] `withChild`
    (joinData "points" "circle" dataPoints $ \\point ->
      T.elem Circle
        [ cx (xScale point.x)
        , cy (yScale point.y)
        , radius 6.0
        , fill "#e74c3c"
        ]))

selections <- renderTree container tree
"""

exampleCode LineChart = """
tree = named SVG "svg" [...] `withChild`
  (named Group "chartGroup" [...] `withChild`
    (named Path "line"
      [ d pathData
      , fill "none"
      , stroke "#2ecc71"
      , strokeWidth 2.0
      ]))

selections <- renderTree container tree
"""

exampleCode GroupedBarChart = """
-- Group data by state
let stateGroups = groupByState data
    ages = getAges data

-- Nested join: states → bars within each state
barsTree = nestedJoin "stateGroups" "g" stateGroups (_.bars) $ \\bar ->
  let
    stateIdx = findIndex (\\g -> g.state == bar.state) stateGroups
    ageIdx = findIndex (\\a -> a == bar.age) ages
    xPos = stateIdx * groupWidth + ageIdx * barWidth
    yPos = yScale bar.population
    barHeight = iHeight - yPos
  in
    T.elem Rect
      [ x xPos
      , y yPos
      , width barWidth
      , height barHeight
      , fill (colorForAge bar.age)
      ]

selections <- renderTree chartGroup barsTree
"""

exampleCode SimpleHierarchy = """
-- Apply layout algorithm
let positioned = tree config hierarchyData
let nodes = Array.fromFoldable positioned
let links = makeLinks positioned

tree = named SVG "svg" [...] `withChildren`
  [ named Group "linksGroup" [...] `withChild`
      (joinData "links" "path" links $ \\link ->
        T.elem Path [d (linkPath link), ...])
  , named Group "nodesGroup" [...] `withChild`
      (joinData "nodes" "g" nodes $ \\node ->
        T.named Group ("node-" <> node.name) [...]
          `withChildren`
            [ T.elem Circle [cx node.x, cy node.y, ...]
            , T.elem Text [textContent node.name, ...]
            ])
  ]
"""

exampleCode ThreeDimensions = """
-- Nested data: [[1,2,3],[4,5,6],[7,8,9]]
tree = named Table "table" [...] `withChild`
  (nestedJoin "rows" "tr" matrixData identity $ \\cellValue ->
    T.elem Td [textContent (show cellValue)])

-- nestedJoin handles type decomposition:
-- - Outer type: Array Int (a row)
-- - Decomposer: identity (extracts row data)
-- - Inner type: Int (a cell value)
-- - Creates: table rows → table cells

-- Power: Works with ANY decomposition!
-- - Array of Records with nested Arrays
-- - Lists of Maps of Sets
-- - Custom ADT decompositions
"""

exampleCode LesMisForce = """
-- Initialize simulation with forces
{ nodes, links } <- init
  { nodes: nodesWithPositions
  , links: model.links
  , forces: [manyBody, center, forceLinks]
  , ...
  }

-- Declarative tree structure
let forceGraphTree =
      named SVG "svg" [...] `withChild`
        (named Group "zoomGroup" [...] `withChildren`
          [ named Group "linksGroup" [...] `withChild`
              (joinData "linkElements" "line" links linkTemplate)
          , named Group "nodesGroup" [...] `withChild`
              (joinData "nodeElements" "circle" nodes nodeTemplate)
          ])

-- Render structure
selections <- renderTree container forceGraphTree

-- Extract selections and add behaviors
on (Zoom ...) svgSel
on (Drag $ simulationDrag ...) nodesSel

-- Add tick functions for position updates
addTickFunction "nodes" $ Step nodesSel
  [cx (\\d -> d.x), cy (\\d -> d.y)]
addTickFunction "links" $ Step linksSel
  [x1 (\\l -> l.source.x), y1 (\\l -> l.source.y), ...]

start
"""

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    -- Render the initial example
    handleAction (SelectExample ThreeCircles)

  SelectExample example -> do
    H.modify_ _ { currentExample = example }

    -- Clear the viz area
    liftEffect $ clearViz

    -- Render the selected example
    case example of
      ThreeCircles -> liftEffect ThreeLittleCircles.threeLittleCircles
      ThreeCirclesTransition -> liftEffect ThreeLittleCirclesTransition.threeLittleCirclesTransition
      SimpleTree -> liftEffect SimpleTreeExample.testSimpleTree
      NestedElements -> liftEffect NestedElementsExample.testNestedElements
      BarChart -> liftEffect BarChartExample.barChart
      ScatterPlot -> liftEffect ScatterPlotExample.scatterPlot
      LineChart -> liftEffect LineChartExample.lineChart
      GroupedBarChart -> liftEffect GroupedBarChartExample.groupedBarChart
      SimpleHierarchy -> liftEffect SimpleHierarchyExample.simpleHierarchy
      ThreeDimensions -> liftEffect ThreeLittleDimensionsExample.threeLittleDimensions
      LesMisForce -> liftEffect LesMisTreeExample.testLesMisTree

-- FFI to clear the viz div
foreign import clearViz :: Effect Unit
