module PSD3.Component.MermaidDiagrams where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Shared.Mermaid (mermaidDiagram, triggerMermaidRendering)
import PSD3 as D3
import PSD3.Attributes (fill, fillOpacity, radius, cx, cy, width, height, strokeWidth, strokeColor, strokeOpacity, x, y, to, transitionWithDuration, classed, fontSize, viewBox, andThen, remove)
import PSD3.Types (Element(..))
import PSD3.Interpreter.MermaidAST (MermaidASTM, MermaidSelection, runMermaidAST)
import PSD3.Data.Node (NodeID)
import Unsafe.Coerce (unsafeCoerce)
import Data.Time.Duration (Milliseconds(..))

type State =
  { test1_simpleAppend :: String
  , test2_multipleAppends :: String
  , test3_selectUnderJoin :: String
  , test4_updatePattern :: String
  , test5_filterSelection :: String
  , test6_mergeSelections :: String
  , test7_nestedGroups :: String
  , test8_mixedElements :: String
  , test9_openSelection :: String
  , test10_attributesOnly :: String
  , test11_transitions :: String
  , viz_parabola :: String
  , viz_multiLineChart :: String
  , viz_stackedBar :: String
  , viz_chordDiagram :: String
  , viz_sankey :: String
  , viz_tree :: String
  , viz_bubbleChart :: String
  , viz_icicle :: String
  , viz_treemap :: String
  , viz_gup :: String
  }

data Action = Initialize

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

initialState :: forall i. i -> State
initialState _ =
  { test1_simpleAppend: "Loading..."
  , test2_multipleAppends: "Loading..."
  , test3_selectUnderJoin: "Loading..."
  , test4_updatePattern: "Loading..."
  , test5_filterSelection: "Loading..."
  , test6_mergeSelections: "Loading..."
  , test7_nestedGroups: "Loading..."
  , test8_mixedElements: "Loading..."
  , test9_openSelection: "Loading..."
  , test10_attributesOnly: "Loading..."
  , test11_transitions: "Loading..."
  , viz_parabola: "Loading..."
  , viz_multiLineChart: "Loading..."
  , viz_stackedBar: "Loading..."
  , viz_chordDiagram: "Loading..."
  , viz_sankey: "Loading..."
  , viz_tree: "Loading..."
  , viz_bubbleChart: "Loading..."
  , viz_icicle: "Loading..."
  , viz_treemap: "Loading..."
  , viz_gup: "Loading..."
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div [ HP.class_ (HH.ClassName "mermaid-diagrams") ]
    [ HH.h1_ [ HH.text "PSD3 MermaidJS AST Visualizations" ]
    , HH.p_
        [ HH.text "The MermaidAST interpreter visualizes PSD3 code structure as flowcharts. "
        , HH.text "Each diagram shows operations (nodes) and how selections flow between them (edges)."
        ]

    , HH.section [ HP.class_ (HH.ClassName "instructions") ]
        [ HH.h3_ [ HH.text "How to Read These Diagrams" ]
        , HH.ul_
            [ HH.li_ [ HH.text "Rectangles represent D3 operations (select, append, join, attr, etc.)" ]
            , HH.li_ [ HH.text "Arrows show how selections flow from one operation to the next" ]
            , HH.li_ [ HH.text "Edge labels describe the relationship (e.g., 'append', 'data', 'join')" ]
            , HH.li_ [ HH.text "These show code structure, not the visual output" ]
            ]
        ]

    , HH.section [ HP.class_ (HH.ClassName "example") ]
        [ HH.h2_ [ HH.text "1. Simple Append" ]
        , HH.p_ [ HH.text "Basic pattern: attach to DOM, append an element with attributes" ]
        , mermaidDiagram state.test1_simpleAppend (Just "test1")
        ]

    , HH.section [ HP.class_ (HH.ClassName "example") ]
        [ HH.h2_ [ HH.text "2. Multiple Appends" ]
        , HH.p_ [ HH.text "Sequential element creation in a group" ]
        , mermaidDiagram state.test2_multipleAppends (Just "test2")
        ]

    , HH.section [ HP.class_ (HH.ClassName "example") ]
        [ HH.h2_ [ HH.text "3. SelectUnder and Data Join" ]
        , HH.p_ [ HH.text "Select existing elements and bind data with simpleJoin" ]
        , mermaidDiagram state.test3_selectUnderJoin (Just "test3")
        ]

    , HH.section [ HP.class_ (HH.ClassName "example") ]
        [ HH.h2_ [ HH.text "4. Update Pattern (Enter/Update/Exit)" ]
        , HH.p_ [ HH.text "The classic D3 General Update Pattern with enter, update, and exit selections" ]
        , mermaidDiagram state.test4_updatePattern (Just "test4")
        ]

    , HH.section [ HP.class_ (HH.ClassName "example") ]
        [ HH.h2_ [ HH.text "5. Filter Selection" ]
        , HH.p_ [ HH.text "Filtering a selection by selector" ]
        , mermaidDiagram state.test5_filterSelection (Just "test5")
        ]

    , HH.section [ HP.class_ (HH.ClassName "example") ]
        [ HH.h2_ [ HH.text "6. Merge Selections" ]
        , HH.p_ [ HH.text "Combining two selections into one" ]
        , mermaidDiagram state.test6_mergeSelections (Just "test6")
        ]

    , HH.section [ HP.class_ (HH.ClassName "example") ]
        [ HH.h2_ [ HH.text "7. Nested Groups" ]
        , HH.p_ [ HH.text "Deep nesting of group elements" ]
        , mermaidDiagram state.test7_nestedGroups (Just "test7")
        ]

    , HH.section [ HP.class_ (HH.ClassName "example") ]
        [ HH.h2_ [ HH.text "8. Mixed Element Types" ]
        , HH.p_ [ HH.text "Creating various SVG elements (Rect, Circle, Line, Text)" ]
        , mermaidDiagram state.test8_mixedElements (Just "test8")
        ]

    , HH.section [ HP.class_ (HH.ClassName "example") ]
        [ HH.h2_ [ HH.text "9. Open Selection (Nested Join)" ]
        , HH.p_ [ HH.text "Opening a selection for nested data joins" ]
        , mermaidDiagram state.test9_openSelection (Just "test9")
        ]

    , HH.section [ HP.class_ (HH.ClassName "example") ]
        [ HH.h2_ [ HH.text "10. Attributes Only" ]
        , HH.p_ [ HH.text "Setting multiple attributes on an element" ]
        , mermaidDiagram state.test10_attributesOnly (Just "test10")
        ]

    , HH.section [ HP.class_ (HH.ClassName "example") ]
        [ HH.h2_ [ HH.text "11. Transitions" ]
        , HH.p_ [ HH.text "Animating attribute changes with transitions" ]
        , mermaidDiagram state.test11_transitions (Just "test11")
        ]

    -- Concrete Visualization Examples
    , HH.h1 [ HP.class_ (HH.ClassName "section-break") ]
        [ HH.text "Concrete Visualizations" ]
    , HH.p_
        [ HH.text "These diagrams show the AST structure of real visualization examples from the PSD3 library." ]

    , HH.section [ HP.class_ (HH.ClassName "example") ]
        [ HH.h2_ [ HH.text "Parabola" ]
        , HH.p_ [ HH.text "Classic Three Little Circles pattern with data-driven positioning" ]
        , mermaidDiagram state.viz_parabola (Just "viz-parabola")
        ]

    , HH.section [ HP.class_ (HH.ClassName "example") ]
        [ HH.h2_ [ HH.text "Multi-Line Chart" ]
        , HH.p_ [ HH.text "Time-series visualization with multiple data series as paths" ]
        , mermaidDiagram state.viz_multiLineChart (Just "viz-multiline")
        ]

    , HH.section [ HP.class_ (HH.ClassName "example") ]
        [ HH.h2_ [ HH.text "Radial Stacked Bar Chart" ]
        , HH.p_ [ HH.text "Circular bar chart with radial layout and stacked segments" ]
        , mermaidDiagram state.viz_stackedBar (Just "viz-stackedbar")
        ]

    , HH.section [ HP.class_ (HH.ClassName "example") ]
        [ HH.h2_ [ HH.text "Chord Diagram" ]
        , HH.p_ [ HH.text "Circular dependency visualization with ribbons and arc groups" ]
        , mermaidDiagram state.viz_chordDiagram (Just "viz-chord")
        ]

    , HH.section [ HP.class_ (HH.ClassName "example") ]
        [ HH.h2_ [ HH.text "Sankey Diagram" ]
        , HH.p_ [ HH.text "Flow diagram with nodes, links, and labels showing value flows" ]
        , mermaidDiagram state.viz_sankey (Just "viz-sankey")
        ]

    , HH.section [ HP.class_ (HH.ClassName "example") ]
        [ HH.h2_ [ HH.text "Tree (Vertical)" ]
        , HH.p_ [ HH.text "Hierarchical tree layout with links and node groups" ]
        , mermaidDiagram state.viz_tree (Just "viz-tree")
        ]

    , HH.section [ HP.class_ (HH.ClassName "example") ]
        [ HH.h2_ [ HH.text "Bubble Chart" ]
        , HH.p_ [ HH.text "Circular packing layout showing hierarchical data as nested circles" ]
        , mermaidDiagram state.viz_bubbleChart (Just "viz-bubble")
        ]

    , HH.section [ HP.class_ (HH.ClassName "example") ]
        [ HH.h2_ [ HH.text "Icicle Chart" ]
        , HH.p_ [ HH.text "Partition layout showing hierarchy as horizontal rectangles" ]
        , mermaidDiagram state.viz_icicle (Just "viz-icicle")
        ]

    , HH.section [ HP.class_ (HH.ClassName "example") ]
        [ HH.h2_ [ HH.text "Treemap" ]
        , HH.p_ [ HH.text "Space-filling treemap with nested rectangular tiles" ]
        , mermaidDiagram state.viz_treemap (Just "viz-treemap")
        ]

    , HH.section [ HP.class_ (HH.ClassName "example") ]
        [ HH.h2_ [ HH.text "General Update Pattern (GUP)" ]
        , HH.p_ [ HH.text "The classic D3 pattern showing enter/update/exit with transitions and openSelection" ]
        , mermaidDiagram state.viz_gup (Just "viz-gup")
        ]
    ]

handleAction :: forall o m. MonadEffect m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    -- Generate Mermaid diagrams from PSD3 code
    test1 <- liftEffect $ runMermaidAST test1_SimpleAppend
    test2 <- liftEffect $ runMermaidAST test2_MultipleAppends
    test3 <- liftEffect $ runMermaidAST test3_SelectUnderJoin
    test4 <- liftEffect $ runMermaidAST test4_UpdatePattern
    test5 <- liftEffect $ runMermaidAST test5_FilterSelection
    test6 <- liftEffect $ runMermaidAST test6_MergeSelections
    test7 <- liftEffect $ runMermaidAST test7_NestedGroups
    test8 <- liftEffect $ runMermaidAST test8_MixedElements
    test9 <- liftEffect $ runMermaidAST test9_OpenSelection
    test10 <- liftEffect $ runMermaidAST test10_AttributesOnly
    test11 <- liftEffect $ runMermaidAST test11_Transitions

    -- Generate concrete visualization examples
    vizParabola <- liftEffect $ runMermaidAST viz_ParabolaAST
    vizMultiLine <- liftEffect $ runMermaidAST viz_MultiLineChartAST
    vizStackedBar <- liftEffect $ runMermaidAST viz_StackedBarAST
    vizChord <- liftEffect $ runMermaidAST viz_ChordDiagramAST
    vizSankey <- liftEffect $ runMermaidAST viz_SankeyAST
    vizTree <- liftEffect $ runMermaidAST viz_TreeAST
    vizBubble <- liftEffect $ runMermaidAST viz_BubbleChartAST
    vizIcicle <- liftEffect $ runMermaidAST viz_IcicleAST
    vizTreemap <- liftEffect $ runMermaidAST viz_TreemapAST
    vizGup <- liftEffect $ runMermaidAST viz_GUPAST

    H.modify_ _
      { test1_simpleAppend = test1
      , test2_multipleAppends = test2
      , test3_selectUnderJoin = test3
      , test4_updatePattern = test4
      , test5_filterSelection = test5
      , test6_mergeSelections = test6
      , test7_nestedGroups = test7
      , test8_mixedElements = test8
      , test9_openSelection = test9
      , test10_attributesOnly = test10
      , test11_transitions = test11
      , viz_parabola = vizParabola
      , viz_multiLineChart = vizMultiLine
      , viz_stackedBar = vizStackedBar
      , viz_chordDiagram = vizChord
      , viz_sankey = vizSankey
      , viz_tree = vizTree
      , viz_bubbleChart = vizBubble
      , viz_icicle = vizIcicle
      , viz_treemap = vizTreemap
      , viz_gup = vizGup
      }

    -- Trigger Mermaid rendering after diagrams are in the DOM
    triggerMermaidRendering

-- Test 1: Simple selection and append
test1_SimpleAppend :: forall d. MermaidASTM (MermaidSelection d)
test1_SimpleAppend = do
  svg <- D3.attach "svg"
  circle <- D3.appendTo svg Circle
    [ cx 100.0
    , cy 100.0
    , radius 50.0
    , fill "red"
    ]
  pure circle

-- Test 2: Multiple appends in sequence
test2_MultipleAppends :: forall d. MermaidASTM (MermaidSelection d)
test2_MultipleAppends = do
  svg <- D3.attach "svg"
  g <- D3.appendTo svg Group []
  circle1 <- D3.appendTo g Circle [cx 50.0, cy 50.0, radius 20.0]
  circle2 <- D3.appendTo g Circle [cx 100.0, cy 50.0, radius 20.0]
  circle3 <- D3.appendTo g Circle [cx 150.0, cy 50.0, radius 20.0]
  pure circle3

-- Test 3: SelectUnder and data join
test3_SelectUnderJoin :: forall d. MermaidASTM (MermaidSelection d)
test3_SelectUnderJoin = do
  svg <- D3.attach "svg"
  circles <- D3.selectUnder svg "circle"
  joined <- D3.simpleJoin circles Circle [1, 2, 3, 4, 5] unsafeCoerce
  D3.setAttributes joined
    [ radius 30.0
    , fill "blue"
    ]
  pure joined

-- Test 4: Update pattern (enter/exit)
test4_UpdatePattern :: forall d. MermaidASTM (MermaidSelection d)
test4_UpdatePattern = do
  svg <- D3.attach "svg"
  circles <- D3.selectUnder svg "circle"
  { enter, update, exit } <- D3.updateJoin circles Circle [10, 20, 30] unsafeCoerce

  D3.setAttributes enter [fill "green", radius 15.0]
  D3.setAttributes update [fill "blue"]

  pure enter

-- Test 5: Filter selection
test5_FilterSelection :: forall d. MermaidASTM (MermaidSelection d)
test5_FilterSelection = do
  svg <- D3.attach "svg"
  allCircles <- D3.selectUnder svg "circle"
  filtered <- D3.filterSelection allCircles ".active"
  D3.setAttributes filtered [fill "orange"]
  pure filtered

-- Test 6: Merge selections
test6_MergeSelections :: forall d. MermaidASTM (MermaidSelection d)
test6_MergeSelections = do
  svg <- D3.attach "svg"
  circles1 <- D3.selectUnder svg ".group1"
  circles2 <- D3.selectUnder svg ".group2"
  merged <- D3.mergeSelections circles1 circles2
  D3.setAttributes merged [fill "purple"]
  pure merged

-- Test 7: Nested groups
test7_NestedGroups :: forall d. MermaidASTM (MermaidSelection d)
test7_NestedGroups = do
  svg <- D3.attach "svg"
  g1 <- D3.appendTo svg Group []
  g2 <- D3.appendTo g1 Group []
  g3 <- D3.appendTo g2 Group []
  rect <- D3.appendTo g3 Rect
    [ x 10.0
    , y 10.0
    , width 100.0
    , height 50.0
    ]
  pure rect

-- Test 8: Mixed element types
test8_MixedElements :: forall d. MermaidASTM (MermaidSelection d)
test8_MixedElements = do
  svg <- D3.attach "svg"
  rect <- D3.appendTo svg Rect [width 200.0, height 100.0, fill "lightblue"]
  circle <- D3.appendTo svg Circle [cx 100.0, cy 50.0, radius 40.0, fill "pink"]
  line <- D3.appendTo svg Line [strokeColor "black", strokeWidth 2.0]
  text <- D3.appendTo svg Text []
  pure text

-- Test 9: Open selection (nested join)
test9_OpenSelection :: forall d. MermaidASTM (MermaidSelection d)
test9_OpenSelection = do
  svg <- D3.attach "svg"
  groups <- D3.simpleJoin svg Group [1, 2, 3] unsafeCoerce
  opened <- D3.openSelection groups "circle"
  pure opened

-- Test 10: Attributes only (no joins)
test10_AttributesOnly :: forall d. MermaidASTM (MermaidSelection d)
test10_AttributesOnly = do
  svg <- D3.attach "svg"
  circle <- D3.appendTo svg Circle []
  D3.setAttributes circle
    [ cx 100.0
    , cy 100.0
    , radius 50.0
    , fill "yellow"
    , strokeColor "black"
    , strokeWidth 3.0
    ]
  pure circle

-- Test 11: Transitions
test11_Transitions :: forall d. MermaidASTM (MermaidSelection d)
test11_Transitions = do
  svg <- D3.attach "svg"
  circle <- D3.appendTo svg Circle [cx 50.0, cy 50.0, radius 20.0, fill "red"]
  let transition = transitionWithDuration $ Milliseconds 1000.0
  D3.setAttributes circle $
    transition `to`
      [ cx 150.0
      , cy 150.0
      , radius 40.0
      , fill "blue"
      ]
  pure circle

-- ============================================================================
-- CONCRETE VISUALIZATION EXAMPLES
-- ============================================================================

-- Parabola (Three Little Circles with data-driven positioning)
viz_ParabolaAST :: forall d. MermaidASTM (MermaidSelection d)
viz_ParabolaAST = do
  root <- D3.attach "div"
  svg <- D3.appendTo root Svg []
  circleGroup <- D3.appendTo svg Group []
  circles <- D3.simpleJoin circleGroup Circle [32, 57, 112] unsafeCoerce
  D3.setAttributes circles
    [ strokeColor "steelblue"
    , strokeWidth 3.0
    , fill "none"
    , cx 100.0
    , cy 50.0
    , radius 10.0
    ]
  pure circles

-- Multi-Line Chart (time-series with multiple paths)
viz_MultiLineChartAST :: forall d. MermaidASTM (MermaidSelection d)
viz_MultiLineChartAST = do
  root <- D3.attach "div"
  svg <- D3.appendTo root Svg []
  chartGroup <- D3.appendTo svg Group []

  -- Draw y-axis
  yAxis <- D3.appendTo chartGroup Group []
  yTick1 <- D3.appendTo yAxis Text [y 0.0, x (-5.0), fill "black"]
  yTick2 <- D3.appendTo yAxis Text [y 100.0, x (-5.0), fill "black"]

  -- Draw x-axis
  xAxis <- D3.appendTo chartGroup Group []
  xTick1 <- D3.appendTo xAxis Text [x 0.0, y 20.0, fill "black"]
  xTick2 <- D3.appendTo xAxis Text [x 200.0, y 20.0, fill "black"]

  -- Draw line paths for each series
  path1 <- D3.appendTo chartGroup Path [strokeColor "#4682b4", strokeWidth 1.5, fill "none"]
  path2 <- D3.appendTo chartGroup Path [strokeColor "#4682b4", strokeWidth 1.5, fill "none"]
  path3 <- D3.appendTo chartGroup Path [strokeColor "#4682b4", strokeWidth 1.5, fill "none"]

  pure path3

-- Radial Stacked Bar Chart
viz_StackedBarAST :: forall d. MermaidASTM (MermaidSelection d)
viz_StackedBarAST = do
  root <- D3.attach "div"
  svg <- D3.appendTo root Svg []
  centerGroup <- D3.appendTo svg Group []

  -- Create bars with data join
  bars <- D3.simpleJoin centerGroup Path [1, 2, 3, 4, 5] unsafeCoerce
  D3.setAttributes bars
    [ fill "orange"
    , strokeColor "#ffffff"
    , strokeWidth 1.0
    ]

  -- Add labels
  labels <- D3.simpleJoin centerGroup Text [1, 2, 3, 4, 5] unsafeCoerce
  D3.setAttributes labels [fill "white", x 0.0, y 0.0]

  pure labels

-- Chord Diagram (circular dependency viz)
viz_ChordDiagramAST :: forall d. MermaidASTM (MermaidSelection d)
viz_ChordDiagramAST = do
  root <- D3.attach "div"
  svg <- D3.appendTo root Svg []
  centerGroup <- D3.appendTo svg Group []

  -- Draw ribbons (the chords)
  ribbonsGroup <- D3.appendTo centerGroup Group []
  ribbons <- D3.simpleJoin ribbonsGroup Path [1, 2, 3, 4, 5] unsafeCoerce
  D3.setAttributes ribbons
    [ fill "#e74c3c"
    , fillOpacity 0.67
    , strokeColor "#000000"
    , strokeWidth 0.5
    ]

  -- Draw arcs (the groups)
  arcsGroup <- D3.appendTo centerGroup Group []
  arcGroups <- D3.simpleJoin arcsGroup Group [1, 2, 3, 4, 5] unsafeCoerce
  arcPaths <- D3.appendTo arcGroups Path
    [ fill "#3498db"
    , strokeColor "#ffffff"
    , strokeWidth 2.0
    ]

  pure arcPaths

-- Sankey Diagram (flow diagram)
viz_SankeyAST :: forall d. MermaidASTM (MermaidSelection d)
viz_SankeyAST = do
  root <- D3.attach "div"
  svg <- D3.appendTo root Svg []

  -- Create groups for links, nodes, and labels
  linksGroup <- D3.appendTo svg Group []
  nodesGroup <- D3.appendTo svg Group []
  labelsGroup <- D3.appendTo svg Group []

  -- Join and render links
  links <- D3.simpleJoin linksGroup Path [1, 2, 3, 4] unsafeCoerce
  D3.setAttributes links
    [ fill "none"
    , strokeWidth 5.0
    , strokeOpacity 0.5
    , strokeColor "#aaa"
    ]

  -- Join and render nodes
  nodes <- D3.simpleJoin nodesGroup Rect [1, 2, 3] unsafeCoerce
  D3.setAttributes nodes
    [ x 0.0
    , y 0.0
    , width 20.0
    , height 50.0
    , fill "steelblue"
    , fillOpacity 0.8
    ]

  -- Add labels
  labels <- D3.simpleJoin labelsGroup Text [1, 2, 3] unsafeCoerce
  D3.setAttributes labels [x 25.0, y 25.0, fill "black"]

  pure labels

-- Tree (Vertical hierarchical layout)
viz_TreeAST :: forall d. MermaidASTM (MermaidSelection d)
viz_TreeAST = do
  root <- D3.attach "div"
  svg <- D3.appendTo root Svg []
  zoomGroup <- D3.appendTo svg Group []

  -- Create groups for links and nodes
  linksGroup <- D3.appendTo zoomGroup Group []
  nodesGroup <- D3.appendTo zoomGroup Group []

  -- Draw links (paths between nodes)
  links <- D3.simpleJoin linksGroup Path [1, 2, 3, 4, 5] unsafeCoerce
  D3.setAttributes links
    [ strokeWidth 1.5
    , strokeColor "#94a3b8"
    , strokeOpacity 0.6
    , fill "none"
    ]

  -- Draw node groups (circles + labels)
  nodeGroups <- D3.simpleJoin nodesGroup Group [1, 2, 3, 4, 5, 6] unsafeCoerce
  circles <- D3.appendTo nodeGroups Circle
    [ fill "#0ea5e9"
    , radius 3.0
    , strokeColor "white"
    , strokeWidth 1.5
    , cx 0.0
    , cy 0.0
    ]
  labels <- D3.appendTo nodeGroups Text
    [ fill "#0c4a6e"
    , x 8.0
    , y 0.0
    ]

  pure labels

-- Bubble Chart (circle pack hierarchy)
viz_BubbleChartAST :: forall d. MermaidASTM (MermaidSelection d)
viz_BubbleChartAST = do
  root <- D3.attach "div"
  svg <- D3.appendTo root Svg []
  chartGroup <- D3.appendTo svg Group []

  -- Draw bubbles (circles with hierarchical data)
  bubbles <- D3.simpleJoin chartGroup Circle [1, 2, 3, 4, 5, 6, 7] unsafeCoerce
  D3.setAttributes bubbles
    [ cx 100.0
    , cy 100.0
    , radius 30.0
    , fill "#e8dcc6"
    , fillOpacity 0.8
    , strokeColor "#ffffff"
    , strokeWidth 2.0
    ]

  -- Draw labels
  labels <- D3.simpleJoin chartGroup Text [1, 2, 3, 4, 5, 6, 7] unsafeCoerce
  D3.setAttributes labels
    [ x 100.0
    , y 100.0
    , fill "#ffffff"
    ]

  pure labels

-- Icicle Chart (partition layout)
viz_IcicleAST :: forall d. MermaidASTM (MermaidSelection d)
viz_IcicleAST = do
  root <- D3.attach "div"
  svg <- D3.appendTo root Svg []
  chartGroup <- D3.appendTo svg Group []

  -- Draw partitions (rectangles)
  partitions <- D3.simpleJoin chartGroup Rect [1, 2, 3, 4, 5, 6] unsafeCoerce
  D3.setAttributes partitions
    [ x 0.0
    , y 0.0
    , width 150.0
    , height 100.0
    , fill "#d4c4b0"
    , fillOpacity 0.85
    , strokeColor "#ffffff"
    , strokeWidth 2.0
    ]

  -- Draw labels
  labels <- D3.simpleJoin chartGroup Text [1, 2, 3, 4, 5, 6] unsafeCoerce
  D3.setAttributes labels
    [ x 4.0
    , y 50.0
    , fill "#ffffff"
    ]

  pure labels

-- Treemap (space-filling hierarchy)
viz_TreemapAST :: forall d. MermaidASTM (MermaidSelection d)
viz_TreemapAST = do
  root <- D3.attach "div"
  svg <- D3.appendTo root Svg []
  chartGroup <- D3.appendTo svg Group []

  -- Draw tiles (rectangles)
  tiles <- D3.simpleJoin chartGroup Rect [1, 2, 3, 4, 5, 6, 7] unsafeCoerce
  D3.setAttributes tiles
    [ x 0.0
    , y 0.0
    , width 120.0
    , height 80.0
    , fill "#d4c4b0"
    , fillOpacity 0.85
    , strokeColor "#ffffff"
    , strokeWidth 2.0
    ]

  -- Draw labels
  tileLabels <- D3.simpleJoin chartGroup Text [1, 2, 3, 4, 5, 6, 7] unsafeCoerce
  D3.setAttributes tileLabels
    [ x 2.0
    , y 12.0
    , fill "#ffffff"
    ]

  pure tileLabels

-- General Update Pattern (GUP)
viz_GUPAST :: forall d. MermaidASTM (MermaidSelection d)
viz_GUPAST = do
  root <- D3.attach "div"
  svg <- D3.appendTo root Svg [viewBox 0.0 100.0 800.0 350.0, classed "d3svg gup"]
  letterGroup <- D3.appendTo svg Group []

  -- Simulate the updateJoin call (this is what gets called repeatedly)
  enterSelection <- D3.openSelection letterGroup "text"
  { enter, update, exit } <- D3.updateJoin enterSelection Text [1, 2, 3] unsafeCoerce

  -- Set attributes on exit selection with transition
  let transition = transitionWithDuration $ Milliseconds 2000.0
  let exitAttrs = [classed "exit", fill "brown"] `andThen` (transition `to` [y 400.0, remove])
  D3.setAttributes exit exitAttrs

  -- Set attributes on update selection with transition
  let updateAttrs = [classed "update", fill "gray", y 200.0] `andThen` (transition `to` [x 50.0])
  D3.setAttributes update updateAttrs

  -- Append new text elements to enter selection
  newlyEntered <- D3.appendTo enter Text []
  let enterAttrs = [ classed "enter"
                    , fill "green"
                    , x 50.0
                    , y 0.0
                    , fontSize 60.0
                    ] `andThen` (transition `to` [y 200.0])
  D3.setAttributes newlyEntered enterAttrs

  pure newlyEntered
