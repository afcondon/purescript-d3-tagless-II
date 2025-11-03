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
import PSD3.Attributes (fill, radius, cx, cy, width, height, strokeWidth, strokeColor, x, y, to, transitionWithDuration)
import PSD3.Types (Element(..))
import PSD3.Interpreter.MermaidAST (MermaidASTM, runMermaidAST)
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
      }

    -- Trigger Mermaid rendering after diagrams are in the DOM
    triggerMermaidRendering

-- Test 1: Simple selection and append
test1_SimpleAppend :: MermaidASTM NodeID
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
test2_MultipleAppends :: MermaidASTM NodeID
test2_MultipleAppends = do
  svg <- D3.attach "svg"
  g <- D3.appendTo svg Group []
  circle1 <- D3.appendTo g Circle [cx 50.0, cy 50.0, radius 20.0]
  circle2 <- D3.appendTo g Circle [cx 100.0, cy 50.0, radius 20.0]
  circle3 <- D3.appendTo g Circle [cx 150.0, cy 50.0, radius 20.0]
  pure circle3

-- Test 3: SelectUnder and data join
test3_SelectUnderJoin :: MermaidASTM NodeID
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
test4_UpdatePattern :: MermaidASTM NodeID
test4_UpdatePattern = do
  svg <- D3.attach "svg"
  circles <- D3.selectUnder svg "circle"
  { enter, update, exit } <- D3.updateJoin circles Circle [10, 20, 30] unsafeCoerce

  D3.setAttributes enter [fill "green", radius 15.0]
  D3.setAttributes update [fill "blue"]

  pure enter

-- Test 5: Filter selection
test5_FilterSelection :: MermaidASTM NodeID
test5_FilterSelection = do
  svg <- D3.attach "svg"
  allCircles <- D3.selectUnder svg "circle"
  filtered <- D3.filterSelection allCircles ".active"
  D3.setAttributes filtered [fill "orange"]
  pure filtered

-- Test 6: Merge selections
test6_MergeSelections :: MermaidASTM NodeID
test6_MergeSelections = do
  svg <- D3.attach "svg"
  circles1 <- D3.selectUnder svg ".group1"
  circles2 <- D3.selectUnder svg ".group2"
  merged <- D3.mergeSelections circles1 circles2
  D3.setAttributes merged [fill "purple"]
  pure merged

-- Test 7: Nested groups
test7_NestedGroups :: MermaidASTM NodeID
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
test8_MixedElements :: MermaidASTM NodeID
test8_MixedElements = do
  svg <- D3.attach "svg"
  rect <- D3.appendTo svg Rect [width 200.0, height 100.0, fill "lightblue"]
  circle <- D3.appendTo svg Circle [cx 100.0, cy 50.0, radius 40.0, fill "pink"]
  line <- D3.appendTo svg Line [strokeColor "black", strokeWidth 2.0]
  text <- D3.appendTo svg Text []
  pure text

-- Test 9: Open selection (nested join)
test9_OpenSelection :: MermaidASTM NodeID
test9_OpenSelection = do
  svg <- D3.attach "svg"
  groups <- D3.simpleJoin svg Group [1, 2, 3] unsafeCoerce
  opened <- D3.openSelection groups "circle"
  pure opened

-- Test 10: Attributes only (no joins)
test10_AttributesOnly :: MermaidASTM NodeID
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
test11_Transitions :: MermaidASTM NodeID
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
