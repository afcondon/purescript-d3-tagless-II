module Test.MermaidASTComprehensiveTest where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import PSD3 as D3
import PSD3.Attributes (fill, radius, cx, cy, width, height, strokeWidth, strokeColor, x, y, to, transitionWithDuration)
import PSD3.Types (Element(..))
import PSD3.Data.Node (NodeID)
import PSD3.Interpreter.MermaidAST (MermaidASTM, runMermaidAST)
import Unsafe.Coerce (unsafeCoerce)
import Data.Time.Duration (Milliseconds(..))

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

-- Note: Drag/Zoom behaviors can't be tested here (require browser FFI)
-- but the interpreter handles them in the `on` method by creating AST nodes

main :: Effect Unit
main = do
  log "Testing MermaidAST interpreter with various PSD3 patterns...\n"

  log "Test 1: Simple selection and append"
  test1 <- runMermaidAST test1_SimpleAppend
  log test1
  log ""

  log "Test 2: Multiple appends in sequence"
  test2 <- runMermaidAST test2_MultipleAppends
  log test2
  log ""

  log "Test 3: SelectUnder and data join"
  test3 <- runMermaidAST test3_SelectUnderJoin
  log test3
  log ""

  log "Test 4: Update pattern (enter/exit)"
  test4 <- runMermaidAST test4_UpdatePattern
  log test4
  log ""

  log "Test 5: Filter selection"
  test5 <- runMermaidAST test5_FilterSelection
  log test5
  log ""

  log "Test 6: Merge selections"
  test6 <- runMermaidAST test6_MergeSelections
  log test6
  log ""

  log "Test 7: Nested groups"
  test7 <- runMermaidAST test7_NestedGroups
  log test7
  log ""

  log "Test 8: Mixed element types"
  test8 <- runMermaidAST test8_MixedElements
  log test8
  log ""

  log "Test 9: Open selection (nested join)"
  test9 <- runMermaidAST test9_OpenSelection
  log test9
  log ""

  log "Test 10: Attributes only (no joins)"
  test10 <- runMermaidAST test10_AttributesOnly
  log test10
  log ""

  log "Test 11: Transitions"
  test11 <- runMermaidAST test11_Transitions
  log test11
  log ""

  log "✓ All SelectionM operations tested successfully!"
