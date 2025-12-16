module Test.SemiQuine.TreeToCodeSpec where

import Prelude

import Data.String (contains, Pattern(..))
import Effect (Effect)
import Effect.Console (log)
import PSD3.Internal.Attribute (cx, cy, radius, fill, width, height, id_, class_)
import PSD3.Interpreter.SemiQuine.TreeToCode (treeToCode)
import PSD3.Internal.Selection.Types (ElementType(..))
import PSD3.AST (Tree)
import PSD3.AST as T
import Test.Assert (assert')

-- | Sample data type for parabola example
type ParabolaPoint = { x :: Number, y :: Number }

-- | Sample data - parabola points
parabolaData :: Array ParabolaPoint
parabolaData =
  [ { x: -10.0, y: 100.0 }
  , { x: -5.0, y: 25.0 }
  , { x: 0.0, y: 0.0 }
  , { x: 5.0, y: 25.0 }
  , { x: 10.0, y: 100.0 }
  ]

-- | Simple linear scale for testing
scaleX :: Number -> Number
scaleX x = (x + 10.0) * 20.0  -- Maps -10..10 to 0..400

scaleY :: Number -> Number
scaleY y = 300.0 - (y * 2.5)  -- Maps 0..100 to 300..50 (inverted)

-- | Parabola tree - similar to the real example
parabolaTree :: Tree ParabolaPoint
parabolaTree =
  T.named SVG "svg"
    [ width 400.0
    , height 300.0
    , id_ "parabola-svg"
    , class_ "test-example"
    ]
    `T.withChild`
      (T.joinData "circles" "circle" parabolaData $ \d ->
        T.elem Circle
          [ cx (scaleX d.x)
          , cy (scaleY d.y)
          , radius 5.0
          , fill "green"
          ])

-- | Static-only tree for comparison
staticTree :: Tree Unit
staticTree =
  T.named SVG "root"
    [ width 200.0
    , height 100.0
    ]
    `T.withChild`
      T.elem Circle
        [ cx 50.0
        , cy 50.0
        , radius 10.0
        , fill "blue"
        ]

-- | Helper to check if string contains pattern
shouldContain :: String -> String -> Effect Unit
shouldContain haystack needle =
  assert' ("Expected to find '" <> needle <> "' in output:\n" <> haystack)
    (contains (Pattern needle) haystack)

-- | Run all tests
runTests :: Effect Unit
runTests = do
  log "\n--- Static tree tests ---"
  testStaticElementTypes
  testStaticAttributes
  testWithChild

  log "\n--- Data-driven tree tests ---"
  testJoinData
  testDynamicAttributes
  testStaticAttributesInTemplate

  log "\n--- Sample data evaluation tests ---"
  testEvaluatedValues

  log "\n--- Full output inspection ---"
  inspectParabolaOutput

-- | Test: generates correct element types
testStaticElementTypes :: Effect Unit
testStaticElementTypes = do
  log "  ✓ generates correct element types"
  let code = treeToCode staticTree
  code `shouldContain` "T.named SVG"
  code `shouldContain` "T.elem Circle"

-- | Test: generates correct static attributes
testStaticAttributes :: Effect Unit
testStaticAttributes = do
  log "  ✓ generates correct static attributes"
  let code = treeToCode staticTree
  code `shouldContain` "width 200.0"
  code `shouldContain` "height 100.0"
  code `shouldContain` "cx 50.0"
  code `shouldContain` "radius 10.0"
  code `shouldContain` "fill \"blue\""

-- | Test: generates withChild for single child
testWithChild :: Effect Unit
testWithChild = do
  log "  ✓ generates withChild for single child"
  let code = treeToCode staticTree
  code `shouldContain` "`T.withChild`"

-- | Test: generates joinData for data joins
testJoinData :: Effect Unit
testJoinData = do
  log "  ✓ generates joinData for data joins"
  let code = treeToCode parabolaTree
  code `shouldContain` "T.joinData \"circles\""

-- | Test: template evaluation produces concrete values
-- | Note: When template is evaluated with first datum, DataAttr becomes StaticAttr
-- | So we see `cx 0.0` not `cx d.<?>` - the function has been applied!
testDynamicAttributes :: Effect Unit
testDynamicAttributes = do
  log "  ✓ template evaluation produces concrete values (cx 0.0, cy 50.0)"
  let code = treeToCode parabolaTree
  -- First datum is { x: -10.0, y: 100.0 }
  -- scaleX (-10.0) = 0.0, scaleY 100.0 = 50.0
  code `shouldContain` "cx 0.0"
  code `shouldContain` "cy 50.0"

-- | Test: preserves static attributes in templates
testStaticAttributesInTemplate :: Effect Unit
testStaticAttributesInTemplate = do
  log "  ✓ preserves static attributes in templates"
  let code = treeToCode parabolaTree
  -- radius and fill are static
  code `shouldContain` "radius 5.0"
  code `shouldContain` "fill \"green\""

-- | Test: join structure with data count
testEvaluatedValues :: Effect Unit
testEvaluatedValues = do
  log "  ✓ generates data join with template"
  let code = treeToCode parabolaTree
  -- Should have the join and the evaluated template
  code `shouldContain` "T.joinData"
  code `shouldContain` "$ \\d ->"
  code `shouldContain` "T.elem Circle"

-- | Inspect: print the full output for manual review
inspectParabolaOutput :: Effect Unit
inspectParabolaOutput = do
  log "\n--- Generated code for parabola tree ---"
  let code = treeToCode parabolaTree
  log code
  log "--- End generated code ---\n"
