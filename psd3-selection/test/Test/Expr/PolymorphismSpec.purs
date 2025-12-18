-- | Tests for Interpreter Polymorphism
-- |
-- | Demonstrates the key feature of finally-tagless encoding:
-- | The SAME expression produces different outputs depending on interpreter.
module Test.Expr.PolymorphismSpec where

import Prelude hiding (add, sub, mul, div)

import Data.String as Data.String
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assert')

-- DSL imports
import PSD3.Expr.Expr (class NumExpr, class TrigExpr, class StringExpr, class BoolExpr, class CompareExpr, lit, add, mul, sub, div, sin, cos, pi, str, concat, ifThenElse, lt)
import PSD3.Expr.Path (class PathExpr, linePath, linkHorizontal)

-- Interpreter imports
import PSD3.Expr.Interpreter.Eval (Eval, runEval)
import PSD3.Expr.Interpreter.CodeGen (CodeGen, runCodeGen)
import PSD3.Expr.Interpreter.SVG (SVG, runSVG)

-- =============================================================================
-- Polymorphic Expressions (same expression, multiple interpreters!)
-- =============================================================================

-- | A basic arithmetic expression
-- | 2 * 3 + 10 = 16
basicArithmetic :: forall repr. NumExpr repr => repr Number
basicArithmetic = add (mul (lit 2.0) (lit 3.0)) (lit 10.0)

-- | A more complex arithmetic expression
-- | (100 - 25) / 5 = 15
complexArithmetic :: forall repr. NumExpr repr => repr Number
complexArithmetic = div (sub (lit 100.0) (lit 25.0)) (lit 5.0)

-- | A trigonometric expression
-- | sin(pi/4) ≈ 0.707...
trigExpr :: forall repr. NumExpr repr => TrigExpr repr => repr Number
trigExpr = sin (div pi (lit 4.0))

-- | Another trig expression: cos(0) + sin(pi/2) = 2
trigCombo :: forall repr. NumExpr repr => TrigExpr repr => repr Number
trigCombo = add (cos (lit 0.0)) (sin (div pi (lit 2.0)))

-- | A conditional expression
-- | if 5 < 10 then 100 else 200 = 100
conditionalExpr :: forall repr. NumExpr repr => BoolExpr repr => CompareExpr repr => repr Number
conditionalExpr = ifThenElse (lt (lit 5.0) (lit 10.0)) (lit 100.0) (lit 200.0)

-- | String concatenation
-- | "Hello, " ++ "World!" = "Hello, World!"
stringConcat :: forall repr. StringExpr repr => repr String
stringConcat = concat (str "Hello, ") (str "World!")

-- | A path expression
-- | Line from (10, 20) to (100, 80)
simplePath :: forall repr. NumExpr repr => PathExpr repr => repr String
simplePath = linePath (lit 10.0) (lit 20.0) (lit 100.0) (lit 80.0)

-- | A bezier curve path (tree link)
treeLinkPath :: forall repr. NumExpr repr => PathExpr repr => repr String
treeLinkPath = linkHorizontal (lit 50.0) (lit 100.0) (lit 200.0) (lit 150.0)

-- =============================================================================
-- Tests
-- =============================================================================

runTests :: Effect Unit
runTests = do
  log "\n--- Interpreter Polymorphism Tests ---"

  testBasicArithmetic
  testComplexArithmetic
  testTrigExpressions
  testConditionals
  testStrings
  testPaths
  demonstrateThreeInterpreters

-- | Test: basic arithmetic works with all interpreters
testBasicArithmetic :: Effect Unit
testBasicArithmetic = do
  log "\n  Basic Arithmetic: (2 * 3) + 10"

  -- Eval: produces computed value
  let evalResult = runEval (basicArithmetic :: Eval Number)
  assert' ("Eval expected 16.0, got " <> show evalResult) (evalResult == 16.0)
  log $ "    Eval:    " <> show evalResult <> " (computed value)"

  -- CodeGen: produces source code
  let codeResult = runCodeGen (basicArithmetic :: CodeGen Number)
  assert' ("CodeGen expected ((2.0 * 3.0) + 10.0), got " <> codeResult)
    (codeResult == "((2.0 * 3.0) + 10.0)")
  log $ "    CodeGen: " <> codeResult <> " (source code)"

  -- SVG: produces attribute string
  let svgResult = runSVG (basicArithmetic :: SVG Number)
  assert' ("SVG expected 16.0, got " <> svgResult) (svgResult == "16.0")
  log $ "    SVG:     " <> svgResult <> " (attribute string)"

  log "  ✓ All three interpreters produce correct output"

-- | Test: complex arithmetic with subtraction and division
testComplexArithmetic :: Effect Unit
testComplexArithmetic = do
  log "\n  Complex Arithmetic: (100 - 25) / 5"

  let evalResult = runEval (complexArithmetic :: Eval Number)
  assert' ("Eval expected 15.0, got " <> show evalResult) (evalResult == 15.0)
  log $ "    Eval:    " <> show evalResult

  let codeResult = runCodeGen (complexArithmetic :: CodeGen Number)
  assert' ("CodeGen contains / and -, got " <> codeResult)
    (codeResult == "((100.0 - 25.0) / 5.0)")
  log $ "    CodeGen: " <> codeResult

  let svgResult = runSVG (complexArithmetic :: SVG Number)
  assert' ("SVG expected 15.0, got " <> svgResult) (svgResult == "15.0")
  log $ "    SVG:     " <> svgResult

  log "  ✓ Subtraction and division work correctly"

-- | Test: trigonometric expressions
testTrigExpressions :: Effect Unit
testTrigExpressions = do
  log "\n  Trig Expression: sin(pi/4)"

  -- sin(π/4) ≈ 0.7071...
  let evalResult = runEval (trigExpr :: Eval Number)
  assert' ("Eval expected ~0.707, got " <> show evalResult)
    (evalResult > 0.707 && evalResult < 0.708)
  log $ "    Eval:    " <> show evalResult

  let codeResult = runCodeGen (trigExpr :: CodeGen Number)
  log $ "    CodeGen: " <> codeResult

  log "\n  Trig Combo: cos(0) + sin(pi/2)"

  -- cos(0) = 1, sin(π/2) = 1, total = 2
  let comboResult = runEval (trigCombo :: Eval Number)
  assert' ("Eval expected 2.0, got " <> show comboResult)
    (comboResult > 1.99 && comboResult < 2.01)
  log $ "    Eval:    " <> show comboResult

  let comboCode = runCodeGen (trigCombo :: CodeGen Number)
  log $ "    CodeGen: " <> comboCode

  log "  ✓ Trigonometric functions work correctly"

-- | Test: conditional expressions
testConditionals :: Effect Unit
testConditionals = do
  log "\n  Conditional: if 5 < 10 then 100 else 200"

  let evalResult = runEval (conditionalExpr :: Eval Number)
  assert' ("Eval expected 100.0, got " <> show evalResult) (evalResult == 100.0)
  log $ "    Eval:    " <> show evalResult

  let codeResult = runCodeGen (conditionalExpr :: CodeGen Number)
  log $ "    CodeGen: " <> codeResult

  let svgResult = runSVG (conditionalExpr :: SVG Number)
  assert' ("SVG expected 100.0, got " <> svgResult) (svgResult == "100.0")
  log $ "    SVG:     " <> svgResult

  log "  ✓ Conditional expressions work correctly"

-- | Test: string expressions
testStrings :: Effect Unit
testStrings = do
  log "\n  String: \"Hello, \" ++ \"World!\""

  let evalResult = runEval (stringConcat :: Eval String)
  assert' ("Eval expected 'Hello, World!', got " <> evalResult)
    (evalResult == "Hello, World!")
  log $ "    Eval:    " <> evalResult

  let codeResult = runCodeGen (stringConcat :: CodeGen String)
  log $ "    CodeGen: " <> codeResult

  let svgResult = runSVG (stringConcat :: SVG String)
  assert' ("SVG expected 'Hello, World!', got " <> svgResult)
    (svgResult == "Hello, World!")
  log $ "    SVG:     " <> svgResult

  log "  ✓ String concatenation works correctly"

-- | Test: path expressions
testPaths :: Effect Unit
testPaths = do
  log "\n  Path: line from (10,20) to (100,80)"

  let evalResult = runEval (simplePath :: Eval String)
  assert' ("Eval produces M...L... path")
    (evalResult == "M 10.0,20.0 L 100.0,80.0")
  log $ "    Eval:    " <> evalResult

  let codeResult = runCodeGen (simplePath :: CodeGen String)
  assert' ("CodeGen produces linePath call")
    (codeResult == "(linePath 10.0 20.0 100.0 80.0)")
  log $ "    CodeGen: " <> codeResult

  log "\n  Path: tree link from (50,100) to (200,150)"

  let linkResult = runEval (treeLinkPath :: Eval String)
  log $ "    Eval:    " <> linkResult

  let linkCode = runCodeGen (treeLinkPath :: CodeGen String)
  assert' ("CodeGen produces linkHorizontal call")
    (linkCode == "(linkHorizontal 50.0 100.0 200.0 150.0)")
  log $ "    CodeGen: " <> linkCode

  log "  ✓ Path expressions work correctly"

-- | THE KEY DEMONSTRATION: Same expression, three completely different outputs!
demonstrateThreeInterpreters :: Effect Unit
demonstrateThreeInterpreters = do
  log "\n--- The Key Insight: One Expression, Three Outputs ---"
  log ""
  log "  Expression: (2 * 3) + 10"
  log "  This is a SINGLE polymorphic definition, not three separate ones!"
  log ""
  log "  │ Interpreter │ Output           │ Purpose                    │"
  log "  │─────────────│──────────────────│────────────────────────────│"
  log $ "  │ Eval        │ " <> padRight 16 (show (runEval (basicArithmetic :: Eval Number))) <> " │ D3 runtime computation     │"
  log $ "  │ CodeGen     │ " <> padRight 16 (runCodeGen (basicArithmetic :: CodeGen Number)) <> " │ Source code for SemiQuine  │"
  log $ "  │ SVG         │ " <> padRight 16 (runSVG (basicArithmetic :: SVG Number)) <> " │ Server-side SVG rendering  │"
  log ""
  log "  This is the Expression Problem solved:"
  log "  - Add new operations (sin, cos) without changing interpreters"
  log "  - Add new interpreters (English, AST) without changing operations"
  log "  - All combinations work automatically!"

-- Helper to pad strings
padRight :: Int -> String -> String
padRight n s = s <> replicateStr (max 0 (n - strLen s)) " "
  where
    strLen :: String -> Int
    strLen str = go str 0
      where
        go "" acc = acc
        go remaining acc = go (drop 1 remaining) (acc + 1)

    drop :: Int -> String -> String
    drop = Data.String.drop

    replicateStr :: Int -> String -> String
    replicateStr 0 _ = ""
    replicateStr i c = c <> replicateStr (i - 1) c
