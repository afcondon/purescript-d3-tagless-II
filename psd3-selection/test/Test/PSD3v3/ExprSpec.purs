-- | Tests for PSD3v3 Expression DSL
-- |
-- | Demonstrates the key benefit: one expression, multiple interpretations
module Test.PSD3v3.ExprSpec where

import Prelude hiding (add, sub, mul, div)

import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assert')
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- DSL imports
import PSD3v3.Expr (class NumExpr, lit, add, mul)
import PSD3v3.Units (class UnitExpr, class UnitArith, Pixels, Em, px, em, addU, scaleU)
import PSD3v3.Datum (class DatumExpr, field)
import PSD3v3.Sugar ((*:), (+:), (-:), (*~), (+~), n)

-- Interpreter imports
import PSD3v3.Interpreter.Eval (Eval, runEval, EvalD, runEvalD)
import PSD3v3.Interpreter.CodeGen (CodeGen, runCodeGen)
import PSD3v3.Interpreter.SVG (SVG, runSVG, SVGD, runSVGD)

-- =============================================================================
-- Sample datum type (parabola point)
-- =============================================================================

type ParabolaRow = (x :: Number, y :: Number)
type ParabolaPoint = { x :: Number, y :: Number }

-- Field accessors for ParabolaPoint (the "boilerplate" that could be generated)
xField :: forall repr. DatumExpr repr ParabolaRow => repr Number
xField = field (Proxy :: Proxy "x")

yField :: forall repr. DatumExpr repr ParabolaRow => repr Number
yField = field (Proxy :: Proxy "y")

-- =============================================================================
-- Sample expressions (polymorphic - work with any interpreter!)
-- =============================================================================

-- | A simple static expression: 2 * 3 + 10
staticExpr :: forall repr. NumExpr repr => repr Number
staticExpr = add (mul (lit 2.0) (lit 3.0)) (lit 10.0)

-- | A datum-dependent expression: x * 20 + 200 (a simple scale)
-- | This is the kind of thing you'd use for cx, cy attributes
scaleX :: forall repr. NumExpr repr => DatumExpr repr ParabolaRow => repr Number
scaleX = add (mul xField (lit 20.0)) (lit 200.0)

-- | Another datum-dependent: 300 - y * 2.5 (inverted scale for y)
scaleY :: forall repr. NumExpr repr => DatumExpr repr ParabolaRow => repr Number
scaleY = add (lit 300.0) (mul yField (lit (-2.5)))

-- =============================================================================
-- Tests
-- =============================================================================

runTests :: Effect Unit
runTests = do
  log "\n--- PSD3v3 Static Expression Tests ---"
  testStaticEval
  testStaticCodeGen
  testStaticSVG

  log "\n--- PSD3v3 Datum Expression Tests ---"
  testDatumEval
  testDatumCodeGen
  testDatumSVG

  log "\n--- PSD3v3 Unit Tests ---"
  demonstrateUnits

  log "\n--- PSD3v3 Sugar Syntax ---"
  demonstrateSugar

  log "\n--- The Money Shot: Same Expression, Three Outputs ---"
  demonstratePolymorphism

-- | Test: Eval interprets static expression to value
testStaticEval :: Effect Unit
testStaticEval = do
  let result = runEval (staticExpr :: Eval Number)
  assert' ("Expected 16.0, got " <> show result) (result == 16.0)
  log "  ✓ Eval: staticExpr = 16.0"

-- | Test: CodeGen generates PureScript code
testStaticCodeGen :: Effect Unit
testStaticCodeGen = do
  let code = runCodeGen (staticExpr :: CodeGen Number)
  assert' ("Expected code with + and *, got: " <> code)
    (code == "((2.0 * 3.0) + 10.0)")
  log "  ✓ CodeGen: staticExpr = \"((2.0 * 3.0) + 10.0)\""

-- | Test: SVG generates attribute-ready string
testStaticSVG :: Effect Unit
testStaticSVG = do
  let svg = runSVG (staticExpr :: SVG Number)
  assert' ("Expected \"16.0\", got: " <> svg) (svg == "16.0")
  log "  ✓ SVG: staticExpr = \"16.0\""

-- | Test: EvalD with datum
testDatumEval :: Effect Unit
testDatumEval = do
  let
    point :: ParabolaPoint
    point = { x: 5.0, y: 25.0 }
    result = runEvalD (scaleX :: EvalD ParabolaPoint Number) point 0
  -- x * 20 + 200 = 5 * 20 + 200 = 300
  assert' ("Expected 300.0, got " <> show result) (result == 300.0)
  log "  ✓ EvalD: scaleX {x:5} = 300.0"

-- | Test: CodeGen generates datum-accessing code
testDatumCodeGen :: Effect Unit
testDatumCodeGen = do
  let code = runCodeGen (scaleX :: CodeGen Number)
  assert' ("Expected code with d.x, got: " <> code)
    (code == "((d.x * 20.0) + 200.0)")
  log "  ✓ CodeGen: scaleX = \"((d.x * 20.0) + 200.0)\""

-- | Test: SVGD generates SVG from datum
testDatumSVG :: Effect Unit
testDatumSVG = do
  let
    point :: ParabolaPoint
    point = { x: 5.0, y: 25.0 }
    svg = runSVGD (scaleX :: SVGD ParabolaPoint Number) point 0
  assert' ("Expected \"300.0\", got: " <> svg) (svg == "300.0")
  log "  ✓ SVGD: scaleX {x:5} = \"300.0\""

-- | THE KEY DEMONSTRATION
-- | Same polymorphic expression, three completely different outputs!
demonstratePolymorphism :: Effect Unit
demonstratePolymorphism = do
  let
    point :: ParabolaPoint
    point = { x: -10.0, y: 100.0 }

  log "\n  Expression: scaleX = (x * 20) + 200"
  log $ "  With datum: { x: -10.0, y: 100.0 }"
  log ""

  -- Eval: produces the NUMBER
  let evalResult = runEvalD (scaleX :: EvalD ParabolaPoint Number) point 0
  log $ "  → Eval (D3 runtime):  " <> show evalResult

  -- CodeGen: produces PURESCRIPT CODE
  let codeResult = runCodeGen (scaleX :: CodeGen Number)
  log $ "  → CodeGen (code):     " <> codeResult

  -- SVG: produces SVG ATTRIBUTE STRING
  let svgResult = runSVGD (scaleX :: SVGD ParabolaPoint Number) point 0
  log $ "  → SVG (attribute):    " <> svgResult

  log ""
  log "  Same expression, three interpretations!"
  log "  - D3 gets the computed value for DOM manipulation"
  log "  - SemiQuine gets the source code for round-tripping"
  log "  - Server gets the SVG string for zero-JS rendering"

-- =============================================================================
-- Unit Demonstration
-- =============================================================================

-- | A unit-typed expression: 10px + 2em (type-safe!)
-- | Note: You can ONLY add px to px, em to em - this is enforced at compile time
pxExpr :: forall repr. UnitExpr repr => UnitArith repr => repr Pixels
pxExpr = addU (px 10.0) (scaleU (px 5.0) 2.0)  -- 10px + (5px * 2)

emExpr :: forall repr. UnitExpr repr => UnitArith repr => repr Em
emExpr = addU (em 1.5) (em 0.5)  -- 1.5em + 0.5em

-- | This would NOT compile - type error!
-- badExpr = addU (px 10.0) (em 2.0)  -- Error: can't add Pixels to Em

demonstrateUnits :: Effect Unit
demonstrateUnits = do
  log "\n  Expression: pxExpr = addU (px 10) (scaleU (px 5) 2)"
  log ""

  -- Eval: produces raw NUMBER (for D3/SVG attributes)
  let evalResult :: Number
      evalResult = unsafeCoerce $ runEval (pxExpr :: Eval Pixels)
  log $ "  → Eval:    " <> show evalResult <> "  (raw number for D3)"

  -- CodeGen: produces PURESCRIPT with unit constructors
  let codeResult = runCodeGen (pxExpr :: CodeGen Pixels)
  log $ "  → CodeGen: " <> codeResult

  -- SVG: produces CSS STRING with unit suffix
  let svgResult = runSVG (pxExpr :: SVG Pixels)
  log $ "  → SVG:     " <> svgResult <> "  (CSS calc expression)"

  log ""
  log "  Expression: emExpr = addU (em 1.5) (em 0.5)"
  log ""

  let evalEm :: Number
      evalEm = unsafeCoerce $ runEval (emExpr :: Eval Em)
  log $ "  → Eval:    " <> show evalEm
  log $ "  → CodeGen: " <> runCodeGen (emExpr :: CodeGen Em)
  log $ "  → SVG:     " <> runSVG (emExpr :: SVG Em)

  log ""
  log "  Type safety: addU (px 10) (em 2) would NOT compile!"
  log "  The type system prevents mixing incompatible units."

-- =============================================================================
-- Sugar Demonstration
-- =============================================================================

-- | With sugar, the same scaleX becomes much more readable
-- | Compare:
-- |   add (mul xField (lit 20.0)) (lit 200.0)
-- | vs:
-- |   xField *: 20.0 +: 200.0
scaleXSugar :: forall repr. NumExpr repr => DatumExpr repr ParabolaRow => repr Number
scaleXSugar = xField *: 20.0 +: 200.0

-- | Inverted Y scale: 300 - y * 2.5
scaleYSugar :: forall repr. NumExpr repr => DatumExpr repr ParabolaRow => repr Number
scaleYSugar = n 300.0 -: 0.0 +: 0.0  -- TODO: need better way to start with literal

-- | Actually let's show: (300 - (y * 2.5))
-- | We need the full version for this one since we're subtracting
scaleYFull :: forall repr. NumExpr repr => DatumExpr repr ParabolaRow => repr Number
scaleYFull = add (lit 300.0) (mul yField (lit (-2.5)))

-- | Unit sugar: px 10.0 +~ px 5.0 *~ 2.0
pxSugar :: forall repr. UnitExpr repr => UnitArith repr => repr Pixels
pxSugar = px 10.0 +~ (px 5.0 *~ 2.0)

demonstrateSugar :: Effect Unit
demonstrateSugar = do
  log "\n  Without sugar:"
  log "    add (mul xField (lit 20.0)) (lit 200.0)"
  log ""
  log "  With sugar:"
  log "    xField *: 20.0 +: 200.0"
  log ""

  let
    point :: ParabolaPoint
    point = { x: 5.0, y: 25.0 }

  -- Show they produce the same result
  let original = runEvalD (scaleX :: EvalD ParabolaPoint Number) point 0
  let sugared = runEvalD (scaleXSugar :: EvalD ParabolaPoint Number) point 0
  log $ "  Original result: " <> show original
  log $ "  Sugared result:  " <> show sugared
  assert' "Sugar should produce same result" (original == sugared)
  log "  ✓ Both produce identical results"

  log ""
  log "  CodeGen comparison:"
  log $ "    Original: " <> runCodeGen (scaleX :: CodeGen Number)
  log $ "    Sugared:  " <> runCodeGen (scaleXSugar :: CodeGen Number)

  log ""
  log "  Unit sugar: px 10.0 +~ (px 5.0 *~ 2.0)"
  log $ "    → SVG: " <> runSVG (pxSugar :: SVG Pixels)
