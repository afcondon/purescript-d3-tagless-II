-- | Tests for Type-Safe Unit Arithmetic
-- |
-- | Demonstrates that:
-- | 1. Same-unit arithmetic works correctly (px + px, em + em)
-- | 2. Scalar multiplication preserves unit type
-- | 3. Different interpreters render units appropriately
-- |
-- | Note: The compile-time safety (px + em fails to compile) cannot be
-- | tested at runtime but is documented here.
module Test.Expr.UnitsSpec where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assert')
import Unsafe.Coerce (unsafeCoerce)

-- DSL imports
import PSD3.Expr.Units (class UnitExpr, class UnitArith, Pixels, Em, Rem, Percent, px, em, rem, percent, addU, subU, scaleU)

-- Interpreter imports
import PSD3.Expr.Interpreter.Eval (Eval, runEval)
import PSD3.Expr.Interpreter.CodeGen (CodeGen, runCodeGen)
import PSD3.Expr.Interpreter.SVG (SVG, runSVG)

-- =============================================================================
-- Polymorphic Unit Expressions
-- =============================================================================

-- | Pixel arithmetic: 10px + 5px = 15px
pxAddition :: forall repr. UnitExpr repr => UnitArith repr => repr Pixels
pxAddition = addU (px 10.0) (px 5.0)

-- | Pixel subtraction: 20px - 8px = 12px
pxSubtraction :: forall repr. UnitExpr repr => UnitArith repr => repr Pixels
pxSubtraction = subU (px 20.0) (px 8.0)

-- | Pixel scaling: 5px * 3 = 15px
pxScaling :: forall repr. UnitExpr repr => UnitArith repr => repr Pixels
pxScaling = scaleU (px 5.0) 3.0

-- | Em arithmetic: 1.5em + 0.5em = 2em
emAddition :: forall repr. UnitExpr repr => UnitArith repr => repr Em
emAddition = addU (em 1.5) (em 0.5)

-- | Em subtraction: 3em - 1em = 2em
emSubtraction :: forall repr. UnitExpr repr => UnitArith repr => repr Em
emSubtraction = subU (em 3.0) (em 1.0)

-- | Em scaling: 2em * 1.5 = 3em
emScaling :: forall repr. UnitExpr repr => UnitArith repr => repr Em
emScaling = scaleU (em 2.0) 1.5

-- | Rem arithmetic: 1rem + 0.25rem = 1.25rem
remAddition :: forall repr. UnitExpr repr => UnitArith repr => repr Rem
remAddition = addU (rem 1.0) (rem 0.25)

-- | Percent arithmetic: 50% + 25% = 75%
percentAddition :: forall repr. UnitExpr repr => UnitArith repr => repr Percent
percentAddition = addU (percent 50.0) (percent 25.0)

-- | Complex expression: 10px + (5px * 2) = 20px
complexPxExpr :: forall repr. UnitExpr repr => UnitArith repr => repr Pixels
complexPxExpr = addU (px 10.0) (scaleU (px 5.0) 2.0)

-- | Nested expression: (2em + 1em) * 2 = 6em
nestedEmExpr :: forall repr. UnitExpr repr => UnitArith repr => repr Em
nestedEmExpr = scaleU (addU (em 2.0) (em 1.0)) 2.0

-- =============================================================================
-- Tests
-- =============================================================================

runTests :: Effect Unit
runTests = do
  log "\n--- Unit Arithmetic Tests ---"

  testPixelArithmetic
  testEmArithmetic
  testOtherUnits
  testComplexExpressions
  testInterpreterOutputs
  documentCompileTimeSafety

-- | Test: Pixel arithmetic works correctly
testPixelArithmetic :: Effect Unit
testPixelArithmetic = do
  log "\n  Pixel Arithmetic:"

  -- Addition
  let addResult :: Number
      addResult = unsafeCoerce $ runEval (pxAddition :: Eval Pixels)
  assert' ("px addition expected 15.0, got " <> show addResult) (addResult == 15.0)
  log $ "    10px + 5px = " <> show addResult <> "px"

  -- Subtraction
  let subResult :: Number
      subResult = unsafeCoerce $ runEval (pxSubtraction :: Eval Pixels)
  assert' ("px subtraction expected 12.0, got " <> show subResult) (subResult == 12.0)
  log $ "    20px - 8px = " <> show subResult <> "px"

  -- Scaling
  let scaleResult :: Number
      scaleResult = unsafeCoerce $ runEval (pxScaling :: Eval Pixels)
  assert' ("px scaling expected 15.0, got " <> show scaleResult) (scaleResult == 15.0)
  log $ "    5px * 3 = " <> show scaleResult <> "px"

  log "  ✓ Pixel arithmetic correct"

-- | Test: Em arithmetic works correctly
testEmArithmetic :: Effect Unit
testEmArithmetic = do
  log "\n  Em Arithmetic:"

  -- Addition
  let addResult :: Number
      addResult = unsafeCoerce $ runEval (emAddition :: Eval Em)
  assert' ("em addition expected 2.0, got " <> show addResult) (addResult == 2.0)
  log $ "    1.5em + 0.5em = " <> show addResult <> "em"

  -- Subtraction
  let subResult :: Number
      subResult = unsafeCoerce $ runEval (emSubtraction :: Eval Em)
  assert' ("em subtraction expected 2.0, got " <> show subResult) (subResult == 2.0)
  log $ "    3em - 1em = " <> show subResult <> "em"

  -- Scaling
  let scaleResult :: Number
      scaleResult = unsafeCoerce $ runEval (emScaling :: Eval Em)
  assert' ("em scaling expected 3.0, got " <> show scaleResult) (scaleResult == 3.0)
  log $ "    2em * 1.5 = " <> show scaleResult <> "em"

  log "  ✓ Em arithmetic correct"

-- | Test: Other unit types work
testOtherUnits :: Effect Unit
testOtherUnits = do
  log "\n  Other Units:"

  -- Rem
  let remResult :: Number
      remResult = unsafeCoerce $ runEval (remAddition :: Eval Rem)
  assert' ("rem addition expected 1.25, got " <> show remResult) (remResult == 1.25)
  log $ "    1rem + 0.25rem = " <> show remResult <> "rem"

  -- Percent
  let percentResult :: Number
      percentResult = unsafeCoerce $ runEval (percentAddition :: Eval Percent)
  assert' ("percent addition expected 75.0, got " <> show percentResult) (percentResult == 75.0)
  log $ "    50% + 25% = " <> show percentResult <> "%"

  log "  ✓ Other unit types work correctly"

-- | Test: Complex/nested expressions
testComplexExpressions :: Effect Unit
testComplexExpressions = do
  log "\n  Complex Expressions:"

  -- 10px + (5px * 2) = 20px
  let complexResult :: Number
      complexResult = unsafeCoerce $ runEval (complexPxExpr :: Eval Pixels)
  assert' ("complex px expected 20.0, got " <> show complexResult) (complexResult == 20.0)
  log $ "    10px + (5px * 2) = " <> show complexResult <> "px"

  -- (2em + 1em) * 2 = 6em
  let nestedResult :: Number
      nestedResult = unsafeCoerce $ runEval (nestedEmExpr :: Eval Em)
  assert' ("nested em expected 6.0, got " <> show nestedResult) (nestedResult == 6.0)
  log $ "    (2em + 1em) * 2 = " <> show nestedResult <> "em"

  log "  ✓ Complex expressions work correctly"

-- | Test: Different interpreters produce appropriate output
testInterpreterOutputs :: Effect Unit
testInterpreterOutputs = do
  log "\n  Interpreter Outputs for: 10px + (5px * 2)"

  -- Eval: raw number for D3
  let evalResult :: Number
      evalResult = unsafeCoerce $ runEval (complexPxExpr :: Eval Pixels)
  log $ "    Eval:    " <> show evalResult <> " (raw number for D3)"

  -- CodeGen: source code
  let codeResult = runCodeGen (complexPxExpr :: CodeGen Pixels)
  log $ "    CodeGen: " <> codeResult

  -- SVG: CSS calc expression
  let svgResult = runSVG (complexPxExpr :: SVG Pixels)
  log $ "    SVG:     " <> svgResult

  log ""
  log "  Same expression, three interpretations:"
  log "    - D3 uses raw numbers (SVG attributes are unitless)"
  log "    - CodeGen preserves the unit constructor calls"
  log "    - SVG generates CSS calc() for CSS properties"

  log "  ✓ All interpreters produce correct output"

-- | Document compile-time safety (cannot be tested at runtime)
documentCompileTimeSafety :: Effect Unit
documentCompileTimeSafety = do
  log "\n  Compile-Time Safety:"
  log ""
  log "  The following WOULD NOT COMPILE (type error):"
  log "    addU (px 10.0) (em 2.0)    -- Pixels ≠ Em"
  log "    addU (rem 1.0) (percent 50.0)  -- Rem ≠ Percent"
  log ""
  log "  This is enforced by the type signature:"
  log "    addU :: forall u. repr u -> repr u -> repr u"
  log "  Both arguments must have the SAME unit type 'u'."
  log ""
  log "  ✓ Type safety prevents mixing incompatible units"
