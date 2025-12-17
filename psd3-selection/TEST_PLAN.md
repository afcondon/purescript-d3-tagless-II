# psd3-selection Test Plan

## Overview

The psd3-selection package has excellent testability from the command line because the core expression DSL and its interpreters are pure computations. The existing test infrastructure (`Test.Expr.ExprSpec`, `Test.Expr.PathSpec`, etc.) provides a solid foundation to build upon.

**Key insight**: The finally-tagless expression DSL allows the same expression to be interpreted multiple ways (evaluation, code generation, SVG strings), making it highly testable without any browser dependencies.

## Test Categories

### 1. Expression Evaluation Tests (`Test.Expr.Eval`)

Test the `Eval` interpreter for correctness of mathematical operations:

- **Basic arithmetic**: `lit`, `add`, `sub`, `mul`, `div`, `negate`
- **Trigonometry**: `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `atan2`, `pi`
- **Comparisons**: `lessThan`, `equals`, boolean operations
- **String operations**: `append`, `show`
- **Datum-dependent expressions**: Field access with various record types

```purescript
-- Example tests
runEval (lit 2.0 `mul` lit 3.0) == 6.0
runEval (sin pi) ≈ 0.0
runEval (cos pi) == -1.0
```

### 2. Code Generation Tests (`Test.Expr.CodeGen`)

Test the `CodeGen` interpreter produces valid, readable PureScript:

- Static expressions → literal strings
- Datum expressions → field access patterns (`d.x`, `d.name`)
- Complex expressions → proper parenthesization
- Round-trip validation: generated code should be compilable

```purescript
-- Example tests
runCodeGen (lit 2.0 `mul` lit 3.0 `add` lit 10.0) == "((2.0 * 3.0) + 10.0)"
runCodeGen (field @"x" `mul` lit 20.0) == "(d.x * 20.0)"
```

### 3. SVG String Generation Tests (`Test.Expr.SVG`)

Test the `SVG` interpreter produces valid SVG attribute values:

- Numbers → string format
- Units → proper CSS format (`"10px"`, `"2em"`)
- Colors → valid color strings

```purescript
-- Example tests
runSVG (lit 16.0) == "16.0"
runSVG (px 10) == "10px"
```

### 4. Path Generation Tests (`Test.Expr.Path`)

Test SVG path string generation:

- `linePath` → `"M... L..."` format
- `linkHorizontal` / `linkVertical` → cubic Bézier curves
- `sankeyLink` → Sankey-specific path with dual y-coordinates
- `arc` → pie/donut arc segments

```purescript
-- Example tests
runEval (linePath 10.0 20.0 100.0 80.0) == "M10,20L100,80"
runEval (arc 0.0 (pi / 2.0) 0.0 100.0) -- returns valid arc path
```

### 5. Unit Arithmetic Tests (`Test.Expr.Units`)

Test type-safe unit operations:

- Same-unit addition: `px 10 + px 5 = 15`
- Multiplication by scalar
- Compile-time rejection of mixed units (document as compile-fails-as-expected)

```purescript
-- Example tests
runEval (addU (px 10.0) (px 5.0)) == 15.0
runEval (mulU (px 10.0) 2.0) == 20.0
-- This should NOT compile: addU (px 10.0) (em 2.0)
```

### 6. Interpreter Polymorphism Tests (`Test.Expr.Polymorphism`)

Demonstrate the key feature: same expression, multiple outputs:

```purescript
expr :: forall repr. NumExpr repr => repr Number
expr = lit 2.0 `mul` lit 3.0 `add` lit 10.0

-- Test all three interpreters produce correct output:
runEval expr == 16.0
runCodeGen expr == "((2.0 * 3.0) + 10.0)"
runSVG expr == "16.0"
```

### 7. AST-to-Code Tests (`Test.SemiQuine.TreeToCode`)

Test visualization AST round-tripping:

- Simple trees → valid PureScript
- Trees with joins → proper `joinData` generation
- Attribute serialization → readable code

```purescript
-- Example: create an AST, convert to code, verify output
let ast = elem "circle" [cx (static 50.0), cy (static 50.0), r (static 10.0)]
let code = treeToCode ast
-- Verify code contains expected elements
```

### 8. Scale Tests (`Test.Scale`)

Test scale functions (pure computation):

- Linear scale domain/range mapping
- Log, pow, sqrt scales
- Band scale with padding
- Ordinal scale lookups
- `applyScale`, `invert`, `ticks`

```purescript
-- Example tests
let scale = linear # domain [0.0, 100.0] # range [0.0, 500.0]
applyScale scale 50.0 == 250.0
invert scale 250.0 == 50.0
```

## Test Infrastructure

- Use existing `Test.Assert` pattern
- Run via `spago test`
- Console output for results
- No browser/jsdom required

## Files to Create/Modify

```
psd3-selection/
  test/
    Test/
      Main.purs                    -- Test runner (exists)
      Expr/
        ExprSpec.purs              -- Expand with more edge cases (exists)
        PathSpec.purs              -- Path tests (exists)
        PolymorphismSpec.purs      -- NEW: Interpreter polymorphism demos
        UnitsSpec.purs             -- NEW: Unit arithmetic tests
      Scale/
        ScaleSpec.purs             -- NEW: Scale function tests
      SemiQuine/
        TreeToCodeSpec.purs        -- AST round-trip tests (exists)
```

## Running Tests

```bash
cd psd3-selection
spago test
```

All tests run from command line, no browser required.

## Existing Tests to Preserve

The following test files already exist and provide good coverage:

- `Test.Expr.ExprSpec` - Expression evaluation tests
- `Test.Expr.PathSpec` - Path DSL tests
- `Test.Expr.ParabolaExample` - Realistic parabola visualization
- `Test.Expr.SankeyExample` - Sankey diagram example
- `Test.Expr.TreeExample` - Tree layout example
- `Test.Expr.UpdatePatternExample` - GUP enter/update/exit
- `Test.SemiQuine.TreeToCodeSpec` - AST round-tripping

## Success Criteria

- All existing tests continue to pass
- New tests cover scale functions
- Interpreter polymorphism is demonstrated
- Unit arithmetic correctness verified
- `spago test` runs successfully with no browser dependencies
