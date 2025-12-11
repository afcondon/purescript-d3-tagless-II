# PSD3 v3 Exploration: Finally Tagless Attributes

## The Problem

Current PSD3 architecture uses finally tagless for Tree structure but ADT for attributes:

```purescript
-- Tree level: Finally tagless ✓
-- Multiple interpreters: D3, Mermaid, English, SemiQuine

-- Attribute level: ADT ✗
data Attribute datum
  = StaticAttr AttributeName AttributeValue
  | DataAttr AttributeName (datum -> AttributeValue)  -- Opaque function!
  | IndexedAttr AttributeName (datum -> Int -> AttributeValue)
```

When `DataAttr` holds a function, we can evaluate it but can't inspect it. This is why SemiQuine TreeToCode can only show evaluated values (`cx 0.0`) rather than the original expression (`cx (scaleX d.x)`).

This is the **Expression Problem** - we hit a boundary where the finally tagless approach stops.

## The Solution: Tagless All The Way Down

If attributes were also finally tagless, we'd have full introspection:

```purescript
class AttrDSL repr where
  cx :: repr Number -> repr Attr
  cy :: repr Number -> repr Attr
  radius :: repr Number -> repr Attr
  fill :: repr String -> repr Attr
  -- ...

class NumExpr repr where
  lit :: Number -> repr Number
  field :: String -> repr Number      -- Access datum field
  scale :: Scale -> repr Number -> repr Number
  add :: repr Number -> repr Number -> repr Number
  mul :: repr Number -> repr Number -> repr Number
  -- ...

class StringExpr repr where
  str :: String -> repr String
  fieldStr :: String -> repr String   -- Access string field
  concat :: repr String -> repr String -> repr String
  -- ...
```

Usage would change from:
```purescript
-- Current (v2)
cx (scaleX d.x)

-- Proposed (v3)
cx (scale scaleX (field "x"))
```

## Interpreter Implementations

### D3 Interpreter (evaluates to DOM operations)

```purescript
newtype D3Eval datum a = D3Eval (datum -> a)

instance numExprD3 :: NumExpr (D3Eval datum) where
  lit n = D3Eval (\_ -> n)
  field name = D3Eval (\datum -> unsafeGet name datum)
  scale s expr = D3Eval (\datum -> runScale s (runD3Eval expr datum))
  add a b = D3Eval (\d -> runD3Eval a d + runD3Eval b d)
```

### CodeGen Interpreter (generates PureScript code)

```purescript
newtype CodeGen a = CodeGen String

instance numExprCodeGen :: NumExpr CodeGen where
  lit n = CodeGen (show n)
  field name = CodeGen ("d." <> name)
  scale s expr = CodeGen (scaleName s <> " (" <> runCodeGen expr <> ")")
  add a b = CodeGen ("(" <> runCodeGen a <> " + " <> runCodeGen b <> ")")
```

### English Interpreter (generates descriptions)

```purescript
newtype English a = English String

instance numExprEnglish :: NumExpr English where
  lit n = English (show n)
  field name = English ("the " <> name <> " field")
  scale s expr = English ("scaled " <> runEnglish expr <> " using " <> scaleName s)
```

## Benefits

### 1. True Round-Tripping
SemiQuine could generate actual compilable code that reproduces the visualization, not just evaluated snapshots.

### 2. Units Support
```purescript
class NumExpr repr where
  px :: Number -> repr Pixels
  em :: Number -> repr Em
  percent :: Number -> repr Percent
  vh :: Number -> repr ViewportHeight

  -- Type-safe operations
  addPx :: repr Pixels -> repr Pixels -> repr Pixels

  -- Conversions
  emToPx :: repr Em -> Number -> repr Pixels  -- base font size
```

The D3 interpreter would resolve units to final pixel values.
The CodeGen interpreter would preserve unit strings (`"10px"`, `"2em"`).
A validation interpreter could check unit consistency.

### 3. Optimization Passes
An optimizer interpreter could simplify expressions:
- `add (lit 0) x` → `x`
- `mul (lit 1) x` → `x`
- Constant folding: `add (lit 2) (lit 3)` → `lit 5`

### 4. Static Analysis
- Dependency tracking: which fields does this attribute use?
- Validation: does this expression make sense for this element type?

## Challenges

### 1. Typed Field Access
`field "x"` is stringly-typed. Options:
- Accept runtime field lookup (current approach essentially)
- Type-level field names: `field @"x" :: repr (Field datum "x")`
- Row polymorphism for record access

### 2. Ergonomics
Writing `scale scaleX (field "x")` is more verbose than `scaleX d.x`.
Could potentially use Template PureScript or macros if available.

### 3. Migration
Would require rewriting attribute expressions throughout codebase.
Could potentially have compatibility shim that converts v2 style to v3.

### 4. Boilerplate
More typeclass instances to write and maintain.
Though PureScript's deriving capabilities could help.

## Tradeoffs Summary

| Aspect | Current (v2) | Proposed (v3) |
|--------|--------------|---------------|
| Syntax | `scaleX d.x` | `scale scaleX (field "x")` |
| Round-trip code gen | Values only | Full expressions |
| Units | Not supported | Type-safe units |
| Optimization | Not possible | Interpreter-based |
| Implementation | Simple | More complex |
| D3 familiarity | Closer to D3 | More abstract |

## Recommendation

This is a significant architectural change worth exploring for a future major version. The benefits (true code generation, units, optimization) are substantial, but the ergonomic cost needs careful consideration.

Could prototype in a separate branch to evaluate real-world usability before committing.

## Related Work

- Oleg Kiselyov's finally tagless papers
- Phil Freeman's PureScript finally tagless examples
- Typed Template Haskell for code generation
- F# type providers for typed field access
