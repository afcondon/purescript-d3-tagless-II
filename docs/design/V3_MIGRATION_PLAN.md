# PSD3 v3 Migration Plan

## Current Status

We have a working prototype in `psd3-selection/src/PSD3v3/` with:
- Core DSL: `Expr`, `Units`, `Datum`, `Attr`, `Sugar`
- Interpreters: `Eval`, `CodeGen`, `SVG`, `PureSVG`
- Tests demonstrating polymorphic expressions

## Edge Cases to Validate

### 1. Scale Functions (HIGH PRIORITY)

**Current v2 pattern:**
```purescript
let scaleX x = padding + (x / 9.0) * plotWidth
cx (scaleX d.x)
```

**v3 approach - Option A: Lift scale into DSL**
```purescript
-- Define scale as polymorphic function
scaleX :: forall repr. NumExpr repr => repr Number -> repr Number
scaleX x = x /: 9.0 *: plotWidth +: padding

-- Use with field accessor
cx (scaleX xVal)
```

**v3 approach - Option B: Scale DSL**
```purescript
class ScaleExpr repr where
  linearScale :: { domain :: {min :: Number, max :: Number}
                 , range :: {min :: Number, max :: Number} }
              -> repr Number -> repr Number

cx (linearScale config xVal)
```

**Test needed:** Parabola example with proper scale configuration.

### 2. Conditionals (MEDIUM PRIORITY)

**Current v2 pattern:**
```purescript
textAnchor ((\_ -> if node.x0 < w / 2.0 then "start" else "end") :: IndexedNode -> String)
```

**v3 approach:**
```purescript
textAnchor (ifThenElse (xVal <. n (w / 2.0)) (s "start") (s "end"))
```

This is actually *cleaner* in v3 - the conditional is explicit in the DSL
and the CodeGen interpreter can output readable code.

**Test needed:** Sankey node label positioning.

### 3. Path Generation (HIGH PRIORITY - POTENTIAL BLOCKER)

**Current v2 pattern:**
```purescript
let linePath points =
  case uncons points of
    Nothing -> ""
    Just {head, tail} ->
      "M" <> show (scaleX head.date) <> "," <> show (scaleY head.rate) <>
      foldl (\acc p -> acc <> "L" <> show (scaleX p.date) <> "," <> show (scaleY p.rate)) "" tail

d (linePath series.points)
```

**Challenge:** Path generation is fundamentally imperative string building
over an array of points. This doesn't fit naturally into the tagless DSL.

**Possible approaches:**

A. **Keep path generation outside DSL** - generate path string, pass as literal:
```purescript
let pathStr = generatePath points scaleX scaleY  -- Pure function
d (s pathStr)
```
CodeGen would show: `d "M50,100 L75,80 L100,120..."`

B. **Path DSL** - tagless path commands:
```purescript
class PathExpr repr where
  moveTo :: repr Number -> repr Number -> repr PathCmd
  lineTo :: repr Number -> repr Number -> repr PathCmd
  curveTo :: ... -> repr PathCmd
  pathCmds :: Array (repr PathCmd) -> repr String
```
This is complex but enables full introspection.

C. **Hybrid** - path generator as interpreter-aware function:
```purescript
generatePath :: forall repr. NumExpr repr => Array Point -> (repr Number -> repr Number) -> repr String
```

**Recommendation:** Start with Option A (outside DSL). Path introspection
is a nice-to-have, not essential for most use cases.

### 4. Closure-Captured External Data (MEDIUM PRIORITY)

**Current v2 pattern:**
```purescript
d ((\_ -> generateLinkPath layoutResult.nodes link) :: IndexedLink -> String)
```

The function uses `layoutResult.nodes` which isn't part of the datum.

**v3 approach:** This is actually a "static computed" value - it doesn't
use the datum at all. In v3:
```purescript
d (s (generateLinkPath layoutResult.nodes link))
```

The CodeGen would show the evaluated string, which is correct -
there's no datum dependency to express.

**Test needed:** Sankey link paths.

### 5. Index-Based Attributes (LOW PRIORITY)

**Current v2 pattern:**
```purescript
IndexedAttr (datum -> Int -> AttributeValue)
```

**v3 approach:** We have `index :: repr Int` in DatumExpr:
```purescript
fill (ifThenElse (index ==. n 0) (s "red") (s "blue"))
```

**Test needed:** Alternating colors based on index.

### 6. Per-Datum Attribute Generation (MEDIUM PRIORITY)

**Current v2 pattern:**
```purescript
applyPerDatumAttrs :: (datum -> Array (Attribute datum)) -> Selection -> Effect Selection
```

Each datum generates its own set of attributes dynamically.

**v3 consideration:** Our polymorphic attributes are defined once and
evaluated for each datum. The `EvalD` interpreter handles this:
```purescript
runEvalD (scaleX :: EvalD ParabolaPoint Number) point index
```

This should work, but we need to verify the pattern holds for complex cases.

## Test Plan

### Phase 1: Simple Charts (Current)
- [x] Parabola scatter plot
- [ ] Bar chart with scales
- [ ] Line chart (single series)

### Phase 2: Complex Layouts
- [ ] Sankey diagram (conditionals, paths, labels)
- [ ] Pack visualization (depth-based coloring)
- [ ] Tree layout (recursive structure)

### Phase 3: Interactive Features
- [ ] Transitions (can we express interpolation?)
- [ ] Event handlers (onClick, onMouseOver)
- [ ] Dynamic data updates

### Phase 4: Full Integration
- [ ] Replace v2 Attribute module with v3
- [ ] Update all demo examples
- [ ] Benchmark performance

## Questions to Resolve

1. **Should scales be part of the DSL?**
   - Pro: Full introspection, CodeGen can show scale config
   - Con: Complexity, D3 scales have many options

2. **How to handle paths?**
   - Introspectable Path DSL vs. pre-computed strings
   - Path DSL enables animation interpolation

3. **Event handlers?**
   - Current: `onClick :: (datum -> Effect Unit) -> Attribute datum`
   - v3: Events are side effects, not values - may need separate handling

4. **Transitions/Animation?**
   - D3 transitions interpolate between attribute values
   - Could have `Interpolate` interpreter that generates tween functions

## Next Steps

1. **Add bar chart example** - validates scale patterns
2. **Try Sankey labels** - validates conditionals
3. **Try line chart paths** - identifies path generation approach
4. **Document findings** - update this plan

## Success Criteria

v3 is viable if:
1. All current demo examples can be expressed
2. CodeGen produces readable, compilable code
3. No significant performance regression
4. Ergonomics are acceptable (sugar helps)
5. No edge cases that fundamentally break the approach
