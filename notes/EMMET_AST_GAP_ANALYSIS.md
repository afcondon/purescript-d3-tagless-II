# Emmet Notation vs Full AST: Gap Analysis

**Date**: 2024-12-24
**Purpose**: Critical evaluation of Emmet notation expressiveness compared to the full PSD3 AST

## Executive Summary

Emmet notation is a **pedagogical success** and a **practical tool for simple visualizations**, but it captures approximately **20-30% of the full AST's expressiveness**. The gap is fundamental and cannot be fully bridged without sacrificing Emmet's core value: simplicity.

**Recommendation**: Position Emmet as a **learning tool** and **rapid prototyping syntax**, not as a replacement for the full AST. Users should "graduate" from Emmet to the full PureScript DSL as their needs grow.

---

## Part 1: What Emmet CAN Express

### 1.1 Element Structure
✅ **Fully Supported**
- 6 element types: `g`, `c`, `r`, `p`, `l`, `t` (Group, Circle, Rect, Path, Line, Text)
- Nesting with `>` operator (parent-child relationships)
- Siblings with `+` operator (adjacent elements)
- Repetition with `*N` multiplier
- Grouping with `()` for precedence control

**Example**: `g>c*3+r*2` creates a group with 3 circles and 2 rectangles

### 1.2 Attributes
✅ **Three attribute types supported**:

1. **Static attributes** (`name=value`)
   ```
   c[r=5,fill=red]
   ```
   Maps to: `StaticAttr "r" (NumValue 5.0)`

2. **Field attributes** (`name:fieldName`)
   ```
   c[cx:x,cy:y]
   ```
   Maps to: `DataAttr "cx" (FieldSource "x") (\d -> NumValue d.x)`

3. **Index attributes** (`name@index`)
   ```
   r[x@index]
   ```
   Maps to: `IndexedAttr "x" IndexSource (\d i -> NumValue (toNumber i))`

### 1.3 Data Joins
✅ **Four join types supported**:

1. **Simple join** (`j(TypeName)`)
   ```
   j(Point)>c[cx:x,cy:y,r=5]
   ```
   Maps to: `Join { name, key, joinData, template }`

2. **Nested join** (`n(TypeName)`)
   ```
   n(Board)>g>n(Row)>g>j(Cell)>r
   ```
   Maps to: `NestedJoin { name, key, joinData, decompose, template }`

3. **Update join** (`u(TypeName)`)
   ```
   u(Point)>c[cx:x,cy:y]
   ```
   Maps to: `UpdateJoin { name, key, joinData, template, behaviors }`

4. **Update nested join** (`x(TypeName)`)
   ```
   x(Board)>g>x(Row)>g>u(Cell)>r
   ```
   Maps to: `UpdateNestedJoin { name, key, joinData, decompose, template, behaviors }`

### 1.4 Type System Integration
✅ **Limited type safety**:
- Validates type names against a fixed set: `Point`, `Node`, `Country`, `Letter`, `Board`, `Row`, `Cell`
- Ensures nested joins only used with array-containing types
- Validates field names exist on data types (at parse time if possible)

---

## Part 2: What the Full AST CAN Express

### 2.1 Full Attribute Capabilities

The `Attribute datum` type supports:

```purescript
data Attribute datum
  = StaticAttr AttributeName AttributeValue
  | DataAttr AttributeName AttrSource (datum -> AttributeValue)
  | IndexedAttr AttributeName AttrSource (datum -> Int -> AttributeValue)
```

**What this enables**:

#### Computed Expressions
```purescript
-- Emmet CANNOT express:
cx (field @"x" `times` 40.0 `plus` 50.0)

-- Emmet CAN approximate with preprocessing:
-- Would need to precompute d.scaledX = d.x * 40.0 + 50.0
c[cx:scaledX]
```

#### Conditional Logic
```purescript
-- Emmet CANNOT express:
fill (ifThen (field @"value" `greaterThan` 50.0)
        (color "red")
        (color "blue"))

-- No Emmet equivalent
```

#### String Operations
```purescript
-- Emmet CANNOT express:
textContent (field @"firstName" `append` text " " `append` field @"lastName")

-- No Emmet equivalent
```

#### Trigonometric Functions
```purescript
-- Emmet CANNOT express (radial layouts):
cx (num 300.0 `plus` (cos (field @"angle") `times` field @"radius"))

-- No Emmet equivalent
```

#### Scale Functions
```purescript
-- Emmet CANNOT express:
cy (from (\d -> yScale d.value))

-- Would need preprocessing to add d.scaledY
```

### 2.2 Transition Capabilities

The `TransitionConfig` type supports:

```purescript
type TransitionConfig =
  { duration :: Milliseconds
  , delay :: Maybe Milliseconds
  , easing :: Maybe Easing
  }
```

**Emmet limitation**: Update joins (`u`, `x`) map to GUP but **cannot configure transitions**.

#### What Emmet Cannot Express:

```purescript
updateJoin "circles" "circle" data
  (\d -> elem Circle [cx (field @"x"), cy (field @"y")])
  { keyFn: Just (\d -> d.id)
  , enter: Just
      { attrs: [opacity (num 0.0)]
      , transition: Just $ transition (Milliseconds 500.0)
      }
  , update: Just
      { attrs: []
      , transition: Just $ transition (Milliseconds 1000.0)
      }
  , exit: Just
      { attrs: [opacity (num 0.0)]
      , transition: Just $ transition (Milliseconds 500.0)
      }
  }
```

**Emmet can only express**: `u(Point)>c[cx:x,cy:y]` with default/no transitions.

### 2.3 Behavior Capabilities

The `Behavior datum` type supports:

```purescript
-- Event handlers
onClick :: (datum -> Effect Unit) -> Behavior datum
onMouseOver :: (datum -> Effect Unit) -> Behavior datum
onDrag :: DragConfig datum -> Behavior datum
onZoom :: ZoomConfig datum -> Behavior datum
```

**Emmet limitation**: **No behavior support whatsoever**.

Cannot express:
- Click handlers
- Drag interactions
- Zoom/pan behaviors
- Hover effects
- Tooltips

### 2.4 Advanced AST Features

#### Named Selections
```purescript
-- Full AST:
named SVG "mySvg" [viewBox "0 0 800 600"]
  `withChildren` [...]

-- Emmet: No support for element naming
```

#### Arbitrary Element Types
```purescript
-- Full AST: Any ElementType
elem (CustomElement "foreignObject") [...]

-- Emmet: Only 6 fixed types
```

#### Behaviors Attachment
```purescript
-- Full AST:
elem Circle [cx (field @"x")]
  `withBehaviors` [onClick handleClick, onDrag dragConfig]

-- Emmet: No behavior support
```

#### Custom Key Functions
```purescript
-- Full AST:
updateJoin "items" "g" data template
  { keyFn: Just (\d -> d.uuid <> "-" <> show d.timestamp)
  , enter: ...
  }

-- Emmet: No key function support (uses default identity)
```

---

## Part 3: The Gap

### 3.1 Quantitative Analysis

| Capability | Emmet Support | Full AST Support | Gap |
|------------|---------------|------------------|-----|
| **Structure** | | | |
| Element types | 6 fixed | Unlimited | 🔴 Limited |
| Nesting | ✅ Full | ✅ Full | ✅ None |
| Siblings | ✅ Full | ✅ Full | ✅ None |
| **Attributes** | | | |
| Static values | ✅ Full | ✅ Full | ✅ None |
| Field access | ✅ Direct only | ✅ Plus computed | 🟡 Partial |
| Index access | ✅ Simple | ✅ Plus computed | 🟡 Partial |
| Expressions | ❌ None | ✅ Full DSL | 🔴 Total |
| Conditionals | ❌ None | ✅ Full | 🔴 Total |
| Scales | ❌ None | ✅ Full | 🔴 Total |
| **Joins** | | | |
| Simple joins | ✅ Full | ✅ Full | ✅ None |
| Nested joins | ✅ Full | ✅ Full | ✅ None |
| Update joins | ✅ Structure only | ✅ Plus GUP config | 🟡 Partial |
| Key functions | ❌ None | ✅ Full | 🔴 Total |
| **Advanced** | | | |
| Transitions | ❌ None | ✅ Full | 🔴 Total |
| Behaviors | ❌ None | ✅ Full | 🔴 Total |
| Named selections | ❌ None | ✅ Full | 🔴 Total |
| Custom elements | ❌ None | ✅ Full | 🔴 Total |

**Coverage Estimate**: Emmet expresses ~25% of AST capabilities (structure + simple attributes)

### 3.2 Qualitative Gap

The fundamental gap is **computational power**:

- **Emmet**: Declarative template with **no computation** (field access only)
- **Full AST**: Embedded PureScript with **arbitrary computation** (lambdas, functions, DSL)

This is not a bug—it's **by design**. Emmet trades expressiveness for:
1. **Simplicity** - Easy to learn and teach
2. **Conciseness** - Fits on one line
3. **Inspectability** - Can analyze and visualize structure
4. **Portability** - Could target multiple backends

---

## Part 4: Round-Trip Analysis

### 4.1 Emmet → AST (Forward Direction)

✅ **This direction works perfectly**

Current implementation:
1. Parse Emmet string → `EmmetAST`
2. Validate types and fields → `Either ValidationError EmmetAST`
3. Convert to PSD3 AST → `Tree datum`

**No information loss**: Every valid Emmet expression maps to exactly one AST.

### 4.2 AST → Emmet (Reverse Direction)

🔴 **This direction is FUNDAMENTALLY LOSSY**

#### Case 1: Simple AST (Emmet-Compatible)

```purescript
-- AST:
Join
  { name: "circles"
  , key: "circle"
  , joinData: points
  , template: \d -> elem Circle
      [ StaticAttr "r" (NumValue 5.0)
      , DataAttr "cx" (FieldSource "x") (\d' -> NumValue d'.x)
      , DataAttr "cy" (FieldSource "y") (\d' -> NumValue d'.y)
      ]
  }

-- Can round-trip to Emmet:
j(Point)>c[cx:x,cy:y,r=5]
```

✅ **Round-trippable**: AST uses only StaticAttr, simple DataAttr with FieldSource

#### Case 2: Computed Attributes (NOT Emmet-Compatible)

```purescript
-- AST:
DataAttr "cx" (ExprSource "x * 40 + 50") (\d -> NumValue (d.x * 40.0 + 50.0))

-- Cannot express in Emmet
-- Options:
-- 1. Reject (throw error)
-- 2. Approximate (show "cx:x" with warning)
-- 3. Pretty-print expression ("cx:(x * 40 + 50)")
```

❌ **Not round-trippable**: Loses computation

#### Case 3: Transitions (NOT Emmet-Compatible)

```purescript
-- AST:
UpdateJoin
  { ...
  , behaviors:
      { enter: Just { transition: Just (transition (Milliseconds 500.0)) }
      , update: Just { transition: Just (transition (Milliseconds 1000.0)) }
      , exit: Just { transition: Just (transition (Milliseconds 300.0)) }
      }
  }

-- Emmet only knows: u(Point)>c[...]
```

❌ **Not round-trippable**: Loses all transition config

#### Case 4: Behaviors (NOT Emmet-Compatible)

```purescript
-- AST:
elem Circle [...]
  `withBehaviors` [onClick (\d -> Console.log d.name)]

-- Emmet has no behavior syntax
```

❌ **Not round-trippable**: Cannot express behaviors at all

### 4.3 Round-Trip Decision Matrix

| AST Feature | Can Emit Emmet? | Lossless? | Strategy |
|-------------|-----------------|-----------|----------|
| Static attrs | ✅ Yes | ✅ Yes | Direct mapping |
| Field attrs | ✅ Yes | ✅ Yes | Direct mapping |
| Index attrs | ✅ Yes | ✅ Yes | Direct mapping |
| Computed attrs | 🟡 Partial | ❌ No | Reject or approximate |
| Conditionals | ❌ No | ❌ No | Reject |
| Scales | ❌ No | ❌ No | Reject |
| Transitions | ❌ No | ❌ No | Reject |
| Behaviors | ❌ No | ❌ No | Reject |
| Named nodes | 🟡 Partial | ❌ No | Ignore names |
| Custom elements | ❌ No | ❌ No | Reject |

**Recommendation**:
- **DO NOT attempt full round-trip** AST → Emmet
- Instead: **AST → Emmet only for Emmet-compatible subset**
- Provide a **validation function** `isEmmetCompatible :: AST datum -> Boolean`

---

## Part 5: Proposed Solutions

### 5.1 Document Limitations (MINIMUM - DO THIS)

Create clear documentation:

1. **"When to Use Emmet"** guide:
   - ✅ Learning PSD3 structure
   - ✅ Rapid prototyping simple charts
   - ✅ Teaching D3 concepts
   - ✅ Static dashboards
   - ❌ Interactive visualizations
   - ❌ Animated transitions
   - ❌ Complex computations
   - ❌ Production applications

2. **"Graduating from Emmet"** tutorial:
   - Show Emmet → AST equivalents
   - Teach when/how to move to full API
   - Provide migration examples

3. **Update all three demos** to show limitations:
   - tree-builder3: Add "Limitations" section
   - simple-tree-builder: Note "For simple charts only"
   - chart-builder: Show when you need full AST

### 5.2 Add Validation Function (RECOMMENDED)

```purescript
module PSD3.Emmet.Compatibility where

-- | Check if an AST can be losslessly represented in Emmet notation
isEmmetCompatible :: forall datum. Tree datum -> Boolean

-- | Try to convert AST to Emmet, returning Nothing if incompatible
toEmmet :: forall datum. Tree datum -> Maybe String

-- | Analyze AST and report what makes it incompatible
analyzeCompatibility :: forall datum. Tree datum -> CompatibilityReport

type CompatibilityReport =
  { compatible :: Boolean
  , reasons :: Array IncompatibilityReason
  }

data IncompatibilityReason
  = HasComputedAttributes
  | HasTransitions
  | HasBehaviors
  | HasConditionals
  | HasCustomElements
  | HasNamedSelections
```

**Use case**: In tree-builder3, show whether current AST is Emmet-compatible.

### 5.3 Extend Emmet with Expression Language (NOT RECOMMENDED)

❌ **We considered but reject** adding expression syntax to Emmet:

```
-- Hypothetical extended Emmet:
c[cx:(x * 40 + 50),cy:(y * 30),fill:if(value > 50, "red", "blue")]
```

**Why reject**:
- Destroys simplicity (Emmet's core value)
- Reinvents PureScript expression syntax (poorly)
- Still can't express behaviors, transitions, etc.
- Creates two competing syntaxes

**Verdict**: If you need expressions, **use the full AST**.

### 5.4 Two-Tier System (RECOMMENDED)

**Position Emmet as Tier 1 of a learning path**:

```
Tier 1: Emmet Notation
  ↓
  Learn: Structure, joins, simple attributes
  Build: Static charts, simple interactions
  ↓
Tier 2: AST with Friendly DSL (Expr.Friendly)
  ↓
  Learn: Computed attributes, expressions, scales
  Build: Animated charts, responsive visualizations
  ↓
Tier 3: Full PSD3 API
  ↓
  Learn: Behaviors, custom interpreters, advanced patterns
  Build: Interactive applications, novel visualizations
```

Documentation should emphasize this progression.

### 5.5 Preprocessing Escape Hatch (OPTIONAL)

Allow users to **preprocess data** before Emmet:

```purescript
-- Instead of trying to express computation in Emmet:
let processedData = data <#> \d ->
      { x: d.x
      , y: d.y
      , scaledX: d.x * 40.0 + 50.0  -- Precompute
      , scaledY: d.y * 30.0
      , color: if d.value > 50.0 then "red" else "blue"  -- Precompute
      }

-- Then use simple Emmet:
let emmet = "j(Point)>c[cx:scaledX,cy:scaledY,fill:color,r=5]"
```

This keeps Emmet simple while enabling complex visualizations.

---

## Part 6: Recommendations

### Immediate Actions

1. **✅ Keep current Emmet exactly as-is** - It's doing its job well
2. **📝 Document limitations clearly** - Update EMMET_GRAMMAR.md and website
3. **🎓 Create "Learning Path" documentation** - Emmet → Friendly → Full API
4. **⚠️ Add warnings in demos** - "For learning/prototyping only"
5. **🔍 Add `isEmmetCompatible` function** - Help users know when to graduate

### Long-term Strategy

1. **Position Emmet as educational tool** - Like training wheels on a bike
2. **Build better Tier 2 (Friendly DSL)** - Make graduation path smooth
3. **Create migration examples** - Show Emmet → AST conversions
4. **Do NOT extend Emmet syntax** - Keep it simple forever
5. **Accept the 25% coverage** - This is a feature, not a bug

### Documentation TODOs

- [ ] Add "Limitations" section to EMMET_GRAMMAR.md
- [ ] Create EMMET_TO_AST_MIGRATION_GUIDE.md
- [ ] Update UnderstandingEmmet page with limitations
- [ ] Add compatibility warnings to all three demos
- [ ] Write "When to Graduate from Emmet" blog post
- [ ] Create comparison chart (Emmet vs Friendly vs Full AST)

---

## Conclusion

**Emmet notation is exactly as powerful as it should be.**

The gap between Emmet and the full AST is not a problem to solve—it's a **design feature**. Emmet succeeds by being:
- Simple enough to teach in 10 minutes
- Powerful enough to build real (simple) visualizations
- Limited enough to avoid complexity paralysis

Users who need more power should **graduate to the full AST**, not wait for Emmet to grow. The AST is there, documented, and ready.

Our job now is to:
1. **Document the limitations clearly**
2. **Make the graduation path obvious**
3. **Resist the temptation to extend Emmet**

Keep Emmet simple. Forever.

---

**Status**: ✅ Analysis complete
**Next**: Documentation updates per recommendations above
