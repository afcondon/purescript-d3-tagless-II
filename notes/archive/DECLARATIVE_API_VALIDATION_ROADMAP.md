# Declarative API Validation Roadmap

**Goal**: Prove the declarative tree API can handle all real-world PSD3v2 use cases

## Core Capabilities to Validate

### 1. Polymorphic Attributes ✓ (Should Already Work)
**Question**: Can attributes be functions of datum like before?

```purescript
-- Does this work?
tree = named "circle" Circle
  [ cx (\d -> d.x)
  , cy (\d -> d.y)
  , radius (\d -> d.r)
  , fill (\d -> if d.selected then "red" else "blue")
  ]
```

**Status**: Should work - attributes are just `Array (Attribute datum)`, same as before.

**Test**: Create example with data-dependent attributes.

---

### 2. Data Joins with Enter/Update/Exit (GUP Pattern)
**Question**: Can we handle the General Update Pattern declaratively?

**Current State**: Data joins are stubbed in the prototype.

**Challenge**: Type-level heterogeneity - parent and child have different datum types.

**Approach Options**:

#### Option A: Keep Joins Imperative
```purescript
-- Structure is declarative
let structure = named "g" Group [...] `withChild` named "circle" Circle [...]

-- But data join is imperative
JoinResult { enter, update, exit } <- joinData nodes "g" container
enterNodes <- renderTree enter structure
-- Handle update/exit separately
```

#### Option B: Embed Joins in Tree
```purescript
-- Specify join point in tree structure
tree = named "svg" SVG [...] `withChild`
  (dataJoin "nodes" "g" nodeData $  -- Type changes here
    named "nodeGroup" Group [...] `withChild`
      named "circle" Circle [...])

-- renderTree handles enter/update/exit internally
selections <- renderTree container tree
```

**Decision Needed**: Which feels more natural?

---

### 3. Structure Transformations
**Question**: Can we modify tree structures dynamically?

**Use Cases**:
- Add/remove labels based on zoom level
- Add/remove links based on filter
- Switch between Circle and Rect based on mode

**Approach**: Trees are just data, so we can transform them

```purescript
-- Add labels conditionally
addLabels :: Boolean -> Tree NodeDatum -> Tree NodeDatum
addLabels showLabels nodeTree =
  if showLabels
    then nodeTree `withChild` (named "label" Text [...])
    else nodeTree

-- Filter which nodes to show
filterNodes :: (NodeDatum -> Boolean) -> Tree NodeDatum -> Tree NodeDatum
filterNodes predicate (Node node) =
  Node node { children = filter (\t -> shouldKeep predicate t) node.children }

-- Compose transformations
finalTree = baseTree
  # addLabels showLabels
  # addLinks showLinks
  # addHoverEffects interactive
```

**Test**: Create example that transforms tree based on state.

---

### 4. Integration with SimulationM
**Question**: Can we use declarative trees with force simulations?

**Challenge**: Simulations need to update element positions on tick.

**Current Pattern**:
```purescript
-- Render nodes
nodeGroups <- renderNodes nodes container attrs

-- Register tick function
addTickFunction "nodes" $ Step nodeGroups
  [ transform (\d -> translateNode d)
  ]
```

**Declarative Equivalent**:
```purescript
-- Render tree
tree = named "nodes" Group [...] `withChild`
  named "circle" Circle [...]

selections <- renderTree container tree

-- Get selection from map
case Map.lookup "nodes" selections of
  Just nodeGroups -> do
    addTickFunction "nodes" $ Step nodeGroups
      [ transform (\d -> translateNode d)
      ]
```

**Question**: Can we specify tick functions in the tree itself?

```purescript
-- Hypothetical syntax
tree = named "nodes" Group [...]
  `withChild` named "circle" Circle [...]
  `withTickFunction` [ transform (\d -> translateNode d) ]
```

**Status**: Needs exploration.

---

### 5. Nested Multi-Level Structures
**Question**: Can we handle Group → Circle + Text pattern?

**Example**:
```purescript
nodeTemplate =
  named "nodeGroup" Group [class_ "node"] `withChildren`
    [ named "circle" Circle [radius 5, fill "steelblue"]
    , named "label" Text [textContent getName, textAnchor "middle"]
    ]
```

**Status**: Should work with current implementation.

**Test**: Create NestedMatrix or code-explorer nodes.

---

### 6. Behaviors (Zoom, Drag, etc.)
**Question**: Can we attach behaviors to tree-rendered elements?

**Current Pattern**:
```purescript
svg <- appendChild SVG [...] container
_ <- on (Zoom $ defaultZoom ...) svg
```

**Declarative Pattern**:
```purescript
tree = named "svg" SVG [...]
selections <- renderTree container tree

case Map.lookup "svg" selections of
  Just svg -> on (Zoom $ defaultZoom ...) svg
```

**Question**: Can we specify behaviors in the tree?

```purescript
-- Hypothetical
tree = named "svg" SVG [...]
  `withBehavior` (Zoom $ defaultZoom ...)
```

**Status**: Needs design.

---

## Validation Strategy

### Phase 1: Simple Cases (Prove Basics Work)
1. ✅ Basic nested structure (SimpleTreeExample - done)
2. **Polymorphic attributes** - Next
3. **Multi-level nesting** (Group → Circle + Text)
4. **Named access from Map**

### Phase 2: Data Joins (The Hard Part)
5. **Single data join** with enter phase only
6. **Full GUP** - enter/update/exit
7. **Nested data joins** (joins within joins)
8. **Heterogeneous data** (different types at different levels)

### Phase 3: Real-World Integration
9. **Force simulation** integration
10. **Behaviors** (zoom, drag)
11. **Transitions** (animate between states)
12. **Structure transformations**

### Phase 4: Complex Examples
13. **NestedMatrix** - NxN grid with nested elements
14. **code-explorer** - Real force-directed graph with labels
15. **LesMis with GUP** - Full enter/update/exit cycle

---

## Implementation Plan

### Today: Validate Basics
- [x] Basic structure rendering
- [ ] Polymorphic attributes example
- [ ] Multi-level nesting example
- [ ] Named selection access

### Next: Data Joins
- [ ] Design data join API
- [ ] Implement enter phase
- [ ] Implement full GUP
- [ ] Test with simple example

### Then: Real Examples
- [ ] Port NestedMatrix
- [ ] Port code-explorer nodes
- [ ] Measure whether this is actually better

---

## Open Design Questions

### 1. Where Do Behaviors Go?
- **Option A**: After rendering (current approach)
  - Pro: Clean separation
  - Con: Boilerplate to extract from Map

- **Option B**: In tree structure
  - Pro: Everything in one place
  - Con: Tree structure knows about behaviors

### 2. How to Handle Data Joins?
- **Option A**: Keep imperative (use joinData, then renderTree on enter)
  - Pro: Explicit control
  - Con: Mixing styles

- **Option B**: Declarative join nodes in tree
  - Pro: Fully declarative
  - Con: Complex implementation

### 3. Tick Functions - Tree or After?
- Same tradeoff as behaviors

### 4. Return Type - Map or Record?
- **Current**: Map String Selection
  - Pro: Simple, flexible
  - Con: Runtime errors, no autocomplete

- **Alternative**: Type-level record derivation
  - Pro: Type-safe, autocomplete
  - Con: Complex, hard errors

---

## Success Criteria

The declarative API is worth adopting if:

1. ✅ **Readability**: Tree structure is obviously more readable than imperative chains
2. **Feature parity**: Can do everything imperative style can do
3. **Ergonomics**: Not significantly more verbose or awkward
4. **Type safety**: Errors are comprehensible
5. **Performance**: No significant overhead
6. **Adoption path**: Can incrementally migrate existing code

If all 6 criteria are met → Adopt fully
If 1-4 are met but 5-6 aren't → Use for new code only
If 1-3 are met but 4 isn't → Keep as experimental feature
If 1-2 are met but 3 isn't → Abandon or redesign

---

## Next Immediate Steps

1. Create polymorphic attributes example
2. Create multi-level nesting example
3. Design data join API
4. Implement one real example (NestedMatrix or simplified code-explorer)

Let's start with #1 - prove attributes work as expected.
