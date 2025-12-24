# Chimeric Visualizations for Tidal Cycles Pattern Trees

## Summary

We've successfully implemented **chimeric visualizations** - hybrid visualizations that render different node types with completely different visual representations based on data properties. The CombinatorTree has been refactored to use the new `ConditionalRender` AST constructor, demonstrating the proper architecture for Tidal pattern visualization.

## What Are Chimeric Visualizations?

A **chimeric visualization** uses different visual representations for different parts of the data **within a single tree structure**. Think of it like the mythological chimera - one creature with parts from different animals.

### Example: Tidal Combinator Tree

In our Tidal combinator tree:
- **Combinator nodes** ("jux rev", "slow 2") → Render as **purple labeled circles**
- **Pattern leaves** ("drums", "hats") → Render as **mini sunburst diagrams** showing internal structure

This is chimeric because each node gets a completely different visual treatment based on its type (`node.isPattern`), but they're all part of one coherent tree visualization.

## The Architecture

### 1. AST Extension: ConditionalRender

We added a new constructor to `PSD3.AST`:

```purescript
data Tree datum
  = ...
  | ConditionalRender (Array { predicate :: datum -> Boolean, spec :: datum -> Tree datum })
```

**How it works:**
- Evaluates predicates in order against the bound datum
- When a predicate returns `true`, renders using that spec (template function)
- Falls through to next predicate if `false`

### 2. Monomorphic Constraint

**Critical insight:** All parts of a chimeric tree must share the same phantom type.

```purescript
-- ✅ CORRECT: All nodes have type Tree LayoutNode
combinatorNodeTemplate :: LayoutNode -> T.Tree LayoutNode
miniSunburstTemplate :: LayoutNode -> T.Tree LayoutNode

-- ❌ WRONG: Mixing phantom types fails type checking
T.withChildren [ treeUnit, treeLayoutNode ]  -- Type error!
```

This is because PureScript's phantom types prevent mixing `Tree Unit` with `Tree LayoutNode` in the same children array.

### 3. Two-Step Rendering Pattern

To handle the phantom type constraint, we use a **two-step rendering pattern**:

```purescript
-- Step 1: Render scaffolding (Tree Unit) with named groups
let scaffolding :: T.Tree Unit
    scaffolding =
      T.named Group "root" []
        `T.withChildren`
          [ T.named Group "links" [] `T.withChildren` linkElements
          , T.named Group "nodes" [ attr "id" $ text "nodes" ]  -- Placeholder
          ]

-- Step 2: Render chimeric nodes (Tree LayoutNode) into the placeholder
let chimericTree :: T.Tree LayoutNode
    chimericTree =
      T.joinData "chimeric-nodes" "g" nodes $ \node ->
        T.elem Group [ attr "transform" $ ... ]
          `T.withChildren`
            [ T.conditionalRender
                [ { predicate: \n -> n.isPattern, spec: miniSunburstTemplate }
                , { predicate: \n -> not n.isPattern, spec: combinatorNodeTemplate }
                ]
            ]

-- Render in two steps
runD3v2M do
  svg <- select selector
  _ <- renderTree svg scaffolding       -- Step 1
  nodesGroup <- select "#nodes"
  _ <- renderTree nodesGroup chimericTree  -- Step 2
```

**Why this works:**
- Step 1 creates the DOM structure (links, placeholder groups)
- Step 2 selects the placeholder and populates it with the chimeric tree
- Each step has a consistent phantom type internally

## The CombinatorTree Refactor

### Before (Imperative)

```purescript
Array.concatMap (\node ->
  if node.isPattern
  then [ buildMiniSunburst node.x node.y 40.0 node.pattern node.label ]
  else [ buildCombinatorNode node.x node.y node.label ]
) nodes
```

**Problems:**
- Imperative: decision logic hidden in if/else
- Hard to extend with more node types
- Not using the TreeAPI declarative style

### After (Declarative with ConditionalRender)

```purescript
T.joinData "chimeric-nodes" "g" nodes $ \node ->
  T.elem Group [ attr "transform" $ text ("translate(" <> show node.x <> "," <> show node.y <> ")") ]
    `T.withChildren`
      [ T.conditionalRender
          [ { predicate: \n -> n.isPattern, spec: miniSunburstTemplate }
          , { predicate: \n -> not n.isPattern, spec: combinatorNodeTemplate }
          ]
      ]
```

**Benefits:**
- Declarative: rendering decision is explicit in the tree structure
- Easy to extend: just add more predicate/spec pairs
- Fits the TreeAPI paradigm
- All interpreters can handle it (D3, Mermaid, English, etc.)

### Template Functions

Template functions take the datum and return a tree with the same phantom type:

```purescript
-- Template for combinator nodes: purple circles
combinatorNodeTemplate :: LayoutNode -> T.Tree LayoutNode
combinatorNodeTemplate node =
  T.named Group ("comb-" <> node.label) []
    `T.withChildren`
      [ T.elem Circle [ r $ num 20.0, fill $ text "#E1BEE7", ... ]
      , T.elem Text [ textContent $ text node.label, ... ]
      ]

-- Template for pattern leaves: mini sunbursts
miniSunburstTemplate :: LayoutNode -> T.Tree LayoutNode
miniSunburstTemplate node =
  case node.pattern of
    Just pattern ->
      -- Build partition layout, create arcs
      let arcs = ... -- sunburst arcs from pattern structure
      in T.named Group ("mini-sunburst-" <> node.label) []
           `T.withChildren`
             (map arcElement arcs <> [centerCircle, label])
    Nothing -> combinatorNodeTemplate node  -- Fallback
```

## How to Use This for Other Tidal Visualizations

### 1. Identify Your Node Types

Determine what makes nodes different. Examples:
- **Leaf vs branch** (terminal sounds vs combinators)
- **Node type** (sequence, parallel, choice, fast, slow, etc.)
- **Depth** (different rendering for different tree levels)
- **Properties** (cycle length, probability, complexity metrics)

### 2. Define Your Datum Type

Create a record type with all the fields needed by any template:

```purescript
type MyPatternNode =
  { label :: String
  , nodeType :: String  -- "sound", "combinator", "sequence", etc.
  , pattern :: Maybe PatternTree
  , metrics :: PatternMetrics
  , x :: Number
  , y :: Number
  , depth :: Int
  -- Add whatever else your templates need
  }
```

**Important:** The datum type must have `Eq` and `Ord` instances:

```purescript
derive instance Eq MyPatternNode
derive instance Ord MyPatternNode
```

### 3. Write Template Functions

Each template is a pure function: `datum -> Tree datum`

```purescript
-- Example: Different templates for different node types
soundTemplate :: MyPatternNode -> T.Tree MyPatternNode
soundTemplate node =
  T.elem Circle
    [ r $ num 10.0
    , fill $ text "#4CAF50"  -- Green for sounds
    ]

sequenceTemplate :: MyPatternNode -> T.Tree MyPatternNode
sequenceTemplate node =
  T.elem Rect
    [ width $ num 40.0
    , height $ num 20.0
    , fill $ text "#2196F3"  -- Blue for sequences
    ]

fastTemplate :: MyPatternNode -> T.Tree MyPatternNode
fastTemplate node =
  -- Fancy visualization showing speed multiplication
  T.named Group "fast-viz" []
    `T.withChildren` [ ... diagonal stripes, etc. ]
```

### 4. Build ConditionalRender Tree

```purescript
T.joinData "pattern-nodes" "g" nodes $ \node ->
  T.elem Group [ attr "transform" $ ... ]
    `T.withChildren`
      [ T.conditionalRender
          [ { predicate: \n -> n.nodeType == "sound", spec: soundTemplate }
          , { predicate: \n -> n.nodeType == "sequence", spec: sequenceTemplate }
          , { predicate: \n -> n.nodeType == "fast", spec: fastTemplate }
          , { predicate: \n -> n.nodeType == "slow", spec: slowTemplate }
          -- Add as many as you need
          ]
      ]
```

### 5. Use Two-Step Rendering

```purescript
runD3v2M do
  container <- select selector

  -- Step 1: Scaffolding (Tree Unit)
  let scaffolding =
        T.named SVG "svg" [...]
          `T.withChildren`
            [ T.named Group "background" []
            , T.named Group "nodes" [ attr "id" $ text "nodes" ]  -- Placeholder
            ]
  _ <- renderTree container scaffolding

  -- Step 2: Chimeric tree (Tree MyPatternNode)
  let chimericTree = ...  -- Your ConditionalRender tree
  nodesGroup <- select "#nodes"
  _ <- renderTree nodesGroup chimericTree

  pure unit
```

## Next Steps for Tidal Visualization

### Immediate Opportunities

1. **Extend CombinatorTree**: Add more combinator types
   - Add templates for `Euclidean`, `Degrade`, `Elongate`
   - Each gets its own visual treatment (currently they fall through to default)

2. **Sunburst Chimera**: Make the sunburst itself chimeric
   - Currently uses patterns/strokes to differentiate combinators
   - Could use ConditionalRender for richer per-arc visuals
   - Example: `fast` arcs could show internal ticks/subdivisions

3. **Mixed Layout Chimera**: Different layouts based on pattern complexity
   - Simple patterns → tree layout
   - Complex patterns → sunburst layout
   - Medium complexity → radial tree
   - All in one forest visualization!

### Advanced Ideas

4. **Depth-based Chimera**: Change representation by tree depth
   - Depth 0-1: Full detail (sunbursts)
   - Depth 2-3: Simplified (just colored circles)
   - Depth 4+: Tiny dots (overview only)

5. **Interactive Chimera**: User toggles representations
   - Click to cycle between tree/sunburst/compact for each pattern
   - Store layout preference in node datum
   - ConditionalRender checks the preference field

6. **Metric-driven Chimera**: Visualization adapts to pattern properties
   - High entropy → sunburst (shows structure)
   - Low entropy (repetitive) → compact linear
   - Probabilistic patterns → fuzzy/dithered rendering

### Pattern Structure Exploration

The Tidal pattern tree is **inherently hierarchical and chimeric**:

```
jux rev                    ← Combinator (affects entire subtree)
├── slow 2                 ← Combinator (time transformation)
│   └── [bd sd:3, ~ hh]   ← Pattern (sequence with parallel children)
│       ├── bd            ← Sound atom
│       └── sd:3          ← Sound atom
└── hh*4                   ← Pattern (fast combinator applied to sound)
    └── hh                ← Sound atom
```

Each level could have different visual treatment:
- **Combinators** (jux, slow): Labeled nodes showing operation
- **Structure** (sequence, parallel): Container shapes (rect vs diamond)
- **Atoms** (sounds): Small colored circles or icons
- **Time transforms** (fast, slow): Show subdivision/expansion visually

## File Locations

- **AST definition**: `psd3-selection/src/PSD3/AST.purs`
  - `ConditionalRender` constructor added

- **Interpreters** (all updated to handle ConditionalRender):
  - `psd3-selection/src/PSD3/Interpreter/D3.purs` - D3 rendering
  - `psd3-selection/src/PSD3/Interpreter/Mermaid.purs` - Mermaid diagrams
  - `psd3-selection/src/PSD3/Interpreter/English.purs` - English descriptions
  - `psd3-selection/src/PSD3/Interpreter/Meta.purs` - AST metadata
  - `psd3-selection/src/PSD3/Interpreter/SemiQuine.purs` - Code generation

- **Example implementations**:
  - `demo-website/src/Viz/SimpleChimera.purs` - Minimal test (circles)
  - `demo-website/src/Viz/PatternTree/CombinatorTree.purs` - Tidal combinator tree

- **Test pages**:
  - http://localhost:1234/#/simple-chimera - Simple test
  - http://localhost:1234/combinator-test.html - Tidal tree

## Important Constraints

### 1. Monomorphic Only (For Now)

All nodes in a chimeric tree must share the same phantom type. You cannot mix:
- `Tree Unit` with `Tree Node`
- `Tree Sound` with `Tree Combinator`

**Workaround:** Use a sum type or a record that can represent all node types:

```purescript
-- ❌ BAD: Different phantom types
type Sound = { name :: String }
type Combinator = { op :: String, arg :: Int }

-- ✅ GOOD: Unified type
type PatternNode = { nodeType :: String, label :: String, ... }
```

### 2. Two-Step Rendering Required

You cannot embed `Tree datum` directly in `Tree Unit` with `withChildren`. Use:
1. Render `Tree Unit` scaffolding with placeholders
2. Select placeholders
3. Render `Tree datum` into them

### 3. Template Functions Must Be Pure

ConditionalRender evaluates templates in a pure context. Templates cannot:
- Perform effects
- Access external state
- Make async calls

All data needed by templates must be in the datum.

## Debugging Tips

1. **Type errors about phantom types:**
   - Check that all templates return `Tree datum` (not `Tree Unit`)
   - Verify you're using two-step rendering

2. **"No Ord instance" errors:**
   - Add `derive instance Eq YourType` and `derive instance Ord YourType`

3. **ConditionalRender not switching:**
   - Log predicates to check they return what you expect
   - Ensure predicates are mutually exclusive (or accept first-match behavior)
   - Check predicate order (first match wins)

4. **Selectors failing in step 2:**
   - Ensure placeholder groups have `attr "id" $ text "id-name"`
   - Use simple selectors: `"#id"` not `"#parent > #id"`

## Related Work

- **Original discussion**: This work was prompted by the need to visualize TidalCycles patterns
  where different node types (combinators, sequences, atoms) need fundamentally different
  visual representations.

- **Sunburst with parallel fix**: The `fixParallelLayout` function in `Sunburst.purs` already
  handles heterogeneous layout (parallel nodes stack radially instead of angularly). This is
  complementary to ConditionalRender.

- **SimpleChimera**: The minimal test case (`SimpleChimera.purs`) validates the architecture
  with static data before applying it to complex hierarchical patterns.

## Questions to Explore

1. **Polymorphic chimeras**: Can we extend this to support different phantom types?
   - Would require heterogeneous lists or existential types
   - May need `Variant` or similar for safe representation

2. **Animation chimeras**: Templates that change over time
   - Predicate could depend on animation phase
   - Would require reactive data or behavior integration

3. **Nested chimeras**: ConditionalRender within ConditionalRender
   - Already works! Just return a tree with more ConditionalRender nodes
   - Example: Combinator chooses template, which itself uses ConditionalRender for children

4. **Performance**: Does ConditionalRender have overhead?
   - Predicate evaluation is cheap (pure functions)
   - Could cache results if predicates are expensive
   - No DOM churn (decision happens before rendering)

## Conclusion

ConditionalRender provides a clean, declarative way to build chimeric visualizations. The
CombinatorTree refactor demonstrates the pattern in action for Tidal Cycles patterns.

The key insight is: **chimeric = different visual treatments, monomorphic = shared type**.
By unifying your data into a single datum type with all necessary fields, you can use
ConditionalRender to choose visual templates dynamically while maintaining type safety.

This opens up rich possibilities for Tidal visualization where each pattern construct
gets its own tailored visual representation, all within PSD3's declarative TreeAPI paradigm.

---

**For the next Claude:** The foundation is solid. You can now extend this pattern to create
rich, multi-faceted Tidal pattern visualizations. The combinator tree is just the beginning -
explore metric-driven chimeras, depth-based rendering, and interactive layout switching!
