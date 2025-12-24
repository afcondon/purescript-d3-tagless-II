# Chimeric Visualizations - Work In Progress Status

**Branch**: `feature/chimeric-viz-ast`
**Date**: 2025-12-24
**Status**: AST implementation complete, proof-of-concept blocked on data binding issues

## What Was Accomplished ✅

### 1. AST Extensions (Complete)
Added two new Tree constructors to `psd3-selection/src/PSD3/AST.purs`:

```purescript
| ConditionalRender
    { cases :: Array { predicate :: datum -> Boolean, spec :: datum -> Tree datum }
    }
| LocalCoordSpace
    { scaleX :: datum -> Number
    , scaleY :: datum -> Number
    , child :: Tree datum
    }
```

Plus smart constructors: `conditionalRender`, `conditionalRenderOr`, `localCoordSpace`, `localCoordSpaceFixed`

### 2. Interpreter Updates (Complete)
All 5 interpreters successfully updated:

- **Selection/Operations.purs**: Implements ConditionalRender by evaluating predicates and rendering matching spec
- **Interpreter/English.purs**: Natural language descriptions ("Render based on conditions...", "Transform coordinate space...")
- **Interpreter/MetaAST.purs**: AST visualization with `ConditionalRenderAST` and `LocalCoordSpaceAST`
- **Interpreter/Mermaid.purs**: Diagram generation with conditional/coord space nodes
- **Interpreter/SemiQuine/TreeToCode.purs**: Code generation (emits `T.conditionalRender`)

### 3. TreeBuilder3 Integration (Complete)
Added node types and pattern matches across all builder modules:

- `TreeBuilder3/Types.purs`: `NodeConditionalRender`, `NodeLocalCoordSpace`
- `TreeBuilder3/Theme.purs`: Colors for new node types
- `TreeBuilder3/TypePropagation.purs`: Type flow logic
- `TreeBuilder3/Converter.purs`: Conversion to AST (marked as TODO)
- `TreeBuilder3/ASTImporter.purs`: Import from existing AST
- `TreeBuilder3/FormInterpreter.purs`: Form rendering
- `TreeBuilder3/App.purs`: Keyboard handlers (stub implementations)

### 4. EmmetParser Updates (Complete)
- Added metadata handling for ConditionalRender in `EmmetParser/Metadata.purs`
- Mark as incompatible with Emmet (uses predicates)

## The Problem: ConditionalChimera Example 🚧

### File Location
`demo-website/src/Viz/ConditionalChimera.purs.WIP` (renamed to prevent build errors)

### Concept
Force-directed network where nodes render differently based on degree:
- Hub nodes (degree >= 3): Large red circles
- Bridge nodes (degree == 2): Medium blue circles
- Leaf nodes (degree == 1): Small green circles

### Critical Issue: Datum Type Mismatches

**The Error:**
```
Could not match type Node with type Unit
while trying to match type Tree Node with type Tree Unit
```

**Root Cause**: TreeAPI has complex phantom type constraints we don't fully understand.

#### What We Tried:

1. **Initial approach** - Used `from` for data binding:
   ```purescript
   linksGroup :: Array SLink -> T.Tree Unit  -- ❌ Wrong!
   linksGroup swizzled =
     T.joinData "links" "line" swizzled $ \link ->
       T.elem Line [...]
   ```
   Error: "`link` has type SLink but expected String"

2. **Fixing type signature**:
   ```purescript
   linksGroup :: Array SLink -> T.Tree SLink  -- Still wrong!
   ```
   Error: Container expects `Tree Unit`, got `Tree SLink`

3. **Understanding Tree phantom types**:
   - Container: `T.Tree Unit` (no datum)
   - After `joinData`: `T.Tree datum` (has datum)
   - They can't be mixed in `withChildren`!

### Confusion About `from` Function

From `PSD3/Expr/Friendly.purs`:
```purescript
from :: forall datum a. Show a => String -> (datum -> a) -> Attribute datum
from name f = DataAttr (AttributeName name) UnknownSource (\d -> StringValue $ show (f d))
```

**Question**: Takes a STRING (attribute name) as first arg, but we see code like:
```purescript
from node $ \n -> cx $ num n.x
```

Where `node` is the bound datum variable, not a string! What's going on?

**Hypothesis**: Maybe there are multiple `from` functions? Or different binding mechanism?

## What Needs Investigation 🔍

### 1. Understand TreeAPI Data Flow
- How do joins work with phantom types?
- Can child trees have different datum types than parent?
- Is there a way to "escape" from a join's datum type back to Unit?

### 2. Study Existing Examples
Look at working force graph examples:
- `Viz/Simpsons/ForceViz.purs` - Uses joins successfully
- `Viz/LesMisV3/Draw.purs` - Complex network viz
- `Viz/SimpleForceGraph.purs` - Basic network

Questions:
- How do they handle links (which need source/target node references)?
- How do they bind positions in tick functions?
- What's the type signature pattern?

### 3. Alternative Approaches

#### Option A: Don't use joins
```purescript
-- Manual DOM manipulation instead of joinData?
-- Like old D3 style?
```

#### Option B: Separate trees per type
```purescript
-- Build three separate trees: hubs, bridges, leaves
-- Combine them somehow?
```

#### Option C: Use UpdateJoin with GUP
```purescript
-- Maybe ConditionalRender needs to work with UpdateJoin?
-- Enter/update/exit could handle different templates?
```

#### Option D: Rethink the API
```purescript
-- Maybe ConditionalRender should work differently
-- Perhaps it shouldn't take (datum -> Tree datum)
-- but instead take pre-built trees?
T.conditionalRender
  [ { predicate: \n -> n.degree >= 3, tree: hubCircle }
  , { predicate: \n -> n.degree == 2, tree: bridgeCircle }
  ]
```

## Files Changed

### Core Library (psd3-selection)
- `src/PSD3/AST.purs` - AST constructors
- `src/PSD3/Internal/Selection/Operations.purs` - D3 rendering
- `src/PSD3/Interpreter/English.purs` - English interpreter
- `src/PSD3/Interpreter/MetaAST.purs` - AST visualization
- `src/PSD3/Interpreter/Mermaid.purs` - Diagram generation
- `src/PSD3/Interpreter/SemiQuine/TreeToCode.purs` - Code generation

### Demo Website (demo-website)
- `src/TreeBuilder3/*.purs` - All builder modules
- `src/EmmetParser/Metadata.purs` - Metadata handling
- `src/Component/Tour/TourInterpreters.purs` - Tour rendering
- `src/TreeBuilder/App.purs` - TreeBuilder app
- `src/Viz/ConditionalChimera.purs.WIP` - Blocked example

## Documentation
- `notes/CHIMERIC_VISUALIZATIONS.md` - Original research/brainstorming
- `notes/CHIMERIC_VIZ_AST_DESIGN.md` - AST design decisions

## Next Session Action Items

1. **Research Phase** (1-2 hours)
   - Read through `Viz/Simpsons/ForceViz.purs` carefully
   - Understand how they bind data in joins
   - Document the pattern

2. **Experiment Phase**
   - Try simplest possible ConditionalRender example (no force sim)
   - Just static data, conditional circles
   - Build intuition for datum type flow

3. **Design Decision**
   - Decide if current API is correct
   - If not, redesign before proceeding
   - Document the chosen pattern

4. **Implementation**
   - Once pattern is clear, rebuild ConditionalChimera
   - Add tests
   - Add to routing/navigation

## Questions for Future Discussion

1. Should ConditionalRender be inside or outside joins?
2. Do we need a different combinator for "chimeric joins"?
3. Is the phantom type approach fighting us here?
4. Would a simpler API (pre-built trees instead of functions) work better?
5. How does this relate to UpdateJoin's GUP phases?

## Success Criteria

Before merging this feature:
- [ ] At least one working chimeric viz example
- [ ] Clear documentation of the data binding pattern
- [ ] Tests demonstrating the feature
- [ ] Added to demo website navigation
- [ ] Updated CHIMERIC_VISUALIZATIONS.md with working code

## Current Build Status

**Main build**: ✅ Passes (ConditionalChimera.purs renamed to .WIP)
**All tests**: Not run yet
**Demo website**: Builds successfully

---

**Key Insight**: The AST extensions work perfectly. The challenge is understanding TreeAPI's data binding model well enough to use ConditionalRender effectively. This is a learning problem, not a design problem.
