# BubblePack Implementation: Lessons Learned

## Date: 2025-11-05

## Context
Built BubblePack visualization from scratch following VISUALIZATION_GUIDE.md to test if the guide + existing examples are sufficient for building complex visualizations.

## Key Lessons

### 1. ✅ Custom `datum_` Accessors Required

**Issue**: Initially imported `datum_` from `D3.Viz.Spago.Model`, causing type mismatches.

**Solution**: Each visualization needs its own `datum_` accessor object tailored to its data structure.

**Pattern**:
```purescript
-- src/website/Viz/YourViz/Model/Accessors.purs
module D3.Viz.YourViz.Model.Accessors where

import Prelude
import D3.Viz.YourData.Files (YourNodeData)
import PSD3.Internal.Types (Datum_)
import Unsafe.Coerce (unsafeCoerce)

datum_ ::
  { id :: Datum_ -> Int
  , name :: Datum_ -> String
  , customField :: Datum_ -> YourType
  , radius :: Datum_ -> Number
  , colorByCategory :: Datum_ -> String
  -- Add all accessors your viz needs
  }
datum_ =
  { id: _.id <<< unbox
  , name: _.name <<< unbox
  , customField: _.customField <<< unbox
  , radius: \d -> calculateRadius (unbox d)
  , colorByCategory: \d -> categoryToColor ((unbox d).category)
  }
  where
    unbox :: Datum_ -> YourNodeData
    unbox = unsafeCoerce
```

**Documentation TODO**: Add HowTo guide "Creating Custom Datum Accessors"

### 2. ✅ Extend `D3_TreeNode` for Hierarchical Data

**Issue**: Created cyclic type synonym `HierarchicalNode` with self-reference.

**Solution**: Extend the library's `D3_TreeNode` type using row types.

**Wrong**:
```purescript
-- ❌ Creates type cycle
type HierarchicalNode =
  { name :: String
  , children :: Maybe (Array HierarchicalNode)  -- Self-reference!
  }
```

**Right**:
```purescript
-- ✅ Extend D3_TreeNode properly
type HierarchicalNodeData =
  { id :: NodeID
  , nodeType :: HierarchyNodeType
  }

type HierarchicalNode = D3_TreeNode (EmbeddedData HierarchicalNodeData)
```

**Documentation TODO**: Add HowTo guide "Extending TreeNode and SimulationNode Row Types"

### 3. Link Accessors Pattern

**Pattern**: Similar to `datum_`, links need their own accessor object:

```purescript
link_ ::
  { source :: Datum_ -> YourNodeData
  , target :: Datum_ -> YourNodeData
  , linkClass :: Datum_ -> String
  , color :: Datum_ -> String
  }
link_ =
  { source: _.source <<< unboxLink
  , target: _.target <<< unboxLink
  , linkClass: \_ -> "link"
  , color: \d -> linkTypeToColor (unboxLink d)
  }
  where
    unboxLink :: Datum_ -> { source :: YourNodeData, target :: YourNodeData }
    unboxLink = unsafeCoerce
```

### 4. Force Configuration Gotchas

**Issue**: Used `F.iterations` which doesn't exist or expects Number not Int.

**Solution**: Check force library for available parameters. Not all D3 force parameters are exposed.

**Available force params** (from `PSD3.Internal.Simulation.Config`):
- `F.strength`
- `F.radius`
- `F.x`, `F.y`
- `F.distance`
- `F.theta`
- `F.distanceMin`, `F.distanceMax`
- `F.numKey`

### 5. State Type Definition Needs Extraction

**Issue**: CodeExplorer's State module contains both generic scene infrastructure AND Spago-specific types mixed together. This makes it unclear what's reusable vs visualization-specific.

**Problem**: When building BubblePack, we hit type errors because:
- `BubblePackAttributes` was defined differently in State.purs vs Attributes.purs
- SceneConfig type parameters weren't clear
- Simulation node types (SpagoSimNode) were visualization-specific but embedded in generic-looking State

**Solution Needed**: Extract generic scene state infrastructure to library, leave only viz-specific types in visualization State module.

**Thrashing observed**: We cycled through multiple type errors trying to understand which types needed to match where, because the CodeExplorer example doesn't clearly separate:
- Generic scene/state infrastructure (should be in library or shared)
- Visualization-specific data types (should be in viz State)
- Type aliases for convenience (SceneConfig, MiseEnScene)

**Documentation TODO**:
- Move generic State infrastructure to library (or create shared Component/Common/)
- Create clear HowTo on "Setting up Component State for a new Visualization"
- Show which types are required, which are optional, which should match library types

### 6. Import Organization

**Best Practice**: Create separate accessor module to avoid circular dependencies:

```
Viz/YourViz/
├── Files.purs           # Data loading
├── Model.purs           # Types, initializers, pack layout
├── Model/
│   └── Accessors.purs   # datum_ and link_ accessors
├── Draw.purs            # Update function
├── Render.purs          # DOM callbacks
└── Attributes.purs      # Visual styling
```

This prevents:
- Files.purs importing Model.purs (for types)
- Model.purs importing Files.purs (for data)
- Both needing datum_ accessors

## Remaining Tasks for Documentation

### New HowTo Guides Needed:

1. **"Creating Custom Datum Accessors"**
   - Why each viz needs its own `datum_`
   - Pattern for unboxing Datum_
   - Common accessor patterns (radius, color, position)
   - Link accessors

2. **"Extending TreeNode and SimulationNode Row Types"**
   - Using `EmbeddedData` to extend D3 types
   - Avoiding type cycles in hierarchical structures
   - Pattern for pack layouts vs force layouts
   - When to use `D3_TreeNode` vs `D3_SimulationNode`

3. **"Force Library Parameters Reference"**
   - Complete list of available F.* parameters
   - Type requirements (Number vs Int)
   - Which parameters work with which force types

### Updates to Existing Guides:

1. **VISUALIZATION_GUIDE.md**: Add note about custom datum_ requirement
2. **QUICK_REFERENCE.md**: Add accessor pattern examples

## Success Metrics

✅ **Guide was sufficient** to build complete visualization architecture
✅ **File structure patterns** worked well (Component/ + Viz/ separation)
✅ **Scene configuration pattern** is declarative and composable
✅ **Force library pattern** is reusable across visualizations

🔧 **Missing pieces** (now documented):
- Custom datum_ accessor requirement
- TreeNode extension pattern
- Force parameter reference
- **State type structure** - unclear what's generic vs viz-specific in CodeExplorer example

## Conclusion

The VISUALIZATION_GUIDE.md + existing examples (CodeExplorer, LesMisGUP) provide a **good foundation** but need refinement:

✅ **What Worked**:
- Overall architecture (Component/ + Viz/ separation)
- Scene configuration pattern
- Force library pattern
- General file organization

⚠️ **What Caused Thrashing**:
- CodeExplorer mixes generic and specific types in State module
- Not clear which types should match library definitions
- Unclear what to copy vs what to adapt from CodeExplorer

**Next Steps**:
1. Extract generic scene/state infrastructure from CodeExplorer to library or shared location
2. Leave only Spago-specific types in CodeExplorer's State
3. Create clear template showing minimal State module structure
4. Update VISUALIZATION_GUIDE.md with State setup section
