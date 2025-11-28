# Code Atlas

**Status:** Phase 1 & 2 Complete | Phase 3 Pending | Phase 4 Design Phase

A comprehensive codebase analysis system that extracts rich metadata from PureScript compiler output to enable advanced visualization and querying capabilities.

## Overview

Code Atlas mines the PureScript compiler's output files (`docs.json`, `corefn.json`, `externs.cbor`) to create a queryable knowledge graph of your codebase. This goes far beyond traditional dependency graphs to provide function-level call graphs, type usage patterns, and declaration metadata.

## Current Capabilities (Phases 1-2)

### Extracted Data
- **7,905 declarations** across 806 modules
- **23,679 cross-module function calls** from 11,858 functions
- **636 types** with usage relationships
- Complete type signatures with referenced types
- Full documentation strings
- Source code locations

### Data Files Generated

```
docs/data/spago-data/
├── declarations.json       # Complete declaration registry (Phase 1)
│   └── 7,905 declarations with signatures, docs, source locations
│
├── function-calls.json     # Function call graph (Phase 2)
│   └── 23,679 cross-module calls, bidirectional index
│
├── type-dependencies.json  # Type usage graph (Phase 1)
│   └── 636 types with usage relationships
│
├── modules.json           # Module dependency graph (Spago)
├── packages.json          # Package dependency graph (Spago)
├── LOC.json              # Lines of code metrics
└── lsdeps.jsonlines      # Package repositories
```

### Sample Queries Currently Possible

```javascript
// What functions does WealthHealth.draw call?
functionCalls.functions["D3.Viz.WealthHealth.Draw.draw"].calls
// → ["PSD3.Capabilities.Selection.attach",
//    "PSD3.Capabilities.Selection.appendTo", ...]

// What calls appendTo?
functionCalls.functions["PSD3.Capabilities.Selection.appendTo"].calledBy
// → ["D3.Viz.BarChart.draw", "D3.Viz.BubbleChart.draw", ...]

// What types does SelectionM typeclass use?
declarations.modules["PSD3.Capabilities.Selection"]
  .declarations[0].members[0].typeSignature.referencedTypes
// → ["PSD3.Internal.Types.Element", ...]

// How many functions in a module?
Object.values(functionCalls.functions)
  .filter(f => f.module === "PSD3.Capabilities.Selection").length
// → 10
```

## Implementation Details

### Phase 1: Declaration Mining
**Source:** `output/*/docs.json` (compiler-generated documentation)

Extracts:
- Function signatures with full type information
- Type definitions (data, typeSynonym, typeClass)
- Documentation strings
- Source locations
- Typeclass members

**Algorithm:**
1. Iterate all module directories in `output/`
2. Parse `docs.json` for each module
3. Extract declarations with metadata
4. Simplify type signatures to extract referenced types
5. Build type dependency graph

**Code:** `scripts/generate-spago-data.js::extractDeclarations()`

### Phase 2: Function Call Graph
**Source:** `output/*/corefn.json` (CoreFn intermediate representation)

Extracts:
- Cross-module function calls (actual usage, not just imports)
- Call graph with both directions (calls & calledBy)
- Distinguishes cross-module vs intra-module

**Algorithm:**
1. Parse CoreFn AST for each declaration
2. Recursively walk expression tree
3. Find all `Var` nodes (variable references)
4. Extract module and identifier
5. Build bidirectional index

**Code:** `scripts/generate-spago-data.js::extractFunctionCalls()`

**CoreFn Node Types Handled:**
- `Var` - Variable/function references
- `App` - Function application
- `Abs` - Lambda abstraction
- `Let` - Let bindings
- `Case` - Case expressions
- `Accessor` - Record field access
- `ObjectUpdate` - Record updates
- `Literal` - Array/Object literals

### Phase 3: Typeclass Instances (Pending)
**Source:** `output/*/externs.cbor` (type checking information)

Will extract:
- All typeclass instances
- Instance → TypeClass + Type mapping
- Orphan instance detection

**Status:** Dependency installed (`cbor` npm package), implementation pending

**Challenges:**
- CBOR binary format requires parsing
- Instance identifiers are mangled in CoreFn
- Need to decode instance dictionaries

## Architecture

### Data Generation
```
PureScript Source Code
         ↓
    spago build
         ↓
    output/ directory
    ├── */docs.json      → Phase 1: Declarations
    ├── */corefn.json    → Phase 2: Function calls
    └── */externs.cbor   → Phase 3: Instances (TODO)
         ↓
  generate-spago-data.js
         ↓
  docs/data/spago-data/
    ├── declarations.json
    ├── function-calls.json
    └── type-dependencies.json
```

### Future: Standalone Tool
Code Atlas is designed to eventually become a standalone npm package:

```
purescript-code-atlas/
├── src/
│   ├── parsers/
│   │   ├── docs.js      # Parse docs.json
│   │   ├── corefn.js    # Parse corefn.json
│   │   └── externs.js   # Parse externs.cbor
│   ├── analyzers/
│   │   ├── types.js
│   │   ├── functions.js
│   │   └── instances.js
│   └── index.js         # CLI entry point
├── bin/
│   └── code-atlas       # CLI executable
└── output/
    └── atlas.json       # Unified output
```

**Benefits:**
- Reusable by IDEs, linters, documentation tools
- Incremental parsing (only changed modules)
- Database storage for time-series analysis
- Git integration for change tracking

## Usage

### Regenerate Data

**Important:** The large data files (`declarations.json`, `function-calls.json`, `type-dependencies.json`) are **not committed to git** (they total ~56MB). You must regenerate them after cloning:

```bash
npm run build              # Compile PureScript first
node scripts/generate-spago-data.js
```

**Output:**
```
📦 Generating modules.json...
✓ modules.json generated
📦 Generating packages.json...
✓ packages.json generated
📊 Generating LOC.json...
✓ LOC.json generated (1753 files)
🔗 Generating lsdeps.jsonlines...
✓ lsdeps.jsonlines generated (322 packages)
📚 Generating declarations.json...
✓ declarations.json generated (806 modules, 7905 declarations)
🔗 Generating type-dependencies.json...
✓ type-dependencies.json generated (636 types)
🔄 Generating function-calls.json...
✓ function-calls.json generated (11858 functions, 23679 cross-module calls)

✅ All Spago data files generated successfully!
```

### Load Data in PureScript

```purescript
-- Example data loading module (to be created)
module CodeAtlas.Data where

import Prelude
import Affjax (get)
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Decode (decodeJson)
import Effect.Aff (Aff)

type AtlasData =
  { declarations :: DeclarationsData
  , functionCalls :: FunctionCallsData
  , typeDependencies :: TypeDependenciesData
  }

loadAtlasData :: Aff (Either String AtlasData)
loadAtlasData = do
  -- Load all data files
  -- Parse JSON
  -- Return combined data structure
```

## Data Model

See [code-atlas-data-model.md](./code-atlas-data-model.md) for comprehensive documentation of:
- All entity types and relationships
- 45+ derived queries possible
- Visualization opportunities
- Missing data points

**Quick Reference:**

**Direct Relationships (8):**
1. Module → Module dependencies
2. Package → Package dependencies
3. Module → Package membership
4. Module → Declarations
5. Declaration → Type references
6. Function → Function calls
7. Type → Type dependencies
8. TypeClass → Members

**Derived Relationships (37+):**
- Coupling/cohesion scores
- Dead code detection
- Refactoring candidates
- Impact analysis
- And many more...

## Next Steps (Phase 4)

### High-Priority Visualizations

1. **Declaration Browser** ⭐
   - Searchable table of all declarations
   - Filter by module, kind, has-documentation
   - Click to see full signature and callers

2. **Function Call Flow** ⭐
   - Interactive graph showing cross-module calls
   - Click function to highlight callers/callees
   - Path finding between any two functions

3. **Module Dependency Explorer** ⭐
   - Current force-directed graph enhanced with:
     - Click module → show its declarations
     - Filter by coupling score
     - Highlight circular dependencies

4. **Type Usage Network**
   - Graph of type dependencies
   - Find types used across many modules
   - Identify coupling via shared types

5. **Metrics Dashboard**
   - Module complexity scores
   - Documentation coverage
   - Dead code report
   - API surface area

### Technical TODOs

- [ ] Create PureScript data loading modules
- [ ] Design efficient data structures for client-side queries
- [ ] Implement search/filter UI components
- [ ] Add lazy loading for large datasets
- [ ] Create reusable graph visualization components
- [ ] Add export functionality (CSV, JSON, SVG)

### Future Enhancements

**Phase 3: Instance Extraction**
- Parse `externs.cbor` for typeclass instances
- Build instance resolution graph
- Find orphan instances
- Visualize typeclass hierarchies

**Database Storage:**
- Store snapshots over time
- Track metrics evolution
- Blame/ownership analysis
- Identify hotspots and churn

**Git Integration:**
- Run extraction on each commit
- Track API surface changes
- Identify breaking changes
- Generate changelogs

**Advanced Analytics:**
- ML-based code smell detection
- Automated refactoring suggestions
- Architecture conformance checking
- Complexity trend prediction

## References

- PureScript Compiler: https://github.com/purescript/purescript
- CoreFn Spec: https://github.com/purescript/documentation/blob/master/language/CoreFn.md
- Spago: https://github.com/purescript/spago

## Contributing

This is part of the PS<$>D3 documentation website codebase. See main README for contribution guidelines.

## License

Same as PS<$>D3 project.
