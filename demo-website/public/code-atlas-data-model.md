# Code Atlas Data Model

This document enumerates all data relationships available in the extracted codebase metadata. Use this as a reference when designing visualizations and queries for Phase 4.

## Data Sources

### 1. `declarations.json` (Phase 1)
**Source:** Compiled `output/*/docs.json` files
**Size:** 7,905 declarations across 806 modules
**Contents:** Complete declaration metadata with type signatures and documentation

### 2. `function-calls.json` (Phase 2)
**Source:** Compiled `output/*/corefn.json` files (CoreFn AST)
**Size:** 11,858 functions with 23,679 cross-module calls
**Contents:** Function call graph with bidirectional relationships

### 3. `type-dependencies.json` (Phase 1)
**Source:** Derived from declarations.json type signatures
**Size:** 636 types
**Contents:** Type → Type usage relationships

### 4. `modules.json` (Existing)
**Source:** `spago graph modules --json`
**Contents:** Module → Module dependency graph

### 5. `packages.json` (Existing)
**Source:** `spago graph packages --json`
**Contents:** Package → Package dependency graph

### 6. `LOC.json` (Existing)
**Source:** File system line counts
**Contents:** Module → Lines of Code metrics

---

## Entity Types

### Module
- **Primary Key:** Module name (string, e.g., "PSD3.Capabilities.Selection")
- **Sources:** All data files
- **Attributes:**
  - name (string)
  - comments/documentation (string)
  - declarations (array)
  - dependencies (from modules.json)
  - dependents (derived)
  - package (from modules.json)
  - LOC (from LOC.json)
  - sourceFilePath (from LOC.json)

### Declaration (Function/Type/TypeClass)
- **Primary Key:** Module.DeclarationName (e.g., "PSD3.Capabilities.Selection.appendTo")
- **Sources:** declarations.json
- **Attributes:**
  - title (string)
  - kind (value | data | typeClass | typeSynonym | alias | externData)
  - module (string)
  - comments (string)
  - typeSignature (object with referencedTypes array)
  - sourceSpan (location in source file)
  - members (for typeclasses - array of member declarations)

### Function
- **Primary Key:** Module.FunctionName
- **Sources:** function-calls.json, declarations.json
- **Attributes:**
  - name (string)
  - module (string)
  - calls (array of function references)
  - calledBy (array of function references)
  - typeSignature (from declarations.json)
  - comments (from declarations.json)

### Type
- **Primary Key:** Module.TypeName
- **Sources:** type-dependencies.json, declarations.json
- **Attributes:**
  - name (string)
  - module (string)
  - kind (data | typeSynonym)
  - usedBy (array of types that reference this type)
  - uses (array of types this type references)

### Package
- **Primary Key:** Package name (string)
- **Sources:** packages.json, lsdeps.jsonlines
- **Attributes:**
  - name (string)
  - version (string)
  - repository (from lsdeps.jsonlines)
  - dependencies (array of package names)
  - modules (derived from modules.json)

---

## Direct Relationships

### 1. Module → Module Dependencies
**Source:** modules.json
**Direction:** Directed graph
**Type:** Many-to-many
**Query:** "What modules does module X import?"
**Reverse Query:** "What modules import module X?"

### 2. Package → Package Dependencies
**Source:** packages.json
**Direction:** Directed graph
**Type:** Many-to-many
**Query:** "What packages does package X depend on?"
**Reverse Query:** "What packages depend on package X?"

### 3. Module → Package Membership
**Source:** modules.json
**Direction:** Many-to-one
**Type:** Each module belongs to exactly one package
**Query:** "What package contains module X?"
**Reverse Query:** "What modules are in package X?"

### 4. Module → Declarations
**Source:** declarations.json
**Direction:** One-to-many
**Type:** A module contains multiple declarations
**Query:** "What declarations are in module X?"
**Reverse Query:** "What module defines declaration X?"

### 5. Declaration → Type References
**Source:** declarations.json (typeSignature.referencedTypes)
**Direction:** Directed graph
**Type:** Many-to-many
**Query:** "What types does function X use in its signature?"
**Reverse Query:** "What functions/declarations use type X?"

### 6. Function → Function Calls (Cross-Module)
**Source:** function-calls.json
**Direction:** Directed graph
**Type:** Many-to-many
**Query:** "What functions does function X call?"
**Reverse Query:** "What functions call function X?"
**Note:** Only includes cross-module calls (intra-module calls filtered out)

### 7. Type → Type Dependencies
**Source:** type-dependencies.json
**Direction:** Directed graph
**Type:** Many-to-many
**Query:** "What types does type X reference?"
**Reverse Query:** "What types reference type X?"

### 8. TypeClass → Members
**Source:** declarations.json (children array)
**Direction:** One-to-many
**Type:** A typeclass contains member functions
**Query:** "What are the members of typeclass X?"
**Reverse Query:** "What typeclass defines member function X?"

---

## Derived/Indirect Relationships

### 9. Module → Functions Defined
**Derived from:** function-calls.json (filter by module)
**Query:** "What functions are defined in module X?"

### 10. Module → Functions Used (Imported)
**Derived from:** function-calls.json (aggregate calls from module X's functions)
**Query:** "What external functions does module X use?"
**Insight:** Compare with Module → Module Dependencies to find unused imports

### 11. Module → Types Defined
**Derived from:** declarations.json (filter by kind=data/typeSynonym)
**Query:** "What types are defined in module X?"

### 12. Module → Types Used (Imported)
**Derived from:** Aggregate all typeSignature.referencedTypes from module's declarations
**Query:** "What external types does module X use?"

### 13. Module → TypeClasses Defined
**Derived from:** declarations.json (filter by kind=typeClass)
**Query:** "What typeclasses are defined in module X?"

### 14. Function → Module Dependencies (Transitive)
**Derived from:** function-calls.json → module extraction
**Query:** "What modules does function X transitively depend on?"
**Algorithm:** Walk call graph, collect unique modules

### 15. Package → Functions Exposed
**Derived from:** Module → Package + Function → Module joins
**Query:** "What functions does package X expose?"

### 16. Package → Types Exposed
**Derived from:** Module → Package + Type → Module joins
**Query:** "What types does package X expose?"

### 17. Function → Depth in Call Graph
**Derived from:** function-calls.json (BFS/DFS from entry points)
**Query:** "How deep in the call hierarchy is function X?"
**Insight:** Identify leaf functions (depth 0) vs coordinator functions

### 18. Function → Popularity (Call Count)
**Derived from:** function-calls.json (count calledBy array length)
**Query:** "What are the most-called functions?"
**Insight:** Core utility functions vs specialized functions

### 19. Module → Cohesion Score
**Derived from:** Intra-module calls vs cross-module calls ratio
**Algorithm:** (Would need intra-module calls, currently filtered out)
**Query:** "How cohesive is module X?"

### 20. Module → Coupling Score
**Derived from:** Count of modules X depends on + modules that depend on X
**Query:** "How coupled is module X to the rest of the codebase?"

### 21. Declaration → Kind Distribution by Module
**Derived from:** declarations.json stats.byKind aggregated per module
**Query:** "What's the ratio of functions to types in module X?"

### 22. Circular Dependencies (Modules)
**Derived from:** modules.json (cycle detection)
**Query:** "What modules are in dependency cycles?"
**Algorithm:** Tarjan's strongly connected components

### 23. Circular Dependencies (Functions)
**Derived from:** function-calls.json (cycle detection)
**Query:** "What functions are in call cycles?"
**Note:** Mutual recursion is valid, but cross-module cycles may indicate design issues

### 24. Orphan Declarations
**Derived from:** declarations.json + function-calls.json
**Query:** "What functions/types are defined but never used?"
**Algorithm:** Find declarations with empty calledBy and not referenced in any type signature

### 25. Hub Modules
**Derived from:** modules.json (high in-degree and out-degree)
**Query:** "What modules are dependency hubs?"
**Insight:** Candidates for refactoring/splitting

### 26. Leaf Modules
**Derived from:** modules.json (zero out-degree, non-zero in-degree)
**Query:** "What modules have no dependencies?"
**Insight:** Self-contained utilities, good candidates for extraction

### 27. Root Modules (Entry Points)
**Derived from:** modules.json (zero in-degree, non-zero out-degree)
**Query:** "What modules are never imported?"
**Insight:** Main modules, test files, or dead code

### 28. Type Complexity Score
**Derived from:** Type signature depth/nesting in declarations.json
**Algorithm:** Recursively walk type AST, measure max depth
**Query:** "What are the most complex type signatures?"

### 29. Function Purity Analysis
**Derived from:** Type signature analysis (presence of Effect/Aff)
**Query:** "What functions perform effects?"
**Algorithm:** Check if typeSignature.referencedTypes includes Effect/Aff

### 30. Module Documentation Coverage
**Derived from:** declarations.json (ratio of declarations with non-empty comments)
**Query:** "What's the documentation coverage of module X?"

---

## Cross-Cutting Queries (Multi-Source)

### 31. Module Complexity Score
**Sources:** LOC.json + declarations.json + function-calls.json
**Metrics:**
  - Lines of code
  - Number of declarations
  - Number of dependencies
  - Average function call complexity
**Query:** "What are the most complex modules?"

### 32. Package Health Score
**Sources:** packages.json + modules.json + declarations.json + LOC.json
**Metrics:**
  - Dependency count
  - Number of modules
  - Total LOC
  - Documentation coverage
  - Coupling metrics
**Query:** "What packages need attention?"

### 33. Code Ownership by Package
**Sources:** modules.json + LOC.json
**Query:** "What percentage of the codebase is in package X?"
**Algorithm:** Sum LOC for all modules in package / total LOC

### 34. Dependency Impact Analysis
**Sources:** modules.json + function-calls.json
**Query:** "If I change module X, what's the blast radius?"
**Algorithm:**
  1. Find all modules that depend on X (direct from modules.json)
  2. Find all functions that call X's functions (from function-calls.json)
  3. Union of both sets

### 35. Dead Code Detection
**Sources:** declarations.json + function-calls.json + modules.json
**Query:** "What code is likely unused?"
**Algorithm:**
  1. Find exported declarations (from modules.json exports)
  2. Find functions with empty calledBy
  3. Intersect to find exported-but-unused declarations

### 36. API Surface Area
**Sources:** modules.json + declarations.json
**Query:** "What's the public API surface of package X?"
**Algorithm:**
  1. Get modules in package
  2. Get exported declarations per module
  3. Count total public declarations

### 37. Cross-Package Coupling
**Sources:** modules.json + packages.json + function-calls.json
**Query:** "How coupled are packages X and Y?"
**Algorithm:**
  1. Get modules in each package
  2. Count cross-package function calls
  3. Normalize by package sizes

### 38. Refactoring Candidates
**Sources:** function-calls.json + declarations.json + LOC.json
**Query:** "What functions should be extracted to separate modules?"
**Heuristics:**
  - High call count from multiple modules
  - Low coupling to current module
  - Self-contained functionality (no calls to module-mates)

### 39. Import Optimization Suggestions
**Sources:** modules.json + function-calls.json + declarations.json
**Query:** "What imports in module X are unused?"
**Algorithm:**
  1. Get declared imports (from modules.json)
  2. Get actually used functions/types (from function-calls + type refs)
  3. Find set difference

### 40. Type Usage Patterns
**Sources:** declarations.json + type-dependencies.json
**Query:** "What are the most commonly used types across the codebase?"
**Algorithm:** Aggregate all referencedTypes, rank by frequency

### 41. Typeclass Ecosystem
**Sources:** declarations.json (filter kind=typeClass)
**Query:** "What typeclasses exist and what are their hierarchies?"
**Note:** Would require Phase 3 (instances) for complete picture

### 42. Module Similarity
**Sources:** function-calls.json + declarations.json
**Query:** "What modules are structurally similar?"
**Algorithm:**
  - Compare exported declaration signatures
  - Compare dependency patterns
  - Compute similarity score

### 43. Codebase Evolution Over Time
**Sources:** All data + git history (future Phase)
**Query:** "How has module X changed over time?"
**Note:** Requires running extraction on historical commits

### 44. Function Call Chains
**Sources:** function-calls.json
**Query:** "What's the call path from function X to function Y?"
**Algorithm:** Graph traversal (BFS/DFS) to find path

### 45. Critical Path Analysis
**Sources:** function-calls.json + modules.json
**Query:** "What's the longest dependency chain in the codebase?"
**Algorithm:** Find longest path in combined module + function call graph

---

## Data Quality Insights

### Missing Data Points (To Consider for Future Phases)

1. **Intra-module function calls** - Currently filtered out, but useful for module cohesion analysis
2. **Typeclass instances** - Phase 3 target, enables instance resolution analysis
3. **Data constructor usage** - Which constructors are actually used vs defined
4. **Record field access patterns** - What fields are commonly accessed together
5. **Pattern matching exhaustiveness** - Coverage of data constructor patterns
6. **Foreign function bindings** - Which JS functions are called from PureScript
7. **Test coverage mapping** - Which declarations are tested
8. **Performance metrics** - Function call costs, allocation patterns
9. **Source code locations** - Precise line/column positions (partially available)
10. **Git blame/history** - Authors, commit messages, change frequency

### Data Consistency Checks

1. **Orphaned references** - Functions referenced in calls but not in declarations
2. **Module consistency** - Modules in modules.json vs declarations.json should match
3. **Function consistency** - Functions in function-calls.json should exist in declarations.json
4. **Type reference validity** - All referencedTypes should resolve to actual types

---

## Visualization Opportunities (Phase 4)

### Graph Visualizations
1. **Module Dependency Graph** - Force-directed with package clustering
2. **Function Call Flow** - Sankey or chord diagram for cross-module calls
3. **Type Dependency Network** - Hierarchical or circular layout
4. **Package Dependency Tree** - Tree or treemap layout

### Hierarchical Visualizations
5. **Package/Module/Declaration Treemap** - Size by LOC, color by metrics
6. **Call Hierarchy Tree** - Expandable tree from entry points
7. **Type Hierarchy** - Inheritance/usage trees

### Matrix Visualizations
8. **Module Dependency Matrix** - Adjacency matrix with reordering
9. **Function Call Matrix** - Who calls whom
10. **Type Usage Matrix** - Types × Declarations

### List/Table Visualizations
11. **Declaration Browser** - Searchable table with filters
12. **Function Catalog** - Sortable by popularity, complexity, etc.
13. **Module Health Dashboard** - Table with composite metrics

### Specialized Visualizations
14. **Impact Radius** - Given a declaration, show blast radius
15. **Dependency Path Explorer** - Find and visualize paths between entities
16. **Dead Code Report** - List of unused declarations
17. **API Surface Visualizer** - Public vs private declarations
18. **Complexity Heatmap** - Modules colored by complexity score

---

## Query Language Possibilities

Based on these relationships, we could build a query interface supporting:

```
// Find functions
FIND functions WHERE popularity > 50
FIND functions WHERE module = "PSD3.Capabilities.Selection"
FIND functions WHERE calls.includes("Data.Array.map")

// Find modules
FIND modules WHERE coupling > 0.8
FIND modules WHERE LOC > 500 AND documentation_coverage < 0.5
FIND modules WHERE dependencies.length = 0

// Find types
FIND types WHERE usedBy.length > 100
FIND types WHERE module.startsWith("PSD3.Internal")

// Relationships
FIND path FROM "Main.main" TO "PSD3.Interpreter.D3.eval_D3M"
FIND blast_radius OF "PSD3.Capabilities.Selection.appendTo"
FIND unused IN package "my-project"

// Aggregations
COUNT declarations BY kind
AVG LOC BY package
MAX coupling_score BY module
```

---

## Next Steps for Phase 4

1. **Choose initial visualizations** based on highest-value queries
2. **Design data loading strategy** - Lazy load vs preload all data
3. **Implement search/filter UI** - Autocomplete, faceted search
4. **Create interactive graph components** - Click to expand, hover for details
5. **Build metric dashboards** - Summary views of codebase health
6. **Add export capabilities** - CSV, JSON, SVG for external analysis

