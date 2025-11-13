# Branch Structure

## Current Branch Hierarchy

```
main
├── purescript-sankey-hierarchy
│   └── Pure PureScript hierarchy layouts (tree, pack, treemap, cluster, partition, sunburst)
│   └── Extracted exact D3 formulas through instrumentation
│   └── Status: Complete, all layouts match D3 within ±1px
│
└── feature/integrate-phantom-types-and-hierarchy (CURRENT)
    └── Rebased purescript-sankey-hierarchy onto main
    └── Integrating phantom types with hierarchy layouts
    └── Status: In progress - fixing compilation errors
```

## Branch History

### purescript-sankey-hierarchy
- **Created:** From main
- **Purpose:** Implement D3 hierarchy layouts in pure PureScript
- **Key commits:**
  - Sankey layout port
  - Tree layout (Reingold-Tilford algorithm)
  - Pack layout (circle packing)
  - Treemap layout
  - Cluster layout (dendrogram)
  - Partition layout (icicle/sunburst)
- **Status:** Complete, validated against D3

### feature/integrate-phantom-types-and-hierarchy
- **Created:** Rebased purescript-sankey-hierarchy onto main
- **Purpose:** Make hierarchy layouts work with phantom-typed D3Selection_
- **Key changes:**
  - Added DatumFn/DatumFnI wrappers for Datum_ accessor compatibility
  - Fixed type annotations throughout (D3Selection_ → D3Selection_ Unit, etc.)
  - Archived CodeAtlas for future reconstruction
  - Fixed MermaidAST to use concrete NodeID
- **Current state:** 2 errors remaining (excluding AnimatedRadialTree)
  - LesMisGUP: SimulationM2 example (archiving for now)
  - Spago Render: Datum_ vs d0 type mismatch

## Archived Code (on current branch)

### Already Archived
- `archived/CodeAtlas/` - Force simulation code atlas visualizer
  - **Reason:** Complex force simulation code, needs reconstruction with phantom types
  - **Future:** Rebuild with simplified row types

### To Archive
- `archived/LesMisGUP/` - Les Misérables General Update Pattern example
  - **Reason:** Uses SimulationM2 with complex row types, multiple type errors
  - **Future:** Return after hierarchy layouts verified working in browser
  - **Goal:** Simplify to only use Simulation row types (not Hierarchy row types)

## Next Steps

1. Archive LesMisGUP (SimulationM2 example)
2. Fix final Spago Render error
3. Build and test hierarchy layouts in browser
4. Return to SimulationM2 with lessons learned
5. Potential simplification: Remove Hierarchy row types, keep only Simulation row types

## Future: Indirect Layout Strategy

Consider implementing D3 hierarchy layouts as **indirect layouts**:
- Layout on JS copy of hierarchy structure
- Copy positions back into PureScript data
- Avoids phantom type complexity at FFI boundary
- Could also work for Graphviz or other external layout engines
