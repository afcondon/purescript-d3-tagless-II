# Hierarchy Layouts Port Plan

## Goal
Port the pure PureScript hierarchy layout implementations from `purescript-sankey-hierarchy` branch to `main` branch without merging (since this branch has deleted code that should remain on main).

## Strategy
Create a new feature branch from `main` and selectively copy files from `purescript-sankey-hierarchy`.

## Files to Port

### Core Library Files (src/lib/PSD3/Layout/Hierarchy/)
These are the pure PureScript layout algorithms - completely new, no conflicts:

1. **Core.purs** - Base hierarchy data structure and utilities
   - `HierarchyNode` type
   - `hierarchy` function (creates hierarchy from data + accessor)
   - `sum` function (aggregates values bottom-up)
   - `sort` function (sorts children)
   - `eachBefore`, `eachAfter` traversals

2. **Types.purs** - Shared types for hierarchy layouts
   - `ValuedNode` (node with aggregated value)
   - `getValue` accessor

3. **Tree.purs** - Tree layout (Reingold-Tilford algorithm)
   - `TreeNode` data type
   - `tree` function
   - `TreeConfig` with size and separation
   - Pure PureScript implementation matching D3 exactly

4. **Treemap.purs** - Treemap layout (squarified tiling)
   - `TreemapNode` data type
   - `treemap` function
   - `TreemapConfig` with size, padding, tile strategy
   - Squarify algorithm for aspect ratio optimization

5. **Pack.purs** - Pack layout (circle packing)
   - `PackNode` data type
   - `pack` function
   - `PackConfig` with size and padding
   - Iterative relaxation for optimal circle placement

6. **Cluster.purs** - Cluster layout (dendrogram)
   - `ClusterNode` data type
   - `cluster` function
   - `ClusterConfig` with size and separation
   - Post-order traversal, parent centering

7. **Partition.purs** - Partition layout (icicle/sunburst base)
   - `PartitionNode` data type
   - `partition` function
   - `PartitionConfig` with size, padding, round
   - Horizontal dice partitioning by value

### Visualization Files (src/website/Viz/Hierarchy/)
These render the layouts using SelectionM - may need adaptation for main's structure:

1. **FlareData.purs** - Shared Flare dataset
   - `HierData` type for hierarchical JSON
   - `sampleData` constant
   - `getChildren` accessor
   - `DecodeJson` instance for loading from JSON

2. **TreeViz.purs** - Tree visualization
   - Renders node-link diagram
   - Curved link paths (Bezier curves)
   - Loads JSON, applies layout, renders SVG

3. **TreemapViz.purs** - Treemap visualization
   - Renders rectangles with borders
   - Depth-based colors
   - Labels for large rectangles

4. **PackViz.purs** - Pack visualization
   - Renders nested circles
   - Depth-based colors
   - Labels for large circles

5. **ClusterViz.purs** - Cluster visualization
   - Renders dendrogram with links
   - CRITICAL: Uses corrected link generator (child→parent with levelSpacing)
   - Sorting by height+value to minimize crossings

6. **PartitionViz.purs** - Partition/Icicle visualization
   - Renders rectangular layers
   - Equal height per layer
   - Depth-based colors

7. **SunburstViz.purs** - Sunburst visualization
   - Converts rectangular to polar coordinates
   - SVG arc paths with trig functions
   - Smart text rotation for readability

### Test Files (test/PSD3/)
Simple validation tests:

- **TreeTest.purs** - Validates tree layout positions
- **TreemapTest.purs** - Validates treemap rectangles
- **PackTest.purs** - Validates pack circles
- **ClusterTest.purs** - Validates cluster positions and crossings
- **PartitionSimple.purs** - Validates partition coordinates

### Reference D3 Test Files (root/)
JavaScript reference implementations for debugging:

- test-d3-tree.js
- test-d3-treemap.js
- test-d3-pack.js
- test-d3-cluster-*.js
- test-d3-partition-simple.js

### Documentation Files
Important notes from the implementation process:

- **CLUSTER_FIX_NOTE.md** - Documents the critical link generator fix
- **HIERARCHY_PLAN.md** - Original implementation plan
- **notes/** directory - Contains detailed analysis documents:
  - D3_SANKEY_EXACT_FORMULAS.md
  - PURESCRIPT_VS_D3_COMPARISON.md
  - SESSION_SUMMARY.md

## Integration with Main

### Main Branch Structure
Main has a different structure with:
- Routes defined in `src/website/Types.purs` (ExampleMetadata, Route with Example String)
- Component-based architecture via `src/website/Component/Example.purs`
- ExamplesGallery showing all visualizations
- More sophisticated routing system

### Integration Approach

**Option 1: Add to existing Hierarchies page**
- Main already has a `/hierarchies` route and Hierarchies component
- Could add the pure PureScript examples to that page
- Show comparison between D3 FFI and pure PureScript versions

**Option 2: Create individual example pages**
- Add each layout as a separate example in ExamplesGallery
- Examples: "tree-pure", "treemap-pure", "pack-pure", etc.
- Use Example component system

**Option 3: Hybrid approach (RECOMMENDED)**
- Keep existing D3 FFI hierarchy examples on `/hierarchies`
- Add new pure PureScript examples to ExamplesGallery
- Create metadata for each:
  - tree-purescript
  - treemap-purescript
  - pack-purescript
  - cluster-purescript
  - partition-purescript
  - sunburst-purescript
- Link between the two approaches in documentation

### Steps to Port

1. **Create feature branch from main:**
   ```bash
   git switch main
   git checkout -b feature/pure-hierarchy-layouts
   ```

2. **Copy library files:**
   ```bash
   mkdir -p src/lib/PSD3/Layout/Hierarchy
   git checkout purescript-sankey-hierarchy -- src/lib/PSD3/Layout/Hierarchy/
   ```

3. **Copy visualization files:**
   ```bash
   git checkout purescript-sankey-hierarchy -- src/website/Viz/Hierarchy/
   ```

4. **Create example metadata in src/website/ExamplesData.purs:**
   - Add entries for each hierarchy layout
   - Category: AdvancedLayout
   - Difficulty: Intermediate/Advanced
   - Tags: ["hierarchy", "tree", "pure-purescript", "algorithm"]

5. **Update Example.purs component:**
   - Add cases for rendering hierarchy examples
   - Load Flare data from JSON
   - Call appropriate Viz module

6. **Test and verify:**
   - Build and check for errors
   - Verify all examples render correctly
   - Check that routing works

7. **Update documentation:**
   - Update Hierarchies.purs to mention pure PureScript implementations
   - Add links to individual examples
   - Document the link generator fix for cluster

## Key Technical Details

### Critical Fix: Cluster Link Generator
The cluster layout required a specific link path generator to avoid crossings:
- Links draw from **child to parent** (not parent to child)
- Control points use `parentY + levelSpacing/2` (not simple midpoint)
- levelSpacing = chartHeight / maxDepth

See CLUSTER_FIX_NOTE.md for full details.

### Dependencies
All hierarchy layouts depend on:
- PSD3.Internal.Attributes.Sugar (for SVG attributes)
- PSD3.Capabilities.Selection (for SelectionM)
- Data.Argonaut (for JSON loading)

No new external dependencies required.

### Data Format
All examples use the Flare dataset (docs/data/flare.json):
- Hierarchical JSON with name, value, children
- 252 nodes, 4 levels deep
- Total value: 956,129

## Timeline Estimate

1. Create feature branch and copy files: 5 minutes
2. Update example metadata and routing: 30 minutes
3. Test each visualization: 15 minutes
4. Fix any integration issues: 30 minutes
5. Update documentation: 20 minutes

**Total: ~1.5-2 hours**

## Success Criteria

- [ ] All 7 hierarchy library modules compile
- [ ] All 7 visualization modules render correctly
- [ ] Examples accessible via ExamplesGallery
- [ ] Flare data loads from JSON
- [ ] Layouts match D3 output exactly
- [ ] No regressions in existing main branch features
- [ ] Documentation updated
