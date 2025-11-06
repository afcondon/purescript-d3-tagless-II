# Data Loading Architecture Analysis

## Date: 2025-11-06

## Goal

Make it easy for developers with graph or hierarchical data to load and visualize it using the library's hierarchy layouts (trees, bubble pack, treemap, icicle, etc.) or force-directed graph layouts.

## Current State Analysis

### 1. Library Layer (`src/lib/`)

#### PSD3.Data.Tree (58 lines)
**Purpose**: Bridge between PureScript Tree and D3 hierarchy layouts

**What's generic:**
- `TreeType` ADT: `TidyTree | Dendrogram`
- `TreeLayout` ADT: `Radial | Horizontal | Vertical`
- `TreeModel` type: Container for tree + layout configuration
- `makeD3TreeJSONFromTreeID`: Converts `Tree NodeID` → `TreeJson_` using a `Map NodeID d`

**What's Spago-specific:**
- **Nothing!** This module is already generic, just needs better documentation

**Key insight**: The library already has a generic tree conversion function, but it's hidden and undocumented.

#### PSD3.Internal.FFI (hierarchy functions)
**Exports for hierarchy layouts:**
- `hierarchyFromJSON_`: `TreeJson_ -> D3_TreeNode d` - Core conversion
- Tree layouts: `treeSortForTree_`, `treeSetSize_`, `treeSetNodeSize_`, `treeSetSeparation_`
- Pack layout: `packLayout_`, `packSetSize_`, `packSetPadding_`, `runPackLayout_`
- Treemap layout: `treemapLayout_`, `treemapSetSize_`, `treemapSetPadding_`, `runTreemapLayout_`
- Partition layout: `treeSortForPartition_`, `partitionLayout_`, `runPartitionLayout_`

**What's missing:**
- High-level wrappers that package these together
- Clear entry points for "I have a Tree, give me a layout"
- Documentation of the workflow

### 2. CodeExplorer Example

#### Component/CodeExplorer/Data.purs (30 lines)
**Current workflow:**
1. Fetch 4 JSON files via Affjax (modules, packages, lsdeps, LOC)
2. Call `convertFilesToGraphModel` from Viz layer
3. Call `addTreeToModel` to compute tree reduction
4. Return `Maybe SpagoModel`

**Observations:**
- Very Spago-specific data fetching
- Model conversion tightly coupled to file format
- Tree computation embedded in Spago-specific logic

#### Viz/Spago/Files.purs (310 lines)
**Responsibilities (mixed concerns):**
1. **Type definitions** (lines 1-106):
   - JSON row types for files
   - Spago-specific node/link data structures
   - Graph model type

2. **Data synthesis** (lines 108-287):
   - `getGraphJSONData`: 180 lines of Spago-specific logic
   - Merges 4 files into unified graph model
   - Computes package containment, LOC rollup, etc.
   - Creates bidirectional maps (name↔ID, id→node, etc.)

**Problem**: This mixes generic graph concepts (nodes, links, maps) with Spago domain logic (packages contain modules, dependency resolution, etc.).

#### Viz/Spago/Model.purs (300+ lines)
**Responsibilities:**
1. **Type definitions**: SpagoSimNode, SpagoTreeNode, SpagoModel
2. **Accessors**: `datum_` (70 lines), `link_` (10 lines)
3. **Model conversion**: `convertFilesToGraphModel` - FFI call to merge files
4. **Initializers**: Position functions for nodes

**Problem**: Model conversion, domain logic, and visualization helpers all mixed together.

#### Viz/Spago/Tree.purs (150+ lines)
**Responsibilities:**
1. `treeReduction`: Compute tree structure from graph using dependency analysis
2. Tree layout application (radial/horizontal positioning)
3. Node positioning based on tree layout

**Spago-specific parts:**
- Uses `getReachableNodes` from dependency graph analysis
- Filters by package vs module link types
- Custom sorting function `treeSortForTree_Spago_`

**Generic parts:**
- Tree layout application (lines 50-60)
- Radial/horizontal translation functions (lines 78-110)
- Mapping tree positions onto nodes (lines 86-110)

## Problem Statement

### Current Pain Points:

1. **No clear entry point** for developers who want to:
   - Load a simple JSON file with nodes and links
   - Display it as a force-directed graph
   - OR display it as a tree/hierarchy if it has a tree structure

2. **Mixed concerns**: Files.purs mixes:
   - Raw file format parsing
   - Domain model construction
   - Graph analysis
   - Data transformations

3. **Hidden generic code**: `PSD3.Data.Tree.makeD3TreeJSONFromTreeID` is generic but:
   - Not exported from top-level module
   - No documentation
   - No examples outside Spago

4. **Duplicated patterns**: Each viz re-implements:
   - File loading boilerplate (Affjax + ResponseFormat)
   - Node/link type definitions
   - Accessor pattern (`datum_`, `link_`)
   - Map construction (name→ID, id→node)

## What Users Need

### Use Case 1: Simple Graph from JSON

"I have a JSON file with nodes and links. I want to see it as a force-directed graph."

**Expected workflow:**
```purescript
-- Load data
model <- loadGraphFromJSON "data.json"

-- Define node type
type MyNode = { id :: Int, name :: String, value :: Number }

-- Visualize
renderForceGraph model myForces myRenderCallbacks
```

### Use Case 2: Hierarchical Data from JSON

"I have hierarchical JSON (tree structure). I want to see it as a tree/pack/treemap."

**Expected workflow:**
```purescript
-- Load data
tree <- loadTreeFromJSON "tree.json"

-- Choose layout
layout <- configureTreeLayout TidyTree Radial

-- Visualize
renderTreeVisualization tree layout myRenderCallbacks
```

### Use Case 3: Convert Graph to Tree

"I have a graph, but I want to extract a tree (like CodeExplorer does with dependencies)."

**Expected workflow:**
```purescript
-- Load graph
graph <- loadGraphFromJSON "graph.json"

-- Extract tree from root
tree <- extractTreeFromGraph rootID graph

-- Apply layout
positioned <- applyTreeLayout tree (tidyTreeLayout radial)

-- Visualize
renderTreeVisualization positioned myRenderCallbacks
```

## Proposed Refactoring

### Phase 1: Extract Generic Data Loading

#### New Module: `PSD3.Data.Loaders`

**Purpose**: High-level functions for common data loading patterns

```purescript
module PSD3.Data.Loaders where

-- Simple graph loading
loadGraphJSON :: forall n l.
  FilePath ->
  Aff (Either Error (GraphData n l))

-- Tree loading
loadTreeJSON :: forall d.
  FilePath ->
  Aff (Either Error (Tree d))

-- Custom loader with transform
loadAndTransform :: forall raw cooked.
  FilePath ->
  (raw -> Either Error cooked) ->
  Aff (Either Error cooked)
```

**Benefits:**
- Single import for data loading
- Consistent error handling
- Handles Affjax boilerplate

#### New Module: `PSD3.Data.Graph`

**Purpose**: Generic graph data structures and utilities

```purescript
module PSD3.Data.Graph where

-- Generic graph model (not Spago-specific)
type GraphModel node link =
  { nodes :: Array node
  , links :: Array link
  , maps ::
      { nodeById :: Map NodeID node
      , linksBySource :: Map NodeID (Array link)
      , linksByTarget :: Map NodeID (Array link)
      }
  }

-- Build model from arrays
buildGraphModel :: forall node link.
  (node -> NodeID) ->          -- Extract ID from node
  (link -> NodeID) ->          -- Extract source from link
  (link -> NodeID) ->          -- Extract target from link
  Array node ->
  Array link ->
  GraphModel node link

-- Convert to D3 simulation nodes
prepareForSimulation :: forall node link.
  GraphModel node link ->
  (node -> SimulationNode) ->  -- Conversion function
  Array D3_SimulationNode
```

#### New Module: `PSD3.Data.TreeBuilder`

**Purpose**: Convert various sources into D3 tree structures

```purescript
module PSD3.Data.TreeBuilder where

-- From PureScript Tree
fromPureScriptTree :: forall d.
  Tree d ->
  TreeJson_

-- From graph (generic tree extraction)
fromGraph :: forall node.
  NodeID ->                    -- Root
  GraphModel node link ->
  (link -> NodeID) ->          -- Extract target
  Tree node

-- From hierarchical JSON (automatic)
fromHierarchicalJSON :: forall d.
  String ->                    -- JSON string
  Either Error (Tree d)
```

### Phase 2: Extract Generic Layout Application

#### New Module: `PSD3.Layouts.Tree`

**Purpose**: High-level tree layout application

```purescript
module PSD3.Layouts.Tree where

-- Configuration ADT
data TreeLayoutConfig
  = TidyTree { layout :: TreeLayout, nodeSize :: Maybe (Array Number) }
  | Dendrogram { layout :: TreeLayout, nodeSize :: Maybe (Array Number) }
  | Pack { size :: { width :: Number, height :: Number }, padding :: Number }
  | Treemap { size :: { width :: Number, height :: Number }, padding :: Number }
  | Partition { size :: { width :: Number, height :: Number } }

-- One function to rule them all
applyTreeLayout :: forall d.
  Tree d ->
  TreeLayoutConfig ->
  D3_TreeNode (EmbeddedData d + D3_XY + ...)

-- Extract positions for simulation nodes
treePositionsToSimNodes :: forall d.
  D3_TreeNode d ->
  Array D3_SimulationNode ->
  Array D3_SimulationNode  -- With tree positions applied
```

### Phase 3: Update CodeExplorer to Use Generic Infrastructure

#### Refactored Component/CodeExplorer/Data.purs

```purescript
module PSD3.CodeExplorer.Data where

import PSD3.Data.Loaders (loadAndTransform)
import D3.Viz.Spago.Files (Spago_Raw_JSON_, buildSpagoModel)

-- Use generic loader with Spago-specific transform
readModelData :: Aff (Maybe SpagoModel)
readModelData = do
  result <- loadAndTransform "data/spago-data/"
    { files: ["modules.json", "packages.json", "lsdeps.jsonlines", "LOC.json"]
    , transform: buildSpagoModel
    , addTree: Just "PSD3.Main"
    }
  pure $ hush result
```

#### Keep Spago-Specific: Viz/Spago/Files.purs

**Simplified to only domain logic:**
- Spago-specific type definitions (NodeType, PackageInfo, etc.)
- `buildSpagoModel`: Transform raw JSON → SpagoModel
- Domain-specific calculations (package containment, LOC rollup)

**Remove generic parts:**
- Move graph building to `PSD3.Data.Graph`
- Move file loading to `PSD3.Data.Loaders`

#### Keep Spago-Specific: Viz/Spago/Tree.purs

**Simplified to only domain logic:**
- `computeDependencyTree`: Spago-specific tree extraction using `getReachableNodes`
- Custom sorting `treeSortForTree_Spago_`
- Link type partitioning (M2M_Tree vs M2M_Graph)

**Remove generic parts:**
- Move tree layout application to `PSD3.Layouts.Tree`
- Move position transformation (radial, horizontal) to library

### Phase 4: Documentation

#### New Guide: `docs/guides/DATA_LOADING_GUIDE.md`

**Sections:**
1. **Quick Start**: Load and visualize in 10 lines
2. **Graph Data**: JSON schema, loading, transforming
3. **Hierarchical Data**: JSON schema, Tree structure, layouts
4. **Custom Loaders**: Roll your own for exotic formats
5. **Advanced**: Graph → Tree extraction, custom layouts

#### Update: `VISUALIZATION_GUIDE.md`

Add new early section:
- **Step 0: Data Loading** (before "Step 1: Define Your Data Types")
- Show generic loading patterns
- Link to DATA_LOADING_GUIDE.md for details

## Benefits

### For Library Users:

1. **Clear entry points**:
   - `loadGraphJSON` for graphs
   - `loadTreeJSON` for trees
   - `applyTreeLayout` for hierarchy layouts

2. **Less boilerplate**:
   - No need to write Affjax code
   - Automatic map construction
   - Standard error handling

3. **Reusable patterns**:
   - Copy the accessor pattern from examples
   - Standard Model structure
   - Common transformations available

### For Library Maintainers:

1. **Clear separation**:
   - Generic infrastructure in `PSD3.Data.*`
   - Spago domain logic in `Viz.Spago.*`
   - Examples show how to extend

2. **Less duplication**:
   - One place for file loading
   - One place for graph building
   - One place for tree layouts

3. **Easier testing**:
   - Generic functions testable in isolation
   - Clear interfaces between layers

## Implementation Plan

### Step 1: Analysis & Design (Current)
- ✅ Analyze current architecture
- ✅ Identify generic vs specific code
- ✅ Propose module structure
- ⏳ Get feedback on design

### Step 2: Extract Generic Loaders (New Feature Branch)
1. Create `src/lib/PSD3/Data/Loaders.purs`
2. Create `src/lib/PSD3/Data/Graph.purs`
3. Create `src/lib/PSD3/Data/TreeBuilder.purs`
4. Add tests with simple fixtures

### Step 3: Extract Generic Layouts (Same Branch)
1. Create `src/lib/PSD3/Layouts/Tree.purs`
2. Move tree positioning logic from Spago
3. Add configuration types
4. Add tests

### Step 4: Refactor CodeExplorer (Same Branch)
1. Update `Component/CodeExplorer/Data.purs` to use generic loaders
2. Simplify `Viz/Spago/Files.purs` to domain logic only
3. Simplify `Viz/Spago/Tree.purs` to domain logic only
4. Verify build succeeds

### Step 5: Documentation (Same Branch)
1. Create `docs/guides/DATA_LOADING_GUIDE.md`
2. Update `VISUALIZATION_GUIDE.md` with Step 0
3. Add examples to library docs

### Step 6: Create Example (Optional, Separate Branch)
1. Build simple tree visualization using generic infrastructure
2. Demonstrate: Load JSON → Apply layout → Render
3. Show that it's ~50 lines total vs ~500 for Spago

## Design Decisions (2025-11-06)

1. **Error handling**: ✅ Use `Either Error` for explicit error propagation

2. **File formats**: ✅ Support generic JSON/CSV for graph data
   - Initial: CodeExplorer formats (ensure backward compatibility)
   - Extend later as needed
   - Goal: Be flexible enough for common graph/tree data

3. **Naming**: ✅ Use `PSD3.Data.Loaders`
   - Rationale: Future extensibility (network, SQL, etc.)
   - Related modules: `PSD3.Data.Graph`, `PSD3.Layouts.Tree`

4. **Graph algorithms**: ✅ Build generic library of graph algorithms
   - Move to library where reasonable
   - Goal: Develop useful graph algorithm library as side effect
   - `getReachableNodes` → generic traversal in `PSD3.Data.Graph.Algorithms`

5. **Accessor generation**: ✅ Generate `datum_` accessors
   - Consistency in naming across visualizations
   - Consider separate utility object for attribute lambdas
   - Rethink tidying functions - may separate concerns:
     - Raw accessors (generated)
     - Computed attributes (separate utilities)

## Success Metrics

### After refactoring:

1. **New viz with graph data**:
   - Before: ~500 lines (copy Spago Files + Model)
   - After: ~100 lines (load + accessors + render)

2. **New viz with tree data**:
   - Before: ~300 lines + studying CodeExplorer
   - After: ~50 lines (load + layout + render)

3. **Library comprehensibility**:
   - Before: "Study CodeExplorer for 2 hours to understand data flow"
   - After: "Read DATA_LOADING_GUIDE.md, copy example, customize"

4. **Code reuse**:
   - Generic loading: Used by all visualizations
   - Generic layouts: Used by all hierarchies
   - Spago-specific: Only in Viz/Spago
