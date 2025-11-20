# PSD3 Library Cleanup & Code Explorer Plan

**Date**: 2025-11-15
**Goal**: Clean house, remove old code, and build Code Explorer
**Target**: Push overhauled library to GitHub by end of day

---

## Phase 1: Morning - Clean House (Target: Mid-morning completion)

### 1.1 Website Pages Inventory

#### ✅ KEEP - New PSD3v2 Examples (Already Converted)
- **LesMisGUPTree** (`/lesmis-gup-tree`) - Les Mis with Tree API + GUP + Dynamic Layouts ✨ **WORKING**
- **TreeAPI** (`/tree-api`) - Tree API examples (declarative tree API)
- **PSD3v2Examples** (`/psd3v2-examples`) - PSD3v2 showcase page with:
  - Three Little Circles V2
  - Three Little Circles Transition V2
  - GUP V2
  - Tree Viz V2
  - Animated Tree V2
  - Les Mis V2
  - Les Mis GUP V2

#### 🗑️ ARCHIVE - "Understanding" Pages (Old Tour Content)
These pages document the OLD library architecture (PSD3 v1):

**Core Understanding Pages**:
- `UnderstandingIndex` - Index page with card grid
- `UnderstandingFinallyTagless` - Finally Tagless pattern
- `UnderstandingSelectionM` - SelectionM Monad (OLD approach)
- `UnderstandingCapabilities` - OLD Capabilities & Interpreters
- `UnderstandingTypeSystem` - Type-Safe Attribute System (still relevant?)
- `UnderstandingDatumPattern` - datum_/Datum_ Pattern (OLD approach)
- `UnderstandingGrammar` - Grammar of D3 in SelectionM (OLD)
- `UnderstandingConcepts` - Redirects to UnderstandingIndex
- `UnderstandingPatterns` - Redirects to UnderstandingIndex
- `UnderstandingPhilosophy` / `About` - Design philosophy

**Demo/Experiment Pages**:
- `SimpleCharts1` - Simplest Charts
- `SimpleCharts2` - Lines and Bars
- `DataFlowViz` - Data Flow Visualizations
- `Movement` - Movement & Transition
- `Hierarchies` - Hierarchies demo page
- `IsometricCurveExperiment` - Isometric curve experimentation
- `Interpreters` - Interpreters demo
- `MermaidDiagrams` - Mermaid AST visualization

**Actions for Understanding Pages**:
1. Move all Understanding component files to `src/website/Component/Understanding/Archive/`
2. Remove routes from `Types.purs`
3. Remove imports and slots from `Main.purs`
4. Keep a README in the archive explaining what these were

#### 🗑️ ARCHIVE - Old "Examples Gallery" & Individual Examples
The `ExamplesGallery` page lists many examples using the OLD library. Most need conversion.

**Examples to Archive** (use OLD PSD3 library):
- All Simple Charts examples (bar-chart, line-chart, scatter-plot, etc.)
- Old hierarchy examples (if they use D3 FFI instead of pure PureScript layouts)
- Chord diagram (needs conversion)
- Old force examples (lesmis-force - superseded by LesMisGUPTree)
- lesmisgup (old GUP - superseded by LesMisGUPTree)
- Map quartet, nested data, working with sets
- Transitions examples (three-circles-transition, general-update-pattern, animated-tree-cluster, animated-radial-tree)

**Examples Status Check**:
- Check which hierarchy examples use pure PureScript layouts vs D3 FFI
- Pure PureScript layouts are in `PSD3/Layout/Hierarchy/` - these might be salvageable
- D3 FFI-based examples need full rewrite

**Actions for Examples**:
1. Archive `ExamplesGallery.purs` to `src/website/Component/ExamplesGallery/Archive/`
2. Archive `Example.purs` component to `src/website/Component/Example/Archive/`
3. Remove `ExamplesGallery` and `Example` routes
4. Keep visualization files temporarily in case we want to convert them later

#### ✅ KEEP - Other Pages
- `Home` - Landing page
- `GettingStarted` - Tutorial (UPDATE to reflect new library)
- `Wizard` - Interactive wizard (CHECK if it works with v2)
- `HowtoIndex` - How-to guides (UPDATE or ARCHIVE)
- `Reference` - API documentation (UPDATE to PSD3v2 modules)
- `ForceNavigator` - Force-directed navigation (CHECK status)
- `CodeExplorer` - **BUILD TODAY**
- `CodeExploration` - Individual snippet exploration pages
- `WealthHealth` - Wealth & Health visualization (CHECK status)
- `FpFtw` - FP examples (CHECK status)
- `Acknowledgements` - Credits

**Status Check Needed**:
- ForceNavigator - does it use old or new library?
- WealthHealth - does it use old or new library?
- FpFtw - does it use old or new library?

---

### 1.2 Library Modules Inventory

#### ✅ KEEP - PSD3v2 (New Architecture)
**Core**:
- `PSD3v2/Interpreter/D3v2.purs` - Main interpreter with SimulationM2
- `PSD3v2/Capabilities/Selection.purs` - Selection capabilities
- `PSD3v2/Capabilities/Simulation.purs` - Simulation capabilities (SimulationM2)
- `PSD3v2/Capabilities/Transition.purs` - Transition capabilities

**Selection System**:
- `PSD3v2/Selection/Types.purs` - Phantom types for selection states
- `PSD3v2/Selection/Operations.purs` - Core operations (Tree API, GUP)
- `PSD3v2/Selection/Join.purs` - Data join implementation
- `PSD3v2/Selection/Indexed.purs` - Indexed selections

**Tree API**:
- `PSD3v2/VizTree/Types.purs` - Tree types
- `PSD3v2/VizTree/Tree.purs` - Tree API (joinData, elem, withChild)
- `PSD3v2/VizTree/Core.purs` - Tree rendering core

**Attributes & Behaviors**:
- `PSD3v2/Attribute/Types.purs` - Attribute types
- `PSD3v2/Behavior/Types.purs` - Behavior types (drag, zoom)
- `PSD3v2/Behavior/FFI.purs` - Behavior FFI

**Transitions**:
- `PSD3v2/Transition/Types.purs` - Transition types
- `PSD3v2/Transition/FFI.purs` - Transition FFI

**Axes**:
- `PSD3v2/Axis/Axis.purs` - Axis rendering

#### ✅ KEEP - Shared Core Modules (Used by Both)
These are used by both PSD3 and PSD3v2:

**Data Structures**:
- `PSD3/Data/Node.purs` - SimulationNode, D3Link types
- `PSD3/Data/Tree.purs` - Tree data structure
- `PSD3/Data/Graph.purs` - Graph utilities
- `PSD3/Data/Graph/Algorithms.purs` - Graph algorithms
- `PSD3/Data/Loaders.purs` - Data loading utilities
- `PSD3/Data/Utility.purs` - Utility functions

**Layouts** (Pure PureScript - can be used with either):
- `PSD3/Layout/Hierarchy/Core.purs` - Hierarchy core
- `PSD3/Layout/Hierarchy/Tree.purs` - Tree layout
- `PSD3/Layout/Hierarchy/Tree4.purs` - Tree4 layout (Reingold-Tilford)
- `PSD3/Layout/Hierarchy/Cluster.purs` - Cluster layout
- `PSD3/Layout/Hierarchy/Cluster4.purs` - Cluster4 layout
- `PSD3/Layout/Hierarchy/Pack.purs` - Circle packing
- `PSD3/Layout/Hierarchy/Partition.purs` - Partition layout
- `PSD3/Layout/Hierarchy/Treemap.purs` - Treemap layout
- `PSD3/Layout/Hierarchy/Projection.purs` - Radial projection
- `PSD3/Layout/Hierarchy/Types.purs` - Hierarchy types
- `PSD3/Layout/Sankey.purs` - Sankey layout (pure PureScript)
- `PSD3/Layout/Sankey/*.purs` - Sankey support modules

**Simulation** (Used by both):
- `PSD3/Internal/Simulation/Types.purs` - Simulation types
- `PSD3/Internal/Simulation/Config.purs` - Force configuration
- `PSD3/Internal/Simulation/Forces.purs` - Force creation
- `PSD3/Internal/Simulation/Functions.purs` - Simulation functions

**FFI & Utilities**:
- `PSD3/Internal/FFI.purs` - D3 FFI bindings
- `PSD3/Internal/Types.purs` - Internal types
- `PSD3/Internal/Utility.purs` - Utilities
- `PSD3/Internal/Zoom.purs` - Zoom functionality
- `PSD3/Internal/Axes.purs` - Axes (might be superseded by PSD3v2/Axis)
- `PSD3/Internal/Scales/*.purs` - Scale functions
- `PSD3/Internal/Generators/Line.purs` - Line generator

#### 🗑️ ARCHIVE - Old PSD3 (v1 Architecture)
These represent the OLD approach and should be archived:

**Old Capabilities & Interpreter**:
- `PSD3/Capabilities/Selection.purs` - OLD selection capability
- `PSD3/Capabilities/Simulation.purs` - OLD simulation capability
- `PSD3/Capabilities/Sankey.purs` - OLD sankey capability
- `PSD3/Interpreter/D3.purs` - OLD D3 interpreter
- `PSD3/Interpreter/MermaidAST.purs` - Mermaid interpreter
- `PSD3/Interpreter/MetaTree.purs` - MetaTree interpreter
- `PSD3/Interpreter/String.purs` - String interpreter

**Old Selection System**:
- `PSD3/Internal/Selection/Types.purs` - OLD selection types
- `PSD3/Internal/Selection/Functions.purs` - OLD selection functions

**Old Attributes**:
- `PSD3/Attributes.purs` - OLD attributes module
- `PSD3/Internal/Attributes/Instances.purs` - OLD attribute instances
- `PSD3/Internal/Attributes/Sugar.purs` - OLD attribute sugar

**Old Simulation Helpers**:
- `PSD3/Simulation/RunSimulation.purs` - OLD simulation runner
- `PSD3/Simulation/Scene.purs` - OLD scene management
- `PSD3/Simulation/SceneTransition.purs` - OLD scene transitions
- `PSD3/Simulation/Update.purs` - OLD simulation update

**Old Hierarchical**:
- `PSD3/Internal/Hierarchical.purs` - OLD hierarchical helpers (check if used)

**Old Sankey Internals** (if Sankey layout is pure PureScript, keep it):
- `PSD3/Internal/Sankey/Types.purs` - Check if used by Layout/Sankey
- `PSD3/Internal/Sankey/Functions.purs` - Check if used by Layout/Sankey

**Top-level Module**:
- `PSD3.purs` - Main module (UPDATE to export PSD3v2 instead)
- `PSD3/Types.purs` - OLD types (check what's still needed)

**Component Helpers**:
- `PSD3/Component/SimulationState.purs` - OLD component helper (archive?)

**Actions for Old Library**:
1. Create `src/lib/PSD3_v1_Archive/` directory
2. Move old modules there
3. Update `PSD3.purs` to be a thin wrapper that exports PSD3v2 modules
4. Ensure no examples import from archived modules

---

### 1.3 Visualization Files Status

Need to check which viz files use old vs new library:

**Check These Viz Files** (in `src/website/Viz/`):
- AnimatedTree4Cluster4.purs - Uses Tree4/Cluster4 layouts, check if it uses old interpreter
- AnimatedTreeCluster.purs - Check
- BarChart.purs - Likely OLD
- ChordDiagram.purs - Likely OLD, needs conversion
- ClusterViz*.purs - Check
- FpFtw/MapQuartet.purs - Check
- GroupedBarChart.purs - Likely OLD
- GUP.purs - OLD (superseded by GUPV2)
- Hierarchies.purs - Check
- HorizontalClusterViz.purs - Check
- HorizontalTreeViz.purs - Check
- LesMis/* - Some old, some new (LesMisGUPV2 is NEW)
- LineChart.purs - Likely OLD
- MultiLineChart.purs - Likely OLD
- PackViz.purs - Check (uses pure layout, but what interpreter?)
- Parabola.purs - Likely OLD
- PartitionViz.purs - Check
- RadialClusterViz.purs - Check
- RadialStackedBar.purs - Likely OLD
- RadialTreeViz.purs - Check
- SankeyDiagram.purs - Check (uses pure Sankey layout, but what interpreter?)
- ScatterPlot.purs - Likely OLD
- SunburstViz.purs - Check
- ThreeLittleCircles*.purs - Some old, some V2
- TreeAPI/* - NEW Tree API examples
- TreeViz*.purs - Some old, some V2
- TreemapViz.purs - Check
- WealthHealth/Draw.purs - Check

**Action**:
1. For each file, check imports:
   - If imports from `PSD3.Interpreter.D3`, it's OLD
   - If imports from `PSD3v2.Interpreter.D3v2`, it's NEW
   - If imports from `PSD3.Capabilities.Selection`, it's OLD
   - If imports from `PSD3v2.Capabilities.Selection`, it's NEW
2. Create list of OLD files to archive
3. Create list of files that need conversion

---

## Phase 2: Late Morning/Early Afternoon - Review & Document

### 2.1 Review New Library Architecture

**PSD3v2 Core Concepts**:
1. **Phantom Types**: Selection states tracked at type level (Unbound, Bound, Joined)
2. **SimulationM2**: New simulation capability with init/update/start/stop
3. **Tree API**: Declarative DOM structure with `joinData`, `elem`, `withChild`
4. **General Update Pattern**: Automatic enter/update/exit handling
5. **Type-safe Behaviors**: Drag, zoom with proper selection state requirements

**Key Working Examples**:
1. **LesMisGUPTree** - Full-featured force simulation with:
   - Tree API for structure
   - SimulationM2 for simulation
   - GUP for dynamic data
   - Layout transitions (grid, phylotaxis, force)
   - Smooth position caching (sx, sy)

2. **Tree4/Cluster4** - Hierarchy layouts with:
   - Pure PureScript layouts
   - Tree API rendering
   - Data joins with proper keys
   - Smooth transitions

3. **Three Little Circles V2** - Simplest example
4. **GUP V2** - Basic enter/update/exit

### 2.2 Document Code Explorer Requirements

**What Code Explorer Needs**:
1. Load and parse codebase data (likely from Spago output or custom parser)
2. Force-directed graph with nodes = modules, links = imports
3. Interactive features:
   - Click node → show module details
   - Hover → highlight dependencies
   - Filter by namespace (PSD3, PSD3v2, website, etc.)
   - Search by module name
4. Use SimulationM2 with dynamic data updates
5. Use Tree API for clean DOM structure
6. Proper GUP for showing/hiding filtered nodes

**Data Structure**:
```purescript
type ModuleNode =
  { id :: String           -- Module name
  , namespace :: String    -- "PSD3", "PSD3v2", "Website", etc.
  , path :: String         -- File path
  , loc :: Int             -- Lines of code
  }

type ImportLink =
  { source :: String       -- Module name
  , target :: String       -- Module name
  , count :: Int           -- Number of imports (optional)
  }
```

---

## Phase 3: Afternoon - Build Code Explorer

### 3.1 Data Preparation
1. Create script/tool to extract module dependency graph from codebase
2. Generate JSON with modules and imports
3. Load in Code Explorer component

### 3.2 Implementation
1. Create `Component/CodeExplorer.purs` (if not exists)
2. Use SimulationM2 for force layout
3. Use Tree API for rendering
4. Add interactive behaviors:
   - Drag nodes
   - Zoom/pan
   - Click for details
   - Filter controls

### 3.3 Integration
1. Add route to `Types.purs` (already exists)
2. Add to `Main.purs` (already exists)
3. Link from Home page

---

## Phase 4: End of Day - Push to GitHub

### 4.1 Final Checks
- [ ] All old routes removed
- [ ] All old library modules archived
- [ ] Website builds successfully
- [ ] All kept examples work
- [ ] Code Explorer works
- [ ] README updated with new architecture

### 4.2 Git Operations
```bash
# Create feature branch
git checkout -b feature/psd3v2-cleanup

# Stage changes
git add .

# Commit
git commit -m "Major refactor: Archive PSD3 v1, finalize PSD3v2 architecture

- Archive old Understanding pages to src/website/Component/Understanding/Archive/
- Archive old Examples Gallery and individual examples
- Archive old PSD3 v1 library modules to src/lib/PSD3_v1_Archive/
- Keep PSD3v2 as the new core library
- Keep pure PureScript layouts (Hierarchy, Sankey)
- Keep shared data structures and utilities
- Implement Code Explorer with SimulationM2 + Tree API
- Update main PSD3.purs module to export PSD3v2

Breaking changes:
- All old SelectionM/Capabilities examples no longer work
- Use PSD3v2 examples (LesMisGUPTree, TreeAPI) as reference
- Refer to archived Understanding pages for historical context

New examples:
- LesMisGUPTree: Full-featured force simulation with dynamic layouts
- Code Explorer: Interactive codebase visualization

🤖 Generated with Claude Code"

# Push to remote
git push origin feature/psd3v2-cleanup

# Create PR or merge to main
```

### 4.3 Documentation Updates
- Update README.md with:
  - New PSD3v2 architecture overview
  - Link to working examples (LesMisGUPTree, Code Explorer)
  - Migration guide from v1 to v2 (if needed)
  - Archive explanation

---

## Quick Reference: File Operations

### Directories to Create
```bash
mkdir -p src/website/Component/Understanding/Archive
mkdir -p src/website/Component/ExamplesGallery/Archive
mkdir -p src/website/Component/Example/Archive
mkdir -p src/lib/PSD3_v1_Archive/Capabilities
mkdir -p src/lib/PSD3_v1_Archive/Interpreter
mkdir -p src/lib/PSD3_v1_Archive/Internal/Selection
mkdir -p src/lib/PSD3_v1_Archive/Internal/Attributes
mkdir -p src/lib/PSD3_v1_Archive/Simulation
```

### Files to Move (Examples)
```bash
# Understanding pages
mv src/website/Component/Understanding/*.purs src/website/Component/Understanding/Archive/

# Examples gallery
mv src/website/Component/ExamplesGallery/ExamplesGallery.purs src/website/Component/ExamplesGallery/Archive/
mv src/website/Component/Example/Example.purs src/website/Component/Example/Archive/

# Old library modules
mv src/lib/PSD3/Capabilities/ src/lib/PSD3_v1_Archive/
mv src/lib/PSD3/Interpreter/ src/lib/PSD3_v1_Archive/
# ... etc
```

---

## Summary Checklist

**Morning (Clean House)**:
- [ ] Archive Understanding pages
- [ ] Archive Examples Gallery
- [ ] Archive old library modules
- [ ] Remove routes from Types.purs
- [ ] Remove imports from Main.purs
- [ ] Verify build succeeds
- [ ] Test kept examples still work

**Late Morning/Afternoon (Review & Build)**:
- [ ] Review PSD3v2 architecture
- [ ] Document working examples
- [ ] Extract codebase data for Code Explorer
- [ ] Implement Code Explorer
- [ ] Test Code Explorer

**End of Day (Ship It)**:
- [ ] Final build check
- [ ] Update README
- [ ] Git commit and push
- [ ] Celebrate! 🎉

---

**Notes**:
- Keep this document updated as you work through tasks
- Mark items complete with ✅
- Add notes about issues encountered
- Update plan if priorities change
