# CodeExplorer Component Architecture Exploration

## Overview
Complete analysis of how the CodeExplorer and ExpandableBubbles components implement spotlight mode and force management in PureScript with D3.

## Documents Created

### 1. CODEEXPLORER_SPOTLIGHT_FORCES_ANALYSIS.md (615 lines, 25 KB)
**Purpose:** Deep technical analysis with code examples

**Contents:**
1. Force Library Setup - Centralized force configuration with node filtering
2. Scene Configuration - How scenes encapsulate complete visualization configs
3. Simple Spotlight Pattern - CodeExplorer's straightforward approach
4. Advanced Spotlight Mode - ExpandableBubbles' complex multi-step process
5. Node/Link Filtering & Restoration - FFI-driven filtering mechanics
6. Force Actualization Functions - Both monadic and non-monadic versions
7. The Critical runSimulation Pattern - Three-step pipeline with proper ordering
8. D3 Interpreter Force Handling - Critical timing rules
9. Architecture Differences - CodeExplorer vs ExpandableBubbles comparison
10. Key Patterns to Adopt - Best practices for implementation
11. FFI Functions Reference - Table of external JS functions
12. Concrete Example - Complete spotlight implementation walkthrough

**Use this when:** You need deep understanding of implementation details, debugging specific issues, or learning the complete architecture.

---

### 2. QUICK_REFERENCE.md (272 lines, 8.9 KB)
**Purpose:** Quick lookup guide for common tasks

**Contents:**
- File locations with descriptions
- Key concepts at a glance with line numbers
- 6 concrete code examples
- State type overview
- Common tasks with step-by-step instructions
- Debugging tips
- Important insights summary

**Use this when:** You need quick lookups, file locations, or step-by-step instructions for common tasks.

---

### 3. EXPLORATION_INDEX.md (this file)
**Purpose:** Navigation and summary of exploration results

---

## Key Discoveries

### Force Library Architecture
**File:** `src/website/Component/CodeExplorer/Forces.purs`

A centralized Map of 16 forces, each with:
- Unique string label (key)
- Force type (Center, X, Y, Collide, ManyBody, Radial, Link)
- Optional node filter (ForceFilter)
- Configuration parameters (strength, radius, distance, etc.)

Example node filters:
- `packagesOnly` - applies only to nodes where `datum_.isPackage`
- `modulesOnly` - applies only to nodes where `datum_.isModule`
- `treeExceptLeaves` - applies only to tree parent nodes
- `usedModulesOnly` - applies only to modules in dependency tree

### Scene Configuration Pattern
**File:** `src/website/Component/CodeExplorer/Scenes.purs`

Each `SceneConfig` is a complete specification of:
1. **Data Filtering** - `chooseNodes`, `linksShown`, `linksActive` predicates
2. **Force Configuration** - `activeForces` Set of force labels
3. **Visual Styling** - CSS class and D3 attributes
4. **Node Positioning** - Initialization functions for alternative layouts

5 Predefined Scenes:
- **packageGridScene** - Hierarchical grid layout
- **packageGraphScene** - Force-directed package dependencies
- **layerSwarmScene** - Horizontal layering of modules
- **radialTreeScene** - Dependency tree radiating from root
- **horizontalTreeScene** - Tree layout flowing left-to-right
- **verticalTreeScene** - Tree layout flowing top-to-bottom

### Spotlight Implementation - Two Approaches

**Simple (CodeExplorer):**
```purescript
ToggleChildrenOfNode id -> do
  H.modify_ $ setChooseNodes (isPackageOrVisibleModule id)
  runSimulation
```

**Advanced (ExpandableBubbles):**
1. Store original nodes/links in Refs
2. Call FFI: `filterToConnectedNodes_` to filter simulation
3. Call FFI: `expandNodeById_` to show detailed declarations
4. Switch forces with `actualizeForcesDirect`
5. Show/hide labels with FFI calls
6. Track state in multiple Refs for restoration

### The Critical runSimulation Pipeline
**File:** `src/website/Component/CodeExplorer/CodeExplorer.purs` lines 289-338

Three-step process (ORDER IS CRITICAL):

```purescript
-- Step 1: FILTER - Apply node filter predicate
let filteredNodes = filter nodeFilter allModelNodes

-- Step 2: INITIALIZE - Run positioning functions on filtered data
let initializedNodes = foldl (\nodes fn -> fn nodes) filteredNodes nodeInitializers

-- Step 3: SIMULATE - Update D3 with filtered+initialized nodes
-- IMPORTANT: Pass nodeFilter: Nothing to prevent double-filtering!
update { nodes: initializedNodes, ... , nodeFilter: Nothing, ... }
```

**Why This Order Matters:**
- Tree layouts expect homogeneous, pre-filtered data (only modules, not mixed nodes)
- Initializers run on filtered data to generate correct positions
- Passing `nodeFilter: Nothing` in Step 3 prevents double-filtering

**Historical Bug:** When filtering moved to Step 3, tree layout received all 884 nodes instead of 90 modules, generating coordinates for only 1 node.

### Force Switching Mechanics
**File:** `src/lib/PSD3/Internal/Simulation/Functions.purs` lines 35-103

Two versions:

**1. simulationActualizeForces (Monadic)**
- Works in MonadState (action handlers)
- Used in CodeExplorer for scene switching
```purescript
simulationActualizeForces activeForces = do
  handle <- use _handle
  library <- use _forceLibrary
  let enableLabels = activeForces ∩ (keys library)
      disableLabels = (keys library) \ activeForces
  simulationEnableForcesByLabel enableLabels
  simulationDisableForcesByLabel disableLabels
  updatedLibrary <- use _forceLibrary
  let _ = (updateForceInSimulation handle) <$> updatedLibrary
  pure unit
```

**2. actualizeForcesDirect (Non-Monadic)**
- Works in Effect callbacks
- Used in ExpandableBubbles for spotlight switching
```purescript
actualizeForcesDirect activeForces simState =
  let enableLabels = activeForces ∩ (keys library)
      disableLabels = (keys library) \ activeForces
      updatedLibrary = (enableByLabels handle enableLabels) 
                    <$> ((disableByLabels handle disableLabels) <$> library)
  in SimState_ $ record { forceLibrary = updatedLibrary }
```

Both compute intersection (enable) and difference (disable) of force sets.

### Critical Ordering Rule
**File:** `src/lib/PSD3/Interpreter/D3.purs`

Forces MUST be activated BEFORE links are set:
```purescript
-- During init:
simulationSetNodes config.nodes           -- Step 2
simulationActualizeForces config.activeForces  -- Step 3 (BEFORE links!)
linksInSim <- simulationSetLinks ...      -- Step 4 (AFTER forces)

-- During update:
simulationActualizeForces forces           -- Must come BEFORE
linksInSim <- simulationSetLinks ...       -- Must come AFTER
```

D3 needs the link force registered in the simulation before links can reference it.

---

## Architecture Comparison: CodeExplorer vs ExpandableBubbles

### CodeExplorer (Simpler, 884 nodes)
- **Approach:** Scene-based configuration with automatic management
- **Force Changes:** Toggle individual forces one at a time
- **Node Filtering:** Simple predicates (isPackage, isUsedModule, etc.)
- **Spotlight:** Just change node filter + runSimulation
- **Restoration:** Automatic when changing scenes
- **State Management:** Scene configuration + simulation state
- **Code Location:** 10 files in src/website/Component/CodeExplorer/

### ExpandableBubbles (Complex, 100+ modules)
- **Approach:** Manual state management with Refs
- **Force Changes:** Switch entire force sets at once (compact vs spotlight)
- **Node Filtering:** FFI-driven filtering with connectedness lookup
- **Spotlight:** Involves filtering, force switching, node expansion, label toggling
- **Restoration:** Explicit FFI call to restore all nodes/links
- **State Management:** Multiple Refs for tracking filtering state
- **Code Location:** Single file with 717 lines of implementation

### Key Differences

| Aspect | CodeExplorer | ExpandableBubbles |
|--------|--------------|-------------------|
| Scale | 884 nodes, 5 scenes | 100+ modules with declarations |
| Force Configuration | Single library | Dual configurations (compact/spotlight) |
| Force Switching | Single toggle | Full set replacement |
| Filtering | Predicates | FFI calls |
| Restoration | Automatic (via scene) | Explicit (via FFI) |
| Expansion | Not used | Complex (expandNodeById_) |
| Force Sync | simulationActualizeForces | actualizeForcesDirect + Ref |
| State Tracking | Scene-based | Multiple Refs |
| Collision Radius | Single function | Dual functions (compact/spotlight) |

---

## Essential Files to Reference

### For Force Organization
- `src/website/Component/CodeExplorer/Forces.purs` - Centralized force library
- `src/lib/PSD3/Internal/Simulation/Types.purs` - Force type definitions
- `src/lib/PSD3/Internal/Simulation/Config.purs` - Force parameter builders

### For Scene Configuration
- `src/website/Component/CodeExplorer/Scenes.purs` - All scene definitions
- `src/website/Component/CodeExplorer/State.purs` - SceneConfig type and helpers

### For Spotlight Implementation
- `src/website/Component/CodeExplorer/CodeExplorer.purs` - Simple spotlight (lines 126-134)
- `src/website/Component/CodeAtlas/Tabs/ExpandableBubbles.purs` - Advanced spotlight (lines 500-675)

### For Force Mechanics
- `src/lib/PSD3/Internal/Simulation/Functions.purs` - Force actualization (lines 35-103)
- `src/lib/PSD3/Interpreter/D3.purs` - D3 interpreter with force handling
- `src/lib/PSD3/Internal/FFI.purs` - FFI declarations for filtering/restoration

---

## Patterns to Adopt

1. **Centralized Force Library** - Define all forces once in a Map
2. **Scenes as Configurations** - Encapsulate filters, forces, styling, init in SceneConfig
3. **Three-Step Pipeline** - Filter → Initialize → Update (critical order!)
4. **Dual Actualization Functions** - Monadic for handlers, non-monadic for callbacks
5. **State Preservation** - Store original data in Refs before filtering
6. **Force Ordering** - Always activate forces BEFORE setting links
7. **Complete Scene Switching** - Change all aspects (filters, forces, styling) atomically

---

## Quick Navigation

**Looking for...**

- How forces are organized? → See Forces.purs section in CODEEXPLORER_SPOTLIGHT_FORCES_ANALYSIS.md
- Scene examples? → See Scenes.purs section in QUICK_REFERENCE.md
- Simple spotlight? → See section 3 in CODEEXPLORER_SPOTLIGHT_FORCES_ANALYSIS.md
- Complex spotlight? → See section 4 in CODEEXPLORER_SPOTLIGHT_FORCES_ANALYSIS.md + ExpandableBubbles.purs
- Force switching code? → See section 6 in CODEEXPLORER_SPOTLIGHT_FORCES_ANALYSIS.md
- Pipeline explanation? → See section 7 in CODEEXPLORER_SPOTLIGHT_FORCES_ANALYSIS.md
- Common tasks? → See "Common Tasks" in QUICK_REFERENCE.md
- Debugging help? → See "Debugging Tips" in QUICK_REFERENCE.md
- File locations? → See "File Locations" in QUICK_REFERENCE.md

---

## Document Statistics

- **CODEEXPLORER_SPOTLIGHT_FORCES_ANALYSIS.md:** 615 lines, 25 KB, 12 major sections
- **QUICK_REFERENCE.md:** 272 lines, 8.9 KB, quick lookup format
- **EXPLORATION_INDEX.md:** This file, navigation and summary

**Total:** 887 lines of documentation covering:
- 16 forces with detailed descriptions
- 5 complete scenes with configurations
- 2 spotlight implementation approaches
- 2 force actualization mechanisms
- 7 critical insights
- 10+ code examples
- 10+ debugging tips
- Complete file location reference

---

## For Next Steps

When implementing spotlight in your ExpandableBubbles component:

1. Create dual force sets (compact vs spotlight)
2. Store original nodes/links in Refs for restoration
3. Implement FFI calls for filtering and restoration
4. Use actualizeForcesDirect for non-monadic force switching
5. Track filtering state in multiple Refs
6. Expand nodes and show additional details in spotlight mode
7. Hide labels/details when exiting spotlight

All patterns and examples are documented in the analysis files.

---

Generated: 2024-11-04
Files: 3 documents, 887 lines of analysis
