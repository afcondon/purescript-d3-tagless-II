# CodeExplorer Unsafe Coerce Analysis

## Overview

This document analyzes all uses of `unsafeCoerce` in the CodeExplorer (Spago) visualization and identifies opportunities for leveraging the generic `GraphModel` infrastructure more extensively.

## Current State

### Type Hierarchy

```purescript
-- Core opaque types (defined in PSD3.Data.Node)
foreign import data D3Link_Unswizzled :: Type  -- Links with ID references
foreign import data D3Link_Swizzled :: Type    -- Links with object references (after D3 processes)

-- Spago-specific type alias
type SpagoGraphLinkID = D3Link_Unswizzled

-- Spago model structure
type SpagoModel =
  { links :: Array SpagoGraphLinkID
  , nodes :: Array SpagoSimNode
  , graph :: Graph NodeID SpagoNodeData          -- Data.Graph (purescript-graphs)
  , tree  :: Maybe (Tuple NodeID SpagoTreeNode)
  , maps  :: { name2ID    :: M.Map String NodeID
             , id2Name    :: M.Map NodeID String
             , id2Node    :: M.Map NodeID SpagoNodeData
             , id2Package :: M.Map NodeID NodeID
             , id2LOC     :: M.Map NodeID Number
             , id2TreeData :: M.Map NodeID TreeFields
             }
  }
```

## Unsafe Coerce Usage Catalog

### 1. Node Unboxing (Unsafe.purs)

**Purpose:** Extract data from D3's opaque simulation node wrappers

```purescript
-- Line 14-16: Unbox simulation nodes
unboxD3SimNode :: Datum_ -> SpagoDataRecord
unboxD3SimNode datum = d
  where (D3SimNode d) = unsafeCoerce datum
```

**Rationale:**
- Performance-critical path (called on every render tick)
- Comment explicitly states: "why a single unsafeCoerce and not, say, some kind of lens for each field ? PERFORMANCE"
- D3 simulation nodes are opaque but have known structure after initialization

**Risk Level:** LOW
- Well-documented and centralized
- Type structure is stable and controlled by library
- Alternative would be parsing/validation on every access (prohibitive cost)

### 2. Link Unboxing (Unsafe.purs)

**Purpose:** Extract link data from D3's opaque link references

```purescript
-- Line 18-19: Unbox simulation links
unboxD3SimLink :: Datum_ -> { source :: SpagoDataRecord, target :: SpagoDataRecord | SpagoLinkData }
unboxD3SimLink datum = unsafeCoerce datum
```

**Rationale:**
- Links become swizzled by D3 (IDs replaced with object references)
- Must extract for rendering/interaction
- Performance-critical (rendered every frame)

**Risk Level:** LOW
- Centralized in single function
- Structure determined by D3 library behavior (stable)
- Used consistently throughout codebase

### 3. Index Coercion (Unsafe.purs)

**Purpose:** Convert NodeIDs to D3's internal index type

```purescript
-- Line 21-22: Convert to D3 index
coerceToIndex_ :: forall a. (Ord a) => a -> Index_
coerceToIndex_ = unsafeCoerce

-- Line 24-27: Extract node index as key
spagoNodeKeyFunction :: Datum_ -> Index_
spagoNodeKeyFunction d = index
  where
    index = unsafeCoerce $ (unboxD3SimNode d).id
```

**Rationale:**
- D3's internal index system is opaque
- NodeID (Int) and Index_ are structurally identical at runtime
- Required for D3 join operations

**Risk Level:** LOW
- Type-level safety wrapper around runtime equivalence
- Well-understood pattern in D3 bindings

### 4. Tree Node Recovery (Unsafe.purs)

**Purpose:** Extract Spago data from D3 hierarchy nodes

```purescript
-- Line 42-45: Recover tree object from datum
recoverSpagoTreeObj :: Datum_ -> SpagoTreeObj
recoverSpagoTreeObj datum = t'
  where
    (t' :: SpagoTreeObj) = unsafeCoerce datum
```

**Rationale:**
- D3 hierarchy wraps user data in opaque structure
- Need to access original data for rendering

**Risk Level:** LOW
- D3 hierarchy structure is well-documented and stable
- Used only in tree layout rendering path

### 5. Link Construction (Files.purs)

**Purpose:** Create link objects with proper type wrapper

```purescript
-- Line 236-237: Pack record into opaque link type
packLink :: forall r. { source :: NodeID, target :: NodeID | r } -> SpagoGraphLinkID
packLink = unsafeCoerce

-- Line 239-240: Construct typed link
makeLink :: LinkType -> Tuple NodeID NodeID -> SpagoGraphLinkID
makeLink linktype (Tuple source target) = packLink { source, target, linktype, inSim: true }
```

**Rationale:**
- `SpagoGraphLinkID` is opaque (`D3Link_Unswizzled`)
- Must construct from plain records during data loading
- Type system enforces swizzled vs unswizzled at compile time

**Risk Level:** LOW
- Controlled construction site
- Record structure matches D3 expectations
- Type safety preserved at API boundaries

### 6. Link Unpacking (Files.purs)

**Purpose:** Extract link data for processing

```purescript
-- Line 262-263: Full link unpacking
unpackLink :: SpagoGraphLinkID -> { source :: NodeID, target :: NodeID, linktype :: LinkType, inSim :: Boolean }
unpackLink = unsafeCoerce

-- Line 288-289: Partial link unpacking (just linktype)
unpackLink' :: SpagoGraphLinkID -> { linktype :: LinkType }
unpackLink' = unsafeCoerce
```

**Rationale:**
- Need to inspect link properties for filtering/processing
- `unpackLink'` is optimization (extract only needed field)
- Used in link filtering predicates (lines 291-298)

**Risk Level:** LOW
- Controlled extraction points
- Well-typed target records
- Used for read-only inspection

### 7. Link Construction in Model.purs

**Purpose:** Create links during graph model construction

```purescript
-- Line 439-440: GraphConfig accessors
spagoGraphConfig :: GraphConfig SpagoNodeData SpagoGraphLinkID
spagoGraphConfig =
  { getNodeId: _.id
  , getLinkSource: \link -> (unsafeCoerce link).source
  , getLinkTarget: \link -> (unsafeCoerce link).target
  }

-- Line 450: Link construction in makeGraph
links = nodes >>= \node ->
  node.links.targets <#> \target ->
    unsafeCoerce { source: node.id, target: target, linktype: M2M_Graph, inSim: true }
```

**Rationale:**
- Same as #5 - constructing opaque link types from records
- Required to interface with generic GraphModel infrastructure
- NEW CODE from our refactoring

**Risk Level:** LOW
- Follows established pattern from Files.purs
- Enables generic infrastructure usage
- Type-safe at API boundaries

### 8. Link Operations in Tree.purs

**Purpose:** Link manipulation during tree reduction

```purescript
-- Line 28: Construct link from tuple
tupleToLink linktype (Tuple source target) = unsafeCoerce { source, target, linktype, inSim: true }

-- Line 34-36: Change link type
changeLinkType newLinktype link =
  let oldLink = unsafeCoerce link :: { source :: Int, target :: Int, linktype :: t149, inSim :: Boolean }
  in unsafeCoerce $ oldLink { linktype = newLinktype }

-- Line 46, 136: Unpack links for filtering
unpackLink :: D3Link_Unswizzled -> { source :: NodeID, target :: NodeID }
unpackLink = unsafeCoerce
```

**Rationale:**
- Tree reduction changes link semantics (M2M_Graph → M2M_Tree)
- Must inspect and modify link properties
- Performance-critical path (graph reachability analysis)

**Risk Level:** LOW
- Localized to tree reduction algorithm
- Well-typed target structures
- Follows pack/unpack pattern from elsewhere

## Opportunities for GraphModel Integration

### Current Situation

The SpagoModel currently has:
- **Separate concerns:** `links` and `nodes` arrays (raw data), `graph` (Data.Graph for algorithms)
- **Redundant indexing:** `maps.id2Node` duplicates what GraphModel provides
- **Mixed representations:** Some code uses Data.Graph, other code uses arrays

### Opportunity 1: Add GraphModel to SpagoModel

**Proposal:** Add `graphModel :: GraphModel SpagoNodeData SpagoGraphLinkID` field

```purescript
type SpagoModel =
  { links :: Array SpagoGraphLinkID
  , nodes :: Array SpagoSimNode
  , graph :: Graph NodeID SpagoNodeData      -- Keep for compatibility
  , graphModel :: GraphModel SpagoNodeData SpagoGraphLinkID  -- NEW
  , tree  :: Maybe (Tuple NodeID SpagoTreeNode)
  , maps  :: { ... }
  }
```

**Benefits:**
- O(1) link lookups: `getLinksFrom`, `getLinksTo`
- Bidirectional graph queries
- Foundation for more graph algorithms
- Already computed in `makeGraph` (line 453)

**Implementation:**
```purescript
makeGraph :: Array SpagoNodeData -> Graph NodeID SpagoNodeData
makeGraph nodes = do
  let
    links = nodes >>= \node ->
      node.links.targets <#> \target ->
        unsafeCoerce { source: node.id, target: target, linktype: M2M_Graph, inSim: true }
    graphModel = buildGraphModel spagoGraphConfig nodes links
  toDataGraph spagoGraphConfig graphModel

-- Instead, return both:
buildSpagoGraph :: Array SpagoNodeData ->
  { graph :: Graph NodeID SpagoNodeData
  , graphModel :: GraphModel SpagoNodeData SpagoGraphLinkID
  }
buildSpagoGraph nodes =
  let
    links = nodes >>= \node ->
      node.links.targets <#> \target ->
        unsafeCoerce { source: node.id, target: target, linktype: M2M_Graph, inSim: true }
    graphModel = buildGraphModel spagoGraphConfig nodes links
    graph = toDataGraph spagoGraphConfig graphModel
  in { graph, graphModel }
```

**Migration Path:**
1. Add `graphModel` field, compute alongside `graph`
2. Update Tree.purs to use GraphModel for reachability
3. Remove redundant `maps.id2Node` (use `graphModel.maps.nodeById`)
4. Eventually deprecate `graph` field if all code migrated

### Opportunity 2: Use GraphModel in treeReduction

**Current Code (Tree.purs:39-41):**
```purescript
treeReduction :: NodeID -> SpagoModel -> SpagoModel
treeReduction rootID model = do
  let reachable = getReachableNodes rootID model.graph  -- Uses Data.Graph
```

**Proposed Refactoring:**
```purescript
treeReduction :: NodeID -> SpagoModel -> SpagoModel
treeReduction rootID model = do
  let reachable = PSD3.Data.Graph.Algorithms.getReachableNodes
                    spagoGraphConfig rootID model.graphModel  -- Uses GraphModel
```

**Benefits:**
- Uses generic infrastructure instead of CodeExplorer-specific algorithm
- Returns `ReachabilityResult` with better structure:
  - `nodes :: Set NodeID` (faster membership tests)
  - `spanningTree :: Array (Tuple NodeID NodeID)` (direct tree edges)
  - `redundantEdges :: Array (Tuple NodeID NodeID)` (cycle-creating edges)
  - `paths :: Array (Array NodeID)` (all paths from root)
- Removes dependency on `Data.DependencyGraph` module (old implementation)

**Compatibility Notes:**
- Old `GraphSearchRecord` has slightly different structure
- Need to map field names (e.g., `redundantLinks` → `redundantEdges`)
- Old returns `Array id` for nodes, new returns `Set NodeID`

### Opportunity 3: Eliminate Redundant Indexes

**Current Redundancy:**
```purescript
-- In SpagoModel.maps
id2Node :: M.Map NodeID SpagoNodeData  -- Duplicates GraphModel.maps.nodeById

-- In buildModelFromJSON
id2Node = M.fromFoldable $ nodes <#> \node -> Tuple node.id node
```

**Proposal:**
- Use `graphModel.maps.nodeById` directly
- Remove `id2Node` from SpagoModel.maps
- Update all access sites: `model.maps.id2Node` → `model.graphModel.maps.nodeById`

**Impact Analysis Needed:**
- Search for all uses of `model.maps.id2Node`
- Check if type compatibility (most likely yes - both are `Map NodeID SpagoNodeData`)
- Some code may need `model.graphModel` passed in addition to/instead of `model.maps`

### Opportunity 4: Link Filtering with GraphModel

**Current Pattern (Files.purs:265-268):**
```purescript
getSourceLinks :: SpagoNodeData -> Tuple NodeID (Array NodeID)
getSourceLinks { id } = Tuple id sources
  where
    sources = foldl (\acc link -> let l = unpackLink link in if id == l.target then (l.source:acc) else acc ) [] links
```

**With GraphModel:**
```purescript
-- Already available as:
getLinksTo :: NodeID -> GraphModel node link -> Array link
```

**Benefits:**
- O(1) lookup instead of O(n) fold
- No manual unpacking needed
- Returns full link objects (with linktype, etc.)

## Summary of Unsafe Coercions

### By Risk Level

**LOW RISK (All current uses):**
- Node/Link unboxing for D3 interop (performance-critical, well-understood)
- Index type coercions (type-level wrappers around runtime equivalence)
- Link pack/unpack (controlled construction/inspection of opaque types)

### By Purpose

1. **D3 Interop (lines where D3 produces opaque types):** 8 uses
   - Node unboxing: 2
   - Link unboxing: 3
   - Index coercion: 2
   - Tree recovery: 1

2. **Opaque Type Construction (our own type wrappers):** 5 uses
   - Link packing: 4
   - Link modification: 1

### Recommendations

**Keep Current Patterns:**
- All current `unsafeCoerce` uses are justified and low-risk
- Centralizing in Unsafe.purs is good practice
- Performance requirements justify the approach

**Improve with GraphModel:**
- Add `graphModel` field to SpagoModel (already computed, minimal cost)
- Migrate tree reduction to use generic algorithms
- Remove redundant `id2Node` map
- Use O(1) link lookups instead of linear scans

**Don't Change:**
- Performance-critical unboxing functions
- D3 interop layer (stable and necessary)
- Link pack/unpack pattern (type safety boundary)

## Next Steps

1. **Experiment:** Add `graphModel` field to SpagoModel, ensure no performance regression
2. **Migrate:** Update `treeReduction` to use `PSD3.Data.Graph.Algorithms.getReachableNodes`
3. **Simplify:** Remove `id2Node` redundancy once GraphModel integrated
4. **Document:** Add comments explaining the pack/unpack pattern for future maintainers
5. **Test:** Verify all link filtering operations still work correctly

## Appendix: Type Equivalences

At runtime, these are structurally equivalent but type-system-distinct:

```purescript
-- Opaque wrappers
D3Link_Unswizzled        ≈ { source :: NodeID, target :: NodeID, ... }
D3Link_Swizzled          ≈ { source :: NodeObject, target :: NodeObject, ... }
Datum_                   ≈ D3_SimulationNode d  OR  D3_TreeNode d
Index_                   ≈ Int (or String, depending on key type)

-- Type aliases
SpagoGraphLinkID         = D3Link_Unswizzled
SpagoDataRecord          = { id :: NodeID, name :: String, loc :: Number, ... }
```

The `unsafeCoerce` operations bridge between these representations where the type system cannot track D3's runtime transformations (swizzling, wrapping, etc.).
