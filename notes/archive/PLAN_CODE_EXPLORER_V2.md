# Plan: Port Code Explorer to PSD3v2

## Executive Summary

The code-explorer is the most complex visualization in the codebase, featuring:
- **884 nodes** (79 packages + 805 modules) with multi-level DOM structure (Group → Circle + Text)
- **~2000 links** across 4 link types (tree, graph, module-to-package, package-to-package)
- **6 interactive scenes** (PackageGrid, PackageGraph, 3 tree orientations, LayerSwarm)
- **20+ configurable forces** with real-time enable/disable
- **Rich interactions**: zoom, pan, node dragging, click events, force toggling, filters

**Goal**: Replace all D3 Selection usage with PSD3v2's phantom-typed API while preserving functionality.

**Estimated Effort**: 5-7 weeks

**Status**: Analysis complete, ready for implementation

---

## Current Architecture

### File Structure

**Halogen Layer** (UI & State):
- `Component/CodeExplorer/CodeExplorer.purs` - Main component (500 lines)
- `Component/CodeExplorer/State.purs` - State management
- `Component/CodeExplorer/Scenes.purs` - 6 scene definitions
- `Component/CodeExplorer/Forces.purs` - Force library (20+ forces)
- `Component/CodeExplorer/HTML.purs` - UI rendering (buttons, controls, tables)
- `Component/CodeExplorer/Actions.purs` - Action types
- `Component/CodeExplorer/Data.purs` - JSON data loading

**D3/Visualization Layer** (Needs Porting):
- `Viz/Spago/Draw.purs` - **NEEDS PORT** - D3 initialization & updateSimulation
- `Viz/Spago/Render.purs` - **NEEDS PORT** - RenderCallbacks (enter/update/exit/tick)
- `Viz/Spago/Attributes.purs` - No changes (pure attribute calculation)
- `Viz/Spago/Model.purs` - Minimal changes (node initializers are pure)
- `Viz/Spago/Files.purs` - No changes (JSON parsing)

### Data Flow

```
User Action (click scene button)
  ↓
Halogen Action (SwitchScene)
  ↓
State Update (scene field changed)
  ↓
runSimulation (Halogen Effect)
  ↓
runWithD3_Simulation (interprets D3M monad)
  ↓
genericUpdateSimulation (library function)
  ↓
RenderCallbacks (onNodeEnter/Update/Exit, tick functions)
  ↓
D3 DOM Updates
  ↓
Force Simulation (ticks update positions)
  ↓
Tick Callbacks (update cx/cy attributes)
```

---

## Key Challenges

### 1. Multi-Level Node Structure (CRITICAL)

**Current Pattern**:
```purescript
-- Render.purs lines 61-76
nodeEnter <- appendTo enterSel Group enterAttrsWithTags
circleChild <- appendTo nodeEnter Circle attrs.circles
textChild <- appendTo nodeEnter Text attrs.labels

-- Later, to update children:
updateCircles <- selectUnder updateSel (show Circle)
setAttributes updateCircles attrs.circles
```

**Problem**: PSD3v2 needs type-safe child selection and updates.

**Solution Options**:

**Option A: Nested Builders** (Recommended)
```purescript
-- Proposed PSD3v2 API
nodeEnter <- append Group enterAttrs $ nested do
  circle <- append Circle attrs.circles
  text <- append Text attrs.labels
  pure { circle, text }

-- Update children via record:
nodeEnter.circle # setAttrs updatedCircleAttrs
nodeEnter.text # setAttrs updatedTextAttrs
```

**Option B: Composite Element Type**
```purescript
-- Define custom element type
data NodeGroup = NodeGroup
  { group :: ElementType
  , circle :: ElementType
  , text :: ElementType
  }

-- Use in join
JoinResult { enter: nodeEnter } <- joinData nodes "g" parent
nodes <- appendComposite NodeGroup
  { groupAttrs: [...]
  , circleAttrs: [...]
  , textAttrs: [...]
  }
```

**Decision**: Start with Option A (nested builders), evaluate if Option B is needed for complex updates.

### 2. General Update Pattern with Merge (CRITICAL)

**Current Pattern**:
```purescript
-- Update.purs lines 214-230
node' <- updateJoin node nodeElement enhanced.nodes nodeKeyFn
nodeEnter <- callbacks.onNodeEnter node'.enter attrs
callbacks.onNodeUpdate node'.update attrs
callbacks.onNodeExit node'.exit
mergedNodes <- mergeSelections nodeEnter node'.update

-- Tick function needs merged selection
addTickFunction "nodes" $ Step mergedNodes (callbacks.nodeTickAttrs attrs)
```

**Problem**: Tick functions operate on ALL nodes (enter + update), not just one subset.

**Solution**:
```purescript
-- PSD3v2 needs merge operation
JoinResult { enter, update, exit } <- joinData nodes "g" parent

-- Process each
nodeEnter <- onNodeEnter enter
nodeUpdate <- onNodeUpdate update
nodeExit <- onNodeExit exit

-- Merge for tick function (preserves phantom type)
merged <- merge nodeEnter nodeUpdate :: Selection SBound Group SpagoSimNode

-- Register tick
addTickFunction "nodes" $ Step merged tickAttrs
```

**Action Required**: Implement `merge` function in PSD3v2 that preserves phantom types.

### 3. Behavior Attachment (MEDIUM)

**Current Pattern**:
```purescript
-- Draw.purs lines 47-52
_ <- inner `on` Drag DefaultDrag
_ <- svg `on` Zoom { extent, scaleExtent, name, target }

-- Render.purs line 75
_ <- nodeEnter `on` Drag (CustomDrag "spago" simdrag_)
```

**Solution**: Already implemented in PSD3v2!
```purescript
import PSD3v2.Behavior.Types (Behavior(..), defaultDrag, defaultZoom, ScaleExtent(..))

_ <- on (Drag defaultDrag) inner
_ <- on (Zoom $ defaultZoom scaleExtent target) svg
_ <- on (Drag $ simulationDrag "spago") nodeEnter
```

**Status**: ✅ Complete (from LesMisGUPV2 work)

### 4. Tick Function Registration (MEDIUM)

**Current Pattern**:
```purescript
-- Update.purs lines 249-250
addTickFunction "nodes" $ Step mergedNodes (callbacks.nodeTickAttrs attrs)
addTickFunction "links" $ Step mergedLinks callbacks.linkTickAttrs
```

**Solution**: Already implemented in PSD3v2 SimulationM!
```purescript
addTickFunction "nodes" $ Step mergedNodes
  [ transform (\d -> "translate(" <> show d.x <> "," <> show d.y <> ")")
  ]
```

**Status**: ✅ Complete (from LesMisV2 work)

**Remaining Challenge**: Multi-level nodes need to update children in tick:
```purescript
-- Need to update both circle and text children on each tick
addTickFunction "nodes" $ Step mergedNodes
  [ -- How to update children here?
  ]
```

**Solution**: Use attribute that updates via selector:
```purescript
-- Option: Use foreign function to update children
foreign import updateNodeChildren_ :: Element -> Datum_ -> Unit

addTickFunction "nodes" $ Step mergedNodes
  [ customSync (\elem datum -> updateNodeChildren_ elem datum)
  ]
```

### 5. Click Event Flow (LOW-MEDIUM)

**Current Pattern**:
```purescript
-- CodeExplorer.purs lines 196-200
handleD3Event :: VizEvent -> Action
handleD3Event = case _ of
  NodeClick nodeID -> HandleNodeClick nodeID

-- Render.purs line 74
, onClick (DatumFn \d -> emitVizEvent $ NodeClick (unsafeCoerce d).id)
```

**Solution**: Same pattern works with PSD3v2!
```purescript
-- Use onClick attribute with emitter
nodeEnter <- append Group
  [ onClick (\d -> liftEffect $ emitVizEvent $ NodeClick d.id)
  ]
```

**Status**: Should work with minimal changes (needs testing)

---

## Implementation Plan

### Phase 1: Foundation (Week 1-2)

**Goal**: Get simplest scene working (PackageGraph with circles only)

**Tasks**:
1. ✅ Create `Viz/Spago/DrawV2.purs` module
2. ✅ Port `initialize` function (SVG structure, zoom/pan)
3. ✅ Create minimal `RenderCallbacksV2` (circles only, no text)
4. ✅ Port `updateSimulation` to use PSD3v2 join
5. ✅ Register tick function for node positions
6. ✅ Test PackageGraph scene loads and animates

**Deliverable**: PackageGraph scene with circles only, draggable, zoom/pan working

**Risks**: Merge operation may need implementation in PSD3v2

### Phase 2: Multi-Level Nodes (Week 3)

**Goal**: Add text labels using nested structure

**Tasks**:
1. ✅ Design nested builder API for Group → Circle + Text
2. ✅ Implement child selection for updates
3. ✅ Update tick function to position both circle and text
4. ✅ Test label visibility and styling

**Deliverable**: All scenes show labels correctly

**Risks**: Phantom types for nested structures may be complex

### Phase 3: Scene Transitions (Week 4)

**Goal**: Support all 6 scenes with smooth transitions

**Tasks**:
1. ✅ Port node initializers (grid, tree, phyllotaxis)
2. ✅ Add transition support for scene changes
3. ✅ Test each scene: PackageGrid, PackageGraph, Tree (3 variants), LayerSwarm
4. ✅ Verify force behavior in each scene

**Deliverable**: All 6 scenes working with transitions

**Risks**: Tree layout initializers may need adjustments

### Phase 4: Interactions (Week 5)

**Goal**: Full interactivity parity with v1

**Tasks**:
1. ✅ Wire up node click events (expand/collapse packages)
2. ✅ Add spotlight mode (stop simulation on click)
3. ✅ Test drag behavior with force simulation
4. ✅ Add force toggling UI integration
5. ✅ Add filter controls (node/link visibility)

**Deliverable**: All interactive features working

**Risks**: Event emitter integration may need debugging

### Phase 5: Polish & Optimization (Week 6-7)

**Goal**: Production-ready code-explorer v2

**Tasks**:
1. ✅ Optimize rendering performance (large dataset)
2. ✅ Add welcome overlay
3. ✅ Test all simulation controls (start/stop/heat/cool)
4. ✅ Add alpha/alphaTarget sliders
5. ✅ Test tagging system
6. ✅ Cross-browser testing
7. ✅ Documentation
8. ✅ Remove old v1 code

**Deliverable**: Code Explorer V2 shipped to production

---

## API Gaps in PSD3v2

Based on this analysis, PSD3v2 needs these additions:

### Critical (Blocking)
1. **Nested Builders**: `nested :: Builder m a -> Builder m (Selection state elem datum, a)`
2. **Merge Operation**: `merge :: Selection s1 e d -> Selection s2 e d -> m (Selection SBound e d)`
3. **Child Selection**: `selectAllChildren :: String -> Selection s e d -> m (Selection s2 e2 d)`

### Important (Can Workaround)
4. **Custom Tick Attributes**: `customSync :: (Element -> Datum_ -> Unit) -> Attribute d`
5. **Click Events**: `onClick :: (d -> Effect Unit) -> Attribute d`

### Nice to Have (Can Defer)
6. **Transition Helpers**: Wrap D3 transitions for scene changes
7. **Force Predicates**: Type-safe force filtering

---

## Success Metrics

**Code Explorer V2 is successful when**:
1. ✅ All 6 scenes render correctly with 884 nodes + ~2000 links
2. ✅ Node clicking expands/collapses packages
3. ✅ Force toggling works in real-time
4. ✅ Zoom/pan/drag interactions match v1 UX
5. ✅ Scene transitions are smooth and correct
6. ✅ Performance is equal or better than v1
7. ✅ No D3 Selection usage remains (100% PSD3v2)
8. ✅ Code is cleaner and more maintainable than v1

---

## Next Steps

1. **Review this plan** - Confirm approach and timeline
2. **Prioritize API gaps** - Which PSD3v2 features to build first?
3. **Create branch** - `feature/code-explorer-v2`
4. **Start Phase 1** - Begin with PackageGraph (circles only)
5. **Iterate rapidly** - Small PRs, frequent testing

---

## Open Questions

1. **Nested builders vs composite types** - Which approach for multi-level nodes?
2. **Merge implementation** - Where should this live in PSD3v2? Interpreter or Capabilities?
3. **Tick function children** - FFI helper or pure PureScript solution?
4. **Testing strategy** - Unit tests for rendering? Visual regression tests?
5. **Migration path** - Gradual (dual implementations) or big bang?

---

## Notes

- Code Explorer is the **flagship visualization** - getting this right validates PSD3v2
- Multi-level nodes (Group → Circle + Text) is **unique to this viz** - solution will help others
- Force library is **reusable** - already works with PSD3v2 API
- Data model is **independent** - no changes needed
- Halogen integration is **proven** - LesMisGUPV2 shows the pattern works

**Bottom Line**: Porting Code Explorer will complete the PSD3v2 transition and prove the architecture works for complex, production visualizations.
