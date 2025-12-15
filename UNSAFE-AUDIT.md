# Unsafe Code Audit

Audit of `unsafeCoerce`, `Effect.Ref`, and FFI patterns in the psd3-* libraries.

## Summary

| Library | unsafeCoerce | Effect.Ref | FFI Imports |
|---------|--------------|------------|-------------|
| psd3-selection | Heavy | None | ~80 |
| psd3-simulation | Light | Heavy | Few |
| psd3-layout | None | None | None |

---

## psd3-selection

### unsafeCoerce Usage

**High density - architectural necessity for type-changing operations:**

1. **Operations.purs** (~20 uses)
   - `withDatumType` - intentionally changes datum type
   - SceneNestedJoin type erasure for heterogeneous data
   - Exit/enter/update behavior handling
   - Selection map storage (erasing inner types)
   - Element data get/set

2. **VizTree/Tree.purs** (~10 uses)
   - `sceneNestedJoin` - type erasure for nested data structures
   - `joinWithBehaviors` - erasing behavior types
   - Template function handling

3. **Interpreter/Eval.purs** (~15 uses)
   - Unit types (px, em, rem, etc.) - erasing phantom types
   - Arithmetic on CSS units

4. **Internal/Types.purs** (2 uses)
   - `index_ToInt` / `intToIndex_` - D3 index conversion

5. **Capabilities/Selection.purs** (1 use)
   - `withDatumType` helper

### FFI Imports

Mostly safe D3 wrappers:
- **Behavior/FFI.purs**: zoom, drag, click handlers, simulation registry
- **Brush/FFI.purs**: D3 brush behavior
- **Transition/FFI.purs**: D3 transitions
- **Transform.purs**: DOM manipulation helpers
- **Classify.purs**: CSS class manipulation
- **Tooltip.purs**: Tooltip display (has global state!)
- **Scale.purs**: D3 scales (pure, stateless)

**Concern:** `Tooltip.purs` has global state (`showTooltip_`, `hideTooltip_`, `configureTooltip_`).

---

## psd3-simulation

### unsafeCoerce Usage

**Light - only in config application:**

1. **Config/Apply.purs** (3 uses)
   - Converting `Value` sum type to D3-compatible functions
   - `StaticValue`, `DynamicValue`, `DynamicIndexedValue` cases

### Effect.Ref Usage

**Heavy - simulation state management:**

1. **ForceEngine/Simulation.purs** - Core simulation handle wraps Ref
2. **ForceEngine/Events.purs** - Callback refs for tick/end events
3. **ForceEngine/Registry.purs** - Global simulation registry for drag
4. **ForceEngine/Halogen.purs** - Emitter creation
5. **ForceEngine/Setup.purs** - Ref for tracking applied forces
6. **ForceEngine/Demo.purs** - Demo state
7. **Scene/Handle.purs** - Scene handle state
8. **Scene/Engine.purs** - Scene engine state

**This is by design** - D3 simulations are inherently stateful.

---

## psd3-layout

**Clean!** No unsafe code detected.

---

## Recommendations

### Option A: Explicit Unsafe Modules

Move unsafe operations to clearly-named modules:

```
PSD3.Selection.Unsafe.TypeErasure  -- unsafeCoerce for type changes
PSD3.Selection.Unsafe.FFI          -- DOM manipulation FFI
PSD3.ForceEngine.Unsafe.State      -- Ref-based simulation state
PSD3.Tooltip.Unsafe.Global         -- Global tooltip state
```

### Option B: Document in Module Headers

Add clear warnings in module documentation:

```purescript
-- | WARNING: This module uses unsafeCoerce for type erasure.
-- | The type changes are safe when used through the public API.
-- | Do not use internal functions directly.
module PSD3v2.Selection.Operations where
```

### Option C: Hybrid

- Keep FFI where it is (unavoidable for D3 interop)
- Move `unsafeCoerce` helpers to `.Unsafe` submodules
- Document Ref usage as architectural decision
- Fix `Tooltip` global state (make it take a container ID)

---

## Files to Consider Renaming/Moving

| Current | Proposed | Reason |
|---------|----------|--------|
| PSD3v2/Tooltip.purs | PSD3v2/Tooltip/Global.purs | Global state |
| (new) | PSD3v2/Selection/Unsafe.purs | Extract coercions |
| PSD3/ForceEngine/Registry.purs | (document only) | Intentional global registry |

---

## Safe Modules (No Changes Needed)

- psd3-layout/* (all pure)
- PSD3/Scale.purs (pure D3 wrappers)
- PSD3v3/Interpreter/English.purs (pure)
- PSD3v3/Interpreter/MermaidTree.purs (pure)
- PSD3v3/Interpreter/PureSVG.purs (pure, minor TODO for escaping)
