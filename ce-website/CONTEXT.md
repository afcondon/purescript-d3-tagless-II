# Code Explorer V4 - Development Context

## Status: Foundation Complete

Created 2024-12-01. Basic structure is in place, ready for Scene 1 implementation.

## What's Done

### Subrepo Structure
- `spago.yaml` with dependencies on psd3-selection, psd3-simulation
- `src/Main.purs` - Halogen entry point
- `src/Types.purs` - Core types (Scene, SimNode, SimLink, NodeType, etc.)
- `src/Component/App.purs` - Shell UI with scene buttons
- `public/index.html`, `public/styles.css` - Dark theme UI shell
- `public/data` -> symlink to demo-website's data

### npm Scripts
- `npm run build:ce` / `bundle:ce` / `dev:ce` / `serve:ce`
- Server runs on http://localhost:1235

### Library Additions (psd3-simulation)
Added filtered/dynamic force variants to `PSD3.ForceEngine.Core`:
- `createManyBodyFiltered` - Charge with `(node -> Boolean)` filter
- `createRadialFiltered` - Radial with filter
- `createCollideDynamic` - Collision with `(node -> Number)` radius accessor
- `createForceXDynamic` - forceX with `(node -> Number)` target accessor
- `createForceYDynamic` - forceY with dynamic target

These are the generalizations that were in demo-website's SimulationManager.js, now properly in the library.

## Next Steps

1. **Data Loading** - Fetch modules.json, packages.json, LOC.json and transform to SimNode/SimLink
2. **Scene 1 (Orbit)** - Packages on radial ring, modules clustered around them
   - Use `createRadialFiltered` for packages
   - Use `createForceXDynamic`/`createForceYDynamic` for module clustering
   - Use `createCollideDynamic` for collision with node.r
3. **Scene 2 (Tree)** - Dependency tree with animated transition
4. **Scene 3 (BubblePack)** - Internal module structure

## Reference Implementation

Check `demo-website/src/Viz/LesMisV3/` for the most up-to-date example of:
- Using `PSD3.ForceEngine.Simulation`
- Force configuration with `ForceSpec` sum type
- GUP (General Update Pattern) with the library's selection API

The old CodeExplorerV2/V3 has its own SimulationManager which duplicates library functionality - don't use that as a reference.

## Data Format

From `public/data/spago-data/`:

**modules.json**:
```json
{
  "ModuleName": {
    "depends": ["Dep1", "Dep2"],
    "package": "package-name",
    "path": ".spago/..."
  }
}
```

**packages.json**:
```json
{
  "package-name": {
    "depends": ["dep-pkg1", "dep-pkg2"]
  }
}
```

**LOC.json**:
```json
{
  "loc": [
    { "loc": 123, "path": ".spago/.../File.purs" }
  ]
}
```

## Architecture Notes

- No JavaScript in this demo (per CLAUDE.md rules)
- All visualization via psd3-selection and psd3-simulation APIs
- If something is missing, add it to the library first
- Halogen manages UI state, library manages simulation state
