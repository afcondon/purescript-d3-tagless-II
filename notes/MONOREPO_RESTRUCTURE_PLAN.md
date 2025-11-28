# Monorepo Restructure Plan

## Overview

Transform the current single-package repository into a Spago monorepo with three packages:

1. **psd3-selection** - Core D3 selection/attribute library (publishable)
2. **psd3-simulation** - Force simulation library (publishable, depends on selection)
3. **demo-website** - Tutorial/documentation website (internal, depends on both)

Future separate projects (different repos):
- **code-explorer** - Standalone PS web app for exploring codebases
- **code-analyzer** - Standalone PS tool for cbor/spago → DB

---

## Target Directory Structure

```
.
├── spago.yaml                      # WORKSPACE ONLY
├── spago.lock
├── package.json                    # npm scripts for build/dev/bundle
├── output/                         # shared compilation output
├── .spago/                         # shared dependencies
│
├── psd3-selection/                 # (a) Core library - PUBLISHABLE
│   ├── spago.yaml                  # package only
│   ├── src/
│   │   ├── PSD3.purs               # Main re-export module
│   │   ├── PSD3/
│   │   │   ├── Capabilities/
│   │   │   ├── Internal/
│   │   │   ├── Interpreter/
│   │   │   ├── Layout/
│   │   │   └── Data/
│   │   ├── PSD3v2/
│   │   │   ├── Attribute/
│   │   │   ├── Capabilities/
│   │   │   ├── Interpreter/
│   │   │   ├── Selection/
│   │   │   ├── Transition/
│   │   │   └── VizTree/
│   │   └── Data/                   # Tree, Graph utilities
│   └── test/
│
├── psd3-simulation/                # (b) Force simulation - PUBLISHABLE
│   ├── spago.yaml                  # package only, depends on psd3-selection
│   ├── src/
│   │   └── PSD3/
│   │       ├── ForceEngine/
│   │       │   ├── Core.purs
│   │       │   ├── Core.js
│   │       │   ├── Simulation.purs
│   │       │   └── Types.purs
│   │       └── Config/
│   │           ├── Force.purs
│   │           ├── Scene.purs
│   │           └── Apply.purs
│   └── test/
│
├── demo-website/                   # (c) Documentation site - INTERNAL
│   ├── spago.yaml                  # package only, depends on both
│   ├── src/
│   │   ├── Main.purs
│   │   ├── Component/
│   │   ├── D3/Viz/
│   │   └── PSD3/                   # Website-specific (Home, Shared, etc.)
│   └── public/                     # static assets (currently docs/)
│
├── archived/                       # Keep as-is (not compiled)
├── notes/                          # Planning docs
└── test-harness/                   # Keep as-is
```

---

## Module Classification

### psd3-selection (63 modules → ~55 after cleanup)

**Core Types & FFI:**
- `PSD3.Internal.Types`
- `PSD3.Internal.FFI`
- `Utility`

**Attributes:**
- `PSD3.Internal.Attributes.Instances`
- `PSD3.Internal.Attributes.Sugar`
- `PSD3v2.Attribute.Types`

**Selection (v1):**
- `PSD3.Capabilities.Selection`
- `PSD3.Internal.Selection.Functions`
- `PSD3.Internal.Selection.Types`

**Selection (v2):**
- `PSD3v2.Selection.Types`
- `PSD3v2.Selection.Operations`
- `PSD3v2.Selection.Query`
- `PSD3v2.Selection.Join`
- `PSD3v2.Capabilities.Selection`
- `PSD3v2.Capabilities.GUP`
- `PSD3v2.Capabilities.Transition`

**Interpreters:**
- `PSD3v2.Interpreter.D3v2`
- `PSD3v2.Interpreter.English`
- `PSD3v2.Interpreter.MermaidTree`
- `PSD3v2.Interpreter.MetaAST`
- `PSD3.Interpreter.MermaidAST`

**Transitions:**
- `PSD3v2.Transition.Types`
- `PSD3v2.Transition.FFI`
- `PSD3v2.Transition.Scene`

**Behaviors (zoom, drag):**
- `PSD3v2.Behavior.Types`
- `PSD3v2.Behavior.FFI`
- `PSD3.Internal.Zoom`

**Scales & Axes:**
- `PSD3.Internal.Scales.Scales`
- `PSD3v2.Axis.Axis`

**Layouts (hierarchical):**
- `PSD3.Layout.Hierarchy.Core`
- `PSD3.Layout.Hierarchy.Types`
- `PSD3.Layout.Hierarchy.Tree4`
- `PSD3.Layout.Hierarchy.Cluster4`
- `PSD3.Layout.Hierarchy.Pack`
- `PSD3.Layout.Hierarchy.Partition`
- `PSD3.Layout.Hierarchy.Treemap`
- `PSD3.Layout.Hierarchy.Link`

**Layouts (Sankey):**
- `PSD3.Layout.Sankey.Types`
- `PSD3.Layout.Sankey.Compute`
- `PSD3.Layout.Sankey.Path`
- `PSD3.Layout.Sankey.CSV`
- `PSD3.Capabilities.Sankey`
- `PSD3.Internal.Sankey.Types`
- `PSD3.Internal.Sankey.Functions`

**Data Structures:**
- `Data.Tree`
- `Data.Graph.Algorithms`
- `Data.DependencyGraph`
- `PSD3.Data.Tree`
- `PSD3.Data.Node`
- `PSD3.Data.Graph`
- `PSD3.Data.Graph.Algorithms`

**Tree API:**
- `PSD3v2.VizTree.Tree`
- `PSD3v2.Tooltip`

**Re-export module:**
- `PSD3` (main entry point)

### psd3-simulation (8 modules)

**Force Engine:**
- `PSD3.ForceEngine` (re-export)
- `PSD3.ForceEngine.Core` + `.js`
- `PSD3.ForceEngine.Simulation`
- `PSD3.ForceEngine.Types`
- `PSD3.ForceEngine.Demo` (could move to demo-website)

**Configuration:**
- `PSD3.Config.Force`
- `PSD3.Config.Scene`
- `PSD3.Config.Apply`

### demo-website (~130 modules)

**Everything in src/website/ currently:**
- `Component.*` (all Halogen components)
- `D3.Viz.*` (all visualization implementations)
- `PSD3.Main`, `PSD3.Home`, `PSD3.RoutingDSL`
- `PSD3.Shared.*` (website utilities)
- `PSD3.Reference.*` (API reference pages)
- `PSD3.Tutorial.*`, `PSD3.Understanding.*`
- `PSD3.Wizard.*`
- `CodeSnippet`, `CodeSnippets`, `Snippets`

**Move to demo-website from src/lib:**
- `D3.Viz.Spago.Data` (spago-specific, not general library)
- `src/lib/Viz/*` (if any remain)

---

## spago.yaml Files

### Root spago.yaml (workspace only)

```yaml
workspace:
  packageSet:
    registry: 67.0.1
  extraPackages:
    html-parser-halogen:
      dependencies:
        - halogen
        - string-parsers
      git: https://github.com/afcondon/purescript-html-parser-halogen.git
      ref: esmodules
```

### psd3-selection/spago.yaml

```yaml
package:
  name: psd3-selection
  description: Type-safe D3 selection and attribute library for PureScript
  dependencies:
    - prelude
    - effect
    - arrays
    - maybe
    - either
    - foldable-traversable
    - foreign-object
    - functions
    - integers
    - lists
    - newtype
    - nullable
    - numbers
    - ordered-collections
    - profunctor
    - strings
    - transformers
    - tuples
    - typelevel-prelude
    - unsafe-coerce
    - web-dom
    - web-events
    - web-html
  publish:
    version: 0.1.0
    license: MIT
    location:
      githubOwner: afcondon
      githubRepo: purescript-psd3-selection
```

### psd3-simulation/spago.yaml

```yaml
package:
  name: psd3-simulation
  description: Force-directed graph simulation for PureScript D3
  dependencies:
    - prelude
    - effect
    - arrays
    - maybe
    - psd3-selection  # <-- internal dependency
  publish:
    version: 0.1.0
    license: MIT
    location:
      githubOwner: afcondon
      githubRepo: purescript-psd3-simulation
```

### demo-website/spago.yaml

```yaml
package:
  name: demo-website
  description: PSD3 documentation and examples website
  dependencies:
    - psd3-selection
    - psd3-simulation
    - aff
    - affjax
    - affjax-web
    - argonaut-codecs
    - argonaut-core
    - console
    - datetime
    - debug
    - halogen
    - halogen-subscriptions
    - halogen-svg-elems
    - halogen-vdom
    - html-parser-halogen
    - routing
    - web-uievents
    # ... other website-specific deps
```

---

## Migration Steps

### Phase 1: Preparation (no functional changes)

1. **Create plan document** ✅ (this file)

2. **Audit module dependencies**
   - Run `spago graph modules` to visualize current dependencies
   - Identify any circular dependencies that need breaking
   - Document which modules import from ForceEngine (to verify split)

3. **Create target directory structure**
   ```bash
   mkdir -p psd3-selection/src
   mkdir -p psd3-simulation/src
   mkdir -p demo-website/src
   mkdir -p demo-website/public
   ```

4. **Verify clean build on main**
   ```bash
   npm run clean-build
   ```

### Phase 2: Create psd3-selection package

5. **Move selection modules**
   - Move `src/lib/PSD3/` (except ForceEngine, Config) → `psd3-selection/src/PSD3/`
   - Move `src/lib/PSD3v2/` → `psd3-selection/src/PSD3v2/`
   - Move `src/lib/Data/` → `psd3-selection/src/Data/`
   - Move `src/lib/PSD3.purs` → `psd3-selection/src/PSD3.purs`
   - Move `src/lib/Utility.purs` → `psd3-selection/src/Utility.purs`

6. **Create psd3-selection/spago.yaml**
   - Package section only (no workspace)
   - Minimal dependencies for selection

7. **Update PSD3.purs re-exports**
   - Remove ForceEngine exports (will be separate package)

8. **Test selection package builds**
   ```bash
   spago build -p psd3-selection
   ```

### Phase 3: Create psd3-simulation package

9. **Move simulation modules**
   - Move `src/lib/PSD3/ForceEngine/` → `psd3-simulation/src/PSD3/ForceEngine/`
   - Move `src/lib/PSD3/Config/` → `psd3-simulation/src/PSD3/Config/`

10. **Create psd3-simulation/spago.yaml**
    - Package section only
    - Depends on psd3-selection

11. **Test simulation package builds**
    ```bash
    spago build -p psd3-simulation
    ```

### Phase 4: Create demo-website package

12. **Move website modules**
    - Move `src/website/` → `demo-website/src/`
    - Move `docs/` → `demo-website/public/`

13. **Create demo-website/spago.yaml**
    - Package section only
    - Depends on psd3-selection and psd3-simulation
    - All Halogen/routing/etc dependencies

14. **Update import paths if needed**
    - Should be minimal since module names don't change

15. **Test website package builds**
    ```bash
    spago build -p demo-website
    ```

### Phase 5: Update build tooling

16. **Update package.json scripts**
    ```json
    {
      "scripts": {
        "build": "spago build",
        "build:selection": "spago build -p psd3-selection",
        "build:simulation": "spago build -p psd3-simulation",
        "build:website": "spago build -p demo-website",
        "bundle": "spago bundle -p demo-website --module PSD3.Main --outfile demo-website/public/bundle.js",
        "dev": "spago build -p demo-website --watch",
        "serve": "http-server demo-website/public -p 1234"
      }
    }
    ```

17. **Update any CI/CD if present**

18. **Test full build**
    ```bash
    npm run clean-build
    npm run bundle
    npm run serve
    ```

### Phase 6: Cleanup & Documentation

19. **Delete old src/lib and src/website directories**
    - After verifying everything works

20. **Update README.md**
    - Document new structure
    - Update build instructions

21. **Clean up archived/ folder**
    - Decide what to keep/delete

22. **Final verification**
    - Run through entire website
    - Test all demos work
    - Verify bundle size is reasonable

---

## Potential Issues & Mitigations

### Issue 1: Module name conflicts
- **Risk**: Two packages can't have same module name
- **Mitigation**: Already using distinct prefixes (PSD3.*, Component.*, D3.Viz.*)
- **Check**: `grep -r "^module " */src/ | cut -d: -f2 | sort | uniq -d`

### Issue 2: Circular dependencies
- **Risk**: Selection might import from Simulation or vice versa
- **Mitigation**: Audit with `spago graph modules` before splitting
- **Fix**: Break cycles by extracting shared types to selection package
- **STATUS**: ✅ VERIFIED - No circular deps. ForceEngine/Config only import from:
  - `PSD3.Internal.FFI` (selection)
  - `PSD3.Internal.Types` (selection)
  - `PSD3.Internal.Attributes.Instances` (selection)
  - Standard library modules
  - The force-related FFI functions in `Internal/FFI.js` stay in selection (they're just D3 bindings)

### Issue 3: FFI files (.js)
- **Risk**: JS files might not move correctly
- **Mitigation**: Ensure .js files move with their .purs counterparts
- **Check**: `find . -name "*.js" | grep -v node_modules`
- **INVENTORY**:
  - psd3-selection JS files (18):
    - `PSD3/Internal/FFI.js`, `Axes.js`, `Hierarchical.js`
    - `PSD3/Internal/Scales/Scales.js`, `Linear.js`
    - `PSD3/Internal/Sankey/Types.js`, `Functions.js`
    - `PSD3/Internal/Generators/Line.js`
    - `PSD3/Interpreter/MetaTree.js`, `String.js`
    - `PSD3/Data/Loaders.js`, `Tree.js`
    - `PSD3v2/Selection/Operations.js`, `Indexed.js`
    - `PSD3v2/Transition/FFI.js`
    - `PSD3v2/Behavior/FFI.js`
    - `PSD3v2/Tooltip.js`
  - psd3-simulation JS files (1):
    - `PSD3/ForceEngine/Core.js`

### Issue 4: Build output path
- **Risk**: Bundle path changes break website
- **Mitigation**: Update package.json bundle script
- **Check**: Verify docs/bundle.js or demo-website/public/bundle.js

### Issue 5: Data files (JSON, etc.)
- **Risk**: Data files referenced by relative path
- **Mitigation**: Move data files to demo-website/public/data/
- **Check**: grep for "data/" or "./data/" in source

---

## Verification Checklist

After each phase, verify:

- [ ] `spago build` succeeds (builds all packages)
- [ ] `spago build -p <package>` succeeds for each package
- [ ] No compiler warnings about missing modules
- [ ] Website loads and all pages render
- [ ] All interactive demos work (GUP, AnimatedTree, LesMis, etc.)
- [ ] Code Explorer works (if still present)

---

## Registry Publishing (Future)

Once stable, publish to PureScript registry:

1. **Create GitHub releases** with semver tags
2. **Run `spago publish -p psd3-selection`**
3. **Run `spago publish -p psd3-simulation`**

Note: Both packages should version together initially since simulation depends on selection.

---

## Timeline Estimate

- Phase 1 (Preparation): 1 session
- Phase 2 (psd3-selection): 1-2 sessions
- Phase 3 (psd3-simulation): 1 session
- Phase 4 (demo-website): 1-2 sessions
- Phase 5 (Build tooling): 1 session
- Phase 6 (Cleanup): 1 session

**Total: 6-9 sessions** (can be done incrementally, each phase leaves repo in working state)

---

## Notes

- Each phase should end with a working build
- Commit after each phase for easy rollback
- Test website manually after each phase
- Keep archived/ folder unchanged throughout
