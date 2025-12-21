# Build System Cleanup Analysis

## Current State

### Package Structure (10 packages)

```
Core Libraries (publishable):
├── psd3-tree          # Rose tree data structure
├── psd3-selection     # D3 selection/attribute library (depends on psd3-tree)
├── psd3-layout        # Pure layout algorithms (depends on psd3-tree)
├── psd3-simulation    # Force simulation (depends on psd3-selection)
├── psd3-music         # Audio/sonification (depends on psd3-selection)
└── psd3-tidal         # TidalCycles parser (standalone)

Applications:
├── demo-website       # Main documentation site
├── ce-website         # Code Explorer standalone app
├── ce2-website        # Experimental (unused?)
├── ce-server          # Code Explorer API server
└── ce-database        # Database utilities
```

### Dependency Graph

```
psd3-tree
    ↓
psd3-selection ←─────────────────────┐
    ↓              ↓                 │
psd3-simulation   psd3-layout    psd3-music
    ↓                  ↓             ↓
    └──────── demo-website ──────────┘
                   ↓
              ce-website (also depends on psd3-layout, psd3-selection, psd3-simulation)
```

## Issues Found

### 1. Bundle Output Confusion

**Problem**: Multiple bundle locations with unclear relationships:

| Location | Size | Last Modified | Purpose |
|----------|------|---------------|---------|
| `docs/bundle.js` | 2.89MB | Dec 20 | Served website |
| `demo-website/public/bundle.js` | 777KB | Dec 16 | Unclear |
| `ce-website/public/bundle.js` | 1.17MB | Dec 15 | CE standalone |

**Root cause**: The bundle command uses a relative path:
```json
"bundle": "spago bundle -p demo-website --module PSD3.Main --outfile ../docs/bundle.js"
```

This is confusing because:
- Spago runs from the workspace root
- `../docs` is interpreted relative to demo-website directory
- Different behavior depending on where you run commands from

### 2. Stale/Mystery Files

| File | Size | Likely Status |
|------|------|---------------|
| `index.js` (root) | 1.5MB | Stale - old bundle? |
| `demo-website/index.js` | 778KB | Stale - old bundle? |
| `ce-website/public/bundle.js.bak` | 916KB | Backup - can delete |

### 3. Broken Imports in demo-website

`demo-website/src/Component/MiniNotation.purs` uses deprecated modules:
```purescript
import Text.Parsing.Parser (...)           -- OLD
import Text.Parsing.Parser.Combinators (...) -- OLD
import Text.Parsing.Parser.String (...)    -- OLD
import Text.Parsing.Parser.Token (...)     -- OLD
```

Should use `purescript-parsing` (the new name) or better yet, use `psd3-tidal`.

### 4. Broken Symlink

`ce-website/public/data` symlinks to `../../demo-website/public/data` but that directory doesn't exist. The actual data is in `docs/data/`.

### 5. Script Naming Inconsistency

```json
"bundle": "... --outfile ../docs/bundle.js"      // Outputs to docs/
"bundle:ce": "... --outfile public/bundle.js"    // Outputs to ce-website/public/
"serve": "http-server docs -p 1234"              // Serves docs/
"serve:ce": "http-server ce-website/public -p 1235"  // Serves ce-website/public/
```

The main website is confusingly split between `demo-website/` (source) and `docs/` (output).

## Recommended Cleanup

### Option A: Clean Up In Place (Minimal Disruption)

1. **Fix bundle paths to be absolute from root**:
   ```json
   "bundle": "spago bundle -p demo-website --module PSD3.Main --outfile docs/bundle.js"
   ```

2. **Remove stale files**:
   ```bash
   rm index.js demo-website/index.js ce-website/public/bundle.js.bak
   ```

3. **Fix broken imports**:
   - Replace `Text.Parsing.Parser` with `Parsing` in MiniNotation.purs
   - Or better: import from `psd3-tidal` instead

4. **Fix broken symlink**:
   ```bash
   rm ce-website/public/data
   ln -s ../../docs/data ce-website/public/data
   ```

5. **Document the structure** in README/CLAUDE.md

### Option B: Restructure for Clarity

1. **Rename `docs/` to `demo-website/public/`** - consolidate source and output
2. **Move static assets** into demo-website properly
3. **Single source of truth** for each website

### Option C: Split Into Separate Repos (For Publication)

Core libraries as separate repos:
- `purescript-psd3-tree`
- `purescript-psd3-selection`
- `purescript-psd3-layout`
- `purescript-psd3-simulation`

Keep in monorepo:
- `psd3-music` (experimental)
- `psd3-tidal` (new, experimental)
- `demo-website`
- `ce-*` (Code Explorer suite)

## Proposed package.json Cleanup

```json
{
  "scripts": {
    "build": "spago build",
    "build:libs": "spago build -p psd3-selection -p psd3-simulation -p psd3-layout",
    "build:demo": "spago build -p demo-website",
    "build:ce": "spago build -p ce-website",

    "bundle:demo": "spago bundle -p demo-website --module PSD3.Main --outfile docs/bundle.js",
    "bundle:ce": "spago bundle -p ce-website --module Main --outfile ce-website/public/bundle.js",

    "dev:demo": "npm run build:demo && npm run bundle:demo",
    "dev:ce": "npm run build:ce && npm run bundle:ce",

    "serve:demo": "npx http-server docs -p 1234",
    "serve:ce": "npx http-server ce-website/public -p 1235",

    "test": "spago test",
    "test:libs": "spago test -p psd3-selection -p psd3-simulation -p psd3-layout -p psd3-tidal",

    "clean": "rm -rf output",
    "clean:bundles": "rm -f docs/bundle.js ce-website/public/bundle.js"
  }
}
```

## Recommended Immediate Actions

1. **Delete stale files** (safe, no impact) ✅ DONE
2. **Fix bundle path** (use `../docs/bundle.js` relative to package) ✅ DONE
3. **Fix broken symlink** ✅ DONE
4. **Update MiniNotation.purs** to use psd3-tidal ✅ DONE
5. **Fix generate-snippets script name** ✅ DONE

## Module Naming Conflicts

**Problem**: Running bare `spago build` fails because multiple packages have the same module names:

| Module | Defined In |
|--------|------------|
| `Main` | ce-website, ce2-website |
| `Types` | ce-website, ce2-website |
| `Data.Loader` | ce-website, ce2-website |
| `Data.ColorPalette` | ce-website, ce2-website |
| `Test.Main` | Multiple test directories |

**Current Workaround**: `npm run build` now builds `demo-website` specifically instead of all packages.

**Proper Fix** (future): Rename modules to be package-prefixed:
- `ce-website/Main` → `CodeExplorer.Main`
- `ce-website/Types` → `CodeExplorer.Types`
- etc.

**Note**: `demo-website` uses `PSD3.Main` and `ce-server` uses `Server.Main`, so these don't conflict.

## Updated npm Scripts

```bash
npm run build         # Generate snippets + build demo-website
npm run build:libs    # Build core libraries only
npm run build:all-libs # Build all library packages
npm run build:demo    # Build demo-website
npm run build:ce      # Build ce-website (separate)
npm run build:tidal   # Build psd3-tidal

npm run bundle        # Bundle demo-website → docs/bundle.js
npm run dev           # Build + bundle demo-website
npm run serve         # Serve docs/ on port 1234
```
