# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a PureScript library implementing a Finally Tagless embedded DSL for building interactive data visualizations. The library wraps D3.js functionality in a purely functional, composable API with strong type safety. The project targets PureScript 0.15.

## Repository Structure

This is a **Spago monorepo** with four packages:

```
├── psd3-selection/       # Core D3 selection/attribute library (publishable)
│   ├── spago.yaml        # Package config only
│   └── src/PSD3/         # Selection, attributes, layouts, interpreters
├── psd3-simulation/      # Force simulation library (publishable)
│   ├── spago.yaml        # Depends on psd3-selection
│   └── src/PSD3/         # ForceEngine, Config
├── psd3-tidal/           # TidalCycles mini-notation parser (publishable)
│   ├── spago.yaml        # Standalone package
│   └── src/Tidal/        # AST, Parser, Pretty-printer
├── demo-website/         # Documentation and examples website
│   ├── spago.yaml        # Depends on all libraries
│   ├── src/              # Halogen components, visualizations
│   └── public/           # Static assets, bundle.js
└── spago.yaml            # Workspace-only config (no package section)
```

## Build Commands

- **Install dependencies**: `npm install`
- **Build all packages**: `npm run build`
- **Build individual packages**:
  - `npm run build:selection`
  - `npm run build:simulation`
  - `npm run build:website`
- **Bundle website**: `npm run bundle` (outputs to `demo-website/public/bundle.js`)
- **Serve website**: `npm run serve` (http://localhost:1234)
- **Dev workflow**: `npm run dev` (build + bundle)

**Important**: This repo uses Spago 0.93 (via npm). Configuration is in `spago.yaml` files (one per package + workspace root).

## Architecture

### Finally Tagless Pattern

The library uses a Finally Tagless encoding that allows multiple interpreters for the same DSL. The core type classes define capabilities without tying them to specific implementations:

- **`SelectionM` capability**: Defines operations for DOM manipulation via selections (appending, selecting, joining data to DOM elements)
- **`SimulationM` capability**: Extends SelectionM with physics simulation capabilities for force-directed graphs

### Interpreters

Three interpreters demonstrate the pattern:

1. **D3 Interpreter** (psd3-selection/src/PSD3v2/Interpreter/D3v2.purs): Primary interpreter using D3.js via FFI
2. **MetaTree Interpreter** (psd3-selection/src/PSD3v2/Interpreter/MetaAST.purs): Generates visualizations of the DSL syntax tree itself
3. **English Interpreter** (psd3-selection/src/PSD3v2/Interpreter/English.purs): Generates human-readable descriptions

### psd3-selection Package

Core library for D3 selections, attributes, and layouts:

**Selection API** (PSD3v2/Selection/):
- `Types.purs`, `Operations.purs`, `Query.purs`, `Join.purs`
- Defines typed selection operations and the General Update Pattern

**Attributes** (PSD3v2/Attribute/, PSD3/Internal/Attributes/):
- Type-safe attribute setters with phantom types

**Layouts** (PSD3/Layout/):
- `Hierarchy/`: Tree, cluster, pack, partition, treemap layouts
- `Sankey/`: Sankey diagram layout

**Data Structures** (Data/, PSD3/Data/):
- `Tree.purs`, `Graph.purs`, `Node.purs`: Type-safe data structures
- Graph algorithms and dependency graph utilities

### psd3-simulation Package

Force-directed graph simulation:

**ForceEngine** (PSD3/ForceEngine/):
- `Core.purs/.js`: D3 force simulation wrapper
- `Simulation.purs`: High-level simulation API
- `Types.purs`: Force and simulation types

**Configuration** (PSD3/Config/):
- `Force.purs`: Immutable force configuration
- `Scene.purs`: Scene definitions
- `Apply.purs`: Apply configs to simulations

### psd3-tidal Package

TidalCycles mini-notation parser for visual pattern editing:

**Core** (Tidal/Core/):
- `Types.purs`: Time (Rational), SourceSpan, Seed, ControlName

**AST** (Tidal/AST/):
- `Types.purs`: TPat data type with 14 constructors (Atom, Silence, Seq, Stack, Fast, Slow, Polyrhythm, Euclid, etc.)
- `Pretty.purs`: Round-trip serialization back to mini-notation strings

**Parser** (Tidal/Parse/):
- `Class.purs`: AtomParseable class with instances for String, Number, Int, Rational
- `Combinators.purs`: Parser implementation using purescript-parsing
- `Parser.purs`: Entry points (parseTPat, parseMini)
- `State.purs`: Parser state for deterministic seed generation

**Usage**:
```purescript
import Tidal.Parse.Parser (parseTPat)
import Tidal.AST.Pretty (pretty)

-- Parse mini-notation
let ast = parseTPat "bd sn [hh hh]*2" :: Either _ (TPat String)

-- Round-trip back to string
let src = pretty <$> ast  -- Right "bd sn [hh hh]*2"
```

See `notes/TIDAL_VISUAL_EDITOR_GUIDE.md` for integration with PSD3 visual editing.

### demo-website Package

Halogen web application demonstrating the libraries:

**Structure** (demo-website/src/):
- `Main.purs`, `RoutingDSL.purs`, `Types.purs`: Top-level application
- **Component/**: Halogen components (CodeExplorerV3, ForceControlPanel, etc.)
- **HTML/**: Reusable HTML helpers
- **Viz/**: D3 visualization implementations (LesMis, Spago, Trees, etc.)

**Key Examples**:
- Code Explorer (Component/CodeExplorerV3.purs): Force-directed module graph
- General Update Pattern with transitions (GUP)
- D3 hierarchy layouts (AnimatedTreeCluster, etc.)
- Interactive behaviors (dragging, zooming, panning)

## Key Design Decisions

**State Isolation**: D3's inherent statefulness is isolated to `Selection` and `Simulation` monads. This is a pragmatic compromise - explicit state modeling via State monads was attempted but sacrificed readability without sufficient benefit.

**Partial D3 Coverage**: Only D3 APIs that benefit from idiomatic PureScript wrapping are exposed. Many D3 functions that are already functional in style are accessed directly via simple FFI wrappers as needed.

**Performance**: Uses D3 for attribute assignment to DOM elements rather than pure PureScript implementations, prioritizing readability and leveraging D3's battle-tested optimizations.

**Type Safety**: Uses `Datum_` and `Index_` as opaque types for D3 data, with typed wrappers (`D3_SimulationNode`, `D3Link`, `D3LinkSwizzled`) providing safety at usage sites.

**MiseEnScene Pattern** (demonstrated in Spago example): A configuration record pattern for complex force simulations that packages together data filters, force settings, visual styling, initialization functions, and event callbacks. This enables declarative scene switching for multiple views of the same data.

## Project Vision and Roadmap

See `notes/VISION.md` for the long-term vision and `TODO.md` for the current roadmap. Key themes:

1. **Enhanced examples**: Polish existing visualizations, fix broken examples, add interactive features
2. **Page consolidation**: Remove Gallery indirection, create focused pages (About/Tutorial, Simple Charts, Hierarchies, Interpreters, Code Explorer)
3. **Multiple interpreters**: Demonstrate Finally Tagless by showing same code interpreted as English, D3.js, Vega-Lite, and AST visualization
4. **Code Explorer**: Evolve the Spago example into a comprehensive code analysis tool (current focus: visualization and insights; future: standalone app with database backend)
5. **Educational focus**: Progressive learning from Three Little Circles to complex force simulations, with embedded code examples

The project aims to be both an impressive demonstration of functional visualization techniques AND an instructional resource for learning PureScript, D3, and Finally Tagless patterns.

## Development Notes

- Configuration is in `spago.yaml` (Spago 0.93) with dependencies for Halogen, lenses, graphs, and web APIs
- JavaScript dependencies (D3 v7, d3-color, d3-interpolate, d3-scale-chromatic) are managed via npm/yarn
- Tests are in `test/` but test infrastructure is minimal - examples serve as integration tests
- See `notes/Project Coding Standards.md` and `notes/Project Documentation Standards.md` for detailed guidelines

## Coding Conventions (Summary)

- **Lenses**: Prefix with `_` (e.g., `_chooseNodes`, `_simulation`)
- **Foreign functions and types**: Postfix with `_` (e.g., `Datum_`, `d3SelectFirstInDOM_`)
- **datum_ pattern**: Record of accessor functions for safely extracting data from D3's opaque `Datum_` type
- **FFI consolidation**: D3.js FFI lives in psd3-selection/src/PSD3/Internal/FFI.purs/.js
- **Debug statements**: Use purescript-debug library's `spy` function (generates warnings to prevent shipping debug code)

## Demo Website Rules

**No JavaScript in demos unless explicitly discussed.** The demo-website exists to showcase the PSD3 library's capabilities, not to build a commercial application. If functionality is missing:

1. **Add it to the library** (psd3-selection or psd3-simulation) with proper PureScript bindings
2. Only use JavaScript FFI within the library packages, not in demo-website
3. Demo code should demonstrate what's possible with the library's PureScript API

This rule ensures demos accurately represent library capabilities and drives library improvements.

## Code Explorer (ce-website) Rules

**The Code Explorer is both a showcase AND a proof-of-concept for the library.** It must adhere to even stricter principles:

1. **Never use FFI workarounds** - If the library doesn't support something, improve the library
2. **Exemplary code clarity** - This code will be read by library users as reference implementation
3. **Principled library usage** - Use the library's intended patterns (e.g., General Update Pattern for DOM updates)
4. **If goals conflict, improve the library** - Code clarity and principled usage must never be at odds; if they are, it signals a library design problem

See `demo-website/src/Viz/GUP.purs` and `demo-website/src/Viz/LesMisGUP.purs` for examples of proper update patterns with motion and transitions.

## Common PureScript Gotchas

- **Math functions**: There is NO `Math` module in PureScript! Use `Data.Number` for floating-point math functions (sqrt, sin, cos, etc.) or `Data.Int` for integer operations. The module `Data.Math` is deprecated and should not be used.
