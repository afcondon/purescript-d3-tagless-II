# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a PureScript library implementing a Finally Tagless embedded DSL for building interactive data visualizations. The library wraps D3.js functionality in a purely functional, composable API with strong type safety. The project targets PureScript 0.15.

## Build Commands

- **Install dependencies**: `yarn install` or `npm install`
- **Build PureScript**: `npm run build`
- **Bundle application**: `npm run bundle` (outputs to `docs/bundle.js`)
- **Upgrade package set**: `npm run upgrade-set`
- **Extract code snippets**: `npm run snippets`

The website/demo is served from the `docs/` directory (GitHub Pages hosting).

**Important**: This repo uses Spago 0.93 (via npm), NOT the system spago (0.21). Always use `npm run build`, not `spago build` directly. Configuration is in `spago.yaml`, not `spago.dhall`.

## Architecture

### Finally Tagless Pattern

The library uses a Finally Tagless encoding that allows multiple interpreters for the same DSL. The core type classes define capabilities without tying them to specific implementations:

- **`SelectionM` capability**: Defines operations for DOM manipulation via selections (appending, selecting, joining data to DOM elements)
- **`SimulationM` capability**: Extends SelectionM with physics simulation capabilities for force-directed graphs

### Interpreters

Three interpreters demonstrate the pattern:

1. **D3 Interpreter** (src/lib/Interpreters/D3/): Primary interpreter using D3.js via FFI
2. **MetaTree Interpreter** (src/lib/Interpreters/MetaTree/): Generates visualizations of the DSL syntax tree itself
3. **String Interpreter** (src/lib/Interpreters/String/): Generates code or documentation from visualization definitions

### Core Library Modules (src/lib/)

**Data Layer** (src/lib/D3/Data/):
- `Tree.purs`, `Graph.purs`, `Node.purs`: Type-safe data structures for visualizations
- Separate from visualization logic to enable clear separation of concerns

**Selection API** (src/lib/D3/Selection/):
- `Selection.purs`: Defines `SelectionAttribute` ADT and behaviors (drag, zoom)
- `Functions.purs`: FFI wrappers and interpreter implementation for selections

**Simulation API** (src/lib/D3/Layouts/Simulation/):
- `Types.purs`: Simulation state, forces, and configuration
- `Forces.purs`: Force constructors (manybody, center, collide, link, radial)
- `Functions.purs`: Implementation of simulation operations
- Manages stateful D3 simulation engine with typed PureScript interface

**FFI Layer** (src/lib/D3/FFI/):
- Low-level JavaScript interop
- Keeps state management and side effects isolated to FFI boundary

### Website/Demo Application (src/website/)

The website demonstrates library usage with multiple examples:

**Structure**:
- `Main.purs`, `Router.purs`, `Types.purs`: Top-level Halogen application
- **Component/**: Reusable Halogen components
  - Complex components have subdirectories (e.g., Component/Spago/, Component/ForceNavigator/)
  - Simpler components are single files (e.g., Button.purs, Checkbox.purs)
- **HTML/**: Non-Halogen reusable HTML chunks
- **Viz/**: D3 visualization implementations organized by example
  - Each visualization has Model, Draw, Unsafe, and often Attributes modules
  - Examples: Spago (force-directed graph), LesMis, Trees, Sankey, BubbleChart, etc.

**Key Examples**:
- **Spago** (Component/Spago/ + Viz/Spago/): The flagship example showing the MiseEnScene pattern for complex force simulations with multiple scenes/views
- Simple selections and attributes (ThreeLittleCircles)
- General Update Pattern with transitions (GUP)
- D3 hierarchy layouts (Trees, MetaTree)
- Force-directed graphs (LesMis)
- Interactive behaviors (dragging, zooming, panning)

## Key Design Decisions

**State Isolation**: D3's inherent statefulness is isolated to `Selection` and `Simulation` monads. This is a pragmatic compromise - explicit state modeling via State monads was attempted but sacrificed readability without sufficient benefit.

**Partial D3 Coverage**: Only D3 APIs that benefit from idiomatic PureScript wrapping are exposed. Many D3 functions that are already functional in style are accessed directly via simple FFI wrappers as needed.

**Performance**: Uses D3 for attribute assignment to DOM elements rather than pure PureScript implementations, prioritizing readability and leveraging D3's battle-tested optimizations.

**Type Safety**: Uses `Datum_` and `Index_` as opaque types for D3 data, with typed wrappers (`D3_SimulationNode`, `D3Link`, `D3LinkSwizzled`) providing safety at usage sites.

**MiseEnScene Pattern** (demonstrated in Spago example): A configuration record pattern for complex force simulations that packages together data filters, force settings, visual styling, initialization functions, and event callbacks. This enables declarative scene switching for multiple views of the same data.

## Development Notes

- Configuration is in `spago.yaml` (Spago 0.93) with dependencies for Halogen, lenses, graphs, and web APIs
- JavaScript dependencies (D3 v7, d3-color, d3-interpolate, d3-scale-chromatic) are managed via npm/yarn
- Tests are in `test/` but test infrastructure is minimal - examples serve as integration tests
- See `notes/Project Coding Standards.md` and `notes/Project Documentation Standards.md` for detailed guidelines

## Coding Conventions (Summary)

- **Lenses**: Prefix with `_` (e.g., `_chooseNodes`, `_simulation`)
- **Foreign functions and types**: Postfix with `_` (e.g., `Datum_`, `d3SelectFirstInDOM_`)
- **datum_ pattern**: Record of accessor functions for safely extracting data from D3's opaque `Datum_` type
- **FFI consolidation**: All D3.js FFI should live in src/lib/D3/FFI.purs/.js
- **Debug statements**: Use purescript-debug library's `spy` function (generates warnings to prevent shipping debug code)
