# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a PureScript library implementing a Finally Tagless embedded DSL for building interactive data visualizations. The library wraps D3.js functionality in a purely functional, composable API with strong type safety. The project targets PureScript 0.15.

## Build Commands

- **Install dependencies**: `yarn install` or `spago install`
- **Build PureScript**: `yarn run build` or `spago build`
- **Bundle application**: `yarn run bundle` (outputs to `docs/bundle.js`)
- **Build CSS**: `yarn run build-css`
- **Upgrade package set**: `yarn run upgrade-set`
- **Extract code snippets**: `yarn run snippets`

The demo application is served from the `docs/` directory (GitHub Pages hosting).

## Architecture

### Finally Tagless Pattern

The library uses a Finally Tagless encoding that allows multiple interpreters for the same DSL. The core type classes define capabilities without tying them to specific implementations:

- **`SelectionM` capability** (src/lib/Interpreters/Capabilities.purs:15-27): Defines operations for DOM manipulation via selections (appending, selecting, joining data to DOM elements)
- **`SimulationM` capability** (src/lib/Interpreters/Capabilities.purs:42-72): Extends SelectionM with physics simulation capabilities for force-directed graphs

### Interpreters

Three interpreters demonstrate the pattern:

1. **D3 Interpreter** (src/lib/Interpreters/D3/): Primary interpreter using D3.js via FFI
2. **MetaTree Interpreter** (src/lib/Interpreters/MetaTree/): Generates visualizations of the DSL syntax tree itself
3. **String Interpreter** (src/lib/Interpreters/String/): Generates code or documentation from visualization definitions

### Core Modules

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

### Demo Application

**Halogen Integration** (src/DemoApp/):
- `Main.purs`: Top-level Halogen application with navigation
- `Stories/`: Example components (ThreeLittleCircles, GUP, Trees, LesMis, Spago)
- `Viz/`: Visualization implementations for each example
- Demonstrates how to integrate visualizations as Halogen components with bidirectional communication

Examples showcase:
- Simple selections and attributes (ThreeLittleCircles)
- General Update Pattern with transitions (GUP)
- D3 hierarchy layouts (Trees, MetaTree)
- Force-directed graphs (LesMis, Spago)
- Interactive behaviors (dragging, zooming, panning)

## Key Design Decisions

**State Isolation**: D3's inherent statefulness is isolated to `Selection` and `Simulation` monads. This is a pragmatic compromise - explicit state modeling via State monads was attempted but sacrificed readability without sufficient benefit.

**Partial D3 Coverage**: Only D3 APIs that benefit from idiomatic PureScript wrapping are exposed. Many D3 functions that are already functional in style are accessed directly via simple FFI wrappers as needed.

**Performance**: Uses D3 for attribute assignment to DOM elements rather than pure PureScript implementations, prioritizing readability and leveraging D3's battle-tested optimizations.

**Type Safety**: Uses `Datum_` and `Index_` as opaque types for D3 data, with typed wrappers (`D3_SimulationNode`, `D3Link`, `D3LinkSwizzled`) providing safety at usage sites.

## Development Notes

- The `packages.dhall` file pins PureScript package versions and includes a custom fork of `html-parser-halogen` (esmodules branch)
- Spago configuration is in `spago.dhall` with dependencies for Halogen, lenses, graphs, and web APIs
- JavaScript dependencies (D3 v7, d3-color, d3-interpolate, d3-scale-chromatic) are managed via npm/yarn
- Tests are in `test/` but test infrastructure is minimal - examples serve as integration tests
