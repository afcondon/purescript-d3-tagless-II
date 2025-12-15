# PSD3 Documentation Master Outline

This document outlines the three documentation sections. Each bullet point represents content to be written or verified against existing pages.

---

## 1. Getting Started

**Goal**: Get from zero to rendering in minutes. Progressive complexity with working examples.

### Installation
- Prerequisites (Node.js, PureScript, Spago)
- Installing packages: `spago install psd3-selection psd3-layout psd3-simulation`
- D3.js script tag in HTML
- Project structure recommendations

### First Visualization (Static Chart Track)
- Minimal HTML page with scatter plot
  - Plain HTML + bundled PureScript
  - Tree API basics: `T.named`, `T.joined`, `T.elem`
  - Type-safe attributes: `cx`, `cy`, `r`, `fill`
  - `render "#container" tree`
- Same example as Halogen app
  - Halogen component structure
  - Effect to Aff bridging
  - RefLabel for container element

### First Force Layout (Force Track)
- Minimal HTML page with force graph
  - Node/link data types
  - `D3_SimulationNode` wrapper
  - Basic `Setup.setup` configuration
  - `subscribeToSimulation` for tick events
- Same example as Halogen app
  - State ownership in Halogen
  - Subscription bridge pattern
  - Stateless viz module

### Core Concepts Summary
- Tree API vs PSD3 API - when to use which
- Interpreters overview (D3, Mermaid, English)
- Type-safe attributes by element type
- Data joins - declarative binding of arrays to DOM

---

## 2. HowTo Guides

**Goal**: Task-oriented recipes. "How do I X?" format.

### Core Techniques

#### Transitions
- Declaring transitions on attributes
- Transition timing (duration, delay, easing)
- Coordinating multiple transitions
- GUP (General Update Pattern) with transitions

#### Events
- Click, hover, drag handlers
- D3 event object access
- Halogen action bridging
- Zoom and pan behavior

#### Tooltips
- Basic tooltip on hover
- Positioning relative to cursor
- Styling and content formatting
- Global tooltip state (warning about current impl)

### Data & Scales

#### Loading External Data
- Affjax for JSON/CSV
- Decoding with argonaut
- Error handling patterns
- Data transformation pipelines

#### Creating Axes and Scales
- Scale types: linear, ordinal, time, color
- Axis FFI wrappers
- Tick formatting
- Responsive axis updates

### Layouts

#### Force-Directed Graphs
- Setup API: `Setup.setup`, force configurations
- Force types: link, manyBody, collide, center, positionX/Y
- Dynamic force updates
- Node/link filtering with simulation rebuild
- Halogen-first architecture pattern

#### Hierarchical Data
- Tree and cluster layouts
- Treemap layouts
- Circle packing
- Data transformation to hierarchy

#### TreeAPI Patterns
- Static elements with `T.elem`
- Data-bound elements with `T.joined`
- Nested structures with `T.withChildren`
- Named containers for identity
- Combining static and dynamic content

### Development

#### Debugging Visualizations
- Console logging from Effect
- Mermaid interpreter for structure debugging
- English interpreter for attribute debugging
- Browser DevTools for D3 selections
- Common error patterns and fixes

#### Performance Optimization
- Minimizing DOM updates
- Efficient tick handlers
- CSS transitions vs D3 transitions
- Virtual DOM considerations with Halogen

---

## 3. Understanding (Deep Dives)

**Goal**: Architectural explanations for those who want to understand how/why.

### Architecture Overview
- Three libraries: psd3-selection, psd3-simulation, psd3-layout
- Layering: Halogen → PSD3 → D3.js FFI
- Design philosophy: declarative over imperative

### Phantom Types and Type Safety
- What phantom types are
- Element type phantom parameters
- Attribute constraints via type classes
- Why `cx` compiles for Circle but not Rect
- Trade-offs: safety vs flexibility

### The Interpreter Pattern
- Tree as data structure, not instruction sequence
- Multiple interpretations of same tree
- D3 interpreter: DOM mutation
- Mermaid interpreter: diagram generation
- English interpreter: debugging
- Writing custom interpreters

### Selection and Join Operations
- D3's data join model (enter/update/exit)
- PSD3's type-safe wrapper
- `Selection` type and its parameters
- `sceneNestedJoin` for complex data structures
- When to use Selection API vs Tree API

### Simulation Architecture
- D3 force simulation internals
- `D3_SimulationNode` and mutation
- ForceEngine module structure
- Ref-based state (why it's necessary)
- Tick event subscription model

### Halogen Integration Patterns
- State ownership principles
- Subscription/emitter bridge
- RefLabel for D3 container access
- Action bridging for D3 events
- Aff lifecycle and cleanup

### Unsafe Code Locations
- Reference: See UNSAFE-AUDIT.md
- Why `unsafeCoerce` is used in Operations.purs
- Why `Effect.Ref` is used in ForceEngine
- Safe usage through public API

---

## Project Skeletons

Provide copy-paste starting points:

### Static Chart Skeletons
1. **Minimal HTML** - Single file, scatter plot
2. **Halogen Basic** - Simple component, bar chart
3. **Halogen with Callback** - Subscriber/emitter pattern
4. **Halogen GUP** - General Update Pattern with Aff lifecycle

### Force Layout Skeletons
1. **Minimal HTML** - Single file, basic graph
2. **Halogen Basic** - Simple force simulation component
3. **Halogen GUP** - Full pattern with dynamic updates

---

## Existing Content to Integrate/Update

| Current Location | Target Section | Status |
|------------------|----------------|--------|
| GettingStarted.purs | Getting Started | Review, update code examples |
| HowtoIndex.purs | HowTo index | Good structure, flesh out pages |
| HowtoForceGraphs.purs | HowTo > Force Graphs | Update with Setup API |
| HowtoTreeAPI.purs | HowTo > TreeAPI | Review |
| Understanding/*.purs | Understanding | Review coverage |
| VISUALIZATION_GUIDE.md | Understanding > Architecture | Integrate |
| psd3-simulation-overview.md | Understanding > Simulation | Integrate |
| UNSAFE-AUDIT.md | Understanding > Unsafe Code | Reference |

---

## Writing Guidelines

- Lead with working code, explain after
- Keep code examples minimal but complete
- Link between sections (HowTo → Understanding for "why")
- Include common gotchas and fixes
- Test all code examples compile
