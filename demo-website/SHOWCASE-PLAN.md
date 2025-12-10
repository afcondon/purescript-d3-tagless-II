# Demo Website Showcase Examples Plan

Showcase examples to demonstrate PSD3's capabilities.

---

## Current Status

| Example | Status | Notes |
|---------|--------|-------|
| Code Explorer | In Progress | Neighborhood view, triptych, search working |
| Simpson's Paradox | In Progress | Force viz with multi-phase animation done, needs polish |
| WealthHealth | In Progress | Gapminder-style, partially complete |
| Force Playground | ✅ Done | Fullscreen showcase with floating control panel |
| Brushable SPLOM | ✅ Done | Palmer Penguins with d3-brush cross-filtering |
| Wizard (v2) | Planned | This document |
| Interactive Tree Builder | ✅ Done | CodePen-style 4-column layout, Stringify interpreter |

---

## 1. Force Simulation Playground

**Goal**: An interactive, app-like playground for exploring force simulations. Builds on LesMisGUP.

### Features

#### A. Force Control Panel
Interactive controls to manipulate simulation forces in real-time:

- **ManyBody Force**
  - Toggle on/off
  - Strength slider (-300 to 0)
  - Distance min/max sliders

- **Link Force**
  - Toggle on/off
  - Distance slider
  - Strength slider
  - Iterations slider

- **Collision Force**
  - Toggle on/off
  - Radius slider
  - Strength slider

- **Center vs Position Forces** (key insight from disjoint graph example)
  - Radio toggle: Center force vs ForceX + ForceY
  - When using position forces, disconnected components stay in view
  - Strength sliders for each

- **Simulation Controls**
  - Alpha/temperature display
  - Reheat button
  - Stop/start toggle
  - Alpha decay slider
  - Velocity decay slider

#### B. Node Management (existing from LesMisGUP)
- Add N random nodes
- Remove N random nodes
- Reset to full graph

#### C. Link Management (NEW)
- Remove N random links
- Remove all links from selected node
- "Disconnect subgraph" - remove links to create isolated components
- Add back removed links
- Visual: demonstrate disjoint graph behavior (nodes without links drift vs stay)

#### D. Removed Node Behavior (NEW)
Instead of fading out, removed nodes:
- Animate to a "parking lot" cluster (bottom or side of viewport)
- Use ForceX/ForceY to pull them to parking position
- Remain visible but dimmed
- Can be re-added from the parking lot

#### E. Educational Overlay (Stretch Goal)
Interactive explanation of what's happening under the hood:
- "Show node data" toggle - display id, x, y, vx, vy on hover
- "Show swizzling" - highlight how links reference nodes after simulation processes them
- "Show force vectors" - visualize force contributions per node
- Focus on PSD3 user model, not D3 internals

### Architecture

```
demo-website/src/Viz/ForcePlayground/
├── App.purs           -- Halogen component, central state
├── Controls.purs      -- Force control panel UI
├── Simulation.purs    -- Simulation setup and management
├── NodeManager.purs   -- Node add/remove/parking logic
├── LinkManager.purs   -- Link add/remove logic
├── Render.purs        -- Tree-based rendering
└── Types.purs         -- State, config types
```

### Key Technical Challenges

1. **Dynamic force configuration** - Need to update force parameters without recreating
2. **Parking lot animation** - Use separate ForceY target for "parked" nodes
3. **Link removal UI** - Need to track original vs current links
4. **Disjoint demo** - Show difference between Center and ForceX/ForceY

---

## 2. Brushable Scatterplot Matrix (SPLOM)

**Goal**: Implement d3-brush bindings and demonstrate cross-filtering across a scatterplot matrix.

### What is d3-brush?

D3's brush module enables rectangular region selection:
- `d3.brush()` - 2D brush (x and y)
- `d3.brushX()` - 1D horizontal brush
- `d3.brushY()` - 1D vertical brush

Brush events:
- `start` - brush interaction begins
- `brush` - brush is moving
- `end` - brush interaction ends

Selection data:
- `event.selection` - [[x0, y0], [x1, y1]] for 2D brush
- Can be null if brush is cleared

### Features

#### A. Scatterplot Matrix Grid
- N×N grid of scatterplots for N numeric dimensions
- Each cell shows dimension[col] vs dimension[row]
- Diagonal shows dimension labels (or histograms)
- Shared color scale by category (e.g., species for iris/penguins data)

#### B. Brush Interaction
- Click and drag to create brush selection in any cell
- Selected region filters data points
- Non-selected points dimmed across ALL cells (cross-filtering)
- Clicking new cell clears old brush, starts new one
- Clear brush by clicking outside selection

#### C. Datasets
- Iris dataset (classic, small)
- Penguins dataset (like Observable example)
- Option to load custom CSV?

### Library Work: psd3-brush

New package or module in psd3-selection:

```purescript
-- | D3 Brush bindings for PSD3
module PSD3.Brush
  ( Brush
  , BrushSelection
  , BrushEvent
  , brush       -- 2D brush
  , brushX     -- horizontal only
  , brushY     -- vertical only
  , brushExtent -- set extent [[x0,y0], [x1,y1]]
  , brushOn    -- attach event handlers
  , brushMove  -- programmatically move brush
  , brushClear -- clear brush selection
  ) where

-- Types
type BrushSelection = Maybe { x0 :: Number, y0 :: Number, x1 :: Number, y1 :: Number }

type BrushEvent d =
  { selection :: BrushSelection
  , sourceEvent :: Maybe Event
  , target :: Brush
  , mode :: String  -- "drag", "handle", "center", etc.
  }

-- As a Behavior for integration with selection API
data Behavior d
  = ...existing...
  | Brush (BrushConfig d)

type BrushConfig d =
  { extent :: Maybe Extent  -- brush area bounds
  , onStart :: Maybe (BrushEvent d -> Effect Unit)
  , onBrush :: Maybe (BrushEvent d -> Effect Unit)
  , onEnd :: Maybe (BrushEvent d -> Effect Unit)
  }
```

### Architecture

```
psd3-selection/src/PSD3/Brush/
├── Brush.purs         -- Main brush API
├── Brush.js           -- FFI to d3-brush
└── Types.purs         -- BrushSelection, BrushEvent types

demo-website/src/Viz/SPLOM/
├── App.purs           -- Halogen component
├── Matrix.purs        -- Grid layout calculations
├── Cell.purs          -- Individual scatterplot cell
├── Brush.purs         -- Brush interaction handling
├── Data.purs          -- Dataset loading/parsing
└── Types.purs         -- State types
```

### Key Technical Challenges

1. **Brush FFI** - Need clean PureScript wrapper around d3.brush()
2. **Cross-cell coordination** - Brush in one cell affects all cells
3. **Efficient re-render** - Only update opacity, not positions
4. **Scale coordination** - Each cell has its own x/y scales

---

## Implementation Order

### Phase 1: Force Playground MVP
1. Copy LesMisGUP as starting point
2. Add force control panel with toggles and sliders
3. Add link management (remove/add)
4. Demo disjoint behavior with ForceX/ForceY toggle

### Phase 2: Force Playground Polish
1. Parking lot for removed nodes
2. Better visual design
3. Educational overlays (stretch)

### Phase 3: Brush Library
1. Create psd3-brush module with FFI
2. Basic brush behavior integration
3. Test with simple single-brush example

### Phase 4: SPLOM
1. Matrix layout and cell rendering
2. Integrate brush per cell
3. Cross-filtering logic
4. Polish and datasets

---

## 3. Wizard v2 (Skeleton Generator)

**Goal**: Rewrite the v1 wizard to generate select example skeletons for common visualization patterns.

### Background

The v1 wizard attempted to be comprehensive but became unwieldy. v2 focuses on a curated set of important, well-documented starting points.

### Target Skeletons

1. **Basic Force Graph** - Nodes and links with standard forces
2. **Hierarchical Tree/Pack** - Tree layout or circle packing
3. **Scatterplot with Axes** - Linear scales, axes, data binding
4. **Bar Chart** - Categorical data, ordinal scale
5. **Line/Area Chart** - Time series or continuous data
6. **Geographic Map** - GeoJSON projection (if geo support exists)

### Features

- Step-by-step wizard UI
- Preview of what you'll get
- Copy-to-clipboard generated code
- Brief explanation of each skeleton's key patterns
- Links to relevant documentation

### Architecture

```
demo-website/src/Wizard/
├── App.purs           -- Wizard Halogen component
├── Skeletons.purs     -- Template definitions
├── Preview.purs       -- Live preview rendering
└── Types.purs         -- Wizard state, skeleton configs
```

### Key Decisions

- **No compilation** - Generated code is plain text for copy/paste
- **Minimal customization** - Pick a skeleton, get working code
- **Self-documenting** - Generated code has explanatory comments
- **Reuse Stringify** - The Tree Builder's `Stringify.purs` interpreter already generates
  compilable PureScript Tree API code. Could potentially reuse/adapt it for skeleton generation.

---

## 4. Interactive Tree Builder

**Goal**: Visual, interactive tool to build Tree API structures and see them render with sample data. Demonstrates the meta-tree/interpreter architecture.

### Concept

User builds a visualization structure graphically:
1. Start with a root element (SVG, Group)
2. Add child elements (Circle, Rect, Text, Line, etc.)
3. For each element, select from pre-canned attributes
4. Pipe sample data through and see result

### Why Pre-canned Attributes?

Avoids needing to compile PureScript at runtime. Instead:
- Predefined attribute functions: `_.x`, `_.y`, `_.radius`, `_.color`, `_.label`
- Sample data with known fields: `{ x: Number, y: Number, radius: Number, color: String, label: String }`
- Dropdown selection rather than code editing

### UI Design

```
┌─────────────────────────────────────────────────────────────┐
│  Tree Structure          │  Preview                        │
│  ─────────────────       │  ────────                       │
│  ▼ SVG                   │  ┌─────────────────────────┐    │
│    ▼ Group               │  │                         │    │
│      ● Circle            │  │   ●  Hello              │    │
│        cx: _.x           │  │      ●  World           │    │
│        cy: _.y           │  │                         │    │
│        r: _.radius       │  │                         │    │
│      ■ Text              │  └─────────────────────────┘    │
│        x: _.x            │                                 │
│        y: _.y            │  Sample Data:                   │
│        text: _.label     │  [{ x: 50, y: 50, radius: 10,  │
│                          │     color: "red", label: "Hi"}] │
│  [+ Add Element]         │                                 │
└─────────────────────────────────────────────────────────────┘
```

### Features

- **Drag-and-drop** element ordering
- **Attribute picker** with type-appropriate options
- **Live preview** updates as you build
- **Data editor** - modify sample data array
- **Export** - generate equivalent PureScript code
- **Load examples** - pre-built trees to explore

### Pre-canned Attribute Library

```purescript
-- Position attributes
attrX_x, attrX_y, attrX_index, attrX_constant :: Number -> ...

-- Size attributes
attrRadius_radius, attrRadius_constant :: ...
attrWidth_width, attrWidth_constant :: ...

-- Color attributes
attrFill_color, attrFill_constant :: ...
attrStroke_color, attrStroke_constant :: ...

-- Text attributes
attrText_label, attrText_name :: ...
```

### Sample Data Schema

```purescript
type SampleDatum =
  { x :: Number
  , y :: Number
  , radius :: Number
  , width :: Number
  , height :: Number
  , color :: String
  , label :: String
  , name :: String
  , value :: Number
  , index :: Int
  }
```

### Architecture

```
demo-website/src/TreeBuilder/
├── App.purs           -- Main Halogen component
├── TreeEditor.purs    -- Tree structure editing UI
├── AttributePicker.purs -- Attribute selection dropdowns
├── DataEditor.purs    -- Sample data editor
├── Preview.purs       -- Live rendering
├── Interpreter.purs   -- Meta-interpreter that runs the tree
├── Export.purs        -- Generate PureScript code
├── Presets.purs       -- Pre-built example trees
└── Types.purs         -- BuilderTree, AttributeChoice types
```

### Key Technical Challenges

1. **Meta-interpretation** - Build Tree structure from UI state, interpret it
2. **Type erasure** - Need to handle attributes uniformly despite different types
3. **Live updates** - Efficient re-render on every change
4. **Code generation** - Produce readable, idiomatic PureScript

### Educational Value

- Shows Tree API is just data - can be built programmatically
- Demonstrates interpreter pattern - same tree, different interpretations
- Gentle introduction before writing code directly

---

## Implementation Order (Updated)

### Phase 1: Force Playground MVP
1. Copy LesMisGUP as starting point
2. Add force control panel with toggles and sliders
3. Add link management (remove/add)
4. Demo disjoint behavior with ForceX/ForceY toggle

### Phase 2: Force Playground Polish
1. Parking lot for removed nodes
2. Better visual design
3. Educational overlays (stretch)

### Phase 3: Brush Library
1. Create psd3-brush module with FFI
2. Basic brush behavior integration
3. Test with simple single-brush example

### Phase 4: SPLOM
1. Matrix layout and cell rendering
2. Integrate brush per cell
3. Cross-filtering logic
4. Polish and datasets

### Phase 5: Wizard v2
1. Define skeleton templates
2. Wizard UI flow
3. Code generation and preview
4. **Note**: Consider reusing Tree Builder's Stringify interpreter for code generation

### Phase 6: Interactive Tree Builder
1. Tree editor UI with element palette
2. Attribute picker with pre-canned options
3. Meta-interpreter for live preview
4. Code export

### Ongoing: Polish Existing
- Simpson's Paradox - finish remaining charts, styling
- WealthHealth - complete implementation
- Code Explorer - refinements per REFINEMENTS-PLAN.md

---

## References

- Disjoint Force Graph: https://observablehq.com/@d3/disjoint-force-directed-graph/2
- Brushable SPLOM: https://observablehq.com/@d3/brushable-scatterplot-matrix
- d3-brush docs: https://github.com/d3/d3-brush
- Penguins dataset: https://github.com/allisonhorst/palmerpenguins
- v1 Wizard: (internal reference)
