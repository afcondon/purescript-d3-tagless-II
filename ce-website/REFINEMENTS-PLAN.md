# CE-Website Refinements Plan

## Quick Wins

### 1. Chord Diagram Label Size
- Increase font size in `ChordDiagram.js` for arc labels
- Currently too small to read
- Simple CSS/attribute change

### 2. Package Names in TopoGraph
- Add text labels for package nodes in TopoGraph view
- Similar to how module names appear in Force view
- Modify `Explorer.purs` or scene rendering

---

## Medium Effort

### 3. Palette/Key Improvements
Current state: Shows 100 package colors - not useful

**Proposed approach:**
- **Treemap**: "Packages colored, modules white"
- **Tree/Force**: "Modules inherit package color"
- **TopoGraph**: Use color for meaningful data:
  - Topological depth (layers in DAG)
  - Dependency fan-out (how many packages depend on this)
  - Could use git age if available

**Implementation:**
- Update `NarrativePanel.purs` legend rendering
- Simplify to show concept, not all colors
- Add semantic color scales where appropriate

### 4. Treemap Visibility/Sync
Treemap is decorative but could be interactive:

**Features:**
- Hover sync: Highlight treemap rect when hovering package elsewhere
- Click sync: Click treemap rect to focus package
- Better labels: Larger text, show on hover

**Implementation:**
- Add hover listeners to treemap rects
- Broadcast package hover events
- Style updates for visibility

### 5. Tree Animation (Nodes + Links Together)
Current: Links appear, then nodes fly to positions
Proposed: Tree "grows" organically from root

**Implementation:**
- Start all nodes at root position
- Animate nodes AND links together
- Links grow/extend as nodes move outward
- Pure visual polish

---

## Larger Features

### 6. Package Click Behavior
Options considered:
- Package README panel (static)
- **Package neighborhood** (chosen) - symmetric with module neighborhood
- Package detail view

**Implementation:**
- Detect package node clicks in Treemap/TopoGraph
- Filter to modules in package + their external dependencies
- Render as force graph (similar to module neighborhood)
- Add navigation back to overview

### 7. Module Search
Elastic/fuzzy search for modules in panel

**Features:**
- Text input in panel
- Live filter as you type
- Dropdown with matches
- Click to navigate to module neighborhood
- Keyboard navigation

**Implementation:**
- Add search input to NarrativePanel
- Fuzzy matching against module names
- Results dropdown with click handlers
- Integration with existing navigation

### 8. Simultaneous Neighborhood Views
Show all three views side-by-side:
- BubblePack (function detail)
- Chord Diagram (connection patterns)
- Adjacency Matrix (precise relationships)

**Features:**
- Triptych layout
- Synchronized hover - highlight in all three
- Shared legend/controls

**Implementation:**
- New layout mode in Explorer
- Shared hover state
- Coordinated rendering
- May need viewport/sizing adjustments

---

## Implementation Order

### Phase 1: Quick Wins
1. Chord diagram labels
2. Package names in TopoGraph

### Phase 2: Polish
3. Palette/key improvements
4. Treemap sync/visibility
5. Tree animation

### Phase 3: Features
6. Package click → neighborhood
7. Module search
8. Simultaneous views (if time permits)

---

## Current Task
Phase 2 complete!
- [x] Chord diagram label size (14px, font-weight 500, #e2e8f0 fill)
- [x] Package names in TopoGraph (text labels below circles, cleared on view switch)
- [x] Palette/key improvements (view-specific legends: Treemap shows packages+modules, Tree/Force shows inheritance, Topo shows dependency order)
- [x] Treemap hover sync (watermark rects have transparent fill for hover detection, show 15% white fill on hover)
- [x] Tree animation (nodes + links together - grow from root with synchronized bezier curve links)

Phase 3 in progress:
- [x] Package click → neighborhood (shows packages in adjacent topo layers)
  - TODO: Add labels to package nodes
  - TODO: Order packages by layer (layered layout)
  - TODO: Highlight clicked package
  - TODO: Size nodes by module count
- [x] Module search (fuzzy substring matching, click to navigate)
  - [x] Arrow key navigation in dropdown (ArrowUp/Down to navigate, Enter to select, Escape to close)
- [ ] Simultaneous views

---

## Future Ideas

### 10. Keyboard Shortcuts
Global keyboard shortcuts for power-user navigation:

**Module-on-hover shortcuts:**
- **B** while hovering module → open BubblePack view
- **C** while hovering module → open Chord diagram
- **M** while hovering module → open Matrix view

**Global shortcuts:**
- **Escape** → go back to previous view
- **1/2/3/4** → switch between the four overview views
- **/** or **Cmd+K** → focus the search box

**Implementation:**
- Attach `onKeyDown` to SVG container or document
- Track hovered module in state
- Pattern match on `KE.key` for key names
- Consider modifier keys for advanced users (Shift, Cmd/Ctrl)

### 9. Sankey TopoGraph
A new overview view showing module flow through the dependency graph as a Sankey diagram.

**Concept:**
- Packages as vertical bands/nodes arranged by topological layer
- Module imports as flows between packages
- Flow thickness proportional to number of module-level imports
- Could distinguish used vs unused modules with color/opacity
- Shows how dependencies "flow" from leaf packages up through Main

**Features:**
- Visual representation of coupling strength between packages
- Identify bottleneck packages (lots of flow through)
- See which packages are heavily interconnected
- Could filter to show only used modules or all modules

**Implementation:**
- Use d3-sankey layout
- Build link data from module import relationships
- Aggregate by package for cleaner visualization
- May need to handle cycles (could use dashed lines or separate treatment)
