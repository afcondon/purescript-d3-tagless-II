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
Phase 1 Complete! Moving to Phase 2: Polish
- [x] Chord diagram label size (14px, font-weight 500, #e2e8f0 fill)
- [x] Package names in TopoGraph (text labels below circles, cleared on view switch)
