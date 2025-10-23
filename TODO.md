# TODO - Organized by Pages/Work Packages

This TODO is derived from `notes/VISION.md` and organized by the major pages and features that will comprise the finished demo site.

---

## 1. About/Tutorial Page (Enhanced)

**Goal:** Progressive learning from simplest examples to more complex patterns, embedded directly in explanatory text.

### Examples to include:
- [ ] **Three Little Circles** - Move from current Stories to About page
- [ ] **Small parabola of circles** - Create simple parametric example
- [ ] **General Update Pattern** - Fix (currently broken, stops after three updates) and move to About page
- [ ] **Simple line chart** - Single line, basic example
- [ ] **Simple bar chart** - Fix first bar offset issue
- [ ] **Anscombe's Quartet** - Keep as scatter plot example (remove basic scatter)

### Requirements:
- [ ] Each example shows code snippet inline with rendered viz
- [ ] Progressive complexity: start with Three Little Circles, build up
- [ ] Good responsive CSS for readability across screen sizes
- [ ] Consider adding collapsible code sections with syntax highlighting

---

## 2. Simple Charts Page

**Goal:** Polished, production-ready examples of common chart types with interesting data.

### Line Chart
- [ ] Multi-line chart with interesting dataset (suggestions: temperature trends, stock prices, COVID data)
- [ ] Hover interaction to highlight individual lines
- [ ] Legend with hover coordination
- [ ] Proper axes and labels

### Bar Chart
- [ ] Fix first bar offset issue
- [ ] Find interesting dataset (suggestions: top programming languages by year, renewable energy adoption, GitHub stars)
- [ ] Add stacked bar variant or grouped bar variant
- [ ] Smooth transitions on data updates

### Anscombe's Quartet
- [ ] Keep current implementation (good scatter plot example)
- [ ] Minor polish: ensure all four charts are clearly labeled
- [ ] Show summary statistics alongside charts

---

## 3. Chord Diagram Page

- [ ] Add labels to arcs and chords
- [ ] Source interesting dataset (not just dependencies - maybe trade flows, migration patterns, or music genre relationships)
- [ ] Respect container height to keep all elements visible
- [ ] Hover interactions: highlight connected arcs
- [ ] Color scheme that's accessible and attractive

---

## 4. Bubble Chart Page

- [ ] Find our own interesting dataset (not Flare)
- [ ] Implement click-to-zoom interactivity: click inner bubble to drill down, click outer to go back up
- [ ] Fix labeling: drop tiny labels, hide labels that don't fit in circles
- [ ] Respect container height to keep visible
- [ ] **Stretch goal:** Live transition to tree, treemap, or graph layout

---

## 5. Hierarchical Data Displays Page

**Goal:** Single comprehensive page showing the same real-world dataset (our codebase, like Mike Bostock's Flare) in multiple hierarchy layouts.

### Layouts to include:
- [ ] Horizontal tree (needs centering, zoom)
- [ ] Vertical tree (needs centering, zoom, 45° rotated labels for readability)
- [ ] Radial tree (needs centering, zoom, click-to-rotate OR auto-rotate clicked label to 3 o'clock)
- [ ] Circle packing
- [ ] Treemap (new - needs implementation)

### Features:
- [ ] Control panel to switch between layouts
- [ ] Code overlay panels showing implementation
- [ ] Use our own codebase as data source (following Bostock's Flare example)
- [ ] **Stretch goal:** Animate transitions between layouts (retain labels, move nodes smoothly)
- [ ] **Stretch goal - FINAL BOSS:** Animate tree into force layout graph

### Debug:
- [ ] Fix the three-layout switcher example (currently not working)

---

## 6. Sankey Diagram Page

- [ ] Add margins for proper spacing
- [ ] Implement hover highlighting for flow paths
- [ ] Find more up-to-date energy flow data
- [ ] **Feature:** Add color toggle to highlight "primary energy fallacy"
  - Show path to lost energy inherent in fossil fuel lines
  - Demonstrate value of interactive climate advocacy visualization
- [ ] Ensure labels are readable
- [ ] Smooth transitions when changing views

---

## 7. Interpreters Page (formerly "String Interpreter")

**Goal:** Demonstrate Finally Tagless pattern by showing multiple interpretations of the same visualization code.

### Four-panel display showing one piece of PureScript code interpreted as:
1. [ ] **English description** - Succinct paragraph describing how the visualization is built
2. [ ] **D3.js JavaScript code** - Pseudo-code or actual D3.js equivalent
3. [ ] **Vega-Lite JSON specification** - Same viz in declarative format
4. [ ] **AST visualization as tree** - Show the visualization code's syntax tree

### Additional content:
- [ ] Diagram explaining Finally Tagless: PureScript code → multiple interpreters → different outputs
- [ ] EBNF-style grammar documentation for SelectionM
- [ ] **Stretch goal:** Side-by-side live code generation as you modify parameters

### Implementation notes:
- [ ] Overhaul string interpreter output for clarity (newlines, indentation)
- [ ] Create Vega-Lite interpreter (new)
- [ ] Create English-language interpreter (new)
- [ ] Create AST-to-tree-data interpreter (new)

---

## 8. Les Mis (Force Layout Example)

**Status:** Currently broken, needs rewrite with new SimulationM API

- [ ] Rewrite using current SimulationM API
- [ ] Ensure drag interactions work smoothly
- [ ] Add hover highlighting for connected nodes
- [ ] Color by community detection or node degree
- [ ] Control panel for force parameters
- [ ] Demonstrate the MiseEnScene pattern if applicable

---

## 9. Code Explorer (formerly "Spago")

**Status:** Semi-standalone app, currently working but needs enhancements

### Current repo goals (visualization and filtering):
- [ ] Rename from "Spago" to "Code Explorer" throughout
- [ ] Add module cohesion metrics visualization
- [ ] Add package coupling visualization
- [ ] Highlight "candidate for extraction" functions (functions that might belong elsewhere)
- [ ] Demonstrate at least one concrete insight about the codebase (TBD which insight)
- [ ] Show multiple interaction patterns: filter, zoom, pan, select, highlight
- [ ] **"Blueprint" CSS style** - Add architectural drawing aesthetic as neutral/default view when not highlighting anything particular

### Long-term vision (for standalone app):
- Database backend with comprehensive dependency analysis
- Every line-level dependency captured (line X uses function Y from module Z)
- Drag-and-drop refactoring tools
- Node/PureScript or Haskell server
- (Not part of current TODO, documented for future reference)

---

## 10. Meta-Tree Page

**Goal:** Description of interactive AST editor concept (separate repo in future)

### Current repo deliverable:
- [ ] Explanatory page describing the vision
- [ ] Diagram showing the formal grammar developed above D3 (expressed in SelectionM)
- [ ] Note that this is now covered in Interpreters page (AST as tree visualization)

### Long-term vision (documented for reference):
- Interactive tool to create visualization AST graphically
- Visual palette/toolbox UI with LEGO-like pieces
- Target bespoke visualizations (maps, choropleths, custom layouts)
- Backend PureScript compilation (like try.purescript.org)
- (Not part of current TODO, separate repo once library is mature)

---

## 11. Site Infrastructure

### Remove Gallery indirection:
- [ ] Eliminate Gallery routing layer
- [ ] Direct navigation to individual example pages
- [ ] Simplify routing (see also: make routing fully idiomatic using `purescript-routing`)

### CSS and layout:
- [ ] Replace Ocelot-derived components with modern CSS
- [ ] Responsive design for all screen sizes
- [ ] Consider thumbnail navigation for examples
- [ ] Clean up and possibly delete Ocelot-derived code

### Accessibility:
- [ ] Ensure main text appears and is navigable for screen readers
- [ ] Handle no-JS case gracefully
- [ ] Explain what sighted users see/do for non-visual users
- [ ] **Stretch goal:** Sonic "visualizations" or audio descriptions

### Home page overhaul:
- [ ] Key sections: what, why, how, code, examples, apps, Finally Tagless
- [ ] Consider generating from markdown
- [ ] Clean, modern design
- [ ] Clear calls to action (view examples, read docs, see code)

---

## 12. Documentation (Separate from examples)

### Four types of documentation:
1. [ ] **API/Reference** - Complete module and function documentation
2. [ ] **Overview** - Architecture, Finally Tagless pattern, interpreter pattern
3. [ ] **Tutorial** - Progressive learning (covered by About page examples)
4. [ ] **Cookbook** - Common patterns and recipes

### Specific improvements:
- [ ] Document datum_ pattern thoroughly and rationalize across ALL examples
- [ ] Add module-level documentation to all core modules
- [ ] Document capability/interpreter relationship
- [ ] Add inline comments for complex FFI interactions
- [ ] Consider hover-over comments or 'i' icons for code sections

---

## 13. Data and Build Pipeline

### Spago data generation:
- [x] ~~Replace Perl scripts with PureScript~~ (using Node.js scripts now)
- [x] Use `spago graph` commands to extract data
- [ ] Consider additional metrics beyond basic dependencies

### Database backend (future):
- [ ] Set up PostgreSQL/SQLite for example datasets
- [ ] Create HTTP API server
- [ ] Migrate static data files to database
- [ ] Update examples to fetch via `purescript-affjax`

---

## 14. Library API Improvements

### SimulationM Refactoring
**Goal:** Offer two levels of SimulationM API for different use cases

- [ ] **SimpleSimulationM** - Simpler API that doesn't support General Update Pattern complexity
  - Easier to write correctly for straightforward force simulations
  - Good for learning and simple use cases
  - Less ceremony, fewer moving parts

- [ ] **AdvancedSimulationM** (current SimulationM, refactored) - Full-featured API for complex scenarios
  - Support for General Update Pattern with enter/update/exit
  - Parameterize with a configuration record containing data and functions
  - Let SimulationM handle complicated interleaving of operations automatically
  - Goal: Make complex scripts easier to write correctly by moving complexity into the API itself
  - Consider what can be automated vs. what must be user-specified

### Testing
- [ ] **Test suite for D3 API wrapper** - Comprehensive tests for library (future work)
  - Unit tests for core Selection and Simulation operations
  - Integration tests for common patterns
  - Property-based testing where applicable

---

## 15. Stretch Goals / Future Considerations

- [ ] Add e-charts support (existing PureScript wrapper available)
- [ ] Parser to convert markdown to tree data (display as text, tree layout, or force layout)
- [ ] Additional advanced visualizations (parallel coordinates, hexbin, etc.)
- [ ] Explore other interpreters beyond visualization (data validation, accessibility descriptions)

---

## Notes

- Priorities can be adjusted based on what's most impactful for demonstrating the library
- "Stretch goals" within sections can be deferred to later phases
- Focus on polish and education: each example should be both impressive and instructive
