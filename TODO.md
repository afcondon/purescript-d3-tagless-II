'''# TODO - Organized by Pages/Work Packages

This TODO is derived from `notes/VISION.md` and organized by the major pages and features that will comprise the finished demo site.

---

## 1. About/Tutorial Page (Enhanced)

**Goal:** Progressive learning from simplest examples to more complex patterns, embedded directly in explanatory text.

### Examples to include:
- [x] **Three Little Circles** - Move from current Stories to About page
- [x] **Small parabola of circles** - Create simple parametric example
- [x] **General Update Pattern** - Fix (currently broken, stops after three updates) and move to About page
- [x] **Simple line chart** - Single line, basic example
- [x] **Simple bar chart** - Fix first bar offset issue
- [x] **Anscombe's Quartet** - Keep as scatter plot example (remove basic scatter)

### Requirements:
- [x] Each example shows code snippet inline with rendered viz
- [x] Progressive complexity: start with Three Little Circles, build up
- [ ] Good responsive CSS for readability across screen sizes
- [x] Consider adding collapsible code sections with syntax highlighting

---

## 2. Simple Charts Page

**Goal:** Polished, production-ready examples of common chart types with interesting data.

### Line Chart
- [x] Multi-line chart with interesting dataset (suggestions: temperature trends, stock prices, COVID data)
- [x] Hover interaction to highlight individual lines
- [x] Legend with hover coordination
- [x] Proper axes and labels

### Bar Chart
- [x] Fix first bar offset issue
- [x] Find interesting dataset (suggestions: top programming languages by year, renewable energy adoption, GitHub stars)
- [x] Add stacked bar variant or grouped bar variant

### Anscombe's Quartet
- [x] Keep current implementation (good scatter plot example)
- [x] Minor polish: ensure all four charts are clearly labeled
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

- [x] Rewrite using current SimulationM API
- [x] Ensure drag interactions work smoothly
- [x] Add hover highlighting for connected nodes
- [x] Color by community detection or node degree

---

## 9. Code Atlas (formerly "Code Explorer"/"Spago")

**Status:** Working with multiple visualization views and interactive features

### Completed:
- [x] Rename from "Spago" to "Code Atlas" throughout
- [x] Declarations tab with filtering by module, type, and source
- [x] Module graph visualization (force-directed layout)
- [x] Interactive graph with drag and zoom
- [x] Expandable bubbles view with nested module/declaration structure
- [x] Spotlight mode for focusing on module dependencies
- [x] Context menu with keyboard shortcuts
- [x] Floating panels for controls, legend, and details
- [x] Multiple interaction patterns: filter, drag, spotlight, context menu

### Current repo goals (enhancements):
- [ ] Add module cohesion metrics visualization
- [ ] Add package coupling visualization
- [ ] Evolve "code-explorer" into package set explorer
- [ ] Evolve "code-atlas" into project explorer
- [ ] Mock up an AWS or Kubernetes explorer
- [ ] Highlight "candidate for extraction" functions (functions that might belong elsewhere)
- [ ] Demonstrate at least one concrete insight about the codebase (TBD which insight)
- [ ] **"Blueprint" CSS style** - Add architectural drawing aesthetic as neutral/default view when not highlighting anything particular

### Long-term vision (for standalone app):
- Database backend with comprehensive dependency analysis
- Every line-level dependency captured (line X uses function Y from module Z)
- Drag-and-drop refactoring tools
- Node/PureScript or Haskell server
- (Not part of current TODO, documented for future reference)

---

## 10. Understanding Section (Documentation Pages)

**Status:** Core pages created with diagrams and explanations

### Completed:
- [x] Create Understanding section with tabbed navigation
- [x] Concepts page with core architectural concepts
  - [x] Finally Tagless architecture diagram
  - [x] Type-safe attribute system explanation
  - [x] SelectionM monad overview
  - [x] Capabilities & Interpreters explanation
- [x] Patterns page with practical patterns
  - [x] datum_/Datum_ pattern explanation with diagram
  - [x] Placeholder sections for additional patterns
- [x] Philosophy page (former About page)
- [x] Mermaid diagram integration for visual explanations
- [x] TOC panels with bookmark imagery
- [x] Section navigation (RHS panel)

### TODO:
- [ ] Complete "Grammar of D3 in SelectionM" section in Patterns page
- [ ] Complete "From DOM to Visualization Elements" section in Patterns page
- [ ] Add more practical examples to Patterns page
- [ ] Create simpler, more beginner-friendly diagrams for:
  - [ ] SelectionM grammar flow (replace removed complex state machine)
  - [ ] General Update Pattern (replace removed complex state machine)
- [ ] Add interactive code examples to Understanding pages
- [ ] Link Understanding pages to relevant Tutorial and How-to examples

---

## 11. API Reference Section

**Status:** Basic structure created with module browser

### Completed:
- [x] Create Reference section with Pursuit-style module browser
- [x] Generate individual pages for all 30 PSD3 library modules
- [x] Implement source code display with syntax highlighting
- [x] Module categories in RHS navigation
- [x] Shared PrismJS module for FFI
- [x] Source file loading system

### TODO:
- [ ] Add detailed explanations/prose for each module
- [ ] Add usage examples for key functions
- [ ] Add diagrams showing relationships between modules
- [ ] Add "See Also" links between related modules
- [ ] Generate API documentation from source code comments
- [ ] Add search functionality across all modules

---

## 12. Meta-Tree Page

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

## 13. Site Infrastructure

### Routing and Navigation:
- [x] Section-based routing (Tutorial, How-to, Understanding, Reference, Code Atlas)
- [x] Direct navigation to individual example and documentation pages
- [ ] Eliminate Gallery routing layer (if still present)
- [ ] Make routing fully idiomatic using `purescript-routing`

### CSS and Layout:
- [x] Editorial/academic aesthetic with beige/brown color palette
- [x] Consistent styling across all sections
- [x] Floating TOC panels with bookmark imagery
- [x] Section navigation (RHS panels)
- [x] Code snippet styling with Prism.js
- [x] Subtle gradients for CTAs matching site style
- [ ] Replace remaining Ocelot-derived components with modern CSS
- [ ] Responsive design improvements for all screen sizes
- [ ] Consider thumbnail navigation for examples
- [ ] Clean up and possibly delete Ocelot-derived code

### Accessibility:
- [ ] Ensure main text appears and is navigable for screen readers
- [ ] Handle no-JS case gracefully
- [ ] Explain what sighted users see/do for non-visual users
- [ ] **Stretch goal:** Sonic "visualizations" or audio descriptions

### Home Page:
- [x] Clean, modern design with editorial aesthetic
- [x] Quadrant-based navigation to main sections
- [x] Clear calls to action (Getting Started, Launch Wizard)
- [x] Bookmark images for each section
- [ ] Add "Why PS<$>D3?" section explaining Finally Tagless benefits
- [ ] Add quick code example on home page
- [ ] Consider generating sections from markdown

---

## 14. Documentation Improvements

### Four types of documentation (status):
1. [x] **API/Reference** - Basic structure created (see Section 11)
2. [x] **Overview** - Architecture, Finally Tagless pattern (see Section 10 - Understanding)
3. [x] **Tutorial** - Getting Started page with embedded examples
4. [ ] **How-to/Cookbook** - Common patterns and recipes (partially complete)

### Completed:
- [x] Create Understanding section with Concepts, Patterns, Philosophy pages
- [x] Create Reference section with module browser
- [x] Create Getting Started tutorial page
- [x] Document datum_ pattern with diagram in Patterns page
- [x] Code snippet extraction and display system

### TODO:
- [ ] Complete How-to guides for all common patterns
- [ ] Add module-level documentation to all core modules
- [ ] Document capability/interpreter relationship in more depth
- [ ] Add inline comments for complex FFI interactions
- [ ] Consider hover-over comments or 'i' icons for code sections
- [ ] Add more code examples throughout documentation
- [ ] Create "migration guide" for D3.js users

---

## 15. Data and Build Pipeline

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

## 16. Library API Improvements

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

## 17. Interactive Wizard

**Status:** Basic structure exists, needs content

### TODO:
- [ ] Design wizard flow for beginners
- [ ] Create step-by-step guided tour through library concepts
- [ ] Add interactive code editing/playground
- [ ] Implement "Choose Your Own Adventure" style branching
- [ ] Add progress tracking
- [ ] Link wizard completions to relevant documentation sections

---

## 18. Stretch Goals / Future Considerations

- [ ] Add e-charts support (existing PureScript wrapper available)
- [ ] Parser to convert markdown to tree data (display as text, tree layout, or force layout)
- [ ] Additional advanced visualizations (parallel coordinates, hexbin, etc.)
- [ ] Explore other interpreters beyond visualization (data validation, accessibility descriptions)
- [ ] Video tutorials for complex topics
- [ ] Interactive playground with live code editing
- [ ] Performance benchmarking suite

---

## Recent Accomplishments (2025)

### Documentation Infrastructure (January 2025):
- [x] Created Understanding section with Concepts, Patterns, and Philosophy pages
- [x] Created Reference section with 30 module pages and source display
- [x] Integrated Mermaid.js for diagram rendering
- [x] Built code snippet extraction and display system
- [x] Added TOC panels with bookmark imagery across all documentation pages
- [x] Implemented tabbed navigation for Understanding section

### Code Atlas Enhancements (January 2025):
- [x] Renamed from "Spago" to "Code Atlas"
- [x] Added Expandable Bubbles visualization with nested structure
- [x] Implemented Spotlight Mode for dependency exploration
- [x] Added context menus with keyboard shortcuts
- [x] Created floating control panels, legend, and details panels
- [x] Added multiple visualization views (Declarations, Module Graph, Interactive Graph)

### Visual Design (January 2025):
- [x] Established editorial/academic aesthetic with beige/brown palette
- [x] Made all gradients subtle to match site style
- [x] Unified styling across all sections
- [x] Added bookmark pin imagery for navigation panels

---

## Notes

- Priorities can be adjusted based on what's most impactful for demonstrating the library
- "Stretch goals" within sections can be deferred to later phases
- Focus on polish and education: each example should be both impressive and instructive
- Recent work has focused on documentation infrastructure and Code Atlas features
