# Full-Screen Visualization Redesign - Proposal

## Objective
Transform the PureScript Tagless D3 demo site from a traditional documentation layout into an immersive, self-hosting visualization platform where the library's capabilities are used to present the library itself.

## Core Design Principles

### 1. Full-Screen First
- Extend the Spago full-screen SVG approach to all examples
- Visualization fills entire viewport (100vw × 100vh)
- UI controls float as semi-transparent panels with backdrop blur
- Maximize visual real estate for data exploration

### 2. Minimal Code Changes
- Preserve PureScript visualization logic
- Changes primarily in CSS and component layout
- Leverage existing D3 capabilities without rewriting interpreters

### 3. Self-Hosting via Visualization
- Use visualizations to navigate and present content
- Examples:
  - **Force-directed graph** for site navigation (examples as nodes, categories as clusters)
  - **Circle packing** for hierarchical content organization
  - **Tree layouts** for code structure exploration
  - **Sankey diagrams** for data flow between examples

### 4. CSS-Driven Transformation
- Create reusable full-screen layout classes
- Floating panel system (top-left controls, top-right info, bottom nav)
- Consistent visual language across all examples
- Responsive breakpoints for mobile/tablet

## Implementation Approach

### Phase 1: CSS Foundation
- Extract Spago's full-screen patterns into shared stylesheet
- Define panel positioning system (`.floating-panel-*` utilities)
- Establish z-index hierarchy and backdrop effects

### Phase 2: Apply to Existing Examples
- Convert each example to full-screen layout
- Position controls/info as floating panels
- Maintain interactivity (drag, zoom, pan)
- Test responsive behavior

### Phase 3: Self-Hosting Navigation
- Create meta-visualization for site navigation
- Possibly replace top nav bar with interactive force graph
- Examples become explorable through the visualization itself

### Phase 4: Exploratory Enhancements
- Experiment with visualization-based content presentation
- Interactive code snippets overlaying visualizations
- Smooth transitions between examples

## Design Inspirations
- Observable notebooks (immersive, data-driven)
- Modern data journalism (full-screen, scrollytelling)
- The Spago example's clean floating panel aesthetic

## Success Criteria
- All examples render full-screen with floating controls
- Navigation feels native to the visualization paradigm
- Site demonstrates library capabilities through self-use
- Code changes remain minimal and maintainable

## Status
Branch: `feature/full-screen`
Created: 2025-10-18
