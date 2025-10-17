# V2 - Redesigned Demo Application

Modern, responsive web application showcasing PureScript Tagless D3 visualizations.

## Overview

This is a complete redesign of the demo application with a focus on:
- **Responsive design**: Mobile-first, works on all devices
- **Educational value**: Clear code examples, explanations, and comparisons
- **Discoverability**: Gallery view, search, filtering
- **Unique features**: Interpreter switcher, animated diffs, code journey mode

See `../PageStructureProposal.md` for the full design proposal.

## Directory Structure

```
v2/
├── index.html              # Main HTML entry point
├── bundle.js               # Compiled PureScript application (generated)
├── bundle.css              # Compiled Tailwind CSS (generated)
├── README.md              # This file
├── src/                   # PureScript components (future)
│   ├── Components/        # UI components
│   │   ├── Gallery/       # Gallery view components
│   │   ├── ExampleView/   # Example detail view components
│   │   ├── Navigation/    # Navigation components
│   │   └── Shared/        # Shared/common components
│   ├── Pages/            # Top-level page components
│   ├── Router/           # Routing logic
│   └── Utils/            # Utility functions
├── styles/               # CSS files
│   └── main.css          # Main stylesheet (design system variables)
└── assets/               # Static assets
    ├── images/           # Images, logos
    ├── icons/            # Icon assets
    └── thumbnails/       # Example preview thumbnails

```

## Design System

### Color Palette
- **Primary**: `#4a90e2` (D3 blue)
- **Secondary**: `#50c878` (PureScript green)
- **Accent**: `#f39c12` (Highlight orange)
- **Dark**: `#2c3e50` (Code background)
- **Light**: `#ecf0f1` (Page background)

### Typography
- **Headers**: Inter, system-ui
- **Body**: -apple-system, system-ui
- **Code**: Fira Code, Monaco, monospace

### Breakpoints
- **Mobile**: < 768px
- **Tablet**: 768px - 1024px
- **Desktop**: > 1024px

## Development

### Build Commands

**Build v2 only:**
```bash
npm run dev-v2
```

**Build both v1 and v2:**
```bash
npm run build && npm run bundle && npm run build-css
```

### File Watching
For development with auto-rebuild:
```bash
# Terminal 1: Watch PureScript files
spago build --watch

# Terminal 2: Watch CSS files
npm run build-css-v2 -- --watch

# Terminal 3: Serve locally
npx http-server . -p 8080
```

Then visit:
- V1: http://localhost:8080/v1/
- V2: http://localhost:8080/v2/

## Implementation Phases

### Phase 1: Foundation ✓ (Current)
- [x] Directory structure created
- [x] Build configuration updated
- [x] Base HTML and CSS setup
- [ ] Basic Halogen routing
- [ ] Gallery component skeleton
- [ ] Split-pane component

### Phase 2: Gallery View (Week 1)
- [ ] Example card component
- [ ] Gallery grid layout
- [ ] Filtering and search
- [ ] Thumbnail generation
- [ ] Responsive breakpoints

### Phase 3: Example Detail View (Week 2)
- [ ] Split-pane layout (desktop)
- [ ] Tabbed layout (mobile)
- [ ] Code syntax highlighting
- [ ] Visualization panel
- [ ] Export functionality

### Phase 4: Educational Features (Week 3)
- [ ] D3 JavaScript comparison toggle
- [ ] Pattern callouts
- [ ] Inline annotations
- [ ] Related examples sidebar
- [ ] Breadcrumb navigation

### Phase 5: Interpreter Switcher (Week 4)
- [ ] Interpreter selector UI
- [ ] Wire up MetaTree interpreter
- [ ] Wire up String interpreter
- [ ] Animated transitions between interpreters
- [ ] Explanation documentation

### Phase 6: Advanced Features (Future)
- [ ] Animated diff mode (General Update Pattern on text)
- [ ] Code journey mode
- [ ] Interactive playground
- [ ] Learning paths
- [ ] Performance metrics

## Key Features

### 1. Gallery View
Card-based browsing with:
- Animated SVG thumbnails
- Tag filtering
- Search functionality
- Category grouping

### 2. Split-Pane View
Desktop layout with:
- Resizable code/visualization panes
- Syntax-highlighted code
- Live visualization
- Export options

### 3. Interpreter Switcher
**The Killer Feature** - Demonstrate the same code with different interpreters:
- D3 Renderer (normal visualization)
- MetaTree Visualizer (DSL structure visualization)
- String Generator (code/documentation output)

### 4. Responsive Design
Works on all devices:
- Desktop: Split-pane layout
- Tablet: Tabbed interface
- Mobile: Stacked layout with touch controls

## Code Conventions

### Component Structure
```purescript
module V2.Components.Example where

import Prelude
import Halogen as H
import Halogen.HTML as HH

type State = { ... }
data Action = ...
data Query a = ...

component :: forall q i o m. MonadEffect m => H.Component q i o m
component = H.mkComponent { ... }
```

### Naming Conventions
- Components: PascalCase (e.g., `GalleryCard`, `ExampleView`)
- Functions: camelCase (e.g., `renderCard`, `handleClick`)
- CSS classes: kebab-case (e.g., `gallery-card`, `example-view`)

### File Organization
- One component per file
- Colocate related files (component + styles + tests)
- Use index files for clean imports

## Testing

(To be added)

## Deployment

V2 will eventually replace V1 as the main demo. Until then:
- V1 is served at the root: https://your-domain.com/v1/
- V2 is served at: https://your-domain.com/v2/
- GitHub Pages configuration will need updating

## Migration Path

1. **Phase 1-2**: V2 in development, V1 remains primary
2. **Phase 3-4**: V2 feature-complete, both versions live
3. **Phase 5**: V2 becomes default, V1 deprecated
4. **Phase 6**: Remove V1, V2 moves to root

## Contributing

(To be added)

## References

- [Full Proposal](../PageStructureProposal.md)
- [Syntax Improvements](../SyntaxImprovements.md)
- [Main README](../README.md)

---

*Last Updated: 2025-10-17*
