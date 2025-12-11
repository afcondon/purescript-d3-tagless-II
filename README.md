# PS<$>D3 - Type-Safe D3 Visualizations in PureScript

- Type-safe D3 visualizations in PureScript
- Better fit for functional programming while retaining D3 performance in the browser
- Declarative Tree API - describe what you want, library renders it
- Same tree can render to DOM (D3), Mermaid diagrams, or English descriptions
- Preserves D3's flexibility while adding compile-time safety
- Attributes are type-checked per element (can't put `cx` on a `Rect`)

## Why use it

- No runtime coercion/casting of data in bindings
- Compiler catches attribute mismatches
- Multiple interpreters from single visualization definition
- Pure PureScript layout algorithms (Sankey, tree, pack) - no D3 layout dependency
- Much better suited to re-use, library creation, complex apps, data viz as control surface
- Higher-level abstractions for graphs and networks using simulation with updates

## Packages

- `psd3-selection` - Core selection system, Tree API, type-safe attributes
- `psd3-simulation` - Force-directed graphs, simulation engine
- `psd3-layout` - Layout algorithms (tree, treemap, pack, Sankey, cluster)

## Two API levels

- **Tree API** - declarative, for static charts (bar, line, tree, Sankey)
- **PSD3 API** - lower-level, for simulations and interactive viz

## Documentation

[PS<$>D3 Documentation](https://afcondon.github.io/purescript-d3-tagless-II/)

- **Getting Started** - installation and first visualization
- **How-To** - step-by-step guides
- **API Reference** - module documentation
- **Understanding** - architecture deep-dives
- **Tour** - simple examples with code side-by-side
- **Showcase** - real-world examples

## Installation

```bash
spago install psd3-selection psd3-layout psd3-simulation
```

## Credits

See the [Acknowledgements](https://afcondon.github.io/purescript-d3-tagless-II/#/acknowledgements) page.

## License

MIT
