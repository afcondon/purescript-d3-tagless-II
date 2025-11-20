# How-To Section Improvements

Notes and TODOs arising from initial review of the how-to guides.

---

## Vision

Think of this as seeding a wiki, like the sadly deceased bl.ocks.org which collected gists. Until user count > 1, won't implement user contributions, but it's a guiding star.

**Structure change**: Categorize cards but keep all topics on the same page (no sub-pages).

---

## Additional How-To's Needed

### Visualization Types (Basic Shells)
- Chord diagram
- Sankey diagram
- Treemap
- Icicle
- Bubblepack
- Tree

### Force Layout Additions
- Scene management
- Transition matrix
- Force library (turning forces on/off)
- Turning simulation on/off
- Use transitions within force layouts
- Information design of scenes (maybe Understanding section instead)
- Create a custom force

---

## Per-Topic Notes

### Performance Optimization
- Put "in work" sticker - we don't know much yet
- TODO: Test how big a force layout can be before browser crashes/crawls
- How-to: "Bless JSON without parsing it"
- How-to: "Write a JSON parser with Argonaut" - need one example using Argonaut

### Creating Animated Transitions
- TransitionConfig → rename to "How to control how your transitions look"
- Staggered animations: Add simple example to movements page (line of dots shuffling left, or something more meaningful)
- Enter/exit/update → Just explain GUP, it's the perfect distillation

### Loading External Data
- Aff example: Yes, but think framework-agnostic (Halogen, React, others)
- CSV: Provide library affordance (wrapper for D3 or update purescript-csv)
- Error handling: Provide affordance beyond unsafeCrashWith, surface to browser or make it easy for viz author

### Using the TreeAPI
- Current content is more Understanding than How-to
- Two actual how-tos: "Spec a viz with Selection API" and "Spec a viz with TreeAPI"

### Responding to User Events
- Hover: Go back and add nice hover to multiline chart, use as example
- Keyboard events: No working example - add to Code Explorer (menu shortcuts) + simple example
- Event delegation: Need to clarify what this means with example

### Creating Axes and Scales
- Review D3 scale docs and ensure we cover full variety with examples

### Building Force-Directed Graphs
- "Three little circles" level simulation (no file loading needed)
- "I have a 'data Graph' in PureScript, now give me a force layout" how-to

### Debugging Visualizations
- Add: Use of Debug.spy
- Consider: Debug flag or other affordances
- Note: Can assume PureScript devs know browser dev tools, but maybe mention anyway

### Adding Tooltips
- Need to add to some example (Code Explorer seems good)
- Probably a single how-to

### Working with Hierarchical Data
- Complicated by d3-hierarchy use in chord AND remaining snags in bubblepack/sankey
- TODO: Prioritize fixing those
- Link generators: Need library affordances + how-to on selecting them (h, v, r, iso)
- Animated trees, update trees, expand/collapse trees
- Zooming treemap example (show max 2 levels, click to dive into subtree)

---

## Questions to Answer

- Event delegation with selections: What does this mean? Need concrete example.
- What's the best way to categorize the how-to cards on a single page?

---

## Related Work

- Multiline chart hover enhancement
- Code Explorer keyboard shortcuts
- CSV library wrapper
- Bubblepack/Sankey d3-hierarchy fixes
