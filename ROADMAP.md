# PSD3 Roadmap

## 1. Documentation: Getting Started, HowTo, Understanding

Now that the library architecture is stable (TreeAPI for visualizations, applySetup for declarative simulations), we can write definitive documentation.

### Project Skeletons (Progressive Complexity)

**Static Charts Track:**
1. Simple HTML page with scatter plot or basic chart
2. Same as minimal Halogen app
3. With callback using Halogen subscriber/emitter
4. General Update Pattern in Halogen with Aff lifecycle

**Force Layout Track:**
1. Simple HTML page with force layout graph
2. Same as minimal Halogen app
3. With GUPDemo pattern and Aff lifecycle

### Documentation Rewrite

Create a master markdown document with three sections:
- **Getting Started** - Installation, first project, core concepts
- **HowTo** - Step-by-step guides for common tasks
- **Understanding** - Deep dives into architecture, phantom types, interpreters

Use bullet points to drive the work, then flesh out each section.

### Defer: Wizard
Wait until libraries are published so wizard output can be properly tested.

---

## 2. Poster/Dashboard Layout System

Create an overarching structure ("poster" or "dashboard") providing:
- Modern responsive CSS for text + chart blocks
- Reusable layout components
- Good defaults for showcase pages

**Use cases:**
- Simpson's showcase (improved)
- HealthWealth example (enhanced)
- New example: "How This Library Actually Works" (see phantom-types-and-interpreters.md)

---

## 3. Library Audit: Surface Unsafe Code

Review libraries with critical eye for:
- `unsafeCoerce` usage
- `Ref` usage
- Global/mutable state

**Action:** Rename modules to make unsafe code explicit:
```
PSD3.Scene.Unsafe.X
PSD3.ForceEngine.Unsafe.Y
```

Users should never be caught unaware that the library consciously hides mutation for better DX.

---

## 4. Two-Pane Example Pages

Get ALL examples displayed in nice two-pane layout:
- Visualization on one side
- Code on the other

---

## 5. TreeBuilder & Quine Interpreter Overhaul

Goals:
- Get as close as possible to emitting correct PureScript code
- Drag and drop to reorder elements in Tree
- WYSIWYG editing in browser

---

## 6. Tour > Interpreters Page Overhaul

- Replace Mermaid diagram
- Use TreeBuilder widget (read-only mode)
- Add button to toggle which interpreter runs
- Better explanation of the interpreter pattern

---

## 7. Library Publishing

Prepare and publish to registry:
- psd3-selection
- psd3-simulation
- psd3-layout

---

## 8. Sankey Diagram: Library Dependencies

Re-create the Sankey diagram showing our dependence on D3 modules.

---

## 9. Showcase Polish & Hero Image

- Make all showcases visually impressive
- Select one or more for hero image on home page

---

## Notes

- Architecture is now stable: TreeAPI + applySetup pattern
- Halogen-first is the recommended approach for stateful visualizations
- Force simulations use subscriptions for tick events
