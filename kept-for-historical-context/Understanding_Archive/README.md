# Understanding Pages Archive

**Archived**: 2025-11-15

These pages documented the **PSD3 v1** architecture (the old library approach).

## What Was Here

### Core Tutorial Pages
- **UnderstandingIndex** - Main index with card grid
- **UnderstandingFinallyTagless** - Finally Tagless pattern explanation
- **UnderstandingSelectionM** - SelectionM Monad (OLD approach, replaced by phantom types)
- **UnderstandingCapabilities** - OLD Capabilities & Interpreters
- **UnderstandingTypeSystem** - Type-Safe Attribute System
- **UnderstandingDatumPattern** - datum_/Datum_ Pattern (OLD approach)
- **UnderstandingGrammar** - Grammar of D3 in SelectionM (OLD)
- **UnderstandingTabs** - Tab navigation component
- **About**, **Concepts**, **Patterns** - Philosophy and concepts pages

### Demo Pages
- **SimpleCharts** - Simple chart demonstrations
- **DataFlowViz** - Data flow visualizations
- **Hierarchies** - Hierarchy examples
- **Interpreters**, **InterpretersDemo** - Interpreter demonstrations
- **IsometricCurveExperiment** - Isometric curve experiments
- **Tutorial** - Tutorial content
- **TOC** - Table of contents component

## Why Archived

These pages used the **OLD PSD3 v1 architecture**:
- SelectionM monad
- Old Capabilities/Interpreter pattern
- datum_/Datum_ pattern
- Old Selection types

## New Architecture (PSD3v2)

See the new examples instead:
- **LesMisGUPTree** - Full-featured example with Tree API + SimulationM2
- **TreeAPI** - Tree API examples
- **PSD3v2Examples** - PSD3v2 showcase

### Key Differences

**OLD (v1)**:
```purescript
import PSD3.Interpreter.D3 (runD3, D3M)
import PSD3.Capabilities.Selection (select, append)

example :: D3M Unit
example = do
  sel <- select "body"
  svg <- append SVG sel
  ...
```

**NEW (v2)**:
```purescript
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2M)
import PSD3v2.Capabilities.Selection (select, renderTree)
import PSD3v2.VizTree.Tree as T

example :: D3v2M SUnbound Unit
example = do
  container <- select "body"
  let tree = T.elem SVG [] `T.withChild` ...
  renderTree container tree
```

## References

- Main cleanup plan: `../../CLEANUP_PLAN.md`
- PSD3v2 documentation: (TODO: add when created)
