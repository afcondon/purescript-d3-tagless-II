# Examples Gallery Archive

**Archived**: 2025-11-15

This directory contains the old Examples Gallery system that showcased examples using **PSD3 v1**.

## What Was Here

- **ExamplesGallery.purs** - Gallery page with cards for all examples
- **Example.purs** - Individual example page component

## Examples Listed in Gallery

### Simple Charts (OLD - used PSD3 v1)
- Three Little Circles
- Bar Chart
- Line Chart
- Scatter Plot (Anscombe's Quartet)
- Grouped Bar Chart
- Multi-Line Chart
- Radial Stacked Bar
- Parabola

### Hierarchies
- Tree (PureScript layout)
- Treemap
- Pack (Circle packing)
- Cluster (Dendrogram)
- Partition/Icicle
- Sunburst

### Force-Directed
- Les Misérables Network
- Les Misérables GUP
- Topological Sort

### Data Flow
- Chord Diagram
- Sankey Diagram

### Rich Data Structures
- Map Quartet
- Nested Data
- Working with Sets

### Animations
- Wealth & Health of Nations

### Transitions
- Color Mixing
- General Update Pattern
- Animated Tree ↔ Cluster
- Animated Radial Tree

## Why Archived

These examples used the OLD PSD3 v1 library:
- Old SelectionM monad
- Old Capabilities/Interpreter pattern
- No phantom types
- No Tree API
- No SimulationM2

## New Examples

See these instead:
- **LesMisGUPTree** (`/lesmis-gup-tree`) - Full-featured force simulation
- **TreeAPI** (`/tree-api`) - Tree API examples
- **PSD3v2Examples** (`/psd3v2-examples`) - PSD3v2 showcase

## Migration Notes

Many of these examples can be converted to PSD3v2:
1. Replace `PSD3.Interpreter.D3` with `PSD3v2.Interpreter.D3v2`
2. Use Tree API for structure: `renderTree` instead of `append`/`select` chains
3. Use phantom types for selection states
4. Use SimulationM2 for force simulations

The pure PureScript layouts (Tree, Cluster, Pack, Treemap, Partition, Sankey) can still be used - they're layout-only and work with any renderer.
