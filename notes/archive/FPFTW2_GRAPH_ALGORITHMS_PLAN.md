# FPFTW-2: Graph Algorithms + Visualization

## Concept

Show the powerful synergy between:
1. **Pure functional graph algorithms** (from topograph or similar)
2. **Declarative visualization** (TreeAPI)
3. **Real algorithms** people recognize (topological sort, transitive reduction, A*)

This demonstrates FP isn't academic - it solves hard problems elegantly.

## Potential Examples (Pick 2-3)

### 1. Topological Sort (ALREADY HAVE!)
- **What**: Order tasks respecting dependencies
- **Algorithm**: Pure functional topological sort
- **Visualization**: Nodes arranged in levels by depth
- **Data**: Build process, course prerequisites, task scheduling
- **FP Win**: Immutable graph, pure algorithm, easy to test
- **Status**: Already implemented in codebase! Just need to showcase it.

### 2. Transitive Reduction
- **What**: Remove redundant edges from a DAG
- **Algorithm**: Floyd-Warshall or similar, pure functional
- **Visualization**: Before/after side-by-side graphs
- **Data**: Dependency graph with implied transitive dependencies
- **FP Win**: Graph transformation as pure function, easy to visualize delta
- **Impact**: "Oh! Those edges were redundant!"

### 3. A* Pathfinding
- **What**: Find shortest path between nodes
- **Algorithm**: A* with heuristic, pure functional
- **Visualization**: Animated search showing explored nodes, final path
- **Data**: Grid graph or network graph
- **FP Win**: Each step is a pure transformation, easy to animate
- **Impact**: Visual + algorithmic, shows search process

### 4. Strongly Connected Components (Tarjan's)
- **What**: Find maximal strongly connected subgraphs
- **Algorithm**: Tarjan's or Kosaraju's algorithm
- **Visualization**: Color-code components, show structure
- **Data**: Social network, web pages, citation network
- **FP Win**: Immutable graph, pure DFS

### 5. Minimum Spanning Tree (Kruskal's/Prim's)
- **What**: Connect all nodes with minimum total edge weight
- **Algorithm**: Kruskal's with union-find, or Prim's
- **Visualization**: Edges highlighted as added, show progression
- **Data**: Network design, clustering
- **FP Win**: Pure algorithm, easy to visualize steps

## Recommendations

### Top Choice: Topological Sort + Transitive Reduction
**Why:**
- We already have topological sort implemented
- Transitive reduction is conceptually clear (remove redundant edges)
- Both are practical (dependency management, build systems)
- Visually distinct but thematically related
- Shows graph transformation (not just layout)

### Alternative: Topological Sort + A* Pathfinding
**Why:**
- Different algorithm types (ordering vs search)
- A* is famous, people recognize it
- Animation potential with A* is high
- Shows FP works for dynamic algorithms too

## Implementation Approach

1. **Find/Create Pure Graph Library**
   - Option A: Port parts of Haskell's topograph
   - Option B: Use existing PureScript graph library
   - Option C: Implement minimal graph ADT ourselves

2. **Graph Data Structure**
   ```purescript
   type Graph node edge =
     { nodes :: Set node
     , edges :: Map node (Set node)  -- adjacency list
     -- or use more sophisticated structure
     }
   ```

3. **Algorithm Implementation**
   - Pure functions: `Graph -> Result`
   - Immutable transformations
   - Easy to test, easy to visualize

4. **Visualization Strategy**
   - Use D3 force layout for node positions (or manual layout)
   - Highlight different states (before/after, steps, paths)
   - Clear visual encoding (colors, animation, annotations)

## Example Structure

### Topological Sort Example
```purescript
-- Data: Build process dependencies
tasks =
  [ ("compile", ["parse", "typecheck"])
  , ("parse", [])
  , ("typecheck", ["parse"])
  , ("link", ["compile"])
  , ("test", ["compile"])
  ]

-- Algorithm: Pure topological sort
sorted = topologicalSort taskGraph

-- Visualization: Layers by depth
renderTopoSort :: Graph -> Tree
renderTopoSort graph =
  let layers = groupByDepth sorted
  in visualizeAsLayers layers
```

### Transitive Reduction Example
```purescript
-- Data: Dependency graph with implied edges
original = buildGraph dependencies
reduced = transitiveReduction original

-- Visualization: Side-by-side before/after
renderComparison :: Graph -> Graph -> Tree
renderComparison before after =
  sideBySide
    (renderGraph "Original" before)
    (renderGraph "Reduced" after highlightRemoved)
```

## Key FP Wins to Highlight

1. **Immutability**: Graph transformations don't mutate, they return new graphs
2. **Purity**: Algorithms are pure functions, easy to test and reason about
3. **Composition**: Algorithms compose (sort, then reduce, then visualize)
4. **Type Safety**: Graph structure is verified by the compiler
5. **Separation**: Algorithm logic separate from visualization logic

## Visual Design Goals

- Clear, intuitive node/edge rendering
- Color-coding for different states/groups
- Annotations explaining what's happening
- Before/after comparisons where applicable
- Animation for multi-step algorithms (optional)

## Next Steps

1. Check what graph code we already have (topological sort)
2. Choose 2 algorithms to implement
3. Implement pure algorithms
4. Create visualizations
5. Write explanatory text
6. Test and refine

## Notes

- Keep it practical, not academic
- Show algorithms people actually use
- Emphasize the FP + visualization synergy
- Make it visually compelling
- Each example should "wow" in a different way
