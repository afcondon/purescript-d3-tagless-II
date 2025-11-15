# Next Critical Step: Data Joins

## What We've Proven So Far

✅ **Basic tree rendering works** (SimpleTreeExample)
✅ **Multi-level nesting works** (NestedElementsExample - Group → Circle + Text)
✅ **Named selection access works** (Map-based lookup)
✅ **Attributes work as before** (same Array (Attribute datum) interface)

## The Blocker: Data Joins

**Problem**: We can create structures, but we're manually writing each node:

```purescript
-- This is tedious and doesn't scale
tree =
  T.named "mainGroup" Group [] `T.withChildren`
    [ T.named "node1" Group [transform "translate(100, 100)"] `T.withChildren` [...]
    , T.named "node2" Group [transform "translate(300, 150)"] `T.withChildren` [...]
    , T.named "node3" Group [transform "translate(500, 100)"] `T.withChildren` [...]
    ]
```

**What we need**: Generate this structure from Array data:

```purescript
nodeData :: Array { x :: Number, y :: Number, label :: String, ... }
nodeData = [...]

-- Want something like:
tree =
  T.named "svg" SVG [...] `T.withChild`
    (T.dataJoin "nodes" "g" nodeData $ \d ->
      T.named "nodeGroup" Group [transform (translate d)] `T.withChildren`
        [ T.named "circle" Circle [cx 0, cy 0, radius d.r]
        , T.named "label" Text [textContent d.name]
        ])
```

## The Type Challenge

Data joins introduce **heterogeneous types** in the tree:

```purescript
-- Parent selection has type A
parent :: Selection SEmpty Element ParentDatum

-- But join data has type B
nodeData :: Array NodeDatum  -- Different type!

-- Template uses NodeDatum
template :: Tree NodeDatum

-- Result mixes both types
```

Current `Tree` type doesn't support this:

```purescript
data Tree datum  -- Single datum type parameter
  = Node (TreeNode datum)
  | Join { ... }
```

## Approaches to Consider

### Option 1: Keep Joins Imperative (Hybrid Approach)

```purescript
-- Structure is declarative
let nodeTemplate =
      T.named "nodeGroup" Group [...] `T.withChildren`
        [ T.named "circle" Circle [...]
        , T.named "label" Text [...]
        ]

-- Data join is imperative
container <- select "#viz"
JoinResult { enter, update, exit } <- joinData nodeData "g" container

-- Render template for enter elements
enterNodes <- renderTree enter nodeTemplate

-- Handle update/exit separately
```

**Pros**:
- Simple - no type-level heterogeneity needed
- Clear separation - structure vs. data binding
- Can use existing joinData with full GUP control

**Cons**:
- Not fully declarative
- Mixing styles (tree + imperative)
- Still need to extract selections from Map

### Option 2: Existential Types (Hide the Heterogeneity)

```purescript
data Tree where
  Node :: TreeNode datum -> Tree
  Join :: String -> Array childDatum -> (childDatum -> Tree) -> Tree
  --      ^key      ^data                ^template builder
```

Use existential quantification to hide the datum type difference.

**Pros**:
- Fully declarative tree definition
- Type-checker handles heterogeneity

**Cons**:
- PureScript doesn't have native existentials (would need encoding)
- More complex implementation
- Harder to provide good error messages

### Option 3: Phantom Type with Constraints

```purescript
data Tree (datums :: Row Type)
  = Node (TreeNode datum)  -- datum is from the row
  | Join
      { key :: String
      , parentDatum :: Proxy pd  -- pd is in datums row
      , childDatum :: Proxy cd   -- cd is in datums row
      , data :: Array cd
      , template :: Tree datums  -- Same row of types
      }
```

Use row types to track all datum types in the tree.

**Pros**:
- Type-safe tracking of heterogeneity
- PureScript already has row types

**Cons**:
- Very complex type-level programming
- Hard to infer
- Errors would be incomprehensible

### Option 4: Separate Join and Render (Current Stub)

```purescript
-- Join returns selections with data bound
JoinResult { enter, update, exit } <- joinData nodeData "g" container

-- Then use renderTree on enter selection ONLY
-- (Not the full tree - just the template for new elements)
enterElements <- renderTree enter nodeTemplate
```

**Pros**:
- Uses existing joinData
- Separates concerns
- Keeps tree simple

**Cons**:
- Have to render tree fragments separately
- Less elegant than single tree definition

## Recommended Approach: Start with Option 1

**Rationale**:
1. **Validates the concept** - Shows declarative trees are useful even with imperative joins
2. **Simplest to implement** - No type-level complexity
3. **Good error messages** - Leverages existing type system
4. **Incremental** - Can explore Options 2/3 later if needed

**Implementation**:

```purescript
-- 1. Define reusable templates
nodeTemplate :: Tree NodeDatum
nodeTemplate =
  T.named "nodeGroup" Group [class_ "node"] `T.withChildren`
    [ T.named "circle" Circle [radius (\d -> d.r), fill (\d -> d.color)]
    , T.named "label" Text [textContent (\d -> d.name)]
    ]

-- 2. Use imperative join
JoinResult { enter, update, exit } <- joinData nodes "g" container

-- 3. Render template for enter
enterGroups <- renderTree enter nodeTemplate

-- 4. Access children from map
case Map.lookup "circle" enterGroups of
  Just circles -> on (Drag ...) circles
  Nothing -> pure unit

-- 5. Update existing elements
updateGroups <- renderTree update nodeTemplate  -- Re-render with new data

-- 6. Remove exiting elements
remove exit
```

## Next Implementation Steps

1. **Create hybrid example** - Mix imperative joinData with declarative renderTree
2. **Test with simple data** - Array of { x, y, label }
3. **Validate GUP works** - Enter, update, exit all handled
4. **Try with force simulation** - See if it integrates smoothly
5. **Port small real example** - Maybe simplified code-explorer

If Option 1 feels good in practice → Keep it
If it feels clunky → Explore Option 2/3

## Success Metric

Can we port code-explorer's node rendering to this style and have it be:
- ✅ More readable than before
- ✅ Not significantly more verbose
- ✅ Type-safe with good errors
- ✅ Works with simulations/behaviors

If yes → Adopt the pattern
If no → Understand why and iterate

## Files to Create Next

1. `DataJoinHybridExample.purs` - Proves Option 1 works
2. `SimpleForceGraphDeclarative.purs` - Validates simulation integration
3. `CodeExplorerNodesV3.purs` - Real-world test

Let's start with #1.
