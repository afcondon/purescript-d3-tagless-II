# Data Holes Design: Deferred Data Binding

## The Insight

Instead of trying to embed heterogeneous types in the tree, create **"holes"** where data will be injected at render time.

## The Pattern

```purescript
-- Define template with a "hole" for data
tree =
  T.named "svg" SVG [...] `T.withChild`
    (T.named "container" Group [...] `T.withChild`
      -- This creates a "hole" - data will be injected here
      T.childrenFromData "nodes" $ \d ->
        T.named "nodeGroup" Group [transform (translate d)] `T.withChildren`
          [ T.named "circle" Circle [radius (\_ -> d.r)]
          , T.named "label" Text [textContent (\_ -> d.name)]
          ])

-- Render with data
selections <- renderTreeWithData container tree
  { nodes: nodeData  -- Inject data into "nodes" hole
  }
```

## How It Works

### 1. Tree Structure with Holes

```purescript
data Tree datum
  = Node (TreeNode datum)
  | Join
      { name :: Maybe String
      , key :: String
      , holeName :: String  -- NEW: Name of the data hole
      , template :: forall childDatum. childDatum -> Tree childDatum  -- Template builder
      }
```

The `Join` constructor now has:
- `holeName`: Identifies which data to use (e.g., "nodes", "links")
- `template`: Function that builds subtree given a datum

### 2. Smart Constructor

```purescript
-- Create a data hole in the tree
childrenFromData
  :: forall datum
   . String  -- Hole name
  -> (forall childDatum. childDatum -> Tree childDatum)  -- Template builder
  -> Tree datum
childrenFromData holeName templateFn =
  Join
    { name: Nothing  -- Or maybe Just holeName?
    , key: "g"  -- Default element type for join
    , holeName: holeName
    , template: templateFn
    }
```

### 3. Render with Data Map

```purescript
-- User provides data for each hole
type DataMap = Map String (Array Unknown)  -- Or use Variant

renderTreeWithData
  :: forall parent datum
   . Selection SEmpty parent datum
  -> Tree datum
  -> DataMap  -- Map from hole names to data arrays
  -> m (Map String Selection)
```

At render time:
1. Walk tree
2. When encountering `Join` node, look up `holeName` in `DataMap`
3. For each datum in the array, call `template datum` to build subtree
4. Use existing `joinData` to bind data and render

## Type Safety Through Phantom Types

The clever part: each hole's template is **polymorphic** in the datum type:

```purescript
template :: forall childDatum. childDatum -> Tree childDatum
```

This means:
- ✅ Template doesn't care what specific type the data is
- ✅ Parent tree stays polymorphic in its datum type
- ✅ No heterogeneous type issues
- ✅ Type checker can still verify attributes match datum

## Example: Code-Explorer Nodes

```purescript
codeExplorerTree =
  T.named "svg" SVG [width 800, height 600] `T.withChildren`
    [ -- Links data hole
      T.childrenFromData "links" $ \link ->
        T.named "link" Line
          [ x1 (\_ -> link.source.x)
          , y1 (\_ -> link.source.y)
          , x2 (\_ -> link.target.x)
          , y2 (\_ -> link.target.y)
          , stroke "gray"
          ]

    , -- Nodes data hole
      T.childrenFromData "nodes" $ \node ->
        T.named "nodeGroup" Group [transform (translate node)] `T.withChildren`
          [ T.named "circle" Circle
              [ radius (\_ -> node.r)
              , fill (\_ -> if node.highlighted then "red" else "blue")
              ]
          , T.named "label" Text
              [ textContent (\_ -> node.name)
              , textAnchor "middle"
              ]
          ]
    ]

-- Render with data
selections <- renderTreeWithData container codeExplorerTree
  { links: linkData
  , nodes: nodeData
  }
```

## Implementation Strategy

### Phase 1: Simple Version (No GUP)

```purescript
-- Just handle enter phase
renderTreeWithData :: ... -> Tree datum -> Record dataHoles -> m (Map String Selection)
renderTreeWithData parent tree dataMap = do
  -- Walk tree, when hitting Join:
  case node of
    Join { holeName, template } -> do
      -- Look up data
      case lookup holeName dataMap of
        Just dataArray -> do
          -- For each datum, build subtree
          let subtrees = map template dataArray
          -- Render each subtree
          -- Collect selections
```

### Phase 2: Add GUP Support

```purescript
-- Track previous render to detect enter/update/exit
renderTreeWithDataGUP
  :: Tree datum
  -> Record dataHoles
  -> Maybe (Map String Selection)  -- Previous render
  -> m { selections :: Map String Selection
       , state :: RenderState  -- For next update
       }
```

### Phase 3: Named Joins

```purescript
-- Allow naming the join itself for selection access
T.namedChildrenFromData
  "nodeGroups"  -- Selection name
  "nodes"       -- Data hole name
  $ \node -> ...

-- Later access:
case Map.lookup "nodeGroups" selections of
  Just groups -> addTickFunction "nodes" $ Step groups [...]
```

## Advantages

✅ **Fully declarative tree structure** - No imperative joins needed
✅ **Type-safe** - Template functions are polymorphic
✅ **Composable** - Can have multiple data holes in one tree
✅ **Flexible** - Can inject different data sets
✅ **Clear** - Hole names document what data is needed

## Open Questions

### 1. How to handle different element types in joins?

```purescript
-- Current: hardcoded "g" for join
childrenFromData holeName template

// Better: specify element type
childrenFromDataAs "circle" "nodes" template
```

### 2. How to name the joined selections?

```purescript
-- Option A: Auto-generate from hole name
childrenFromData "nodes" template
-- Creates selection named "nodes"

-- Option B: Explicit naming
namedChildrenFromData "nodeGroups" "nodes" template
-- Selection named "nodeGroups", data from "nodes" hole

-- Option C: Anonymous joins
childrenFromData' "nodes" template
-- No selection created, just renders
```

### 3. What's the type of the data map?

```purescript
-- Option A: Map String (Array Unknown) + unsafeCoerce
-- Option B: Variant (harder to use)
-- Option C: Record with type-level tracking (complex)

-- Probably start with A, can refine later
```

### 4. How to handle enter/update/exit?

```purescript
-- Option A: renderTreeWithData always re-renders everything
-- Option B: Track previous state and do smart diffing
-- Option C: Explicit update calls with old/new data

-- Start with A (simplest), add B later if needed
```

## Comparison to Alternatives

### vs. Hybrid Approach (Imperative joinData)
- **Data Holes**: Fully declarative, all structure in one place
- **Hybrid**: More explicit control over GUP phases

### vs. Heterogeneous Types (Row types, Existentials)
- **Data Holes**: Simple implementation, good errors
- **Heterogeneous**: Perfect type safety but very complex

### vs. Manual Tree Building
- **Data Holes**: Automatically generates from data
- **Manual**: Full control but tedious

## Implementation Plan

1. ✅ Add `holeName` field to `Join` constructor
2. ✅ Create `childrenFromData` smart constructor
3. ✅ Implement `renderTreeWithData` (enter phase only)
4. ✅ Test with simple example
5. Add GUP support (enter/update/exit)
6. Add named joins
7. Test with code-explorer

Let's start with steps 1-4 to prove the concept!

## Example Test Case

```purescript
-- Simple data
points :: Array { x :: Number, y :: Number, label :: String }
points =
  [ { x: 100.0, y: 100.0, label: "A" }
  , { x: 200.0, y: 150.0, label: "B" }
  , { x: 300.0, y: 120.0, label: "C" }
  ]

-- Tree with data hole
tree =
  T.named "svg" SVG [width 400, height 300] `T.withChild`
    (T.childrenFromData "points" $ \p ->
      T.named "pointGroup" Group [transform (translate p)] `T.withChildren`
        [ T.named "circle" Circle [radius 5.0, fill "blue"]
        , T.named "label" Text [textContent (\_ -> p.label)]
        ])

-- Render
container <- select "#viz"
selections <- renderTreeWithData container tree { points: points }

-- Should create 3 groups, each with circle + text
```

This should feel **much better** than the manual approach!
