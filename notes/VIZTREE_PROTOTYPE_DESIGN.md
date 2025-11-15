# VizTree Prototype Design

## The Core Insight

SelectionM should support **both** imperative and declarative styles:
- **Imperative** (current): Sequence of `appendChild` calls in do-notation
- **Declarative** (new): Tree specification with automatic rendering

## The Challenge

We want syntax like:
```purescript
structure =
  svg "mysvg" [width 800] >:
    group "zoom" [class_ "zoom"] >:
      [ group "links" []
      , group "nodes" [] >:
          [ circle "dots" [radius 5]
          , text "labels" [content "hi"]
          ]
      ]

selections <- render container structure
-- Should return: { mysvg :: ..., zoom :: ..., links :: ..., nodes :: ..., dots :: ..., labels :: ... }
```

The return type must be **automatically derived** from the tree structure.

## Approach 1: Existential Types (Current Attempt)

```purescript
data Tree datum
  = Node
      { name :: String
      , elemType :: ElementType
      , attrs :: Array (Attribute datum)
      , children :: Array (Tree datum)
      }
  | DataJoin ...
```

**Problem**: Can't derive the return type automatically. We'd need dependent types.

## Approach 2: Builder Pattern with Row Types

Instead of building a tree data structure, use **builder functions** that directly construct the result record:

```purescript
type Builder sel datum fields =
  { -- The tree structure (for rendering)
    tree :: TreeSpec datum
    -- The fields that will be in the result
  , fields :: Record fields
  }

-- Smart constructors return builders
svg :: forall datum. String -> Array (Attribute datum) -> Builder sel datum (svg :: Selection ...)
svg name attrs = ...

-- Operators combine builders
(>:) :: Builder sel datum parent -> Builder sel datum child -> Builder sel datum (parent + child)
```

**Problem**: Still need to thread the row types through, gets complex fast.

## Approach 3: Template Haskell Style (Compile-time Generation)

Use a string template and generate the types at compile time:

```purescript
mkVizTree "svg > g#zoom > (g#links + g#nodes > (circle + text))"
```

**Problem**: PureScript doesn't have Template Haskell, and this would require type-level string parsing.

## Approach 4: Keep It Simple - Named Builders

What if we don't try to be too clever? Just use a simple tree structure and a **manual record builder**:

```purescript
-- Build the tree
tree =
  node SVG [width 800]
    [ node Group [id_ "zoom"]
        [ node Group [id_ "links"] []
        , node Group [id_ "nodes"]
            [ node Circle [radius 5] []
            , node Text [content "hi"] []
            ]
        ]
    ]

-- Render and manually extract selections
rendered <- render container tree
let selections =
      { svg: rendered.get "0"           -- Path to svg
      , zoom: rendered.get "0.0"        -- Path to first child of svg
      , links: rendered.get "0.0.0"     -- etc
      , nodes: rendered.get "0.0.1"
      , circles: rendered.get "0.0.1.0"
      , texts: rendered.get "0.0.1.1"
      }
```

**Problem**: Manual path construction is error-prone and defeats the purpose.

## Approach 5: Named Nodes + Automatic Extraction

Combination of Approach 1 and 4:

```purescript
tree =
  named "svg" SVG [width 800] $
    named "zoom" Group [id_ "zoom"] $
      siblings
        [ named "links" Group [id_ "links"] $ children []
        , named "nodes" Group [id_ "nodes"] $
            siblings
              [ named "circles" Circle [radius 5] $ children []
              , named "texts" Text [content "hi"] $ children []
              ]
        ]

selections <- render container tree
-- Returns: Variant or Record with fields based on names
-- Access: selections.svg, selections.zoom, etc.
```

How to implement return type?

### Option A: Use `Variant` (Sum Type)
```purescript
render :: Tree datum -> m (StrMap (Selection ...))
```
Access: `lookup "svg" selections`

**Pros**: Simple, no type complexity
**Cons**: Loses type safety, runtime errors if name wrong

### Option B: Use Type-level String Literals (Available in PS!)
```purescript
tree =
  named (Proxy :: _ "svg") SVG [width 800] $
    named (Proxy :: _ "zoom") Group [] $ ...
```

The `Proxy :: _ "svg"` gives us a type-level string we can use!

Then with RowToList and other type-level machinery:

```purescript
render :: forall fields. Tree fields datum -> m (Record fields)
```

**Pros**: Type-safe field access
**Cons**: Requires significant type-level programming

## Approach 6: Hybrid - Simple Tree + Type-level Annotations

Best of both worlds:

1. **Runtime**: Simple tree structure with string names (easy to render)
2. **Compile-time**: Type-level annotations for safety (caught at compile time)

```purescript
-- Simple runtime tree
type TreeNode datum =
  { name :: String
  , elemType :: ElementType
  , attrs :: Array (Attribute datum)
  , children :: Array (TreeNode datum)
  }

-- Type-safe builder that tracks names
data Builder (names :: Row Type) datum = Builder (TreeNode datum)

named :: forall name names datum sel.
  IsSymbol name =>
  RowLacks name names =>
  Proxy name ->
  ElementType ->
  Array (Attribute datum) ->
  Builder names datum ->
  Builder (name :: sel | names) datum
named proxy elemType attrs (Builder child) =
  Builder { name: reflectSymbol proxy, elemType, attrs, children: [child] }
```

This way:
- **Tree structure is simple** (just records and arrays)
- **Type tracking happens at builder time** (ensures no duplicate names, etc.)
- **Rendering is straightforward** (walk the tree, collect named nodes into record)

## Recommendation: Start with Approach 5A (StrMap)

For the prototype, let's start with the simplest thing that works:

1. Tree structure with string names
2. `render` returns `StrMap (Exists Selection)` or similar
3. Helper function to extract selections: `get :: String -> StrMap -> Maybe Selection`

**Why**: Proves the concept without getting stuck in type-level complexity.

Once we know the tree rendering works, we can **incrementally add type safety** using Approach 6.

## Next Steps

1. Implement simple Tree type with string names
2. Create smart constructors: `named`, `elem`, `siblings`, `child`
3. Implement operators: `>:` for child, `+:` for sibling
4. Add `render` function to SelectionM typeclass
5. Implement `render` in D3v2 interpreter
6. Test with NestedMatrixV2 example

**Philosophy**: Get it working first, then make it type-safe.
