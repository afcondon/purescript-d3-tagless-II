# Join Implementation Strategy

## The Challenge

When we encounter a `Join` node during tree rendering, we need to:
1. Perform a data join (creating enter/update/exit selections)
2. For each datum, build and render the template tree
3. Return a named selection for the collection

## Current Problem

`renderNode :: Selection -> Tree -> Effect (Tuple Element (Map Selection))`

This works element-by-element, but a Join operates on a **collection** of elements.

## Key Insight: Two-Phase Rendering

### Phase 1: Structural Rendering
Build the **top-level element for each datum** using existing `renderData` or `append`

### Phase 2: Children Rendering
For each created element, render its children from the template

## Approach: Use Existing `renderData`

```purescript
renderNode parentSel (Join joinSpec) = do
  -- Get the template for a sample datum to determine structure
  case Array.head joinSpec.joinData of
    Nothing -> pure $ Tuple parentElement Map.empty  -- No data

    Just sampleDatum -> do
      let sampleTree = joinSpec.template sampleDatum

      case sampleTree of
        -- If template is a simple Node, we can use renderData
        Node nodeSpec -> do
          -- Use renderData to create the top-level elements
          boundSel <- renderData
            nodeSpec.elemType
            joinSpec.joinData
            joinSpec.key
            parentSel
            (Just $ \d -> nodeSpec.attrs)  -- Enter attrs
            Nothing  -- No update attrs (for now)
            Nothing  -- No exit attrs

          -- Now we have a bound selection with all the elements
          -- For each element, we need to render its children
          -- But we can't easily access individual elements...

          -- PROBLEM: renderData gives us a Selection of ALL elements,
          -- but we need to render children for EACH element individually
```

## Alternative: Iterate Over Data Directly

```purescript
renderNode parentSel (Join joinSpec) = do
  -- Perform join
  JoinResult { enter } <- joinData joinSpec.joinData joinSpec.key parentSel

  -- Get the pending data
  let Selection pendingImpl = enter
  let PendingSelection { pendingData, parentElements } = pendingImpl

  -- For each datum, build template and render
  results <- for pendingData $ \datum -> do
    let templateTree = joinSpec.template datum
    -- But how do we render this template for just THIS one element?
    -- We need a selection containing just this element...
```

## The Real Solution: Different Tree Semantics for Joins

Actually, the template in a Join should NOT include the top-level element.

The Join itself specifies:
- What element type to create ("g", "circle", etc.)
- What data to bind

The template specifies:
- Children and attributes of each element

```purescript
-- WRONG (template includes top-level Group):
joinData "nodes" "g" nodeData $ \node ->
  T.elem Group [transform (translate node)] `withChildren` [...]

// CORRECT (template is just the children):
joinData "nodes" "g" nodeData $ \node ->
  T.children
    [ T.elem Circle [radius node.r]
    , T.elem Text [textContent node.name]
    ]

// Or even simpler - template is attributes + children:
joinData "nodes" $ joinSpec
  { elemType: Group
  , data: nodeData
  , attrs: \node -> [transform (translate node)]
  , children: \node ->
      [ T.elem Circle [radius node.r]
      , T.elem Text [textContent node.name]
      ]
  }
```

## Simpler Design: Join Specifies Everything

```purescript
data JoinSpec datum = JoinSpec
  { name :: String                        -- Selection name
  , elemType :: ElementType               -- What to create
  , key :: String                         -- Join key selector
  , data :: Array datum                   -- Data to bind
  , attrs :: datum -> Array (Attribute datum)  -- Attributes per datum
  , children :: datum -> Array (Tree datum)     -- Children per datum
  }

joinData :: forall datum. JoinSpec datum -> Tree datum
joinData spec = Join spec

-- Usage:
tree = named "svg" SVG [...] `withChild`
  (joinData $ JoinSpec
    { name: "nodes"
    , elemType: Group
    , key: "g"
    , data: nodeData
    , attrs: \n -> [transform (translate n), class_ "node"]
    , children: \n ->
        [ elem Circle [radius n.r, fill "blue"]
        , elem Text [textContent n.name]
        ]
    })
```

Then rendering is straightforward:

```purescript
renderNode parentSel (Join (JoinSpec spec)) = do
  -- Create top-level elements with data binding
  JoinResult { enter } <- joinData spec.data spec.key parentSel

  boundSel <- append spec.elemType (spec.attrs sampleDatum) enter
  -- TODO: Need to set attrs per datum, not just sample

  -- For each element, render children
  -- Still need to figure out how to iterate...
```

## Wait - Even Simpler: Use Existing Functions!

What if we just use the imperative approach INSIDE renderTree?

```purescript
renderNode parentSel (Join joinSpec) = do
  -- Just use the existing imperative joinData + append
  JoinResult { enter } <- joinData joinSpec.data joinSpec.key parentSel

  -- Append creates the elements
  -- But how do we specify different children for each element?
  -- We can't with current API...
```

## The Fundamental Issue

**appendChild creates ONE child per parent element**
**We need to create DIFFERENT children per parent element**

Current: `appendChild Circle [...] parentSel` → Same circle for each parent

Needed: `appendChildPerDatum (\d -> circle d) parentSel` → Different circle per datum

## Breakthrough: We Need a New Primitive

```purescript
-- New operation: Append children based on bound data
appendChildrenFromTemplate
  :: forall datum
   . (datum -> Array (Tree datum))  -- Template builder
  -> Selection SBound Element datum  -- Parent selection with data
  -> Effect (Map String (Selection SBound Element datum))

-- Implementation:
-- For each element in the bound selection:
--   1. Get its bound datum
--   2. Call template datum to get children trees
--   3. Render those trees as children of this element
--   4. Collect named selections
```

Then our Join rendering becomes:

```purescript
renderNode parentSel (Join joinSpec) = do
  -- 1. Perform data join
  JoinResult { enter } <- joinData joinSpec.data joinSpec.key parentSel

  -- 2. Create top-level elements
  boundSel <- append joinSpec.elemType joinSpec.attrs enter

  -- 3. Render children per datum
  childSelections <- appendChildrenFromTemplate joinSpec.template boundSel

  -- 4. Return the bound selection as named selection
  let selectionsMap = Map.singleton joinSpec.name boundSel
  let combinedMap = Map.union selectionsMap childSelections

  -- 5. Get first element for return value
  let firstElement = ... -- extract from boundSel

  pure $ Tuple firstElement combinedMap
```

This is actually clean! The new primitive we need is `appendChildrenFromTemplate`.

Let me implement this!
