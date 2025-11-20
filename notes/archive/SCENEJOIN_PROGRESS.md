# SceneJoin Implementation Progress

## Completed ✅

1. **Added SceneJoin variant to Tree type** (src/lib/PSD3v2/VizTree/Tree.purs:69-77)
   - Includes name, key, joinData, template, and three optional behavior specs
   - Each behavior has attributes array and optional transition config

2. **Defined GUP behavior types** (src/lib/PSD3v2/VizTree/Tree.purs:79-104)
   - EnterBehavior: initialAttrs + optional transition
   - UpdateBehavior: attrs + optional transition
   - ExitBehavior: attrs + optional transition (element removed after)

3. **Implemented SceneJoin interpreter** (src/lib/PSD3v2/Selection/Operations.purs:931-1045)
   - Performs data join to compute enter/update/exit selections
   - Applies behavior-specific attributes
   - TODO: Transition support (requires TransitionM in monad stack)

4. **Updated all other interpreters**
   - English: Describes SceneJoin with GUP phases (src/lib/PSD3v2/Interpreter/English.purs:48-58)
   - MermaidTree: Shows SceneJoin with {E/U/X} flags (src/lib/PSD3v2/Interpreter/MermaidTree.purs:169-196)
   - MetaAST: Added SceneJoinAST variant (src/lib/PSD3v2/Interpreter/MetaAST.purs:36-44, 75-90)
   - TourInterpreters: Visualizes SceneJoin nodes (src/website/Component/Tour/TourInterpreters.purs:181-210)

5. **Created demo example** (src/website/Viz/TreeAPI/SceneJoinDemo.purs)
   - Simple colored circles demo
   - Uses NestedJoin to transition types (container -> data)
   - Has drawInitial, updateToThree, updateToSeven functions

## Key Design Challenge 🤔

**Type Constraint Issue**: SceneJoin (like Join) requires the same `datum` type throughout the tree. This creates a problem:

```purescript
-- We want to write:
T.named SVG "svg" []       -- Tree Unit (no data)
  `T.withChild`
    (T.sceneJoin "circles" "circle"
      dataPoints           -- Array DataPoint
      template)            -- TYPE MISMATCH! Tree Unit ≠ Tree DataPoint
```

**Current Workaround**: Use NestedJoin to transition types:

```purescript
T.named SVG "svg" []       -- Tree SceneData
  `T.withChild`
    (T.nestedJoin "wrapper" "g"
      [scene]              -- Array SceneData
      (_.points)           -- Decompose to Array DataPoint
      template)            -- Tree DataPoint
```

**Problem with Workaround**: NestedJoin doesn't have GUP behaviors (enter/update/exit)!

## Potential Solutions

### Option A: SceneNestedJoin Variant
Add a new variant that combines NestedJoin's type decomposition with SceneJoin's GUP behaviors:

```purescript
| SceneNestedJoin
    { name :: String
    , key :: String
    , joinData :: Array outerDatum
    , decompose :: outerDatum -> Array innerDatum
    , template :: innerDatum -> Tree innerDatum
    , enterBehavior :: Maybe (EnterBehavior innerDatum)
    , updateBehavior :: Maybe (UpdateBehavior innerDatum)
    , exitBehavior :: Maybe (ExitBehavior innerDatum)
    }
```

### Option B: Add Behaviors to NestedJoin
Extend existing NestedJoin with optional GUP behaviors.

### Option C: Phantom Types for Containers
Make container elements (SVG, Group) use a phantom Unit type that doesn't affect child data types.

### Option D: Accept the Limitation
Document that SceneJoin works best when the whole tree shares a data type, or use the NestedJoin workaround for type transitions (but without GUP transitions).

## Next Steps

1. Decide on approach for type mixing (A, B, C, or D)
2. If adding SceneNestedJoin: implement variant and interpreter
3. Create full LesMis example using chosen approach
4. Test SceneJoin with actual transitions (requires TransitionM)
5. Compare with old LesMisGUP and potentially remove

## Files Modified

### Library Files
- `src/lib/PSD3v2/VizTree/Tree.purs` - Added SceneJoin, behavior types, smart constructor
- `src/lib/PSD3v2/Selection/Operations.purs` - Implemented SceneJoin interpreter
- `src/lib/PSD3v2/Interpreter/English.purs` - Added SceneJoin description
- `src/lib/PSD3v2/Interpreter/MermaidTree.purs` - Added SceneJoin visualization
- `src/lib/PSD3v2/Interpreter/MetaAST.purs` - Added SceneJoinAST variant

### Website Files
- `src/website/Component/Tour/TourInterpreters.purs` - Visualize SceneJoin in tree
- `src/website/Viz/TreeAPI/SceneJoinDemo.purs` - Simple demo (uses NestedJoin workaround)
- `src/website/Viz/LesMis/LesMisGUPTreeAPI.purs` - WIP (has type errors with links)
- `src/website/Viz/LesMis/LesMisGUPTreeAPISimple.purs` - WIP (simpler, still has type issues)

## Build Status

✅ All library code compiles
✅ SceneJoinDemo compiles and works (simple circles with GUP)
✅ SceneNestedJoin implemented - the CLEAN solution!
🔨 LesMisGUPSimple implemented with declarative pattern (compilation issues with Effect/D3v2SimM mixing)
❌ LesMisGUPTreeAPI has type errors (link swizzling + type mixing issues)
❌ LesMisGUPTreeAPISimple has type errors (datum type mismatch)

## New Implementation: LesMisGUPSimple

Created `src/website/Viz/LesMis/LesMisGUPSimple.purs` demonstrating:

### Key Pattern: Declarative GUP with SceneNestedJoin

```purescript
createForceGraphTree :: SceneData -> T.Tree SceneData
createForceGraphTree scene =
  T.sceneNestedJoin "nodeElements" "circle"
    [scene]                    -- Outer data: SceneData
    (_.nodes)                  -- Decompose: extract nodes array
    (\node -> T.elem Circle ...)  -- Template for each node
    { enterBehavior: Just { initialAttrs: [...], transition: ... }
    , updateBehavior: Just { attrs: [...], transition: ... }
    , exitBehavior: Just { attrs: [...], transition: ... }
    }
```

### Update Cycle Pattern

1. **Stop simulation** before GUP transition
2. **Filter data** to new active node set
3. **Call renderTree** with new scene - SceneNestedJoin handles enter/update/exit automatically!
4. **Update simulation** with filtered nodes/links
5. **Wait for transitions** to complete
6. **Restart simulation** with `reheat` and `start`

### Cycle States

- **AllGray**: All nodes gray, simulation running
- **BrownExit**: ~30% of nodes turn brown and exit (shrink away)
- **GreenEnter**: New nodes enter green from phylotaxis positions

### What Works

✅ Declarative scene specification using SceneNestedJoin
✅ Type decomposition (SceneData -> Array LesMisSimNode)
✅ Enter/update/exit behaviors declared upfront
✅ Simulation coordination (stop -> update -> restart)
✅ Cycle state management
✅ Random node selection for dynamic updates

### Compilation Issue

The code structure mixes `Effect` and `D3v2SimM` contexts in a way that the type checker doesn't like. The D3v2SimM monad expects computations to return a record with a `simulation` field, but we're trying to return an update function.

**Solution**: Refactor to match existing pattern where draw functions return `D3v2SimM` computations, and the caller (component) manages execution and update loops.

### Next Steps

1. Fix the Effect/D3v2SimM interaction (follow pattern from LesMisGUPV2/LesMisGUPAuto)
2. Create a Halogen component that calls the draw function
3. Add auto-cycling with timer
4. Test the full cycle in browser
