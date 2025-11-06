# Guide: Building Declarative Force-Directed Visualizations with PSD3

This guide documents the patterns and architecture for building robust, declarative force-directed visualizations using the PSD3 library, based on lessons learned from CodeExplorer.

## Core Philosophy: Declarative Over Imperative

**Key Principle**: Users specify WHAT they want (node filters, forces, positions), and the library handles HOW to execute it. This eliminates entire classes of bugs by making invalid states unrepresentable.

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│                    Halogen Component                         │
│  - State management                                          │
│  - Action handlers                                           │
│  - Scene definitions                                         │
│  - Transition matrix                                         │
└────────────────┬────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────┐
│                   PSD3 Library Layer                         │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐      │
│  │ SceneConfig  │  │ RunSimulation│  │  Transitions │      │
│  │ - Filters    │  │ - Orchestrate│  │  - Animate   │      │
│  │ - Forces     │  │ - Update DOM │  │  - Coordinate│      │
│  │ - Attributes │  │ - Apply init │  │              │      │
│  │ - Initializ. │  │              │  │              │      │
│  └──────────────┘  └──────────────┘  └──────────────┘      │
└─────────────────────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────┐
│                     D3.js (FFI)                              │
│  - Force simulation engine                                   │
│  - DOM manipulation                                          │
│  - Transition execution                                      │
└─────────────────────────────────────────────────────────────┘
```

## File Structure

```
src/website/
├── Component/
│   └── YourViz/
│       ├── YourViz.purs          # Main Halogen component
│       ├── Actions.purs          # Action ADT and Scene ADT
│       ├── State.purs            # State type and helpers
│       ├── Scenes.purs           # Scene configurations
│       └── Forces.purs           # Force library definition
└── Viz/
    └── YourData/
        ├── Model.purs            # Domain-specific types and initializers
        ├── Draw.purs             # Visualization-specific update function
        ├── Render.purs           # DOM rendering callbacks (enter/update/exit)
        ├── Attributes.purs       # Visual attributes per scene
        └── Files.purs            # Data loading and parsing
```

## Step-by-Step Implementation

### 1. Define Your Data Types

**File**: `Viz/YourData/Model.purs`

```purescript
-- Your node type (wraps D3_SimulationNode)
type YourNode = D3_SimulationNode YourNodeData

-- Your custom data embedded in nodes
type YourNodeData =
  { name :: String
  , category :: String
  , value :: Number
  -- Add domain-specific fields
  }

-- Your link type
type YourLink = D3Link_Unswizzled
```

### 2. Create Scene ADT

**File**: `Component/YourViz/Actions.purs`

```purescript
-- Define all possible scenes (views) in your visualization
data Scene
  = EmptyScene           -- Initial state
  | Overview             -- Full view
  | CategoryView String  -- Filtered by category
  | DetailView Int       -- Focus on specific node
  | ComparisonView       -- Side-by-side comparison

derive instance Eq Scene
derive instance Ord Scene
```

### 3. Define State Type

**File**: `Component/YourViz/State.purs`

#### Simple Visualizations

For simple, single-mode visualizations, define state directly:

```purescript
type State =
  { simulation :: D3SimulationState_
  , model :: Maybe YourModel
  , staging :: Staging D3Selection_ YourNodeData
  , scene :: SceneConfig YourNodeData YourSceneAttributes
  -- Add app-specific state
  }
```

#### Complex SimulationM2 Visualizations

For complex visualizations combining force-directed and explicit layouts with scene-based transitions, use the generic `SimulationComponentState`:

```purescript
import PSD3.Component.SimulationState as SimState

-- One-line state specialization!
type State = SimState.SimulationComponentState
  Scene                    -- Your scene ADT
  Action                   -- Your Halogen action type
  YourNodeData            -- Your simulation node row type
  YourSceneAttributes     -- Your scene attributes type
  YourModel               -- Your data model type

-- Type aliases for convenience
type SceneConfig = Scene.SceneConfig YourNodeData YourSceneAttributes
type TransitionMatrix = SimState.TransitionMatrix Scene
```

**What you get for free:**
- Scene management functions: `updateScene`, `applySceneConfig`, `applySceneWithTransition`
- Tag system: `tagNodes`, `untagNodes`, `clearAllTags`, `getNodeTags`, `nodeHasTag`
- Model/staging accessors: `getModelNodes`, `getStagingNodes`, etc.
- Generic lenses: `_model`, `_staging`, `_scene`, `_nodes`, `_links`, etc.
- Transition matrix support for declarative scene choreography

**What you need to implement:**
- `initialScene` function (your default scene configuration)
- Visualization-specific lenses (optional, for cleaner API)
- Re-exports of generic functions with specific types (optional)

**Example State module structure:**

```purescript
module YourViz.State where

import PSD3.Component.SimulationState as SimState
import PSD3.Simulation.Scene as Scene
-- ... other imports

-- Specialize the generic state (1 line!)
type State = SimState.SimulationComponentState Scene Action YourDataRow YourAttributes YourModel

-- Type aliases
type SceneConfig = Scene.SceneConfig YourDataRow YourAttributes
type TransitionMatrix = SimState.TransitionMatrix Scene

-- Visualization-specific initialization
initialScene :: M.Map Label Force -> SceneConfig
initialScene forceLibrary =
  { chooseNodes: myDefaultFilter
  , linksShown: const false
  , linksActive: const false
  , activeForces: Set.fromFoldable (M.keys forceLibrary)
  , cssClass: ""
  , attributes: myDefaultAttributes
  , nodeInitializerFunctions: []
  , transitionConfig: Nothing
  }

-- Re-export generic functions with specific types (optional, for better error messages)
applySceneConfig :: SceneConfig -> State -> State
applySceneConfig = SimState.applySceneConfig

tagNodes :: String -> (YourSimNode -> Boolean) -> Array YourSimNode -> State -> State
tagNodes = SimState.tagNodes

-- Add visualization-specific helpers as needed
```

**When to use SimulationComponentState:**
- ✓ Multiple interactive scenes/views of the same data
- ✓ Combining force-directed and explicit layouts (grid, tree, pack, etc.)
- ✓ Complex transition choreography between scenes
- ✓ Ad-hoc node tagging and filtering

**When NOT to use SimulationComponentState:**
- Simple, single-view visualizations (use direct state definition)
- Static visualizations without scene switching
- Visualizations that don't need simulation state management

See `notes/STATE_REFACTOR_COMPLETE.md` for the full template and detailed documentation.

### 4. Build Force Library

**File**: `Component/YourViz/Forces.purs`

```purescript
forceLibrary :: Map Label Force
forceLibrary = fromFoldable $ createForces
  [ -- Positioning forces
    createForce "centerX" (RegularForce ForceX) Nothing
      [ F.strength 0.1, F.x 0.0 ]
  , createForce "centerY" (RegularForce ForceY) Nothing
      [ F.strength 0.1, F.y 0.0 ]

    -- Collision detection (use node-specific radius)
  , createForce "collide" (RegularForce ForceCollide) Nothing
      [ F.radius datum_.radius, F.strength 1.0 ]

    -- Repulsion between nodes
  , createForce "charge" (RegularForce ForceManyBody) Nothing
      [ F.strength (-100.0) ]

    -- Link force (binds connected nodes)
  , createLinkForce Nothing
      [ F.strength 0.3, F.distance 100.0 ]
  ]
```

**Force Design Principles:**
- Start with minimal forces (center, collide, charge, links)
- Add scene-specific forces as needed (radial, x/y positioning)
- Use filters to apply forces only to specific node types
- Tune strength values iteratively (0.0 - 1.0 typical range)

### 5. Create Node Initializers

**File**: `Viz/YourData/Model.purs`

```purescript
-- Unpin all nodes (let forces position them)
unpinAllNodes :: Array YourNode -> Array YourNode
unpinAllNodes = map \(D3SimNode d) ->
  D3SimNode d { fx = null, fy = null }

-- Position nodes in a grid
nodesToGrid :: Int -> Array YourNode -> Array YourNode
nodesToGrid columns nodes = mapWithIndex setGridPos nodes
  where
    setGridPos i (D3SimNode d) =
      let row = i / columns
          col = i `mod` columns
          x = toNumber col * 100.0
          y = toNumber row * 100.0
      in D3SimNode d { x = x, y = y, fx = null, fy = null }

-- Position nodes in a circle (phyllotaxis)
nodesToCircle :: Array YourNode -> Array YourNode
nodesToCircle nodes = mapWithIndex setCirclePos nodes
  where
    setCirclePos i (D3SimNode d) =
      let angle = toNumber i * goldenAngle
          radius = sqrt (toNumber i) * 10.0
          x = radius * cos angle
          y = radius * sin angle
      in D3SimNode d { x = x, y = y, fx = null, fy = null }
    goldenAngle = pi * (3.0 - sqrt 5.0)

-- Pin specific nodes (e.g., root node at center)
pinNodeAt :: String -> PointXY -> Array YourNode -> Array YourNode
pinNodeAt name point = map \node@(D3SimNode d) ->
  if d.name == name
  then D3SimNode d { fx = notNull point.x, fy = notNull point.y }
  else node
```

**Initializer Patterns:**
- **Grid layouts**: Set `x/y` positions, clear `fx/fy` (unpinned)
- **Tree layouts**: Set `x/y` for transition targets, library pins after transition
- **Force layouts**: Just unpin nodes, let forces position them
- **Swarm layouts**: Set `(x, 0)` start positions, forces spread vertically

### 6. Define Scene Configurations

**File**: `Component/YourViz/Scenes.purs`

```purescript
-- Overview scene: all nodes, force-directed
overviewScene :: SceneConfig
overviewScene =
  { chooseNodes: const true                    -- Show all nodes
  , linksShown: const true                     -- Show all links
  , linksActive: const true                    -- All links exert force
  , activeForces: Set.fromFoldable
      [ "centerX", "centerY", "collide", "charge", "links" ]
  , cssClass: "overview"
  , attributes: overviewAttributes             -- Visual styling
  , nodeInitializerFunctions: [ unpinAllNodes ]  -- Let forces position
  , transitionConfig: Just smoothTransition    -- Animated 1.5s transition
  }

-- Category view: filtered nodes, grid layout
categoryScene :: String -> SceneConfig
categoryScene category =
  { chooseNodes: \(D3SimNode d) -> d.category == category
  , linksShown: const false                    -- Hide links in grid
  , linksActive: const false
  , activeForces: Set.fromFoldable
      [ "collide" ]                            -- Only collision, no forces
  , cssClass: "category-" <> category
  , attributes: categoryAttributes
  , nodeInitializerFunctions: [ nodesToGrid 10 ]  -- 10 columns
  , transitionConfig: Just smoothTransitionPinned  -- Lock at grid positions
  }
```

**Scene Design Checklist:**
- ✓ Define `chooseNodes` predicate (which nodes visible?)
- ✓ Define `linksShown` predicate (which links visible?)
- ✓ Define `linksActive` predicate (which links exert force?)
- ✓ Choose active forces from library
- ✓ Pick initializers for positioning
- ✓ Decide transition behavior (instant, smooth, pinned?)

### 7. Build Transition Matrix

**File**: `Component/YourViz/YourViz.purs`

```purescript
defaultTransitionMatrix :: TransitionMatrix
defaultTransitionMatrix = Map.fromFoldable
  -- Between force-directed scenes: smooth, unpinned
  [ Tuple (Tuple Overview CategoryA) smoothTransition
  , Tuple (Tuple CategoryA Overview) smoothTransition

  -- To grid layouts: smooth, pinned (nodes lock at grid positions)
  , Tuple (Tuple Overview (CategoryView "Grid")) smoothTransitionPinned

  -- From grid layouts: smooth, unpinned (forces take over)
  , Tuple (Tuple (CategoryView "Grid") Overview) smoothTransition

  -- Initial setup: not in matrix → instant (no delay on load)
  ]
```

**Transition Design Rules:**
1. **Force → Force**: Use `smoothTransition` (unpinned)
2. **Force → Fixed layout** (grid/tree): Use `smoothTransitionPinned`
3. **Fixed → Force**: Use `smoothTransition` (unpin, forces activate)
4. **Fixed → Fixed**: Use `smoothTransitionPinned`
5. **Initial load**: Omit from matrix for instant setup

### 8. Implement Render Callbacks

**File**: `Viz/YourData/Render.purs`

```purescript
yourRenderCallbacks :: forall m.
  Monad m =>
  SelectionM D3Selection_ m =>
  RenderCallbacks YourSceneAttributes D3Selection_ m
yourRenderCallbacks = {
  onNodeEnter: \enterSel attrs -> do
    -- Create DOM structure for new nodes
    nodeGroup <- appendTo enterSel Group
      [ classed datum_.nodeClass
      , transform' datum_.translateNode
      ]

    -- Add visual elements (circle, text, etc.)
    _ <- appendTo nodeGroup Circle attrs.circles
    _ <- appendTo nodeGroup Text attrs.labels

    -- Add interaction (drag, click, etc.)
    _ <- nodeGroup `on` Drag (CustomDrag "drag" simdrag_)

    pure nodeGroup

  , onNodeUpdate: \updateSel attrs -> do
      -- Update group attributes
      setAttributes updateSel
        [ classed datum_.nodeClass
        , transform' datum_.translateNode
        ]

      -- Update circle attributes
      circles <- selectUnder updateSel (show Circle)
      setAttributes circles attrs.circles

      -- Update text attributes
      labels <- selectUnder updateSel (show Text)
      setAttributes labels attrs.labels

  , onNodeExit: \exitSel ->
      setAttributes exitSel [ remove ]

  , onLinkEnter: \enterSel attrs -> do
      appendTo enterSel Line attrs.links

  , onLinkUpdate: \updateSel attrs ->
      setAttributes updateSel attrs.links

  , onLinkExit: \exitSel ->
      setAttributes exitSel [ remove ]

  , nodeTickAttrs: \attrs ->
      [ transform' datum_.translateNode ]

  , linkTickAttrs:
      [ x1 datum_.linkX1
      , y1 datum_.linkY1
      , x2 datum_.linkX2
      , y2 datum_.linkY2
      ]
  }
```

**Render Callback Responsibilities:**
- **onNodeEnter**: Create DOM for new nodes (structure + styling)
- **onNodeUpdate**: Update attributes on existing nodes
- **onNodeExit**: Remove old nodes from DOM
- **nodeTickAttrs**: Attributes updated every simulation tick
- **linkTickAttrs**: Link positions updated every tick

### 9. Create Visualization-Specific Update

**File**: `Viz/YourData/Draw.purs`

```purescript
updateSimulation :: forall m d.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  SimulationM2 D3Selection_ m =>
  { nodes :: Maybe D3Selection_
  , links :: Maybe D3Selection_
  } ->
  { allNodes :: Array (D3_SimulationNode d)
  , allLinks :: Array D3Link_Unswizzled
  , nodeFilter :: D3_SimulationNode d -> Boolean
  , linkFilter :: Maybe (D3Link_Unswizzled -> Boolean)
  , nodeInitializers :: Array (Array (D3_SimulationNode d) -> Array (D3_SimulationNode d))
  , activeForces :: Set Label
  , linksWithForce :: Datum_ -> Boolean
  } ->
  YourSceneAttributes ->
  m Unit
updateSimulation selections dataConfig attrs =
  genericUpdateSimulation
    selections
    Group  -- Node element type
    Line   -- Link element type
    { allNodes: dataConfig.allNodes
    , allLinks: dataConfig.allLinks
    , nodeFilter: dataConfig.nodeFilter
    , linkFilter: dataConfig.linkFilter
    , nodeInitializers: dataConfig.nodeInitializers
    , activeForces: Just dataConfig.activeForces
    , config: Nothing
    }
    keyIsID_  -- Key function for data joins
    attrs
    yourRenderCallbacks
```

### 10. Wire Scene Actions

**File**: `Component/YourViz/YourViz.purs`

```purescript
handleAction = case _ of
  Scene Overview -> do
    -- 1. Apply scene configuration (includes transition lookup)
    H.modify_ $ applySceneWithTransition Overview overviewScene

    -- 2. Run simulation (handles filtering, initialization, transitions)
    evalAffSimulation $
      runSimulationFromState
        (_.staging.selections)        -- Get DOM selections
        (_.scene)                     -- Get scene config
        getModelNodes                 -- Get all nodes
        getModelLinks                 -- Get all links
        enhanceAttributes             -- Add callbacks/tags
        updateSimulation              -- Viz-specific update

    -- 3. Update scene tracking (for next transition)
    H.modify_ _ { currentScene = Overview }

  Scene (CategoryView cat) -> do
    H.modify_ $ applySceneWithTransition (CategoryView cat) (categoryScene cat)
    evalAffSimulation $ runSimulationFromState ... updateSimulation
    H.modify_ _ { currentScene = CategoryView cat }
```

**Action Handler Pattern:**
1. Apply scene config with transition lookup
2. Run simulation (declarative!)
3. Update current scene for next transition

## Common Patterns

### Pattern 1: Ad-hoc Node Tagging

```purescript
-- Tag nodes based on predicate
state' <- H.modify $ tagNodes "hot" (\(D3SimNode d) -> d.activity > 100) allNodes

-- Tags become CSS classes automatically
-- Style in CSS: .node.hot { fill: red; }
```

### Pattern 2: Conditional Force Activation

```purescript
-- Only apply force to specific node types
createForce "specialForce" (RegularForce ForceX)
  (Just $ ForceFilter "special only" \d -> d.category == "special")
  [ F.strength 0.5, F.x 100.0 ]
```

### Pattern 3: Multi-Stage Initializers

```purescript
nodeInitializerFunctions:
  [ unpinAllNodes           -- 1. Clear all pins
  , treeLayout              -- 2. Compute tree positions
  , pinRootNode "Main"      -- 3. Pin root at center
  ]
```

### Pattern 4: Symmetric vs Asymmetric Transitions

```purescript
-- A → B: slow, smooth
Tuple (Tuple SceneA SceneB) smoothTransition

-- B → A: quick, instant
Tuple (Tuple SceneB SceneA) instantTransition
```

## Common Pitfalls

### ❌ Don't: Pre-filter data before passing to library
```purescript
-- BAD: Filtering before library sees full dataset
let filteredNodes = filter isPackage allNodes
runSimulation selections scene filteredNodes allLinks updateFn
```

### ✓ Do: Pass full dataset + predicates
```purescript
-- GOOD: Library handles filtering declaratively
runSimulation selections scene allNodes allLinks updateFn
-- scene.chooseNodes = isPackage (predicate, not pre-filtered array)
```

### ❌ Don't: Set fx/fy in initializers for animated layouts
```purescript
-- BAD: Nodes snap instantly to position
nodesToGrid nodes = map \(D3SimNode d) ->
  D3SimNode d { fx = notNull gridX, fy = notNull gridY }
```

### ✓ Do: Set x/y for transition targets, pin after transition
```purescript
-- GOOD: Nodes animate smoothly, then lock
nodesToGrid nodes = map \(D3SimNode d) ->
  D3SimNode d { x = gridX, y = gridY, fx = null, fy = null }
-- Use smoothTransitionPinned to pin after animation completes
```

### ❌ Don't: Manually coordinate simulation lifecycle
```purescript
-- BAD: Manual orchestration (error-prone!)
stop
updateDOM
applyInitializers
startTransition
waitForCompletion
start
```

### ✓ Do: Use runSimulation for automatic orchestration
```purescript
-- GOOD: Library handles entire lifecycle
runSimulation selections scene allNodes allLinks updateFn
```

## Common Pitfalls and Solutions

Lessons learned from building visualizations from scratch:

### 1. Link Force Naming

**Problem**: Link force not activating, error "Cannot read properties of undefined (reading 'links')"

**Solution**: The link force is always named `"links"` (plural) in the library.

```purescript
-- ❌ WRONG
activeForces: Set.fromFoldable ["link"]

-- ✅ CORRECT
activeForces: Set.fromFoldable ["links"]
```

The library uses `linksForceName_` from FFI which is hardcoded to `"links"`.

### 2. ViewBox Centering for Force Layouts

**Problem**: All nodes clustered in top-left corner

**Solution**: Center the viewBox so (0,0) is in the middle of the screen

```purescript
initialize = do
  (Tuple w h) <- liftEffect getWindowWidthHeight
  let halfW = w / 2.0
  let halfH = h / 2.0

  svg <- appendTo root Svg
    [ classed "my-viz-svg"
    , viewBox (-halfW) (-halfH) w h  -- Center coordinate system
    ]
```

This is essential for force layouts where nodes orbit around (0,0).

### 3. Staging Type Requires Row Type

**Problem**: Type error "Could not match kind Type with kind Row Type"

**Solution**: `Staging` expects a row type (not concrete type) for its second parameter

```purescript
-- ❌ WRONG
type MyNode = D3_SimulationNode MyDataRecord
staging :: Staging D3Selection_ MyNode  -- Concrete type

-- ✅ CORRECT
type MyDataRow = (name :: String, value :: Number)
staging :: Staging D3Selection_ MyDataRow  -- Row type
```

### 4. Simulation State Initialization

**Problem**: Empty visualization, simulation not running

**Solution**: Initialize simulation state with force library in component's `where` clause

```purescript
-- ❌ WRONG (in separate initialState)
initialState :: State
initialState = { simulation: unsafeCoerce unit, ... }

-- ✅ CORRECT (in component's where clause)
component = H.mkComponent { ... }
  where
    initialState :: State
    initialState =
      { simulation: initialSimulationState forceLibrary  -- Proper init
      , scene: initialScene forceLibrary
      , ...
      }
```

### 5. Scene Filter Functions (Not Booleans)

**Problem**: Type error on `linksShown` or `linksActive`

**Solution**: These are functions, not boolean values

```purescript
-- ❌ WRONG
sceneConfig =
  { linksShown: true
  , linksActive: true
  }

-- ✅ CORRECT
sceneConfig =
  { linksShown: const true    -- Function: link -> Boolean
  , linksActive: const true   -- Function: datum -> Boolean
  }
```

### 6. Force Parameter Syntax

**Problem**: Type error "Could not match type { name :: String, value :: Number } with type ChainableF"

**Solution**: Use chainable functions from `PSD3.Internal.Simulation.Config`, wrap in `initialize`

```purescript
import PSD3.Internal.Simulation.Config as F

-- ❌ WRONG
createForce "orbit" (RegularForce ForceRadial) Nothing
  [{ name: "strength", value: 0.7 }]

-- ✅ CORRECT
createForce "orbit" (RegularForce ForceRadial) allNodes
  [ F.strength 0.7
  , F.radius 800.0
  , F.x 0.0
  , F.y 0.0
  ]
```

### 7. Link Accessors Need Explicit Types

**Problem**: Type error "No type class instance for ToAttr Number (linkDatum -> Number)"

**Solution**: Add explicit type signature and use `unsafeCoerce` for foreign link types

```purescript
import Unsafe.Coerce (unsafeCoerce)

link_ :: { sourceX :: Datum_ -> Number
        , sourceY :: Datum_ -> Number
        , targetX :: Datum_ -> Number
        , targetY :: Datum_ -> Number
        }
link_ =
  { sourceX: \d -> (unsafeCoerce d).source.x
  , sourceY: \d -> (unsafeCoerce d).source.y
  , targetX: \d -> (unsafeCoerce d).target.x
  , targetY: \d -> (unsafeCoerce d).target.y
  }
```

### 8. Filtering Links by Visible Nodes

**Problem**: Links pointing to non-existent nodes cause errors

**Solution**: Filter links to only those between visible nodes, use `unsafeCoerce` for ID access

```purescript
let nodeIds = Set.fromFoldable $ map (\(D3SimNode n) -> n.id) filteredNodes
let filteredLinks = Array.filter
      (\link ->
        let l = unsafeCoerce link :: { source :: Int, target :: Int }
        in Set.member l.source nodeIds && Set.member l.target nodeIds)
      allLinks
```

### 9. Import Organization for Standalone Viz

**Principle**: Borrow data types but implement logic locally

```purescript
-- ✅ GOOD: Borrow types
import D3.Viz.Spago.Files (NodeType(..), SpagoDataRow)

-- ✅ GOOD: Implement logic locally
isPackage :: BubblePackNode -> Boolean
isPackage (D3SimNode d) =
  case d.nodetype of
    (IsModule _) -> false
    (IsPackage _) -> true

-- ❌ AVOID: Borrowing logic from other viz
import D3.Viz.OtherViz.Model (isPackage)  -- Don't do this
```

This makes your viz truly standalone and testable.

## Testing Checklist

When implementing a new visualization, test these scenarios:

- [ ] Initial load (EmptyScene → FirstScene): Should be instant
- [ ] Scene switching: All transitions smooth, no jumps
- [ ] Force toggle: Should reheat simulation without jump
- [ ] Node dragging: Should work in all scenes
- [ ] Data updates: Should merge gracefully (no flicker)
- [ ] Large datasets: Performance acceptable (use collision optimization)
- [ ] Empty data: Graceful handling, no errors
- [ ] Filter to zero nodes: Clean transition, no console errors

## Performance Optimization

1. **Use quadtree collision**: `F.iterations 1` for O(n log n) collision
2. **Limit forces**: Only activate forces needed for current scene
3. **Debounce data updates**: Don't re-run simulation on every keystroke
4. **Use alpha targets**: `alphaTarget(0.3)` for continuous simulation
5. **Optimize tick functions**: Avoid heavy computation in `nodeTickAttrs`

## Debugging Tips

1. **Console warnings**: Library logs when predicates filter everything
2. **State inspector**: Check `state.scene` to verify configuration
3. **Force toggles**: Use control panel to isolate which forces cause issues
4. **Transition logging**: Check console for transition completion messages
5. **Index mismatches**: If positions wrong, likely ID-based lookup issue

## Summary

The declarative approach eliminates entire classes of bugs:
- ✓ No manual data synchronization (library handles merging)
- ✓ No transition coordination bugs (automatic orchestration)
- ✓ No forgotten filtering (impossible to show invalid links)
- ✓ No imperative state mutations (pure scene configurations)
- ✓ No manual lifecycle management (runSimulation handles everything)

The result: **Robust visualizations with minimal boilerplate and maximum confidence.**
