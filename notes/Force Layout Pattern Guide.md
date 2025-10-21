# Force Layout Pattern Guide

This guide explains how to build complex, interactive force-directed graph visualizations using the **MiseEnScene pattern** demonstrated in the Spago example.

## Overview

The pattern separates concerns into two main layers:
- **Component Layer** (src/website/Component/): Halogen component managing state, UI, and user interactions
- **Viz Layer** (src/website/Viz/): D3 visualization code (data models, drawing functions, attributes)

## File Structure

For a force layout visualization called "MyGraph", create:

```
src/website/Component/MyGraph/
  ├── MyGraph.purs          # Main Halogen component
  ├── State.purs            # State types and lenses
  ├── Actions.purs          # Action types
  ├── Forces.purs           # Force library configuration
  ├── HTML.purs             # UI rendering
  └── Data.purs             # Data loading (if needed)

src/website/Viz/MyGraph/
  ├── Model.purs            # Data types and datum_ accessors
  ├── Draw.purs             # D3 initialize and updateSimulation
  ├── Attributes.purs       # Visual attributes for scenes
  └── Unsafe.purs           # Type coercion helpers
```

## Step-by-Step Implementation

### 1. Define Your Data Model (Viz/MyGraph/Model.purs)

```purescript
module D3.Viz.MyGraph.Model where

import D3.Node (D3_SimulationNode, D3_XY, D3_VxyFxy)
import Type.Row (type (+))

-- Your domain-specific node data
type MyNodeRow = (
  id :: String
, label :: String
, group :: Int
, someProperty :: Number
)

-- Simulation node: your data + D3 simulation fields
type MySimNode = D3_SimulationNode (MyNodeRow + D3_XY + D3_VxyFxy + ())

-- Model contains all your graph data
type MyGraphModel = {
    nodes :: Array MySimNode
  , links :: Array (D3Link String ())
}
```

### 2. Create Safe Accessors (Viz/MyGraph/Model.purs)

The `datum_` pattern provides type-safe access to D3's opaque `Datum_`:

```purescript
import D3.Viz.MyGraph.Unsafe (unboxD3SimNode)

datum_ = {
  -- Direct field accessors
    id: _.id <<< unboxD3SimNode
  , label: _.label <<< unboxD3SimNode
  , group: _.group <<< unboxD3SimNode
  , x: _.x <<< unboxD3SimNode
  , y: _.y <<< unboxD3SimNode

  -- Computed accessors (for attributes)
  , translateNode: \d -> do
      let node = unboxD3SimNode d
      "translate(" <> show node.x <> "," <> show node.y <> ")"

  , nodeRadius: \d -> do
      let node = unboxD3SimNode d
      case node.group of
        0 -> 20.0
        1 -> 15.0
        _ -> 10.0

  , nodeColor: \d -> do
      let node = unboxD3SimNode d
      d3SchemeCategory10N_ (toNumber node.group)
}
```

### 3. Create Unsafe Helpers (Viz/MyGraph/Unsafe.purs)

```purescript
module D3.Viz.MyGraph.Unsafe where

import Unsafe.Coerce (unsafeCoerce)
import D3.Data.Types (Datum_)

-- Unbox simulation node from opaque Datum_
unboxD3SimNode :: Datum_ -> MyNodeRecord
unboxD3SimNode datum = d
  where (D3SimNode d) = unsafeCoerce datum
```

### 4. Configure Forces (Component/MyGraph/Forces.purs)

```purescript
module PSD3.MyGraph.Forces where

import D3.Simulation.Forces (createForce, createLinkForce, initialize)
import D3.Simulation.Types (ForceType(..), RegularForceType(..), allNodes)

forceLibrary :: Map Label Force
forceLibrary = initialize [
    createForce "charge" (RegularForce ForceManyBody) allNodes [
        F.strength (-300.0)
      , F.distanceMin 1.0
      ]

  , createForce "center" (RegularForce ForceCenter) allNodes [
        F.strength 0.5
      , F.x 0.0
      , F.y 0.0
      ]

  , createForce "collision" (RegularForce ForceCollide) allNodes [
        F.strength 1.0
      , F.radius 25.0
      ]

  , createLinkForce Nothing [
        F.strength 0.5
      , F.distance 100.0
      ]
  ]
```

### 5. Define State and MiseEnScene (Component/MyGraph/State.purs)

```purescript
module PSD3.MyGraph.State where

type State = {
    simulation :: D3SimulationState_
  , model :: Maybe MyGraphModel
  , staging :: Staging D3Selection_ MyNodeRow MyLinkData NodeID
  , scene :: MiseEnScene
}

-- MiseEnScene: configuration for a visualization "scene"
type MiseEnScene = {
    chooseNodes :: (MySimNode -> Boolean)        -- which nodes to show
  , linksShown :: (MyGraphLinkID -> Boolean)     -- which links to render
  , linksActive :: (Datum_ -> Boolean)            -- which links exert force
  , forceStatuses :: M.Map Label ForceStatus      -- which forces are active
  , cssClass :: String                             -- top-level CSS class
  , attributes :: MyGraphSceneAttributes           -- visual styling
  , callback :: SelectionAttribute                 -- event callback
  , nodeInitializerFunctions :: Array (Array MySimNode -> Array MySimNode)
}

initialScene :: M.Map Label Force -> MiseEnScene
initialScene forceLibrary = {
    chooseNodes: const true
  , linksShown: const true
  , linksActive: const true
  , forceStatuses: getStatusMap forceLibrary
  , cssClass: ""
  , attributes: defaultSceneAttributes
  , callback: x 0.0
  , nodeInitializerFunctions: []
}

-- Define lenses for all scene properties
_chooseNodes :: Lens' State (MySimNode -> Boolean)
_chooseNodes = _scene <<< prop (Proxy :: Proxy "chooseNodes")

_linksShown :: Lens' State (MyGraphLinkID -> Boolean)
_linksShown = _scene <<< prop (Proxy :: Proxy "linksShown")

-- ... etc for other scene properties
```

### 6. Define Actions (Component/MyGraph/Actions.purs)

```purescript
module PSD3.MyGraph.Actions where

data Scene = DefaultView | ClusteredView | TreeView

data Action
  = Initialize
  | Finalize
  | Scene Scene
  | ToggleForce Label
  | ChangeSimConfig SimVariable
  | StopSim
  | StartSim
  | EventFromVizualization VizEvent

data VizEvent = NodeClick NodeID
```

### 7. Implement D3 Drawing (Viz/MyGraph/Draw.purs)

```purescript
module D3.Viz.MyGraph.Draw where

-- One-time SVG initialization
initialize :: forall m.
  Bind m =>
  MonadEffect m =>
  SimulationM D3Selection_ m =>
  SelectionM D3Selection_ m =>
  m { nodes :: Maybe D3Selection_, links :: Maybe D3Selection_ }
initialize = do
  (Tuple w h) <- liftEffect getWindowWidthHeight
  root <- attach "div.svg-container"

  svg <- appendTo root Svg [ viewBox (-w / 2.0) (-h / 2.0) w h ]
  inner <- appendTo svg Group []
  _ <- inner `on` Drag DefaultDrag
  _ <- svg `on` Zoom {
      extent: ZoomExtent { top: 0.0, left: 0.0, bottom: h, right: w }
    , scale: ScaleExtent 0.1 4.0
    , name: "mygraph"
    , target: inner
    }

  linksGroup <- appendTo inner Group [ classed "links" ]
  nodesGroup <- appendTo inner Group [ classed "nodes" ]

  pure { nodes: Just nodesGroup, links: Just linksGroup }

-- D3 General Update Pattern for data changes
updateSimulation :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  SimulationM D3Selection_ m =>
  (Staging D3Selection_ d r id) ->
  MyGraphSceneAttributes ->
  m Unit
updateSimulation staging@{ selections: { nodes: Just nodesGroup, links: Just linksGroup }} attrs = do
  -- Open selections
  node <- openSelection nodesGroup (show Group)
  link <- openSelection linksGroup (show Line)

  -- Merge new data with simulation (defensive copy for D3's join)
  merged <- mergeNewDataWithSim node keyIsID_ link keyIsID_ staging.rawdata

  -- Update nodes (Enter-Update-Exit pattern)
  node' <- updateJoin node Group merged.nodes keyIsID_
  nodeEnter <- appendTo node'.enter Group [ classed datum_.nodeClass ]
  _ <- appendTo nodeEnter Circle attrs.circles
  _ <- appendTo nodeEnter Text attrs.labels
  setAttributes node'.exit [ remove ]
  mergedNodeSelection <- mergeSelections nodeEnter node'.update

  -- Update links
  link' <- updateJoin link Line merged.links keyIsID_
  linkEnter <- appendTo link'.enter Line attrs.links
  setAttributes link'.exit [ remove ]
  mergedLinksSelection <- mergeSelections linkEnter link'.update

  -- Add to simulation
  setNodesFromSelection mergedNodeSelection
  setLinksFromSelection mergedLinksSelection staging.linksWithForce

  -- Tick functions (update positions on each simulation tick)
  addTickFunction "nodes" $
    Step mergedNodeSelection [ transform' datum_.translateNode ]
  addTickFunction "links" $
    Step mergedLinksSelection [
        x1 (_.x <<< link_.source)
      , y1 (_.y <<< link_.source)
      , x2 (_.x <<< link_.target)
      , y2 (_.y <<< link_.target)
      ]

  pure unit
```

### 8. Define Visual Attributes (Viz/MyGraph/Attributes.purs)

```purescript
module D3.Viz.MyGraph.Attributes where

type MyGraphSceneAttributes = {
    circles :: Array SelectionAttribute
  , labels :: Array SelectionAttribute
  , links :: Array SelectionAttribute
}

defaultSceneAttributes :: MyGraphSceneAttributes
defaultSceneAttributes = {
    circles: [
        radius datum_.nodeRadius
      , fill datum_.nodeColor
      , strokeColor "#fff"
      , strokeWidth 2.0
      ]
  , labels: [
        classed "label"
      , textAnchor "middle"
      , text datum_.label
      , fill "#fff"
      ]
  , links: [
        strokeColor "#999"
      , strokeOpacity 0.6
      , strokeWidth 1.0
      ]
}
```

### 9. Main Component (Component/MyGraph/MyGraph.purs)

```purescript
module PSD3.MyGraph where

component :: forall query output m. MonadAff m => H.Component query Unit output m
component = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    , initialize = Just Initialize
    , finalize = Just Finalize }
  }
  where
  initialState :: State
  initialState = {
      model: Nothing
    , staging: { selections: { nodes: Nothing, links: Nothing }
               , linksWithForce: const true
               , rawdata: { nodes: [], links: [] } }
    , simulation: initialSimulationState forceLibrary
    , scene: initialScene forceLibrary
    }

handleAction :: Action -> HalogenM State Action t316 t317 t318 Unit
handleAction = case _ of

  Initialize -> do
    -- 1. Load data
    maybeModel <- H.liftAff loadMyGraphData
    _model .= maybeModel

    -- 2. Initialize D3 SVG structure
    openSelections <- evalEffectSimulation Draw.initialize
    (_staging <<< _enterselections <<< _nodes) .= openSelections.nodes
    (_staging <<< _enterselections <<< _links) .= openSelections.links

    -- 3. Set up event wiring (D3 → Halogen)
    { emitter, listener } <- liftEffect $ HS.create
    void $ H.subscribe emitter
    _callback .= (simulationEvent listener)

    pure unit

  -- Scene switching: set up configuration and run simulation
  Scene DefaultView -> do
    _chooseNodes .= const true
    _linksShown .= const true
    _forceStatuses %= onlyTheseForcesActive ["charge", "center", "collision", linksForceName]
    _cssClass .= "default"
    _nodeInitializerFunctions .= []
    runSimulation

  -- ... other actions

-- The key orchestrator: filters data, updates D3, restarts simulation
runSimulation :: forall m. MonadEffect m => MonadState State m => m Unit
runSimulation = do
  stageDataFromModel
  staging <- use _staging
  callback <- use _callback
  sceneAttributes <- use _sceneAttributes
  forceStatuses <- use _forceStatuses
  let attributesWithCallback = sceneAttributes { circles = callback : sceneAttributes.circles }
  runWithD3_Simulation do
    stop
    actualizeForces forceStatuses
    Draw.updateSimulation staging attributesWithCallback
    setConfigVariable $ Alpha 1.0
    start

-- Filter and transform model data → staging
stageDataFromModel :: forall m. MonadState State m => m Unit
stageDataFromModel = do
  state <- get
  linksShown <- use _linksShown
  linksActive <- use _linksActive
  chooseNodes <- use _chooseNodes
  nodeInitializerFunctions <- use _nodeInitializerFunctions

  _stagingLinks .= (filter linksShown $ view _modelLinks state)
  _stagingLinkFilter .= linksActive
  let rawnodes = filter chooseNodes $ view _modelNodes state
      initializedNodes = foldl (\b a -> a b) rawnodes nodeInitializerFunctions
  _stagingNodes .= initializedNodes
```

## Key Patterns Explained

### The MiseEnScene Pattern

**Purpose**: Package all configuration for a visualization "scene" in one place

**Benefits**:
- Declarative scene switching (just set properties and call runSimulation)
- Easy to add new scenes without touching drawing code
- Clear separation: what to show vs. how to show it

**Example**:
```purescript
Scene ClusteredView -> do
  _chooseNodes .= isInMainCluster        -- filter data
  _forceStatuses %= onlyTheseForcesActive ["cluster", "collide"]  -- configure forces
  _cssClass .= "clustered"                -- change CSS
  _nodeInitializerFunctions .= [positionToGrid]  -- initialize positions
  runSimulation                            -- apply it all
```

### The runSimulation Pattern

**Purpose**: Bridge Halogen state changes to D3 visualization updates

**Flow**:
1. **stageDataFromModel**: Apply filters from scene config to model data
2. **stop**: Stop the running simulation
3. **actualizeForces**: Enable/disable forces per scene config
4. **updateSimulation**: Run D3 General Update Pattern (enter/update/exit)
5. **start**: Restart simulation with fresh alpha

This pattern ensures the simulation always reflects the current scene configuration.

### The datum_ Pattern

**Purpose**: Type-safe access to D3's opaque `Datum_` type

**Why**: D3 mutates data (adding x, y, vx, vy fields). We use unsafe coercion to access this data, but isolate it to one place.

**Pattern**:
```purescript
datum_ = {
    id: _.id <<< unboxD3SimNode              -- direct accessor
  , translateNode: \d -> ...                  -- computed accessor
}

-- Usage in attributes:
[ transform' datum_.translateNode ]
```

All unsafe coercion is in the Unsafe module and the `datum_` record. The rest of the code is type-safe.

### Bidirectional Events

**Halogen → D3**: Action handlers update state → runSimulation → updateSimulation

**D3 → Halogen**:
1. Create emitter/listener in Initialize
2. Store callback in scene config
3. D3 click events trigger listener
4. Listener emits Halogen Action
5. handleAction processes it

This creates a clean event loop where both sides can trigger updates.

## Common Patterns

### Node Initialization Functions

Functions that run on nodes before adding to simulation (e.g., positioning):

```purescript
-- Position nodes in a grid
positionToGrid :: Array MySimNode -> Array MySimNode
positionToGrid nodes = mapWithIndex positionNode nodes
  where
    columns = ceiling $ sqrt $ toNumber (length nodes)
    positionNode idx (D3SimNode node) =
      let gridPos = numberToGridPoint columns idx
      in D3SimNode node { x = gridPos.x * 100.0, y = gridPos.y * 100.0 }

-- Use in scene:
_nodeInitializerFunctions .= [positionToGrid, unpinAllNodes]
```

### Filter Functions

Functions to show/hide nodes or links based on criteria:

```purescript
-- In scene config:
_chooseNodes .= \(D3SimNode n) -> n.group == 1
_linksShown .= \link -> link.weight > 0.5
_linksActive .= const true  -- all shown links exert force
```

### Force Configuration

Enable/disable specific forces per scene:

```purescript
_forceStatuses %= onlyTheseForcesActive ["charge", "center", linksForceName]
_forceStatuses %= toggleForceStatus "collision"  -- toggle one force
```

## Testing Your Implementation

1. **Build**: `npm run build` - should have no errors
2. **Bundle**: `npm run bundle` - check docs/bundle.js size
3. **View**: Open docs/index.html in browser
4. **Test**:
   - Does the visualization render?
   - Do forces behave correctly?
   - Do scene switches work?
   - Do click/drag interactions work?
   - Does zoom/pan work?

## Troubleshooting

**"No selection found"**: Check that selector in `attach` matches your HTML

**Nodes don't move**: Check that forces are active and alpha > 0

**Links don't connect**: Verify key function matches node IDs

**Simulation runs forever**: Adjust alphaDecay or alphaMin in simulation config

**Events don't fire**: Check that callback is added to selection attributes

## Next Steps

Once you have the basic pattern working:

1. Add more scenes with different force configurations
2. Implement custom node initialization functions
3. Add tooltips or info panels
4. Experiment with different force parameters
5. Add filtering UI controls
6. Implement node/link highlighting on hover

## Reference Implementation

The Spago example (Component/Spago + Viz/Spago) demonstrates all these patterns in a real-world context. It's the definitive reference for:
- Complex scene management (5 different views)
- Multiple force configurations
- Various node initialization strategies
- Bidirectional event flow
- Dynamic filtering

Study it to see how all the pieces fit together in practice.
