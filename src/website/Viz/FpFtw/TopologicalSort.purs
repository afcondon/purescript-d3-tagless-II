module D3.Viz.FpFtw.TopologicalSort where

import Prelude

import Data.Array (length)
import Data.Array as Array
import Data.Foldable (maximum, traverse_, foldl)
import Data.Graph (Graph, fromMap, topologicalSort)
import Data.Int (toNumber)
import Data.Int as Int
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable, notNull, null)
import Data.Tuple (Tuple(..))
import Control.Monad.State (class MonadState)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web as AX
import Data.Either (Either(..))
import Data.Number (sqrt)
import Data.Set as Set
import PSD3.Attributes (DatumFn(..), DatumFnI(..))
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach, simpleJoin, setAttributes)
import PSD3.Capabilities.Simulation (class SimulationM2, addTickFunction, init, start)
import PSD3.Data.Node (D3_SimulationNode(..), D3Link_Swizzled, D3Link_Unswizzled)
import PSD3.Internal.Attributes.Sugar (classed, cx, cy, fill, fontSize, height, radius, strokeColor, strokeWidth, text, textAnchor, transform, viewBox, width, x, x1, x2, y, y1, y2)
import PSD3.Internal.FFI (keyIsID_)
import PSD3.Internal.Scales.Scales (d3SchemeCategory10N_)
import PSD3.Internal.Simulation.Config as F
import PSD3.Internal.Simulation.Forces (createForce, createLinkForce, initialize)
import PSD3.Internal.Simulation.Types (D3SimulationState_, Force, ForceType(..), RegularForceType(..), allNodes, initialSimulationState, Step(..))
import PSD3.Internal.Types (D3Selection_, Datum_, Element(..), Selector)
import PSD3.Shared.ZoomableViewbox (zoomableSVG)
import Unsafe.Coerce (unsafeCoerce)
import Utility (getWindowWidthHeight)

-- | Task in a build pipeline
type Task = {
    id :: String
  , name :: String
  , depends :: Array String
  , group :: Maybe Int  -- Optional group for coloring (used by LesMis)
}

-- | Task with computed layer information
type LayeredTask = {
    id :: String
  , name :: String
  , layer :: Int
  , depends :: Array String
  , group :: Maybe Int
}

-- | Simulation node for layered tasks (with fy fixed to layer)
type LayeredSimNode = D3_SimulationNode (
    id :: String
  , name :: String
  , layer :: Int
  , group :: Maybe Int
  , x :: Number
  , y :: Number
  , vx :: Number
  , vy :: Number
  , fx :: Nullable Number
  , fy :: Nullable Number
)

-- | Link between tasks (for force simulation)
type TaskLink = {
    source :: String
  , target :: String
  , id :: String
}

-- | Sample build pipeline tasks
buildPipelineTasks :: Array Task
buildPipelineTasks =
  [ { id: "clone", name: "Clone Repository", depends: [], group: Nothing }
  , { id: "install", name: "Install Dependencies", depends: ["clone"], group: Nothing }
  , { id: "lint", name: "Run Linter", depends: ["install"], group: Nothing }
  , { id: "test", name: "Run Tests", depends: ["install"], group: Nothing }
  , { id: "build", name: "Build Project", depends: ["lint", "test"], group: Nothing }
  , { id: "docker", name: "Build Docker Image", depends: ["build"], group: Nothing }
  , { id: "deploy", name: "Deploy to Production", depends: ["docker"], group: Nothing }
  ]

-- | Sample recipe tasks (cooking)
recipeTasks :: Array Task
recipeTasks =
  [ { id: "buy", name: "Buy Ingredients", depends: [], group: Nothing }
  , { id: "bread", name: "Slice Bread", depends: ["buy"], group: Nothing }
  , { id: "tomato", name: "Slice Tomato", depends: ["buy"], group: Nothing }
  , { id: "cheese", name: "Slice Cheese", depends: ["buy"], group: Nothing }
  , { id: "toast", name: "Toast Bread", depends: ["bread"], group: Nothing }
  , { id: "assemble", name: "Assemble Sandwich", depends: ["toast", "tomato", "cheese"], group: Nothing }
  ]

-- | LesMiserables JSON types
type LesMisNode = { id :: String, group :: Int }
type LesMisLink = { source :: String, target :: String, value :: Number }
type LesMisGraph = { nodes :: Array LesMisNode, links :: Array LesMisLink }

foreign import readLesMisJSON_ :: String -> LesMisGraph

-- | Load LesMiserables graph from JSON
loadLesMisGraph :: Aff (Either String LesMisGraph)
loadLesMisGraph = do
  result <- AX.get ResponseFormat.string "data/miserables.json"
  pure $ case result of
    Left err -> Left $ "Failed to load: " <> AX.printError err
    Right response -> Right $ readLesMisJSON_ response.body

-- | Convert LesMis graph to tasks (treating links as dependencies)
-- | Note: LesMis graph likely has cycles! We'll need to break them
lesMisToTasks :: LesMisGraph -> Array Task
lesMisToTasks graph =
  let
    -- Create a map of node ID -> list of nodes that depend on it (reverse deps)
    dependenciesMap :: Map String (Array String)
    dependenciesMap = foldl addLink Map.empty graph.links
      where
        addLink acc link =
          Map.insertWith append link.target [link.source] acc

    -- Convert each node to a task with its dependencies (preserving group for coloring)
    nodeToTask :: LesMisNode -> Task
    nodeToTask node =
      { id: node.id
      , name: node.id
      , depends: fromMaybe [] $ Map.lookup node.id dependenciesMap
      , group: Just node.group  -- Preserve group for color coding!
      }
  in
    nodeToTask <$> graph.nodes

-- | Build a Data.Graph from tasks
-- | Graph k v = Graph (Map k (Tuple v (List k)))
-- | This showcases the elegant Graph representation in PureScript!
buildTaskGraph :: Array Task -> Graph String Task
buildTaskGraph tasks =
  let
    -- Build Map String (Tuple Task (List String))
    -- Each entry: task ID -> (task data, list of dependency IDs)
    graphMap :: Map String (Tuple Task (List String))
    graphMap = foldl addTask Map.empty tasks

    addTask :: Map String (Tuple Task (List String)) -> Task -> Map String (Tuple Task (List String))
    addTask acc task =
      Map.insert task.id (Tuple task (List.fromFoldable task.depends)) acc
  in
    fromMap graphMap

-- | Use Data.Graph.topologicalSort to get execution order
-- | Returns tasks in the order they should be executed
getTopologicalOrder :: Array Task -> List String
getTopologicalOrder tasks =
  let graph = buildTaskGraph tasks
  in topologicalSort graph

-- | Compute layers from topological sort result
-- | Process tasks in topological order, so dependencies are always computed first!
-- | This showcases how topological sort enables strict evaluation without cycles
computeLayers :: Array Task -> Map String Int
computeLayers tasks =
  let
    -- topologicalSort returns reverse order (dependents first), so we reverse it
    -- This puts prerequisites at layer 0 (bottom/root)
    sortedIds = List.reverse $ getTopologicalOrder tasks
    taskMap = Map.fromFoldable $ tasks <#> \t -> Tuple t.id t

    -- Process one task at a time, building layers incrementally
    -- The accumulated map contains all previously processed tasks
    processTask :: Map String Int -> String -> Map String Int
    processTask accLayers taskId =
      case Map.lookup taskId taskMap of
        Nothing -> accLayers
        Just task ->
          let layer =
                if Array.null task.depends
                  then 0
                  else
                    -- All dependencies were processed earlier (topological order!)
                    -- So we can safely look them up in accLayers
                    let depLayers = task.depends <#> \depId ->
                          fromMaybe 0 $ Map.lookup depId accLayers
                    in case maximum depLayers of
                         Just maxLayer -> maxLayer + 1
                         Nothing -> 0
          in Map.insert taskId layer accLayers
  in
    foldl processTask Map.empty (List.toUnfoldable sortedIds :: Array String)

-- | Add layer information to tasks
addLayers :: Array Task -> Array LayeredTask
addLayers tasks =
  let layers = computeLayers tasks
  in tasks <#> \t ->
       { id: t.id
       , name: t.name
       , layer: fromMaybe 0 $ Map.lookup t.id layers
       , depends: t.depends
       , group: t.group
       }

-- | Convert layered tasks to simulation nodes with fy fixed to layer
-- | Creates nodes with initial x position spread across width, y fixed to layer
tasksToSimNodes :: Number -> Number -> Array LayeredTask -> Array LayeredSimNode
tasksToSimNodes totalWidth totalHeight layeredTasks =
  let
    layerValues = layeredTasks <#> _.layer
    maxLayer = fromMaybe 0 $ maximum layerValues
    layerHeight = if maxLayer > 0 then (totalHeight - 100.0) / Int.toNumber maxLayer else 100.0

    -- Convert each task to a simulation node
    taskToNode :: LayeredTask -> Int -> LayeredSimNode
    taskToNode task indexInLayer =
      let
        tasksInLayer = Array.filter (\t -> t.layer == task.layer) layeredTasks
        layerCount = length tasksInLayer
        layerWidth = totalWidth - 100.0
        -- Center coordinates around 0,0 for zoomableSVG
        xPos = if layerCount > 1
               then (Int.toNumber indexInLayer * layerWidth / Int.toNumber (layerCount - 1)) - (layerWidth / 2.0)
               else 0.0
        yPos = (Int.toNumber task.layer * layerHeight) - (totalHeight / 2.0) + 50.0
      in
        D3SimNode
          { id: task.id
          , name: task.name
          , layer: task.layer
          , group: task.group  -- Preserve group for color coding!
          , x: xPos      -- Initial x position (centered)
          , y: yPos      -- Initial y position (centered)
          , vx: 0.0      -- Initial velocity
          , vy: 0.0
          , fx: null     -- Allow horizontal movement
          , fy: notNull yPos  -- Fix vertical position to layer!
          }
  in
    -- Process tasks layer by layer, tracking index within each layer
    Array.concat $ (\layer ->
      let tasksInLayer = Array.filter (\t -> t.layer == layer) layeredTasks
      in Array.mapWithIndex (\idx task -> taskToNode task idx) tasksInLayer
    ) <$> (Array.range 0 maxLayer)

-- | Helper to pack links into D3Link_Unswizzled
packTaskLink :: TaskLink -> D3Link_Unswizzled
packTaskLink = unsafeCoerce

-- | Create links from task dependencies
tasksToLinks :: Array LayeredTask -> Array D3Link_Unswizzled
tasksToLinks tasks =
  Array.concat $ tasks <#> \task ->
    task.depends <#> \depId ->
      packTaskLink
        { source: depId
        , target: task.id
        , id: depId <> "->" <> task.id
        }

-- | Draw topological sort visualization (simple build pipeline style)
drawTopologicalSort :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  Array Task -> Selector D3Selection_ -> m Unit
drawTopologicalSort tasks selector = do
  let totalWidth = 900.0
  let totalHeight = 600.0
  let layeredTasks = addLayers tasks

  -- Calculate layout parameters
  let layerValues = layeredTasks <#> _.layer
  let maxLayer = fromMaybe 0 $ maximum layerValues
  let layerHeight = if maxLayer > 0 then (totalHeight - 100.0) / Int.toNumber maxLayer else 100.0

  (root :: D3Selection_ Unit) <- attach selector
  svg <- appendTo root Svg [
      viewBox 0.0 0.0 totalWidth totalHeight
    , classed "topological-sort"
    , width totalWidth
    , height totalHeight
    ]

  -- Create container group
  container <- appendTo svg Group [
      transform [ \_ -> "translate(50, 50)" ]
    ]

  -- Draw dependency links
  let drawLink :: LayeredTask -> String -> m Unit
      drawLink task depId = do
        case Array.find (\t -> t.id == depId) layeredTasks of
          Nothing -> pure unit
          Just depTask -> do
            let sourceY = Int.toNumber task.layer * layerHeight
            let targetY = Int.toNumber depTask.layer * layerHeight
            let tasksInSourceLayer = Array.filter (\t -> t.layer == task.layer) layeredTasks
            let tasksInTargetLayer = Array.filter (\t -> t.layer == depTask.layer) layeredTasks
            let sourceIndex = fromMaybe 0 $ Array.findIndex (\t -> t.id == task.id) tasksInSourceLayer
            let targetIndex = fromMaybe 0 $ Array.findIndex (\t -> t.id == depTask.id) tasksInTargetLayer
            let layerWidth = totalWidth - 100.0
            let sourceCount = length tasksInSourceLayer
            let targetCount = length tasksInTargetLayer
            let sourceX = if sourceCount > 1
                          then Int.toNumber sourceIndex * layerWidth / Int.toNumber (sourceCount - 1)
                          else layerWidth / 2.0
            let targetX = if targetCount > 1
                          then Int.toNumber targetIndex * layerWidth / Int.toNumber (targetCount - 1)
                          else layerWidth / 2.0

            _ <- appendTo container Line [
                x1 targetX
              , y1 targetY
              , x2 sourceX
              , y2 sourceY
              , strokeColor "#bbb"
              , strokeWidth 2.0
              , classed "dependency-link"
              ]
            pure unit

  -- Draw all links
  _ <- traverse_ (\task -> traverse_ (drawLink task) task.depends) layeredTasks

  -- Draw task nodes
  let drawTask :: LayeredTask -> Int -> m Unit
      drawTask task indexInLayer = do
        let yPos = Int.toNumber task.layer * layerHeight
        let tasksInLayer = Array.filter (\t -> t.layer == task.layer) layeredTasks
        let layerCount = length tasksInLayer
        let layerWidth = totalWidth - 100.0
        let xPos = if layerCount > 1
                   then Int.toNumber indexInLayer * layerWidth / Int.toNumber (layerCount - 1)
                   else layerWidth / 2.0

        -- Draw circle for task
        _ <- appendTo container Circle [
            cx xPos
          , cy yPos
          , radius 30.0
          , fill "#4CAF50"
          , strokeColor "#2E7D32"
          , strokeWidth 2.0
          , classed "task-node"
          ]

        -- Draw task name
        _ <- appendTo container Text [
            x xPos
          , y (yPos + 5.0)
          , text task.name
          , textAnchor "middle"
          , fontSize 12.0
          , fill "#fff"
          , classed "task-label"
          ]

        pure unit

  -- Draw tasks layer by layer
  _ <- traverse_ (\layer ->
         let tasksInLayer = Array.filter (\t -> t.layer == layer) layeredTasks
         in traverse_ (\{idx, value: task} -> drawTask task idx) $ Array.mapWithIndex (\idx task -> {idx, value: task}) tasksInLayer
       ) (Array.range 0 maxLayer)

  -- Draw layer labels
  _ <- traverse_ (\layer ->
         appendTo container Text [
             x (-30.0)
           , y (Int.toNumber layer * layerHeight + 5.0)
           , text ("Layer " <> show layer)
           , textAnchor "end"
           , fontSize 14.0
           , fill "#666"
           , classed "layer-label"
           ]
       ) (Array.range 0 maxLayer)

  pure unit

-- | Forces configuration for topological sort
-- | Uses weak charge and collision forces with link forces between dependencies
topologicalForceLibrary :: Map String (Force LayeredSimNode)
topologicalForceLibrary = initialize [ forces.manyBodyWeak, forces.collision, forces.links ]
  where
    forces = {
        manyBodyWeak: createForce "many body weak" (RegularForce ForceManyBody) allNodes [ F.strengthVal (-15.0) ]
      , collision:    createForce "collision"       (RegularForce ForceCollide)  allNodes [ F.radiusVal 7.0 ]
      , links:        createLinkForce Nothing [ F.distanceVal 80.0 ]
    }

-- | Draw force-directed topological sort with nodes fixed to layers (fy positioning)
-- | This version uses D3 force simulation but constrains vertical position by layer
drawTopologicalForceDirected :: forall row d m.
  Bind m =>
  MonadEffect m =>
  MonadState { simulation :: D3SimulationState_ LayeredSimNode | row } m =>
  SimulationM2 D3Selection_ m =>
  Array Task -> Selector (D3Selection_ d) -> m Unit
drawTopologicalForceDirected tasks selector = do
  (Tuple w h) <- liftEffect getWindowWidthHeight
  let totalWidth = w
  let totalHeight = h
  let layeredTasks = addLayers tasks

  -- Calculate max layer for labels
  let layerValues = layeredTasks <#> _.layer
  let maxLayer = fromMaybe 0 $ maximum layerValues
  let layerHeight = if maxLayer > 0 then (totalHeight - 100.0) / Int.toNumber maxLayer else 100.0

  -- Convert to simulation nodes and links
  let simNodes = tasksToSimNodes totalWidth totalHeight layeredTasks
  let simLinks = tasksToLinks layeredTasks

  -- Create forces array
  let forcesArray = [
        createForce "many body weak" (RegularForce ForceManyBody) allNodes [ F.strengthVal (-15.0) ]
      , createForce "collision" (RegularForce ForceCollide) allNodes [ F.radiusVal 7.0 ]
      , createLinkForce Nothing [ F.distanceVal 80.0 ]
      ]
  let activeForces = Set.fromFoldable ["many body weak", "collision", "links"]

  rootSel <- attach selector

  -- Use zoomableSVG helper for consistent zoom/pan behavior
  { svg: _svg, zoomGroup } <- zoomableSVG rootSel
    { minX: -totalWidth / 2.0
    , minY: -totalHeight / 2.0
    , width: totalWidth
    , height: totalHeight
    , svgClass: "topological-force"
    , innerClass: "zoom-group"
    , innerWidth: totalWidth
    , innerHeight: totalHeight
    , scaleMin: 0.5  -- 50% minimum zoom
    , scaleMax: 4.0  -- 400% maximum zoom
    }

  -- Create container group within zoom group (no extra translate needed)
  container <- appendTo zoomGroup Group []

  -- Initialize simulation
  { nodes: nodesInSim, links: linksInSim } <- init
    { nodes: simNodes
    , links: simLinks
    , forces: forcesArray
    , activeForces: activeForces
    , config: { alpha: 1.0, alphaTarget: 0.0, alphaMin: 0.001, alphaDecay: 0.0228, velocityDecay: 0.4 }
    , keyFn: keyIsID_
    , ticks: Map.fromFoldable []
    }

  -- Draw dependency links first (so they appear under nodes)
  linksSelection <- simpleJoin container Line linksInSim keyIsID_
  setAttributes linksSelection [
      strokeColor "#bbb"
    , strokeWidth 2.0
    , classed "dependency-link"
    ]

  -- Draw task nodes with group-based coloring (matches LesMis style)
  nodesSelection <- simpleJoin container Circle nodesInSim keyIsID_
  let colorByGroup = (\d -> case (unsafeCoerce d :: { group :: Maybe Int }).group of
                               Just g -> d3SchemeCategory10N_ (toNumber g)
                               Nothing -> "#4CAF50") :: Datum_ -> String
  setAttributes nodesSelection [
      radius 5.0  -- Same size as LesMis
    , fill (DatumFn colorByGroup)  -- Color by group (matches LesMis style)
    , strokeColor "#fff"  -- White stroke like LesMis
    , strokeWidth 1.5
    , classed "task-node"
    ]

  -- Draw labels
  labelsSelection <- simpleJoin container Text nodesInSim keyIsID_
  setAttributes labelsSelection [
      textAnchor "middle"
    , fontSize 10.0
    , fill "#333"
    , classed "task-label"
    ]

  -- Add tick functions to update positions
  -- Links: extract source/target positions from swizzled link objects
  let unboxLinkObj = unsafeCoerce :: Datum_ -> { source :: { x :: Number, y :: Number }, target :: { x :: Number, y :: Number } }
  addTickFunction "links" $ Step linksSelection [
      x1 (DatumFn \d -> (unboxLinkObj d).source.x)
    , y1 (DatumFn \d -> (unboxLinkObj d).source.y)
    , x2 (DatumFn \d -> (unboxLinkObj d).target.x)
    , y2 (DatumFn \d -> (unboxLinkObj d).target.y)
    ]

  -- Nodes: extract x, y, name from simulation nodes
  let unboxNodeObj = unsafeCoerce :: Datum_ -> { x :: Number, y :: Number, name :: String }
  addTickFunction "nodes" $ Step nodesSelection [
      cx (DatumFn \d -> (unboxNodeObj d).x)
    , cy (DatumFn \d -> (unboxNodeObj d).y)
    ]

  -- Labels follow nodes with slight offset
  addTickFunction "labels" $ Step labelsSelection [
      x (DatumFn \d -> (unboxNodeObj d).x)
    , y (DatumFn \d -> (unboxNodeObj d).y + 20.0)
    , text (DatumFn \d -> (unboxNodeObj d).name)
    ]

  -- Draw layer labels (centered coordinates)
  _ <- traverse_ (\layer ->
         appendTo container Text [
             x (-(totalWidth / 2.0) + 10.0)
           , y ((Int.toNumber layer * layerHeight) - (totalHeight / 2.0) + 50.0 + 5.0)
           , text ("Layer " <> show layer)
           , textAnchor "start"
           , fontSize 12.0
           , fill "#999"
           , classed "layer-label"
           ]
       ) (Array.range 0 maxLayer)

  -- Start the simulation!
  start
  pure unit
