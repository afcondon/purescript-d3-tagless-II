module D3.Viz.FpFtw.TopologicalSort where

import Prelude

import Data.Array (length)
import Data.Array as Array
import Data.Foldable (maximum, traverse_, foldl)
import Data.Graph (Graph, fromMap, topologicalSort)
import Data.Int as Int
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach)
import PSD3.Internal.Attributes.Sugar (classed, cx, cy, fill, fontSize, height, radius, strokeColor, strokeWidth, text, textAnchor, transform, viewBox, width, x, x1, x2, y, y1, y2)
import PSD3.Internal.Types (D3Selection_, Element(..), Selector)

-- | Task in a build pipeline
type Task = {
    id :: String
  , name :: String
  , depends :: Array String
}

-- | Task with computed layer information
type LayeredTask = {
    id :: String
  , name :: String
  , layer :: Int
  , depends :: Array String
}

-- | Sample build pipeline tasks
buildPipelineTasks :: Array Task
buildPipelineTasks =
  [ { id: "clone", name: "Clone Repository", depends: [] }
  , { id: "install", name: "Install Dependencies", depends: ["clone"] }
  , { id: "lint", name: "Run Linter", depends: ["install"] }
  , { id: "test", name: "Run Tests", depends: ["install"] }
  , { id: "build", name: "Build Project", depends: ["lint", "test"] }
  , { id: "docker", name: "Build Docker Image", depends: ["build"] }
  , { id: "deploy", name: "Deploy to Production", depends: ["docker"] }
  ]

-- | Sample recipe tasks (cooking)
recipeTasks :: Array Task
recipeTasks =
  [ { id: "buy", name: "Buy Ingredients", depends: [] }
  , { id: "bread", name: "Slice Bread", depends: ["buy"] }
  , { id: "tomato", name: "Slice Tomato", depends: ["buy"] }
  , { id: "cheese", name: "Slice Cheese", depends: ["buy"] }
  , { id: "toast", name: "Toast Bread", depends: ["bread"] }
  , { id: "assemble", name: "Assemble Sandwich", depends: ["toast", "tomato", "cheese"] }
  ]

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
       }

-- | Draw topological sort visualization
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

  (root :: D3Selection_) <- attach selector
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
