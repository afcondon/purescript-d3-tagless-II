module D3.Viz.FPFTW.TopologicalSort where

-- | Topological Sort Visualization
-- | Demonstrates pure functional graph algorithms + declarative visualization
-- | Shows how DAG dependencies can be ordered into execution layers

import Prelude

import Data.Array (concat, filter, length, mapWithIndex, range)
import Data.Array as Array
import Data.Foldable (maximum)
import Data.Graph.Algorithms (TaskNode, addLayers)
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import PSD3.Internal.Capabilities.Selection (renderTree, select)
import PSD3.Interpreter.D3 (D3v2Selection_, runD3v2M)
import PSD3.Internal.Selection.Types (ElementType(..), SEmpty)
import PSD3.AST as T
import PSD3.Expr.Friendly (num, text, attr, width, height, viewBox, cx, cy, r, x, y, x1, y1, x2, y2, fill, stroke, strokeWidth, textContent, textAnchor)
import Web.DOM.Element (Element)

-- | Task with display name
type Task = TaskNode String

-- | Layered task with display name (extends library LayeredNode with name field)
type LayeredTask =
  { id :: String
  , name :: String
  , layer :: Int
  , depends :: Array String
  }

-- | Sample build pipeline tasks (classic example)
buildPipelineTasks :: Array { id :: String, name :: String, depends :: Array String }
buildPipelineTasks =
  [ { id: "clone", name: "Clone Repo", depends: [] }
  , { id: "install", name: "Install Deps", depends: [ "clone" ] }
  , { id: "lint", name: "Lint", depends: [ "install" ] }
  , { id: "test", name: "Test", depends: [ "install" ] }
  , { id: "build", name: "Build", depends: [ "lint", "test" ] }
  , { id: "docker", name: "Docker Image", depends: [ "build" ] }
  , { id: "deploy", name: "Deploy", depends: [ "docker" ] }
  ]

-- | Add layer information to tasks with names
addLayersWithNames :: Array { id :: String, name :: String, depends :: Array String } -> Array LayeredTask
addLayersWithNames tasks =
  let
    -- Convert to TaskNode format for library function
    taskNodes = tasks <#> \t -> { id: t.id, depends: t.depends }

    -- Use library function to compute layers
    layered = addLayers taskNodes

    -- Add names back in
    nameMap = Map.fromFoldable $ tasks <#> \t -> Tuple t.id t.name
  in
    layered <#> \l ->
      { id: l.id
      , name: fromMaybe l.id $ Map.lookup l.id nameMap
      , layer: l.layer
      , depends: l.depends
      }

-- | Visualize a single task as a circle with label
visualizeTask :: Number -> Number -> Number -> Int -> Int -> LayeredTask -> Array (T.Tree Unit)
visualizeTask totalWidth totalHeight layerHeight tasksInLayerCount indexInLayer task =
  let
    layerWidth = totalWidth - 100.0
    xPos =
      if tasksInLayerCount > 1 then (toNumber indexInLayer * layerWidth / toNumber (tasksInLayerCount - 1)) + 50.0
      else totalWidth / 2.0
    yPos = (toNumber task.layer * layerHeight) + 50.0
  in
    [ -- Circle for task
      T.elem Circle
        [ cx $ num xPos
        , cy $ num yPos
        , r $ num 25.0
        , fill $ text "#4CAF50"
        , stroke $ text "#2E7D32"
        , strokeWidth $ num 2.0
        , attr "class" $ text "task-node"
        ]
    -- Task name label
    , T.elem Text
        [ x $ num xPos
        , y $ num (yPos + 5.0)
        , textContent $ text task.name
        , textAnchor $ text "middle"
        , fill $ text "#fff"
        , attr "class" $ text "task-label"
        ]
    ]

-- | Visualize a dependency link between two tasks
visualizeDependencyLink :: Number -> Number -> Number -> Array LayeredTask -> LayeredTask -> String -> Maybe (T.Tree Unit)
visualizeDependencyLink totalWidth totalHeight layerHeight allTasks task depId = do
  depTask <- Array.find (\t -> t.id == depId) allTasks

  let
    layerWidth = totalWidth - 100.0

    -- Source task (current task)
    tasksInSourceLayer = filter (\t -> t.layer == task.layer) allTasks
    sourceCount = length tasksInSourceLayer
    sourceIndex = fromMaybe 0 $ Array.findIndex (\t -> t.id == task.id) tasksInSourceLayer
    sourceX =
      if sourceCount > 1 then (toNumber sourceIndex * layerWidth / toNumber (sourceCount - 1)) + 50.0
      else totalWidth / 2.0
    sourceY = (toNumber task.layer * layerHeight) + 50.0

    -- Target task (dependency)
    tasksInTargetLayer = filter (\t -> t.layer == depTask.layer) allTasks
    targetCount = length tasksInTargetLayer
    targetIndex = fromMaybe 0 $ Array.findIndex (\t -> t.id == depTask.id) tasksInTargetLayer
    targetX =
      if targetCount > 1 then (toNumber targetIndex * layerWidth / toNumber (targetCount - 1)) + 50.0
      else totalWidth / 2.0
    targetY = (toNumber depTask.layer * layerHeight) + 50.0

  pure $ T.elem Line
    [ x1 $ num targetX
    , y1 $ num targetY
    , x2 $ num sourceX
    , y2 $ num sourceY
    , stroke $ text "#bbb"
    , strokeWidth $ num 2.0
    , attr "class" $ text "dependency-link"
    ]

-- | Draw topological sort visualization
drawTopologicalSort :: String -> Effect Unit
drawTopologicalSort containerSelector = runD3v2M do
  container <- select containerSelector :: _ (D3v2Selection_ SEmpty Element Unit)

  let
    totalWidth = 800.0
    totalHeight = 500.0

    layeredTasks = addLayersWithNames buildPipelineTasks
    layerValues = layeredTasks <#> _.layer
    maxLayer = fromMaybe 0 $ maximum layerValues
    layerHeight =
      if maxLayer > 0 then (totalHeight - 100.0) / toNumber maxLayer
      else 100.0

    -- Generate all dependency links
    allLinks :: Array (T.Tree Unit)
    allLinks = concat $ layeredTasks <#> \task ->
      Array.catMaybes $ task.depends <#> \depId ->
        visualizeDependencyLink totalWidth totalHeight layerHeight layeredTasks task depId

    -- Generate all task nodes
    allNodes :: Array (T.Tree Unit)
    allNodes = concat $ range 0 maxLayer <#> \layer ->
      let
        tasksInLayer = filter (\t -> t.layer == layer) layeredTasks
        count = length tasksInLayer
      in
        concat $ mapWithIndex
          ( \idx task ->
              visualizeTask totalWidth totalHeight layerHeight count idx task
          )
          tasksInLayer

    -- Generate layer labels
    layerLabels :: Array (T.Tree Unit)
    layerLabels = range 0 maxLayer <#> \layer ->
      T.elem Text
        [ x $ num 20.0
        , y $ num ((toNumber layer * layerHeight) + 55.0)
        , textContent $ text ("L" <> show layer)
        , textAnchor $ text "start"
        , fill $ text "#666"
        , attr "class" $ text "layer-label"
        ]

    -- Main SVG tree
    vizTree =
      T.named SVG "svg"
        [ width $ num totalWidth
        , height $ num totalHeight
        , viewBox 0.0 0.0 800.0 500.0
        , attr "class" $ text "topological-sort"
        ]
        `T.withChildren`
          (allLinks <> allNodes <> layerLabels)

  _ <- renderTree container vizTree
  pure unit
