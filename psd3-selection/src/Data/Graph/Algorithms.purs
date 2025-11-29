module Data.Graph.Algorithms
  ( -- Types
    SimpleGraph
  , TaskNode
  , LayeredNode
  -- Reachability
  , reachableFrom
  -- Topological sort
  , buildGraph
  , getTopologicalOrder
  , computeLayers
  , addLayers
  -- Transitive reduction
  , transitiveReduction
  , getRemovedEdges
  , getAllEdges
  -- Conversion
  , taskNodesToSimpleGraph
  , simpleGraphToTaskNodes
  ) where

-- | Graph algorithms for DAGs (Directed Acyclic Graphs)
-- | Provides topological sort, transitive reduction, and reachability analysis

import Prelude

import Data.Array (concat, foldl, (\\))
import Data.Array as Array
import Data.Foldable (maximum)
import Data.Graph (Graph, fromMap, topologicalSort)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))

-- =============================================================================
-- || Simple Graph Representation (Adjacency List)
-- =============================================================================

-- | Simple graph as adjacency list (node -> set of targets)
type SimpleGraph node =
  { nodes :: Array node
  , edges :: Map node (Set node)
  }

-- =============================================================================
-- || Reachability Analysis
-- =============================================================================

-- | Compute all nodes reachable from a given node via BFS
-- | Uses tail recursion for efficiency
reachableFrom :: forall node. Ord node => node -> SimpleGraph node -> Set node
reachableFrom start graph =
  go Set.empty (Set.singleton start)
  where
    go :: Set node -> Set node -> Set node
    go visited frontier
      | Set.isEmpty frontier = visited
      | otherwise =
          let
            -- Get immediate neighbors of all frontier nodes
            neighbors = Set.unions $ Array.catMaybes $
              Set.toUnfoldable frontier <#> \n ->
                Map.lookup n graph.edges

            -- Only visit new nodes
            newNodes = Set.difference neighbors visited
          in
            go (Set.union visited frontier) newNodes

-- =============================================================================
-- || Topological Sort with Layers
-- =============================================================================

-- | A node with dependencies
type TaskNode node =
  { id :: node
  , depends :: Array node
  }

-- | A node with computed layer information
type LayeredNode node =
  { id :: node
  , layer :: Int
  , depends :: Array node
  }

-- | Build a Data.Graph from task nodes
-- | Graph k v = Graph (Map k (Tuple v (List k)))
buildGraph :: forall node. Ord node => Array (TaskNode node) -> Graph node (TaskNode node)
buildGraph tasks =
  let
    graphMap :: Map node (Tuple (TaskNode node) (List node))
    graphMap = foldl addTask Map.empty tasks

    addTask :: Map node (Tuple (TaskNode node) (List node)) -> TaskNode node -> Map node (Tuple (TaskNode node) (List node))
    addTask acc task =
      Map.insert task.id (Tuple task (List.fromFoldable task.depends)) acc
  in
    fromMap graphMap

-- | Get topological order using Data.Graph.topologicalSort
getTopologicalOrder :: forall node. Ord node => Array (TaskNode node) -> List node
getTopologicalOrder tasks =
  let graph = buildGraph tasks
  in topologicalSort graph

-- | Compute layers from topological sort
-- | Layer 0: nodes with no dependencies
-- | Layer N: nodes whose dependencies are all in layers < N
computeLayers :: forall node. Ord node => Array (TaskNode node) -> Map node Int
computeLayers tasks =
  let
    -- Reverse to get dependencies first
    sortedIds = List.reverse $ getTopologicalOrder tasks
    taskMap = Map.fromFoldable $ tasks <#> \t -> Tuple t.id t

    processTask :: Map node Int -> node -> Map node Int
    processTask accLayers taskId =
      case Map.lookup taskId taskMap of
        Nothing -> accLayers
        Just task ->
          let
            layer =
              if Array.null task.depends
                then 0
                else
                  -- All dependencies were processed earlier (topological order!)
                  let depLayers = task.depends <#> \depId ->
                        fromMaybe 0 $ Map.lookup depId accLayers
                  in case maximum depLayers of
                       Just maxLayer -> maxLayer + 1
                       Nothing -> 0
          in Map.insert taskId layer accLayers
  in
    foldl processTask Map.empty (List.toUnfoldable sortedIds :: Array node)

-- | Add layer information to task nodes
addLayers :: forall node. Ord node => Array (TaskNode node) -> Array (LayeredNode node)
addLayers tasks =
  let layers = computeLayers tasks
  in tasks <#> \t ->
       { id: t.id
       , layer: fromMaybe 0 $ Map.lookup t.id layers
       , depends: t.depends
       }

-- =============================================================================
-- || Transitive Reduction
-- =============================================================================

-- | Remove transitive edges from a graph
-- | An edge A → C is transitive if there exists a path A → B → C
-- | Returns a new graph with only the essential (non-transitive) edges
transitiveReduction :: forall node. Ord node => SimpleGraph node -> SimpleGraph node
transitiveReduction graph =
  { nodes: graph.nodes
  , edges: Map.fromFoldable $ map reduceEdge $ (Map.toUnfoldable graph.edges :: Array (Tuple node (Set node)))
  }
  where
    reduceEdge :: Tuple node (Set node) -> Tuple node (Set node)
    reduceEdge (Tuple source targets) =
      Tuple source (Set.filter (not <<< isTransitive source) targets)

    isTransitive :: node -> node -> Boolean
    isTransitive source target =
      let
        -- Get all intermediate nodes (direct targets except this one)
        intermediates = Set.delete target $ fromMaybe Set.empty $ Map.lookup source graph.edges

        -- Check if target is reachable through any intermediate
        reachableThroughIntermediate =
          Array.any (\intermediate ->
            Set.member target $ reachableFrom intermediate graph
          ) (Set.toUnfoldable intermediates)
      in
        reachableThroughIntermediate

-- | Get edges that were removed by transitive reduction
getRemovedEdges :: forall node. Ord node => SimpleGraph node -> SimpleGraph node -> Array (Tuple node node)
getRemovedEdges original reduced =
  let
    originalEdges = getAllEdges original
    reducedEdges = getAllEdges reduced
  in
    originalEdges \\ reducedEdges

-- | Get all edges from a graph as an array of tuples
getAllEdges :: forall node. Ord node => SimpleGraph node -> Array (Tuple node node)
getAllEdges graph =
  concat $ graph.nodes <#> \source ->
    case Map.lookup source graph.edges of
      Nothing -> []
      Just targets -> Set.toUnfoldable targets <#> \target ->
        Tuple source target

-- =============================================================================
-- || Conversion Utilities
-- =============================================================================

-- | Convert TaskNode array to SimpleGraph
taskNodesToSimpleGraph :: forall node. Ord node => Array (TaskNode node) -> SimpleGraph node
taskNodesToSimpleGraph tasks =
  { nodes: tasks <#> _.id
  , edges: Map.fromFoldable $ tasks <#> \task ->
      Tuple task.id (Set.fromFoldable task.depends)
  }

-- | Convert SimpleGraph to TaskNode array
simpleGraphToTaskNodes :: forall node. Ord node => SimpleGraph node -> Array (TaskNode node)
simpleGraphToTaskNodes graph =
  graph.nodes <#> \node ->
    { id: node
    , depends: case Map.lookup node graph.edges of
        Nothing -> []
        Just targets -> Set.toUnfoldable targets
    }
