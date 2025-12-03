module PSD3.Data.Graph.Algorithms
  ( ReachabilityResult
  , getReachableNodes
  , extractSpanningTree
  , depthFirstSearch
  , breadthFirstSearch
  ) where

import Prelude

import Data.Array (filter, head, null, partition, uncons, (:))
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tree (Tree, mkTree)
import Data.Tuple (Tuple(..))
import PSD3.Data.Graph (GraphModel, GraphConfig, getLinksFrom)
import PSD3.Data.Node (NodeID)

-- | Result of reachability analysis from a root node
-- | Includes all reachable nodes, the spanning tree edges,
-- | and any redundant (non-tree) edges
type ReachabilityResult =
  { nodes :: Set NodeID                      -- All reachable node IDs
  , spanningTree :: Array (Tuple NodeID NodeID)  -- Tree edges
  , redundantEdges :: Array (Tuple NodeID NodeID) -- Non-tree edges (back/cross/forward edges)
  , paths :: Array (Array NodeID)            -- All paths from root (closed paths)
  }

-- | Compute all nodes reachable from a starting node via directed edges
-- |
-- | This performs a breadth-first traversal, building a spanning tree
-- | of the reachable portion of the graph. Any edges that would create
-- | cycles are marked as redundant.
-- |
-- | Example:
-- | ```purescript
-- | let result = getReachableNodes config rootId graph
-- | -- result.nodes: Set of all reachable node IDs
-- | -- result.spanningTree: Edges forming a tree
-- | -- result.redundantEdges: Edges that create cycles
-- | ```
getReachableNodes :: forall node link.
  GraphConfig node link ->
  NodeID ->
  GraphModel node link ->
  ReachabilityResult
getReachableNodes config rootId graph =
  let
    initialState =
      { nodes: Set.empty
      , spanningTree: []
      , redundantEdges: []
      , paths: []
      , openPaths: [[rootId]]
      }
    finalState = go initialState
  in
    { nodes: finalState.nodes
    , spanningTree: finalState.spanningTree
    , redundantEdges: finalState.redundantEdges
    , paths: finalState.paths
    }
  where
    go state
      | null state.openPaths = state
      | otherwise = case processNextPath state of
          Nothing -> state
          Just state' -> go state'

    processNextPath state = do
      x <- uncons state.openPaths
      currentId <- head x.head  -- Head of current path

      -- Get outgoing links from this node
      let links = getLinksFrom currentId graph
          targetIds = config.getLinkTarget <$> links

      -- Partition into new nodes vs already-visited
      let newTargets = partition (\targetId -> not $ Set.member targetId state.nodes) targetIds
          newPaths = newTargets.yes <#> \targetId -> targetId : x.head
          prunedEdges = newTargets.no <#> \targetId -> Tuple currentId targetId

      -- If no new children, close this path
      if null newPaths
        then Just $ state
          { openPaths = x.tail
          , paths = x.head : state.paths
          , redundantEdges = state.redundantEdges <> prunedEdges
          }
        -- Otherwise extend the path
        else Just $ state
          { openPaths = x.tail <> newPaths
          , nodes = state.nodes <> Set.fromFoldable newTargets.yes
          , spanningTree = state.spanningTree <> ((\targetId -> Tuple currentId targetId) <$> newTargets.yes)
          , redundantEdges = state.redundantEdges <> prunedEdges
          }

-- | Extract a spanning tree from reachability analysis
-- |
-- | Converts the spanning tree edges into a Tree structure
-- | rooted at the specified node.
extractSpanningTree :: NodeID -> ReachabilityResult -> Tree NodeID
extractSpanningTree rootId result =
  buildTree rootId result.spanningTree
  where
    buildTree :: NodeID -> Array (Tuple NodeID NodeID) -> Tree NodeID
    buildTree nodeId edges =
      let
        -- Find all children of this node
        children = edges
          # filter (\(Tuple source _) -> source == nodeId)
          # map (\(Tuple _ target) -> target)

        -- Recursively build subtrees
        childTrees = buildTree <$> children <@> edges
      in
        mkTree nodeId (L.fromFoldable childTrees)

-- | Depth-first search traversal
-- |
-- | Visits nodes in depth-first order, applying a predicate
-- | to decide whether to continue down each branch.
-- |
-- | Returns nodes in the order they were visited.
depthFirstSearch :: forall node link.
  GraphConfig node link ->
  NodeID ->
  GraphModel node link ->
  (node -> Boolean) ->           -- Visit predicate
  Array NodeID
depthFirstSearch config startId graph shouldVisit =
  go [startId] Set.empty []
  where
    go stack visited result = case uncons stack of
      Nothing -> result
      Just { head: nodeId, tail: rest }
        | Set.member nodeId visited -> go rest visited result
        | otherwise -> case getNode nodeId of
            Nothing -> go rest visited result
            Just node ->
              if shouldVisit node
                then
                  let
                    newVisited = Set.insert nodeId visited
                    links = getLinksFrom nodeId graph
                    children = config.getLinkTarget <$> links
                    -- DFS: push children to front
                    newStack = children <> rest
                  in
                    go newStack newVisited (result <> [nodeId])
                else
                  go rest visited result

    getNode nodeId = do
      let nodes = graph.nodes
      nodes # find (\n -> config.getNodeId n == nodeId)

    find :: forall a. (a -> Boolean) -> Array a -> Maybe a
    find _ [] = Nothing
    find pred arr = case uncons arr of
      Nothing -> Nothing
      Just { head: x, tail: xs } ->
        if pred x then Just x else find pred xs

-- | Breadth-first search traversal
-- |
-- | Visits nodes level-by-level, applying a predicate
-- | to decide whether to continue exploring.
-- |
-- | Returns nodes in the order they were discovered.
breadthFirstSearch :: forall node link.
  GraphConfig node link ->
  NodeID ->
  GraphModel node link ->
  (node -> Boolean) ->           -- Visit predicate
  Array NodeID
breadthFirstSearch config startId graph shouldVisit =
  go [startId] Set.empty []
  where
    go queue visited result = case uncons queue of
      Nothing -> result
      Just { head: nodeId, tail: rest }
        | Set.member nodeId visited -> go rest visited result
        | otherwise -> case getNode nodeId of
            Nothing -> go rest visited result
            Just node ->
              if shouldVisit node
                then
                  let
                    newVisited = Set.insert nodeId visited
                    links = getLinksFrom nodeId graph
                    children = config.getLinkTarget <$> links
                    -- BFS: append children to end
                    newQueue = rest <> children
                  in
                    go newQueue newVisited (result <> [nodeId])
                else
                  go rest visited result

    getNode nodeId = do
      let nodes = graph.nodes
      nodes # find (\n -> config.getNodeId n == nodeId)

    find :: forall a. (a -> Boolean) -> Array a -> Maybe a
    find _ [] = Nothing
    find pred arr = case uncons arr of
      Nothing -> Nothing
      Just { head: x, tail: xs } ->
        if pred x then Just x else find pred xs
