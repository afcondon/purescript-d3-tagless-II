module PSD3.Data.Graph
  ( GraphModel
  , GraphMaps
  , GraphConfig
  , buildGraphModel
  , emptyGraphModel
  , addNode
  , addLink
  , getNode
  , getNodesByIds
  , getLinksFrom
  , getLinksTo
  , getAllNodes
  , getAllLinks
  ) where

import Prelude

import Data.Array (catMaybes, foldl, filter)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import PSD3.Data.Node (NodeID)

-- | Generic graph model parameterized by node and link types
-- | Provides efficient access to nodes and links via multiple indexes
type GraphModel node link =
  { nodes :: Array node
  , links :: Array link
  , maps :: GraphMaps node link
  }

-- | Index structures for efficient graph queries
type GraphMaps node link =
  { nodeById :: Map NodeID node
  , linksBySource :: Map NodeID (Array link)
  , linksByTarget :: Map NodeID (Array link)
  , nodeIds :: Set NodeID
  }

-- | Configuration for building a graph model
-- | Provides functions to extract IDs from nodes and links
type GraphConfig node link =
  { getNodeId :: node -> NodeID
  , getLinkSource :: link -> NodeID
  , getLinkTarget :: link -> NodeID
  }

-- | Build a complete graph model from arrays of nodes and links
-- |
-- | This function constructs all the necessary indexes for efficient
-- | graph queries. It ensures that:
-- | - All nodes are indexed by ID
-- | - Links are indexed by both source and target
-- | - A set of all node IDs is maintained
-- |
-- | Example:
-- | ```purescript
-- | let config = { getNodeId: _.id
-- |              , getLinkSource: _.source
-- |              , getLinkTarget: _.target
-- |              }
-- | let graph = buildGraphModel config nodes links
-- | ```
buildGraphModel :: forall node link.
  GraphConfig node link ->
  Array node ->
  Array link ->
  GraphModel node link
buildGraphModel config nodes links =
  let
    -- Build node index
    nodeById = M.fromFoldable $ (\n -> Tuple (config.getNodeId n) n) <$> nodes

    -- Build node ID set
    nodeIds = Set.fromFoldable $ config.getNodeId <$> nodes

    -- Build link indexes by source
    linksBySource = foldl
      (\acc link ->
        let sourceId = config.getLinkSource link
        in M.alter (addToArray link) sourceId acc
      )
      M.empty
      links

    -- Build link indexes by target
    linksByTarget = foldl
      (\acc link ->
        let targetId = config.getLinkTarget link
        in M.alter (addToArray link) targetId acc
      )
      M.empty
      links

    maps =
      { nodeById
      , linksBySource
      , linksByTarget
      , nodeIds
      }
  in
    { nodes, links, maps }
  where
    addToArray :: forall a. a -> Maybe (Array a) -> Maybe (Array a)
    addToArray item Nothing = Just [item]
    addToArray item (Just arr) = Just (arr <> [item])

-- | Create an empty graph model
emptyGraphModel :: forall node link. GraphModel node link
emptyGraphModel =
  { nodes: []
  , links: []
  , maps:
      { nodeById: M.empty
      , linksBySource: M.empty
      , linksByTarget: M.empty
      , nodeIds: Set.empty
      }
  }

-- | Add a node to an existing graph model
-- | Note: Does not update links; use buildGraphModel for complete rebuilds
addNode :: forall node link.
  GraphConfig node link ->
  node ->
  GraphModel node link ->
  GraphModel node link
addNode config node model =
  let
    nodeId = config.getNodeId node
    newNodes = model.nodes <> [node]
    newMaps = model.maps
      { nodeById = M.insert nodeId node model.maps.nodeById
      , nodeIds = Set.insert nodeId model.maps.nodeIds
      }
  in
    model { nodes = newNodes, maps = newMaps }

-- | Add a link to an existing graph model
-- | Note: Does not validate that source/target nodes exist
addLink :: forall node link.
  GraphConfig node link ->
  link ->
  GraphModel node link ->
  GraphModel node link
addLink config link model =
  let
    sourceId = config.getLinkSource link
    targetId = config.getLinkTarget link
    newLinks = model.links <> [link]

    newLinksBySource = M.alter (addToArray link) sourceId model.maps.linksBySource
    newLinksByTarget = M.alter (addToArray link) targetId model.maps.linksByTarget

    newMaps = model.maps
      { linksBySource = newLinksBySource
      , linksByTarget = newLinksByTarget
      }
  in
    model { links = newLinks, maps = newMaps }
  where
    addToArray :: forall a. a -> Maybe (Array a) -> Maybe (Array a)
    addToArray item Nothing = Just [item]
    addToArray item (Just arr) = Just (arr <> [item])

-- | Get a node by its ID
getNode :: forall node link.
  NodeID ->
  GraphModel node link ->
  Maybe node
getNode nodeId model = M.lookup nodeId model.maps.nodeById

-- | Get multiple nodes by their IDs
-- | Returns only the nodes that exist (filters out missing IDs)
getNodesByIds :: forall node link.
  Array NodeID ->
  GraphModel node link ->
  Array node
getNodesByIds ids model =
  catMaybes $ (\id -> M.lookup id model.maps.nodeById) <$> ids

-- | Get all links originating from a node (by source)
getLinksFrom :: forall node link.
  NodeID ->
  GraphModel node link ->
  Array link
getLinksFrom nodeId model =
  case M.lookup nodeId model.maps.linksBySource of
    Nothing -> []
    Just links -> links

-- | Get all links pointing to a node (by target)
getLinksTo :: forall node link.
  NodeID ->
  GraphModel node link ->
  Array link
getLinksTo nodeId model =
  case M.lookup nodeId model.maps.linksByTarget of
    Nothing -> []
    Just links -> links

-- | Get all nodes in the graph
getAllNodes :: forall node link.
  GraphModel node link ->
  Array node
getAllNodes = _.nodes

-- | Get all links in the graph
getAllLinks :: forall node link.
  GraphModel node link ->
  Array link
getAllLinks = _.links
