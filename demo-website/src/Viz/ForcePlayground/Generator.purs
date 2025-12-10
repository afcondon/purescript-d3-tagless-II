-- | Random Graph Generator
-- |
-- | Generates synthetic network data for demonstrating force-directed layouts.
-- | Creates a mix of connected and disjoint subgraphs with rich attributes
-- | suitable for filtering, sizing, and coloring.
module D3.Viz.ForcePlayground.Generator
  ( GeneratorConfig
  , defaultConfig
  , NodeCategory(..)
  , LinkType(..)
  , GeneratedNode
  , GeneratedLink
  , GeneratedGraph
  , generateGraph
  , categoryToInt
  , linkTypeToInt
  ) where

import Prelude

import Data.Array (concat, filter, length, mapWithIndex, replicate, (..))
import Data.Array as Array
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Random (randomInt, random)

-- | Node categories for visual encoding
data NodeCategory
  = Research    -- Academic/research nodes
  | Industry    -- Industry/commercial nodes
  | Government  -- Government/regulatory nodes
  | Community   -- Community/social nodes

derive instance eqNodeCategory :: Eq NodeCategory

-- | Convert category to integer for JSON/coloring
categoryToInt :: NodeCategory -> Int
categoryToInt = case _ of
  Research -> 0
  Industry -> 1
  Government -> 2
  Community -> 3

-- | Link types for filtering
data LinkType
  = Collaboration  -- Working together
  | Citation       -- References/citations
  | Funding        -- Financial connections
  | Communication  -- Information flow

derive instance eqLinkType :: Eq LinkType

-- | Convert link type to integer
linkTypeToInt :: LinkType -> Int
linkTypeToInt = case _ of
  Collaboration -> 0
  Citation -> 1
  Funding -> 2
  Communication -> 3

-- | Generated node with rich attributes
type GeneratedNode =
  { id :: Int
  , name :: String
  , category :: Int       -- 0-3, maps to NodeCategory
  , sizeClass :: Int      -- 0-2 (small, medium, large)
  , importance :: Number  -- 0.0-1.0, can drive node radius
  , subgraph :: Int       -- Which subgraph this belongs to
  }

-- | Generated link with rich attributes
type GeneratedLink =
  { source :: Int
  , target :: Int
  , weight :: Number      -- 0.0-1.0, can drive link width
  , linkType :: Int       -- 0-3, maps to LinkType
  , distance :: Number    -- Preferred distance (can drive forceLink)
  }

-- | Complete generated graph
type GeneratedGraph =
  { nodes :: Array GeneratedNode
  , links :: Array GeneratedLink
  }

-- | Configuration for graph generation
type GeneratorConfig =
  { minSubgraphs :: Int      -- Minimum number of subgraphs
  , maxSubgraphs :: Int      -- Maximum number of subgraphs
  , minNodesPerSubgraph :: Int
  , maxNodesPerSubgraph :: Int
  , internalConnectivity :: Number  -- 0.0-1.0, density within subgraphs
  , bridgeCount :: Int       -- How many subgraphs to connect together
  , bridgeLinks :: Int       -- Links per bridge connection
  }

-- | Default configuration
defaultConfig :: GeneratorConfig
defaultConfig =
  { minSubgraphs: 20
  , maxSubgraphs: 30
  , minNodesPerSubgraph: 3
  , maxNodesPerSubgraph: 10
  , internalConnectivity: 0.3  -- 30% of possible edges
  , bridgeCount: 8             -- Connect ~8 subgraphs
  , bridgeLinks: 2             -- 2 links per bridge
  }

-- | Generate a complete graph
generateGraph :: GeneratorConfig -> Effect GeneratedGraph
generateGraph config = do
  -- Determine number of subgraphs
  numSubgraphs <- randomInt config.minSubgraphs config.maxSubgraphs

  -- Generate each subgraph
  subgraphs <- generateSubgraphs config numSubgraphs

  -- Flatten into single graph with adjusted IDs
  let { nodes, links, offsets } = flattenSubgraphs subgraphs

  -- Add bridge connections between some subgraphs
  bridgeLinks <- generateBridges config offsets nodes

  pure { nodes, links: links <> bridgeLinks }

-- | Internal: subgraph before flattening
type Subgraph =
  { nodes :: Array GeneratedNode
  , links :: Array GeneratedLink
  }

-- | Generate all subgraphs
generateSubgraphs :: GeneratorConfig -> Int -> Effect (Array Subgraph)
generateSubgraphs config count = do
  Array.foldM
    (\acc i -> do
      sg <- generateSubgraph config i
      pure (acc <> [sg])
    )
    []
    (0 .. (count - 1))

-- | Generate a single subgraph
generateSubgraph :: GeneratorConfig -> Int -> Effect Subgraph
generateSubgraph config subgraphId = do
  -- Random node count
  nodeCount <- randomInt config.minNodesPerSubgraph config.maxNodesPerSubgraph

  -- Pick a dominant category for this subgraph (with some variation)
  dominantCategory <- randomInt 0 3

  -- Generate nodes
  nodes <- generateNodes nodeCount subgraphId dominantCategory

  -- Generate internal links
  links <- generateInternalLinks nodes config.internalConnectivity

  pure { nodes, links }

-- | Generate nodes for a subgraph
generateNodes :: Int -> Int -> Int -> Effect (Array GeneratedNode)
generateNodes count subgraphId dominantCategory = do
  Array.foldM
    (\acc i -> do
      node <- generateNode i subgraphId dominantCategory
      pure (acc <> [node])
    )
    []
    (0 .. (count - 1))

-- | Generate a single node
generateNode :: Int -> Int -> Int -> Effect GeneratedNode
generateNode localId subgraphId dominantCategory = do
  -- 70% chance of dominant category, 30% chance of random
  useDominant <- random
  category <- if useDominant < 0.7
    then pure dominantCategory
    else randomInt 0 3

  sizeClass <- randomInt 0 2
  importance <- random

  let id = localId  -- Will be adjusted during flattening
  let name = "Node " <> show subgraphId <> "-" <> show localId

  pure { id, name, category, sizeClass, importance, subgraph: subgraphId }

-- | Generate internal links within a subgraph
generateInternalLinks :: Array GeneratedNode -> Number -> Effect (Array GeneratedLink)
generateInternalLinks nodes connectivity = do
  let n = length nodes
  if n < 2
    then pure []
    else do
      -- Generate all possible pairs
      let pairs = do
            i <- 0 .. (n - 2)
            j <- (i + 1) .. (n - 1)
            pure (Tuple i j)

      -- Randomly include based on connectivity
      Array.foldM
        (\acc (Tuple i j) -> do
          include <- random
          if include < connectivity
            then do
              link <- generateLink i j
              pure (acc <> [link])
            else pure acc
        )
        []
        pairs

-- | Generate a bimodal weight (either weak ~0.15-0.3 or strong ~0.7-1.0)
-- | This makes the "weighted links" toggle effect more dramatic
generateBimodalWeight :: Effect Number
generateBimodalWeight = do
  r <- random
  if r < 0.5
    then do
      -- Weak link: 0.15 to 0.35
      w <- random
      pure (0.15 + w * 0.2)
    else do
      -- Strong link: 0.7 to 1.0
      w <- random
      pure (0.7 + w * 0.3)

-- | Generate a single link
generateLink :: Int -> Int -> Effect GeneratedLink
generateLink source target = do
  weight <- generateBimodalWeight
  linkType <- randomInt 0 3
  -- Distance inversely related to weight (stronger = closer)
  let distance = 30.0 + (1.0 - weight) * 70.0

  pure { source, target, weight, linkType, distance }

-- | Flatten subgraphs into single graph, adjusting IDs
flattenSubgraphs :: Array Subgraph -> { nodes :: Array GeneratedNode, links :: Array GeneratedLink, offsets :: Array Int }
flattenSubgraphs subgraphs =
  let
    -- Calculate ID offsets for each subgraph
    offsets = foldl
      (\acc _ ->
        let lastOffset = case Array.last acc of
              Nothing -> 0
              Just o -> o
            prevCount = case Array.index subgraphs (length acc - 1) of
              Nothing -> 0
              Just prev -> length prev.nodes
        in acc <> [lastOffset + prevCount]
      )
      [0]
      (Array.take (length subgraphs - 1) subgraphs)

    -- Adjust node IDs
    adjustedNodes = concat $ mapWithIndex
      (\i sg ->
        let offset = case Array.index offsets i of
              Nothing -> 0
              Just o -> o
        in map (\n -> n { id = n.id + offset }) sg.nodes
      )
      subgraphs

    -- Adjust link source/target IDs
    adjustedLinks = concat $ mapWithIndex
      (\i sg ->
        let offset = case Array.index offsets i of
              Nothing -> 0
              Just o -> o
        in map (\l -> l { source = l.source + offset, target = l.target + offset }) sg.links
      )
      subgraphs
  in
    { nodes: adjustedNodes, links: adjustedLinks, offsets }

-- | Generate bridge links between subgraphs
generateBridges :: GeneratorConfig -> Array Int -> Array GeneratedNode -> Effect (Array GeneratedLink)
generateBridges config offsets nodes = do
  let numSubgraphs = length offsets
  if numSubgraphs < 2
    then pure []
    else do
      -- Pick which subgraphs to bridge
      bridgePairs <- pickBridgePairs numSubgraphs config.bridgeCount

      -- Generate links for each bridge
      Array.foldM
        (\acc (Tuple sg1 sg2) -> do
          links <- generateBridgeLinks offsets nodes sg1 sg2 config.bridgeLinks
          pure (acc <> links)
        )
        []
        bridgePairs

-- | Pick random pairs of subgraphs to bridge
pickBridgePairs :: Int -> Int -> Effect (Array (Tuple Int Int))
pickBridgePairs numSubgraphs count = do
  Array.foldM
    (\acc _ -> do
      sg1 <- randomInt 0 (numSubgraphs - 1)
      sg2 <- randomInt 0 (numSubgraphs - 1)
      if sg1 /= sg2
        then pure (acc <> [Tuple (min sg1 sg2) (max sg1 sg2)])
        else pure acc  -- Skip if same subgraph
    )
    []
    (replicate (count * 2) unit)  -- Try more times to get enough valid pairs

-- | Generate bridge links between two subgraphs
generateBridgeLinks :: Array Int -> Array GeneratedNode -> Int -> Int -> Int -> Effect (Array GeneratedLink)
generateBridgeLinks _ nodes sg1 sg2 count = do
  let
    nodesInSg1 = filter (\n -> n.subgraph == sg1) nodes
    nodesInSg2 = filter (\n -> n.subgraph == sg2) nodes

  if length nodesInSg1 == 0 || length nodesInSg2 == 0
    then pure []
    else do
      Array.foldM
        (\acc _ -> do
          -- Pick random nodes from each subgraph
          idx1 <- randomInt 0 (length nodesInSg1 - 1)
          idx2 <- randomInt 0 (length nodesInSg2 - 1)
          case Tuple (Array.index nodesInSg1 idx1) (Array.index nodesInSg2 idx2) of
            Tuple (Just n1) (Just n2) -> do
              link <- generateLink n1.id n2.id
              -- Bridge links are typically weaker/longer
              let bridgeLink = link { weight = link.weight * 0.5, distance = link.distance * 1.5 }
              pure (acc <> [bridgeLink])
            _ -> pure acc
        )
        []
        (replicate count unit)
