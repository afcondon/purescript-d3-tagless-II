-- | DataViz.Layout.Sankey.MarkovChainIntegration
-- |
-- | Integration layer between the Markov Chain ordering algorithm and the existing
-- | Sankey diagram types. Provides functions to compute node orderings using the
-- | spectral method and apply them to SankeyNode/SankeyLink arrays.
module DataViz.Layout.Sankey.MarkovChainIntegration
  ( computeMarkovChainOrdering
  , applyOrderingToNodes
  , sankeyToLayerData
  , bestInNOrdering
  , OrderingResult
  , computeLayoutWithMarkovChain
  ) where

import Prelude

import Data.Array (filter, find, foldl, length, mapWithIndex, sortBy, (!!), (..))
import Data.Array as Array
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import DataViz.Layout.Sankey.Compute (computeLayoutWithConfig)
import DataViz.Layout.Sankey.MarkovChain (LayerEdge, LayerInfo, LayerOrdering, MarkovChainConfig, calculateWeightedCrossing, defaultMarkovChainConfig, markovChainOrdering)
import DataViz.Layout.Sankey.PartitionRefinement (defaultPartitionRefinementConfig, refineOrdering)
import DataViz.Layout.Sankey.Types (LinkCSVRow, NodeID(..), SankeyLayoutResult, SankeyLink, SankeyNode, defaultSankeyConfig)

-- | Result of the Markov Chain ordering computation
type OrderingResult =
  { orderings :: Array LayerOrdering          -- ^ Node orderings per layer (by local index)
  , positions :: Array (Array Number)         -- ^ Position values per layer
  , weightedCrossing :: Number                -- ^ Total weighted crossing metric
  , layers :: Array LayerInfo                 -- ^ Layer information
  , nodeIndexToLocal :: Map.Map NodeID Int    -- ^ Map from global NodeID to local layer index
  , localToNodeIndex :: Array (Array NodeID)  -- ^ Map from (layer, localIndex) to NodeID
  }

-- | Convert Sankey nodes and links to the layer-based format needed by MarkovChain
sankeyToLayerData
  :: Array SankeyNode
  -> Array SankeyLink
  -> { layers :: Array LayerInfo
     , edgesByLayer :: Array (Array LayerEdge)
     , nodeIndexToLocal :: Map.Map NodeID Int
     , localToNodeIndex :: Array (Array NodeID)
     }
sankeyToLayerData nodes links =
  let
    -- Find maximum layer
    maxLayer = foldl (\acc n -> max acc n.layer) 0 nodes

    -- Group nodes by layer, maintaining order within layer by current y position
    nodesByLayer = map (\layerIdx ->
      sortBy (\a b -> compare a.y0 b.y0) $
        filter (\n -> n.layer == layerIdx) nodes
    ) (0 .. maxLayer)

    -- Build layer info
    layers = mapWithIndex (\_ layerNodes ->
      { nodeIndices: map (\n -> let (NodeID i) = n.index in i) layerNodes
      , nodeCount: length layerNodes
      }
    ) nodesByLayer

    -- Build maps from global NodeID to local index within layer
    -- and from (layer, localIndex) to NodeID
    { nodeToLocal, localToNode } = foldl
      (\acc layerIdx ->
        let
          layerNodes = fromMaybe [] (nodesByLayer !! layerIdx)
          layerMappings = mapWithIndex (\localIdx node ->
            Tuple node.index localIdx
          ) layerNodes
          updatedNodeToLocal = foldl
            (\m (Tuple nodeId localIdx) -> Map.insert nodeId localIdx m)
            acc.nodeToLocal
            layerMappings
          layerNodeIds = map _.index layerNodes
        in
          { nodeToLocal: updatedNodeToLocal
          , localToNode: Array.snoc acc.localToNode layerNodeIds
          }
      )
      { nodeToLocal: Map.empty, localToNode: [] }
      (0 .. maxLayer)

    -- Build edges for each layer gap (layer i to layer i+1)
    edgesByLayer = map (\layerIdx ->
      let
        -- Filter links from this layer to the next
        layerLinks = filter (\link ->
          case Array.find (\n -> n.index == link.sourceIndex) nodes of
            Just sourceNode -> sourceNode.layer == layerIdx
            Nothing -> false
        ) links

        -- Convert to LayerEdge format
        layerEdges = Array.mapMaybe (\link ->
          case Map.lookup link.sourceIndex nodeToLocal,
               Map.lookup link.targetIndex nodeToLocal of
            Just fromLocal, Just toLocal ->
              Just { fromLocal, toLocal, weight: link.value }
            _, _ -> Nothing
        ) layerLinks
      in layerEdges
    ) (0 .. (maxLayer - 1))

  in
    { layers
    , edgesByLayer
    , nodeIndexToLocal: nodeToLocal
    , localToNodeIndex: localToNode
    }

-- | Compute the Markov Chain ordering for a Sankey diagram
computeMarkovChainOrdering
  :: MarkovChainConfig
  -> Array SankeyNode
  -> Array SankeyLink
  -> OrderingResult
computeMarkovChainOrdering config nodes links =
  let
    { layers, edgesByLayer, nodeIndexToLocal, localToNodeIndex } =
      sankeyToLayerData nodes links

    -- Run the Markov Chain algorithm
    result = markovChainOrdering config layers edgesByLayer

    -- Calculate weighted crossing for the result
    crossing = calculateWeightedCrossing result.orderings edgesByLayer

  in
    { orderings: result.orderings
    , positions: result.positions
    , weightedCrossing: crossing
    , layers
    , nodeIndexToLocal
    , localToNodeIndex
    }

-- | Apply a computed ordering to reorder nodes within each layer
-- | This updates the y0, y1 positions based on the new ordering
-- | IMPORTANT: All nodes must be preserved - any nodes not in the ordering
-- | are appended at the end of their layer
applyOrderingToNodes
  :: OrderingResult
  -> Array SankeyNode
  -> Number              -- ^ y0 (top of canvas)
  -> Number              -- ^ y1 (bottom of canvas)
  -> Number              -- ^ padding between nodes
  -> Array SankeyNode
applyOrderingToNodes orderingResult nodes y0 y1 padding =
  let
    -- Calculate ky (scale factor) based on most constrained layer
    ky = calculateKy nodes y0 y1 padding

    -- Find the maximum layer
    maxLayer = foldl (\acc n -> max acc n.layer) 0 nodes

    -- Process each layer (not just those with orderings)
    updatedNodesByLayer = map (\layerIdx ->
      let
        -- Get ALL nodes in this layer
        layerNodes = filter (\n -> n.layer == layerIdx) nodes

        -- Get the ordering for this layer (if any)
        maybeOrdering = orderingResult.orderings !! layerIdx

        -- Get the NodeIDs in the new order
        localToNode = fromMaybe [] (orderingResult.localToNodeIndex !! layerIdx)

        -- Reorder the layer nodes according to the ordering
        -- Nodes not in the ordering are appended at the end
        orderedNodes = case maybeOrdering of
          Just ordering ->
            let
              -- Get nodes that ARE in the ordering
              orderedNodeIds = Array.mapMaybe (\localIdx -> localToNode !! localIdx) ordering
              nodesInOrder = Array.mapMaybe (\nodeId ->
                Array.find (\n -> n.index == nodeId) layerNodes
              ) orderedNodeIds

              -- Get nodes that are NOT in the ordering (missing from orderedNodeIds)
              nodesInOrderSet = map _.index nodesInOrder
              nodesNotInOrder = filter (\n -> not (Array.elem n.index nodesInOrderSet)) layerNodes
            in
              nodesInOrder <> nodesNotInOrder
          Nothing ->
            -- No ordering for this layer, keep original order
            layerNodes

        -- Stack nodes vertically with justification
        positioned = positionNodesInLayer orderedNodes ky padding y0 y1
      in positioned
    ) (0 .. maxLayer)

  in Array.concat updatedNodesByLayer

-- | Calculate the global scale factor (pixels per unit of value)
calculateKy :: Array SankeyNode -> Number -> Number -> Number -> Number
calculateKy nodes y0 y1 padding =
  let
    maxLayer = foldl (\acc n -> max acc n.layer) 0 nodes
    totalHeight = y1 - y0

    scaleForLayer layerIdx =
      let
        layerNodes = filter (\n -> n.layer == layerIdx) nodes
        totalValue = foldl (\sum n -> sum + n.value) 0.0 layerNodes
        numNodes = toNumber (length layerNodes)
        availableHeight = totalHeight - (numNodes - 1.0) * padding
      in if totalValue > 0.0 then availableHeight / totalValue else 1.0

    scales = map scaleForLayer (0 .. maxLayer)
    nonEmptyScales = filter (\s -> s > 0.0) scales
  in
    case Array.head nonEmptyScales of
      Just firstScale -> foldl min firstScale nonEmptyScales
      Nothing -> 1.0

-- | Position nodes in a single layer vertically with justification
positionNodesInLayer
  :: Array SankeyNode
  -> Number              -- ^ ky (scale factor)
  -> Number              -- ^ padding
  -> Number              -- ^ starting y position (y0)
  -> Number              -- ^ ending y position (y1)
  -> Array SankeyNode
positionNodesInLayer layerNodes ky padding startY endY =
  let
    -- Phase 1: Stack nodes from the top
    stacked = foldl
      (\acc node ->
        let
          nodeHeight = node.value * ky
          positioned = node { y0 = acc.y, y1 = acc.y + nodeHeight }
        in
          { y: acc.y + nodeHeight + padding
          , nodes: Array.snoc acc.nodes positioned
          }
      )
      { y: startY, nodes: [] }
      layerNodes

    -- Phase 2: Distribute extra space evenly (D3-style justification)
    lastY = stacked.y
    numNodes = length layerNodes
    extraSpacing = if numNodes > 0
      then (endY - lastY + padding) / (toNumber numNodes + 1.0)
      else 0.0

    justified = mapWithIndex
      (\i node ->
        let
          shift = extraSpacing * (toNumber (i + 1))
        in
          node { y0 = node.y0 + shift, y1 = node.y1 + shift }
      )
      stacked.nodes
  in justified

-- | Run multiple trials and return the best ordering
-- | This implements the "best-in-N" approach from the paper
bestInNOrdering
  :: Int                 -- ^ Number of trials
  -> MarkovChainConfig
  -> Array SankeyNode
  -> Array SankeyLink
  -> OrderingResult
bestInNOrdering numTrials config nodes links =
  let
    -- Run multiple trials
    results = map (\_ -> computeMarkovChainOrdering config nodes links) (0 .. (numTrials - 1))

    -- Find the one with minimum weighted crossing
    best = foldl
      (\acc result ->
        if result.weightedCrossing < acc.weightedCrossing
        then result
        else acc
      )
      (fromMaybe defaultResult (Array.head results))
      results

  in best
  where
  defaultResult = computeMarkovChainOrdering config nodes links

-- | Compute Sankey layout using Markov Chain ordering instead of the D3-style heuristic
-- | This is a drop-in replacement for computeLayout that uses the improved ordering algorithm
computeLayoutWithMarkovChain
  :: Array LinkCSVRow
  -> Number  -- ^ SVG width
  -> Number  -- ^ SVG height
  -> SankeyLayoutResult
computeLayoutWithMarkovChain linkInputs width height =
  let
    config = defaultSankeyConfig width height
    extent = config.extent

    -- First, run the standard layout to get initial node positions and values
    initialResult = computeLayoutWithConfig linkInputs config

    -- Compute the Markov Chain ordering
    markovResult = computeMarkovChainOrdering defaultMarkovChainConfig
      initialResult.nodes initialResult.links

    -- Apply Partition Refinement (Stage 2)
    { edgesByLayer } = sankeyToLayerData initialResult.nodes initialResult.links
    refinedResult = refineOrdering defaultPartitionRefinementConfig
      markovResult.orderings edgesByLayer

    -- Apply the refined ordering to reposition nodes
    reorderedNodes = applyOrderingToNodes
      (markovResult { orderings = refinedResult.bestOrderings })
      initialResult.nodes
      extent.y0
      extent.y1
      config.nodePadding

    -- Recalculate link positions based on new node positions
    reorderedLinks = calculateLinkPositions reorderedNodes initialResult.links

  in
    { nodes: reorderedNodes
    , links: reorderedLinks
    }

-- | Calculate link y0/y1 positions based on node positions
calculateLinkPositions :: Array SankeyNode -> Array SankeyLink -> Array SankeyLink
calculateLinkPositions nodes links =
  let
    -- Set y0 for each link (at source node)
    linksWithY0 = foldl
      (\currentLinks node ->
        let
          outgoingLinks = filter (\l -> l.sourceIndex == node.index) currentLinks
          sorted = sortBy
            (\a b ->
              case find (\n -> n.index == a.targetIndex) nodes,
                   find (\n -> n.index == b.targetIndex) nodes of
                Just na, Just nb -> compare na.y0 nb.y0
                _, _ -> EQ
            )
            outgoingLinks
          withY0 = foldl
            (\acc link ->
              let
                y = acc.y + link.width / 2.0
                updated = link { y0 = y }
              in
                { y: acc.y + link.width
                , links: Array.snoc acc.links updated
                }
            )
            { y: node.y0, links: [] }
            sorted
        in
          map
            (\link ->
              case find (\l -> l.index == link.index) withY0.links of
                Just updatedLink -> updatedLink
                Nothing -> link
            )
            currentLinks
      )
      links
      nodes

    -- Set y1 for each link (at target node)
    linksWithY1 = foldl
      (\currentLinks node ->
        let
          incomingLinks = filter (\l -> l.targetIndex == node.index) currentLinks
          sorted = sortBy
            (\a b ->
              case find (\n -> n.index == a.sourceIndex) nodes,
                   find (\n -> n.index == b.sourceIndex) nodes of
                Just na, Just nb -> compare na.y0 nb.y0
                _, _ -> EQ
            )
            incomingLinks
          withY1 = foldl
            (\acc link ->
              let
                y = acc.y + link.width / 2.0
                updated = link { y1 = y }
              in
                { y: acc.y + link.width
                , links: Array.snoc acc.links updated
                }
            )
            { y: node.y0, links: [] }
            sorted
        in
          map
            (\link ->
              case find (\l -> l.index == link.index) withY1.links of
                Just updatedLink -> updatedLink
                Nothing -> link
            )
            currentLinks
      )
      linksWithY0
      nodes
  in
    linksWithY1
