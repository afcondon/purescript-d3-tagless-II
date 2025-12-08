-- | DataViz.Layout.Sankey.PartitionRefinement
-- |
-- | Stage 2 of the Markov Chain barycentre ordering method: Partition Refinement.
-- | Based on "Crossing Reduction of Sankey Diagram with Barycentre Ordering via Markov Chain"
-- | by Li et al. (arXiv:1912.05339), Section 2.2
-- |
-- | Instead of treating each vertex as a point, this method gives each vertex a "block"
-- | (range) within its layer. Edge endpoints get distinct positions within the block
-- | based on the ordering of connected vertices. The algorithm iterates until convergence.
module DataViz.Layout.Sankey.PartitionRefinement
  ( refineOrdering
  , PartitionRefinementConfig
  , defaultPartitionRefinementConfig
  , RefinementResult
  ) where

import Prelude

import Data.Array (filter, foldl, length, mapWithIndex, sortBy, (!!), (..))
import Data.Array as Array
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst, snd)
import DataViz.Layout.Sankey.MarkovChain (LayerEdge, LayerOrdering, calculateWeightedCrossing)

-- ============================================================================
-- Types
-- ============================================================================

-- | Configuration for the Partition Refinement method
type PartitionRefinementConfig =
  { maxIterations :: Int      -- ^ Maximum number of iterations (M in the paper)
  , convergenceThreshold :: Int  -- ^ Number of unchanged iterations before convergence
  }

defaultPartitionRefinementConfig :: PartitionRefinementConfig
defaultPartitionRefinementConfig =
  { maxIterations: 100
  , convergenceThreshold: 3
  }

-- | Result of partition refinement
type RefinementResult =
  { orderings :: Array LayerOrdering       -- ^ Final node orderings per layer
  , positions :: Array (Array Number)      -- ^ Final position values per layer
  , weightedCrossing :: Number             -- ^ Weighted crossing of final result
  , iterations :: Int                      -- ^ Number of iterations performed
  , bestWeightedCrossing :: Number         -- ^ Best weighted crossing seen during refinement
  , bestOrderings :: Array LayerOrdering   -- ^ Orderings with best weighted crossing
  }

-- | Block information for a node
type BlockInfo =
  { base :: Number      -- ^ Base position of the block
  , height :: Number    -- ^ Height of the block
  }

-- | Point position for an edge endpoint within a block
type EdgePoint =
  { fromNode :: Int     -- ^ Local index of source node
  , toNode :: Int       -- ^ Local index of target node
  , position :: Number  -- ^ Position value within [0, 1]
  }

-- | Internal state for refinement iteration
type IterState =
  { orderings :: Array LayerOrdering
  , positions :: Array (Array Number)
  , edgePoints :: Array (Array EdgePoint)  -- Edge points for each layer gap
  , unchangedCount :: Int
  , iteration :: Int
  , bestCrossing :: Number
  , bestOrderings :: Array LayerOrdering
  }

-- ============================================================================
-- Partition Refinement Algorithm
-- ============================================================================

-- | Refine an ordering using the Partition Refinement method
-- | Takes the initial ordering from Stage 1 and iteratively improves it
refineOrdering
  :: PartitionRefinementConfig
  -> Array LayerOrdering                    -- ^ Initial orderings from Stage 1
  -> Array (Array LayerEdge)                -- ^ Edges between consecutive layers
  -> RefinementResult
refineOrdering config initialOrderings edgesByLayer =
  let
    -- Initialize position vectors from orderings
    initialPositions = map orderingToPositions initialOrderings

    -- Initialize edge points based on initial orderings
    initialEdgePoints = initializeEdgePoints initialOrderings edgesByLayer

    -- Calculate initial weighted crossing
    initialCrossing = calculateWeightedCrossing initialOrderings edgesByLayer

    -- Run refinement iterations
    result = iterateRefinement
      { orderings: initialOrderings
      , positions: initialPositions
      , edgePoints: initialEdgePoints
      , unchangedCount: 0
      , iteration: 0
      , bestCrossing: initialCrossing
      , bestOrderings: initialOrderings
      }

  in
    { orderings: result.orderings
    , positions: result.positions
    , weightedCrossing: calculateWeightedCrossing result.orderings edgesByLayer
    , iterations: result.iteration
    , bestWeightedCrossing: result.bestCrossing
    , bestOrderings: result.bestOrderings
    }
  where
  -- Main iteration loop
  iterateRefinement :: IterState -> IterState
  iterateRefinement state
    | state.iteration >= config.maxIterations = state
    | state.unchangedCount >= config.convergenceThreshold = state
    | otherwise =
        let
          -- Perform one full iteration (left-to-right, then right-to-left)
          newState = performIteration state

          -- Check if ordering changed
          orderingChanged = state.orderings /= newState.orderings

          -- Calculate new weighted crossing
          newCrossing = calculateWeightedCrossing newState.orderings edgesByLayer

          -- Track best result
          updatedBest =
            if newCrossing < newState.bestCrossing
            then newState { bestCrossing = newCrossing, bestOrderings = newState.orderings }
            else newState

          -- Update unchanged count
          finalState = updatedBest
            { unchangedCount = if orderingChanged then 0 else state.unchangedCount + 1
            , iteration = state.iteration + 1
            }
        in
          iterateRefinement finalState

  -- Perform one iteration: process all layers left-to-right, then right-to-left
  performIteration :: IterState -> IterState
  performIteration state =
    let
      numLayers = length state.orderings

      -- Left to right pass (layers 1 to n-1, using 0-indexed: 1 to numLayers-1)
      afterLTR = foldl
        (\s layerIdx -> updateLayer s layerIdx)
        state
        (1 .. (numLayers - 1))

      -- Right to left pass (layers n-2 to 0, using 0-indexed: numLayers-2 to 0)
      afterRTL = foldl
        (\s layerIdx -> updateLayer s layerIdx)
        afterLTR
        (Array.reverse (0 .. (numLayers - 2)))

    in afterRTL

  -- Update ordering for a single layer based on barycentres
  updateLayer :: IterState -> Int -> IterState
  updateLayer state layerIdx =
    let
      numLayers = length state.orderings
      layerSize = fromMaybe 0 $ (state.orderings !! layerIdx) <#> length

      -- Calculate barycentre for each node in this layer
      barycentres = map (\localIdx ->
        calculateNodeBarycentre state layerIdx localIdx numLayers
      ) (0 .. (layerSize - 1))

      -- Sort nodes by barycentre (descending, as per paper)
      indexed = mapWithIndex Tuple barycentres
      sorted = sortBy (\a b -> compare (snd b) (snd a)) indexed
      newOrdering = map fst sorted

      -- Update orderings array
      newOrderings = fromMaybe state.orderings $
        Array.updateAt layerIdx newOrdering state.orderings

      -- Recalculate positions for this layer
      newPositions = fromMaybe state.positions $
        Array.updateAt layerIdx (orderingToPositions newOrdering) state.positions

      -- Update edge points based on new ordering
      newEdgePoints = updateEdgePointsForLayer state.edgePoints newOrderings layerIdx edgesByLayer

    in state
      { orderings = newOrderings
      , positions = newPositions
      , edgePoints = newEdgePoints
      }

  -- Calculate two-sided barycentre for a node
  calculateNodeBarycentre :: IterState -> Int -> Int -> Int -> Number
  calculateNodeBarycentre state layerIdx localNodeIdx numLayers =
    let
      -- Left barycentre (from layer layerIdx-1)
      leftBary =
        if layerIdx > 0
        then calculateOneSidedBarycentre state (layerIdx - 1) layerIdx localNodeIdx true
        else 0.5

      -- Right barycentre (from layer layerIdx+1)
      rightBary =
        if layerIdx < numLayers - 1
        then calculateOneSidedBarycentre state layerIdx layerIdx localNodeIdx false
        else 0.5

      -- Two-sided barycentre is average
      -- For first layer, use right only; for last layer, use left only
      bary =
        if layerIdx == 0 then rightBary
        else if layerIdx == numLayers - 1 then leftBary
        else (leftBary + rightBary) / 2.0

    in bary

  -- Calculate one-sided barycentre using edge point positions
  -- edgeLayerIdx is the index into edgesByLayer
  -- isLeft indicates if we're looking at the left neighbor layer
  calculateOneSidedBarycentre :: IterState -> Int -> Int -> Int -> Boolean -> Number
  calculateOneSidedBarycentre state edgeLayerIdx _layerIdx localNodeIdx isLeft =
    let
      edges = fromMaybe [] (edgesByLayer !! edgeLayerIdx)
      edgePoints = fromMaybe [] (state.edgePoints !! edgeLayerIdx)

      -- Find edges connected to this node
      connectedEdges =
        if isLeft
        then filter (\e -> e.toLocal == localNodeIdx) edges
        else filter (\e -> e.fromLocal == localNodeIdx) edges

      -- Get positions from edge points
      -- For left neighbors, we look at the 'from' side; for right, the 'to' side
      positionsAndWeights = Array.mapMaybe (\edge ->
        let
          maybePoint = Array.find (\p ->
            p.fromNode == edge.fromLocal && p.toNode == edge.toLocal
          ) edgePoints
        in case maybePoint of
          Just point -> Just { pos: point.position, weight: edge.weight }
          Nothing -> Nothing
      ) connectedEdges

      -- Weighted average
      totalWeight = foldl (\acc pw -> acc + pw.weight) 0.0 positionsAndWeights
      weightedSum = foldl (\acc pw -> acc + pw.pos * pw.weight) 0.0 positionsAndWeights

    in if totalWeight > 0.0 then weightedSum / totalWeight else 0.5

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- | Convert an ordering to position values (evenly distributed in [0, 1])
orderingToPositions :: LayerOrdering -> Array Number
orderingToPositions ordering =
  let
    n = length ordering
  in if n <= 1
    then [0.5]
    else mapWithIndex (\i _ -> 1.0 - toNumber i / toNumber (n - 1)) ordering

-- | Initialize edge points based on initial orderings
-- | Each edge gets a position based on its rank among edges to/from the same node
initializeEdgePoints
  :: Array LayerOrdering
  -> Array (Array LayerEdge)
  -> Array (Array EdgePoint)
initializeEdgePoints orderings edgesByLayer =
  mapWithIndex (\layerIdx edges ->
    let
      fromOrdering = fromMaybe [] (orderings !! layerIdx)
      toOrdering = fromMaybe [] (orderings !! (layerIdx + 1))
      fromSize = length fromOrdering
      toSize = length toOrdering
    in calculateEdgePointsForLayer edges fromOrdering toOrdering fromSize toSize
  ) edgesByLayer

-- | Calculate edge points for a single layer gap
calculateEdgePointsForLayer
  :: Array LayerEdge
  -> LayerOrdering      -- ^ Ordering of source layer
  -> LayerOrdering      -- ^ Ordering of target layer
  -> Int                -- ^ Size of source layer
  -> Int                -- ^ Size of target layer (unused, kept for API consistency)
  -> Array EdgePoint
calculateEdgePointsForLayer edges fromOrdering toOrdering fromSize _toSize =
  let
    -- Build position maps
    fromPosMap = buildPositionMap fromOrdering
    toPosMap = buildPositionMap toOrdering

    -- For each edge, calculate its position
    edgePoints = map (\edge ->
      let
        fromPos = lookupPos fromPosMap edge.fromLocal

        -- Block parameters for source node
        blockBase = if fromSize > 0
          then (toNumber fromSize - 1.0) / toNumber fromSize * (1.0 - toNumber fromPos / toNumber fromSize)
          else 0.0
        blockHeight = if fromSize > 0 then 1.0 / toNumber fromSize else 1.0

        -- Get edges from same source, sorted by target position
        edgesFromSameSource = filter (\e -> e.fromLocal == edge.fromLocal) edges
        sortedByTarget = sortBy (\a b ->
          compare (lookupPos toPosMap a.toLocal) (lookupPos toPosMap b.toLocal)
        ) edgesFromSameSource

        -- Find rank of this edge among edges from same source
        numEdgesFromSource = length sortedByTarget
        rankAmongSource = fromMaybe 0 $ Array.findIndex (\e ->
          e.fromLocal == edge.fromLocal && e.toLocal == edge.toLocal
        ) sortedByTarget

        -- Calculate position within block (equation 19 from paper)
        position = blockBase + (toNumber (numEdgesFromSource + 1 - rankAmongSource) /
                                toNumber (numEdgesFromSource + 1)) * blockHeight

      in { fromNode: edge.fromLocal, toNode: edge.toLocal, position }
    ) edges
  in edgePoints
  where
  buildPositionMap :: LayerOrdering -> Array (Tuple Int Int)
  buildPositionMap ordering = mapWithIndex (\pos nodeIdx -> Tuple nodeIdx pos) ordering

  lookupPos :: Array (Tuple Int Int) -> Int -> Int
  lookupPos posMap nodeIdx =
    case Array.find (\(Tuple idx _) -> idx == nodeIdx) posMap of
      Just (Tuple _ pos) -> pos
      Nothing -> 0

-- | Update edge points after a layer's ordering changes
updateEdgePointsForLayer
  :: Array (Array EdgePoint)
  -> Array LayerOrdering
  -> Int                      -- ^ Layer that changed
  -> Array (Array LayerEdge)
  -> Array (Array EdgePoint)
updateEdgePointsForLayer currentEdgePoints orderings changedLayer edgesByLayer =
  let
    numLayers = length orderings

    -- Update edge points for layer gap before changed layer (if exists)
    afterBefore =
      if changedLayer > 0
      then fromMaybe currentEdgePoints $ do
        edges <- edgesByLayer !! (changedLayer - 1)
        fromOrd <- orderings !! (changedLayer - 1)
        toOrd <- orderings !! changedLayer
        let newPoints = calculateEdgePointsForLayer edges fromOrd toOrd (length fromOrd) (length toOrd)
        Array.updateAt (changedLayer - 1) newPoints currentEdgePoints
      else currentEdgePoints

    -- Update edge points for layer gap after changed layer (if exists)
    afterAfter =
      if changedLayer < numLayers - 1
      then fromMaybe afterBefore $ do
        edges <- edgesByLayer !! changedLayer
        fromOrd <- orderings !! changedLayer
        toOrd <- orderings !! (changedLayer + 1)
        let newPoints = calculateEdgePointsForLayer edges fromOrd toOrd (length fromOrd) (length toOrd)
        Array.updateAt changedLayer newPoints afterBefore
      else afterBefore

  in afterAfter
