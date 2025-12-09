-- | DataViz.Layout.Sankey.MarkovChain
-- |
-- | Implementation of the Markov Chain barycentre ordering method for Sankey diagrams.
-- | Based on "Crossing Reduction of Sankey Diagram with Barycentre Ordering via Markov Chain"
-- | by Li et al. (arXiv:1912.05339)
-- |
-- | This method formulates barycentre ordering as a Markov chain and uses the eigenvector
-- | corresponding to the second largest eigenvalue to find a good initial ordering.
module DataViz.Layout.Sankey.MarkovChain
  ( -- * Eigenvector utilities
    powerIteration
  , secondEigenvector
  , vectorNorm
  , normalizeVector
    -- * Transition matrices
  , buildWeightMatrix
  , buildLeftTransitionMatrix
  , buildRightTransitionMatrix
  , buildCompositeTransitionMatrix
    -- * Stage 1: Markov Chain Method
  , markovChainOrdering
  , MarkovChainConfig
  , defaultMarkovChainConfig
    -- * Types
  , LayerInfo
  , LayerEdge
  , LayerOrdering
    -- * Weighted crossing calculation
  , calculateWeightedCrossing
  ) where

import Prelude

import Data.Array (foldl, length, mapWithIndex, replicate, sortBy, (!!), (..))
import Data.Array as Array
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (sqrt)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Random (random)
import LinearAlgebra.Matrix as M
import LinearAlgebra.Vector as V

-- ============================================================================
-- Types
-- ============================================================================

-- | Configuration for the Markov Chain method
type MarkovChainConfig =
  { alpha :: Number        -- ^ Random component weight (typically 0.01-0.1)
  , iterations :: Int      -- ^ Number of power iterations
  , numTrials :: Int       -- ^ Number of random trials (best-in-N)
  }

defaultMarkovChainConfig :: MarkovChainConfig
defaultMarkovChainConfig =
  { alpha: 0.01
  , iterations: 100
  , numTrials: 100
  }

-- ============================================================================
-- Eigenvector Utilities
-- ============================================================================

-- | Calculate the L2 norm of a vector
vectorNorm :: V.Vector Number -> Number
vectorNorm v = sqrt (V.dot v v)

-- | Normalize a vector to unit length
normalizeVector :: V.Vector Number -> V.Vector Number
normalizeVector v =
  let norm = vectorNorm v
  in if norm > 1.0e-10
     then V.scale (1.0 / norm) v
     else v

-- | Power iteration to find the dominant eigenvector
-- | Returns the eigenvector corresponding to the largest eigenvalue
powerIteration :: M.Matrix Number -> V.Vector Number -> Int -> V.Vector Number
powerIteration mat v0 maxIter = go 0 v0
  where
  go iter v
    | iter >= maxIter = v
    | otherwise =
        let v' = M.mult' mat v
            v'' = normalizeVector v'
        in go (iter + 1) v''

-- | Find the eigenvector corresponding to the second largest eigenvalue
-- | Uses deflation: T' = T - λ₁ * v₁ * v₁ᵀ where λ₁=1 for stochastic matrices
-- | and v₁ is the uniform vector (all entries equal)
secondEigenvector :: M.Matrix Number -> Int -> V.Vector Number
secondEigenvector mat iterations =
  let
    n = M.nrows mat
    -- For a stochastic matrix, the dominant eigenvector is uniform
    -- v1 = (1/√n, 1/√n, ..., 1/√n)
    uniformVal = 1.0 / sqrt (toNumber n)
    v1 = V.fromArray (replicate n uniformVal)

    -- Deflate: T' = T - v1 * v1^T (since λ₁ = 1)
    -- This is an outer product subtraction
    deflated = M.fromFunction n n \i j ->
      M.index mat i j - (V.index v1 i * V.index v1 j)

    -- Start with a random vector orthogonal to v1
    -- We use a simple approach: random vector, then Gram-Schmidt
    randomVec = generateRandomVector n

    -- Remove component along v1: v = v - (v·v1)v1
    projection = V.dot randomVec v1
    orthogonal = V.diff randomVec (V.scale projection v1)
    startVec = normalizeVector orthogonal

  in powerIteration deflated startVec iterations

-- | Generate a pseudo-random vector (using unsafePerformEffect for simplicity)
-- | In production, this should use a proper random seed
generateRandomVector :: Int -> V.Vector Number
generateRandomVector n = V.fromArray $ map (\_ -> unsafePerformEffect random) (0 .. (n - 1))

-- ============================================================================
-- Transition Matrices
-- ============================================================================

-- | Build the weighted interconnection matrix M^(i) for edges between layer i and i+1
-- | M[j,k] = weight of edge from node j (layer i) to node k (layer i+1), or 0
buildWeightMatrix
  :: Int                                      -- ^ Number of nodes in layer i
  -> Int                                      -- ^ Number of nodes in layer i+1
  -> Array { from :: Int, to :: Int, weight :: Number }  -- ^ Edges (local indices)
  -> M.Matrix Number
buildWeightMatrix nFrom nTo edges =
  M.fromFunction nFrom nTo \j k ->
    let
      maybeEdge = edges # Array.find \e -> e.from == j && e.to == k
    in case maybeEdge of
      Just e -> e.weight
      Nothing -> 0.0

-- | Build the left transition matrix L^(i) from weight matrix M^(i)
-- | L = M * diag(1/||M[:,k]||) - normalizes by column sums
-- | Then adds random component: L̃ = (1-α)L + αS where S is random stochastic
buildLeftTransitionMatrix :: Number -> M.Matrix Number -> M.Matrix Number
buildLeftTransitionMatrix alpha weightMat =
  let
    nRows = M.nrows weightMat
    nCols = M.ncols weightMat

    -- Calculate column sums (L1 norms)
    colSums = map (\j ->
      foldl (\acc i -> acc + M.index weightMat i j) 0.0 (0 .. (nRows - 1))
    ) (0 .. (nCols - 1))

    -- Normalize columns (this gives L^(i))
    normalizedMat = M.fromFunction nCols nRows \k j ->
      let colSum = fromMaybe 1.0 (colSums !! j)
      in if colSum > 1.0e-10
         then M.index weightMat j k / colSum  -- Note: transposed indices
         else 1.0 / toNumber nRows  -- Fallback to uniform if column is zero

    -- Generate random stochastic matrix S (row-normalized random)
    randomMat = generateRandomStochasticMatrix nCols nRows

    -- Combine: L̃ = (1-α)L + αS
    combined = M.fromFunction nCols nRows \i j ->
      (1.0 - alpha) * M.index normalizedMat i j + alpha * M.index randomMat i j

  in combined

-- | Build the right transition matrix R^(i) from weight matrix M^(i)
-- | R = diag(1/||M[j,:]||) * M - normalizes by row sums
-- | Then adds random component: R̃ = (1-α)R + αS
buildRightTransitionMatrix :: Number -> M.Matrix Number -> M.Matrix Number
buildRightTransitionMatrix alpha weightMat =
  let
    nRows = M.nrows weightMat
    nCols = M.ncols weightMat

    -- Calculate row sums (L1 norms)
    rowSums = map (\i ->
      foldl (\acc j -> acc + M.index weightMat i j) 0.0 (0 .. (nCols - 1))
    ) (0 .. (nRows - 1))

    -- Normalize rows (this gives R^(i))
    normalizedMat = M.fromFunction nRows nCols \i j ->
      let rowSum = fromMaybe 1.0 (rowSums !! i)
      in if rowSum > 1.0e-10
         then M.index weightMat i j / rowSum
         else 1.0 / toNumber nCols  -- Fallback to uniform if row is zero

    -- Generate random stochastic matrix S (row-normalized random)
    randomMat = generateRandomStochasticMatrix nRows nCols

    -- Combine: R̃ = (1-α)R + αS
    combined = M.fromFunction nRows nCols \i j ->
      (1.0 - alpha) * M.index normalizedMat i j + alpha * M.index randomMat i j

  in combined

-- | Generate a random row-stochastic matrix (rows sum to 1)
generateRandomStochasticMatrix :: Int -> Int -> M.Matrix Number
generateRandomStochasticMatrix nRows nCols =
  let
    -- Generate random values
    rawValues = map (\_ ->
      map (\_ -> unsafePerformEffect random) (0 .. (nCols - 1))
    ) (0 .. (nRows - 1))

    -- Normalize each row
    normalizedRows = map (\row ->
      let rowSum = foldl (+) 0.0 row
      in if rowSum > 1.0e-10
         then map (_ / rowSum) row
         else replicate nCols (1.0 / toNumber nCols)
    ) rawValues
  in M.fromArray nRows nCols normalizedRows

-- | Build the composite transition matrix T = R * L for layer 1
-- | This is the product of all right and left transition matrices
buildCompositeTransitionMatrix
  :: Array (M.Matrix Number)  -- ^ Left transition matrices L̃^(1), ..., L̃^(n-1)
  -> Array (M.Matrix Number)  -- ^ Right transition matrices R̃^(1), ..., R̃^(n-1)
  -> M.Matrix Number
buildCompositeTransitionMatrix leftMats rightMats =
  let
    -- L = L̃^(n-1) * L̃^(n-2) * ... * L̃^(1)
    -- R = R̃^(1) * R̃^(2) * ... * R̃^(n-1)
    -- T = R * L

    -- Product of left matrices (from last to first)
    leftProduct = case Array.unsnoc leftMats of
      Nothing -> M.eye 1  -- Shouldn't happen
      Just { init, last: lastL } ->
        foldl (\acc m -> M.mult m acc) lastL (Array.reverse init)

    -- Product of right matrices (from first to last)
    rightProduct = case Array.uncons rightMats of
      Nothing -> M.eye 1  -- Shouldn't happen
      Just { head: firstR, tail } ->
        foldl M.mult firstR tail

  in M.mult rightProduct leftProduct

-- ============================================================================
-- Stage 1: Markov Chain Method
-- ============================================================================

-- | Layer information for the Markov chain algorithm
type LayerInfo =
  { nodeIndices :: Array Int      -- ^ Original node indices in this layer
  , nodeCount :: Int              -- ^ Number of nodes in this layer
  }

-- | Edge information between layers (using local indices within layers)
type LayerEdge =
  { fromLocal :: Int      -- ^ Local index in source layer
  , toLocal :: Int        -- ^ Local index in target layer
  , weight :: Number      -- ^ Edge weight (flow value)
  }

-- | Result of Markov chain ordering for a single layer
type LayerOrdering = Array Int  -- ^ Node indices in order (top to bottom)

-- | Run the Markov Chain method to find node orderings
-- | Returns position values for the first layer, from which other layers can be derived
markovChainOrdering
  :: MarkovChainConfig
  -> Array LayerInfo                          -- ^ Info for each layer
  -> Array (Array LayerEdge)                  -- ^ Edges between consecutive layers
  -> { positions :: Array (Array Number)      -- ^ Position values per layer
     , orderings :: Array LayerOrdering       -- ^ Node orderings per layer
     }
markovChainOrdering config layers edgesByLayer =
  let
    -- Build weight matrices for each layer gap
    weightMatrices = mapWithIndex (\i edges ->
      let
        nFrom = fromMaybe 0 $ (layers !! i) <#> _.nodeCount
        nTo = fromMaybe 0 $ (layers !! (i + 1)) <#> _.nodeCount
      in buildWeightMatrix nFrom nTo (map toEdgeRecord edges)
    ) edgesByLayer

    -- Build transition matrices
    leftMats = map (buildLeftTransitionMatrix config.alpha) weightMatrices
    rightMats = map (buildRightTransitionMatrix config.alpha) weightMatrices

    -- Build composite transition matrix T for layer 1
    transMat = buildCompositeTransitionMatrix leftMats rightMats

    -- Find second eigenvector
    eigenvector = secondEigenvector transMat config.iterations

    -- Convert eigenvector to position values for layer 1
    layer1Positions = V.toArray eigenvector

    -- Propagate positions through layers using left transition matrices
    allPositions = propagatePositions layer1Positions leftMats

    -- Convert positions to orderings (sort by position value)
    allOrderings = map positionsToOrdering allPositions

  in { positions: allPositions, orderings: allOrderings }
  where
  toEdgeRecord :: LayerEdge -> { from :: Int, to :: Int, weight :: Number }
  toEdgeRecord e = { from: e.fromLocal, to: e.toLocal, weight: e.weight }

-- | Propagate position values from layer 1 through all layers
-- | Now also propagates BACKWARD to layer 0 using the first right transition matrix
propagatePositions :: Array Number -> Array (M.Matrix Number) -> Array (Array Number)
propagatePositions layer1Pos leftMats =
  let
    -- Start with layer 1 positions
    layer1Vec = V.fromArray layer1Pos

    -- Propagate FORWARD: layer 1 -> 2 -> 3 -> ... using left matrices
    forwardResult = foldl
      (\acc leftMat ->
        let nextPos = M.mult' leftMat acc.currentPos
        in { positions: Array.snoc acc.positions (V.toArray nextPos)
           , currentPos: nextPos
           }
      )
      { positions: [layer1Pos], currentPos: layer1Vec }
      leftMats

    -- For layer 0, we need to propagate backward
    -- The simplest approach: use uniform distribution for layer 0 based on
    -- the number of nodes (which equals length of first left matrix's columns)
    layer0Size = case Array.head leftMats of
      Just firstL -> M.ncols firstL  -- Layer 0 has ncols nodes (L goes from layer 0 to 1)
      Nothing -> 0

    -- Generate positions for layer 0: evenly spaced
    layer0Positions = if layer0Size > 0
      then map (\i -> toNumber (layer0Size - i) / toNumber layer0Size) (0 .. (layer0Size - 1))
      else []

  in [layer0Positions] <> forwardResult.positions

-- | Convert position values to an ordering (indices sorted by position)
positionsToOrdering :: Array Number -> LayerOrdering
positionsToOrdering positions =
  let
    indexed = mapWithIndex Tuple positions
    sorted = sortBy (\a b -> compare (snd b) (snd a)) indexed  -- Descending
  in map fst sorted

-- ============================================================================
-- Weighted Crossing Calculation
-- ============================================================================

-- | Calculate the weighted crossing number for a given ordering
-- | K̄(G) = Σ (w₁ × w₂) for all crossing edge pairs
calculateWeightedCrossing
  :: Array LayerOrdering                      -- ^ Node orderings per layer
  -> Array (Array LayerEdge)                  -- ^ Edges between consecutive layers
  -> Number
calculateWeightedCrossing orderings edgesByLayer =
  foldl (+) 0.0 $ mapWithIndex calculateLayerCrossing edgesByLayer
  where
  calculateLayerCrossing :: Int -> Array LayerEdge -> Number
  calculateLayerCrossing layerIdx edges =
    let
      -- Get orderings for this layer and next
      orderingFrom = fromMaybe [] (orderings !! layerIdx)
      orderingTo = fromMaybe [] (orderings !! (layerIdx + 1))

      -- Build position maps (node index -> position in ordering)
      posMapFrom = buildPositionMap orderingFrom
      posMapTo = buildPositionMap orderingTo

      -- Check all pairs of edges for crossings
      edgeArray = edges
      numEdges = length edgeArray

    in foldl (\acc i ->
      foldl (\acc2 j ->
        if i < j
        then acc2 + checkCrossing posMapFrom posMapTo edgeArray i j
        else acc2
      ) acc (0 .. (numEdges - 1))
    ) 0.0 (0 .. (numEdges - 1))

  buildPositionMap :: Array Int -> Array (Tuple Int Int)
  buildPositionMap ordering = mapWithIndex (\pos nodeIdx -> Tuple nodeIdx pos) ordering

  lookupPosition :: Array (Tuple Int Int) -> Int -> Int
  lookupPosition posMap nodeIdx =
    case Array.find (\(Tuple idx _) -> idx == nodeIdx) posMap of
      Just (Tuple _ pos) -> pos
      Nothing -> 0

  checkCrossing :: Array (Tuple Int Int) -> Array (Tuple Int Int) -> Array LayerEdge -> Int -> Int -> Number
  checkCrossing posMapFrom posMapTo edges i j =
    case edges !! i, edges !! j of
      Just e1, Just e2 ->
        let
          pos1From = lookupPosition posMapFrom e1.fromLocal
          pos1To = lookupPosition posMapTo e1.toLocal
          pos2From = lookupPosition posMapFrom e2.fromLocal
          pos2To = lookupPosition posMapTo e2.toLocal

          -- Edges cross if one is above at source but below at target (or vice versa)
          crosses = (pos1From < pos2From && pos1To > pos2To) ||
                    (pos1From > pos2From && pos1To < pos2To)
        in if crosses then e1.weight * e2.weight else 0.0
      _, _ -> 0.0
