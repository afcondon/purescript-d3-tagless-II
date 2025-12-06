-- | Adjacency matrix visualization for neighborhood module relationships
-- |
-- | Shows import/dependency relationships between a central module
-- | and its neighbors as a grid where cells represent connections.
module Engine.AdjacencyMatrix
  ( renderNeighborhoodMatrix
  , clearAdjacencyMatrix
  ) where

import Prelude

import Data.Array (length, (!!), mapWithIndex, nub)
import Data.Array as Array
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import DataViz.Layout.Adjacency (layout, layoutWithConfig, defaultConfig, MatrixLayout, MatrixCell, MatrixLabel)
import Effect (Effect)
import Effect.Class.Console (log)
import Types (SimNode)

-- | FFI imports for D3 rendering
foreign import clearMatrixSvg_ :: Effect Unit
foreign import renderAdjacencyMatrix_
  :: MatrixLayout
  -> String  -- central module name
  -> Array String  -- import names (green)
  -> Array String  -- dependent names (orange)
  -> Effect Unit
foreign import splitOnDotImpl :: String -> Array String

-- | Color palette for modules
centralModuleColor :: String
centralModuleColor = "#4299e1"  -- Blue for central module

neighborImportColor :: String
neighborImportColor = "#48bb78"  -- Green for modules we import

neighborDependentColor :: String
neighborDependentColor = "#ed8936"  -- Orange for modules that depend on us

-- | Convert IDs to names using a node list
idsToNames :: Array Int -> Array SimNode -> Array String
idsToNames ids nodes =
  let idToName = Map.fromFoldable $ map (\n -> Tuple n.id n.name) nodes
  in Array.mapMaybe (\id -> Map.lookup id idToName) ids

-- | Build adjacency matrix from neighborhood nodes
-- | Returns (matrix, moduleNames in order)
buildNeighborhoodMatrix :: String -> Array SimNode -> { matrix :: Array (Array Number), names :: Array String }
buildNeighborhoodMatrix centralName nodes =
  let
    -- Get all unique module names, putting central module first
    allNames = nub $ [centralName] <> map _.name nodes

    -- Build a map from name to matrix index
    nameToIdx = Map.fromFoldable $ mapWithIndex (\i n -> Tuple n i) allNames

    -- Build a map from node ID to node name
    idToName = Map.fromFoldable $ map (\n -> Tuple n.id n.name) nodes

    -- Initialize zero matrix
    zeroRow = map (const 0.0) allNames

    -- For each node, add its connections to the matrix
    addConnections :: Array (Array Number) -> SimNode -> Array (Array Number)
    addConnections matrix node =
      let
        nodeIdx = fromMaybe 0 $ Map.lookup node.name nameToIdx
        matrixWithOutgoing = foldlTargets node.targets nodeIdx matrix
        matrixWithIncoming = foldlSources node.sources nodeIdx matrixWithOutgoing
      in
        matrixWithIncoming

    foldlTargets :: Array Int -> Int -> Array (Array Number) -> Array (Array Number)
    foldlTargets targets fromIdx m = Array.foldl (addTargetById fromIdx) m targets

    foldlSources :: Array Int -> Int -> Array (Array Number) -> Array (Array Number)
    foldlSources sources toIdx m = Array.foldl (addSourceById toIdx) m sources

    addTargetById :: Int -> Array (Array Number) -> Int -> Array (Array Number)
    addTargetById fromIdx m targetId =
      case Map.lookup targetId idToName of
        Just targetName ->
          case Map.lookup targetName nameToIdx of
            Just toIdx -> setMatrixValue m fromIdx toIdx 1.0
            Nothing -> m
        Nothing -> m

    addSourceById :: Int -> Array (Array Number) -> Int -> Array (Array Number)
    addSourceById toIdx m sourceId =
      case Map.lookup sourceId idToName of
        Just sourceName ->
          case Map.lookup sourceName nameToIdx of
            Just fromIdx -> setMatrixValue m fromIdx toIdx 1.0
            Nothing -> m
        Nothing -> m

    setMatrixValue :: Array (Array Number) -> Int -> Int -> Number -> Array (Array Number)
    setMatrixValue m row col val =
      case m !! row of
        Just rowArr ->
          let newRow = fromMaybe rowArr $ Array.updateAt col val rowArr
          in fromMaybe m $ Array.updateAt row newRow m
        Nothing -> m

    initialMatrix = map (const zeroRow) allNames
    finalMatrix = Array.foldl addConnections initialMatrix nodes
  in
    { matrix: finalMatrix, names: allNames }

-- | Render an adjacency matrix for a neighborhood
renderNeighborhoodMatrix :: String -> Array SimNode -> Number -> Number -> Effect Unit
renderNeighborhoodMatrix centralName nodes containerWidth containerHeight = do
  log $ "[AdjacencyMatrix] Rendering matrix for " <> centralName <> " with " <> show (length nodes) <> " nodes"

  -- Clear any existing matrix
  clearMatrixSvg_

  -- Find the central node to get its imports/dependents
  let mCentralNode = Array.find (\n -> n.name == centralName) nodes
  let importIds = case mCentralNode of
                    Just cn -> cn.targets
                    Nothing -> []
  let dependentIds = case mCentralNode of
                       Just cn -> cn.sources
                       Nothing -> []
  let imports = idsToNames importIds nodes
  let dependents = idsToNames dependentIds nodes

  -- Build adjacency matrix
  let { matrix, names } = buildNeighborhoodMatrix centralName nodes

  log $ "[AdjacencyMatrix] Matrix has " <> show (length names) <> " modules"

  -- Skip if matrix is trivial
  if length names < 2 then do
    log "[AdjacencyMatrix] Not enough modules for matrix"
    pure unit
  else do
    -- Compute layout using library function
    -- Adjust cell size based on number of nodes and available space
    let n = length names
    let maxCellSize = min containerWidth containerHeight / (toNumber n + 6.0)  -- Leave room for labels
    let cellSize = min 25.0 maxCellSize
    let config = defaultConfig { cellSize = cellSize, labelWidth = 100.0, labelHeight = 100.0 }
    let matrixLayout = layoutWithConfig config { matrix, names }

    log $ "[AdjacencyMatrix] Layout computed with " <> show (length matrixLayout.cells) <> " cells"

    -- Render via FFI
    renderAdjacencyMatrix_ matrixLayout centralName imports dependents

    log "[AdjacencyMatrix] Matrix rendered"

-- | Clear the adjacency matrix
clearAdjacencyMatrix :: Effect Unit
clearAdjacencyMatrix = clearMatrixSvg_
