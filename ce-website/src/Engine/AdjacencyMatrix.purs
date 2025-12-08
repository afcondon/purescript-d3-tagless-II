-- | Adjacency matrix visualization for neighborhood module relationships
-- |
-- | Shows import/dependency relationships between a central module
-- | and its neighbors as a grid where cells represent connections.
-- |
-- | Each cell is split diagonally to show bidirectional connections:
-- | - Upper-left triangle (green): outbound - row imports col
-- | - Lower-right triangle (orange): inbound - col imports row
module Engine.AdjacencyMatrix
  ( renderNeighborhoodMatrix
  , clearAdjacencyMatrix
  ) where

import Prelude

import Data.Array (length, (!!), mapWithIndex, nub)
import Data.Array as Array
import Data.Foldable (maximum)
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Neighborhood (getNeighborhoodInfo)
import Data.Tuple (Tuple(..))
import DataViz.Layout.Adjacency (layoutWithConfig, defaultConfig)
import Effect (Effect)
import Effect.Class.Console (log)
import Types (SimNode)

-- | Extended cell type with bidirectional connection data
type BidirectionalCell =
  { row :: Int
  , col :: Int
  , outbound :: Number    -- row → col (row imports col)
  , inbound :: Number     -- col → row (col imports row)
  , value :: Number       -- for layout compatibility (outbound + inbound)
  , rowName :: String
  , colName :: String
  , position :: { x :: Number, y :: Number, width :: Number, height :: Number }
  }

-- | Layout with bidirectional cells
type BidirectionalLayout =
  { cells :: Array BidirectionalCell
  , rowLabels :: Array { index :: Int, name :: String, displayName :: String, isRow :: Boolean, position :: { x :: Number, y :: Number, anchor :: String, rotation :: Number } }
  , colLabels :: Array { index :: Int, name :: String, displayName :: String, isRow :: Boolean, position :: { x :: Number, y :: Number, anchor :: String, rotation :: Number } }
  , gridWidth :: Number
  , gridHeight :: Number
  , totalWidth :: Number
  , totalHeight :: Number
  }

-- | FFI imports for D3 rendering
foreign import clearMatrixSvg_ :: Effect Unit
foreign import renderAdjacencyMatrix_
  :: BidirectionalLayout
  -> String  -- central module name
  -> Array String  -- import names (green)
  -> Array String  -- dependent names (orange)
  -> Number  -- max connections for normalization
  -> Effect Unit

-- | Cell with separate outbound and inbound counts
type DirectionalCell = { outbound :: Number, inbound :: Number }

-- | Build bidirectional adjacency matrix from neighborhood nodes
-- | Returns matrix with separate outbound/inbound counts per cell
buildBidirectionalMatrix :: String -> Array SimNode
  -> { outboundMatrix :: Array (Array Number)
     , inboundMatrix :: Array (Array Number)
     , names :: Array String
     }
buildBidirectionalMatrix centralName nodes =
  let
    -- Get all unique module names, putting central module first
    allNames = nub $ [centralName] <> map _.name nodes

    -- Build a map from name to matrix index
    nameToIdx = Map.fromFoldable $ mapWithIndex (\i n -> Tuple n i) allNames

    -- Build a map from node ID to node name
    idToName = Map.fromFoldable $ map (\n -> Tuple n.id n.name) nodes

    -- Initialize zero matrices
    zeroRow = map (const 0.0) allNames

    -- For each node, add its connections to the matrices
    -- outbound[i][j] = 1 if node i imports node j (i.e., j is in i's targets)
    -- inbound[i][j] = 1 if node j imports node i (i.e., i is in j's targets, or j is in i's sources)
    addConnections :: { out :: Array (Array Number), in_ :: Array (Array Number) }
                   -> SimNode
                   -> { out :: Array (Array Number), in_ :: Array (Array Number) }
    addConnections { out, in_ } node =
      let
        nodeIdx = fromMaybe 0 $ Map.lookup node.name nameToIdx
        -- Add outbound: this node imports these targets
        outUpdated = Array.foldl (addOutbound nodeIdx) out node.targets
        -- Add inbound: these sources import this node
        inUpdated = Array.foldl (addInbound nodeIdx) in_ node.sources
      in
        { out: outUpdated, in_: inUpdated }

    addOutbound :: Int -> Array (Array Number) -> Int -> Array (Array Number)
    addOutbound fromIdx m targetId =
      case Map.lookup targetId idToName of
        Just targetName ->
          case Map.lookup targetName nameToIdx of
            Just toIdx -> setMatrixValue m fromIdx toIdx 1.0
            Nothing -> m
        Nothing -> m

    addInbound :: Int -> Array (Array Number) -> Int -> Array (Array Number)
    addInbound toIdx m sourceId =
      case Map.lookup sourceId idToName of
        Just sourceName ->
          case Map.lookup sourceName nameToIdx of
            Just fromIdx -> setMatrixValue m toIdx fromIdx 1.0  -- Note: reversed for inbound
            Nothing -> m
        Nothing -> m

    setMatrixValue :: Array (Array Number) -> Int -> Int -> Number -> Array (Array Number)
    setMatrixValue m row col val =
      case m !! row of
        Just rowArr ->
          let newRow = fromMaybe rowArr $ Array.updateAt col val rowArr
          in fromMaybe m $ Array.updateAt row newRow m
        Nothing -> m

    initialOut = map (const zeroRow) allNames
    initialIn = map (const zeroRow) allNames
    { out: finalOut, in_: finalIn } = Array.foldl addConnections { out: initialOut, in_: initialIn } nodes
  in
    { outboundMatrix: finalOut, inboundMatrix: finalIn, names: allNames }

-- | Render an adjacency matrix for a neighborhood
renderNeighborhoodMatrix :: String -> Array SimNode -> Number -> Number -> Effect Unit
renderNeighborhoodMatrix centralName nodes containerWidth containerHeight = do
  log $ "[AdjacencyMatrix] Rendering matrix for " <> centralName <> " with " <> show (length nodes) <> " nodes"

  -- Clear any existing matrix
  clearMatrixSvg_

  -- Get neighborhood info using shared utility
  let info = getNeighborhoodInfo centralName nodes
  let imports = info.importNames
  let dependents = info.dependentNames

  -- Build bidirectional adjacency matrices
  let { outboundMatrix, inboundMatrix, names } = buildBidirectionalMatrix centralName nodes

  log $ "[AdjacencyMatrix] Matrix has " <> show (length names) <> " modules"

  -- Skip if matrix is trivial
  if length names < 2 then do
    log "[AdjacencyMatrix] Not enough modules for matrix"
    pure unit
  else do
    -- Compute layout using library function (using outbound matrix for positioning)
    -- Adjust cell size based on number of nodes and available space
    let n = length names
    let maxCellSize = min containerWidth containerHeight / (toNumber n + 6.0)  -- Leave room for labels
    let cellSize = min 25.0 maxCellSize
    let config = defaultConfig { cellSize = cellSize, labelWidth = 100.0, labelHeight = 100.0 }

    -- Use the combined matrix for layout (outbound + inbound for cell positioning)
    let combinedMatrix = Array.zipWith (Array.zipWith (+)) outboundMatrix inboundMatrix
    let baseLayout = layoutWithConfig config { matrix: combinedMatrix, names }

    -- Build bidirectional cells with outbound and inbound values
    let bidirectionalCells = mapWithIndex (\_ cell ->
          let
            rowIdx = cell.row
            colIdx = cell.col
            outVal = fromMaybe 0.0 $ (outboundMatrix !! rowIdx) >>= (_ !! colIdx)
            inVal = fromMaybe 0.0 $ (inboundMatrix !! rowIdx) >>= (_ !! colIdx)
          in
            { row: cell.row
            , col: cell.col
            , outbound: outVal
            , inbound: inVal
            , value: outVal + inVal
            , rowName: cell.rowName
            , colName: cell.colName
            , position: cell.position
            }
        ) baseLayout.cells

    -- Calculate max connections for color normalization
    let allOutbound = map _.outbound bidirectionalCells
    let allInbound = map _.inbound bidirectionalCells
    let maxConnections = fromMaybe 1.0 $ maximum (allOutbound <> allInbound)

    let bidirectionalLayout :: BidirectionalLayout
        bidirectionalLayout =
          { cells: bidirectionalCells
          , rowLabels: baseLayout.rowLabels
          , colLabels: baseLayout.colLabels
          , gridWidth: baseLayout.gridWidth
          , gridHeight: baseLayout.gridHeight
          , totalWidth: baseLayout.totalWidth
          , totalHeight: baseLayout.totalHeight
          }

    log $ "[AdjacencyMatrix] Layout computed with " <> show (length bidirectionalCells) <> " cells, max connections: " <> show maxConnections

    -- Render via FFI
    renderAdjacencyMatrix_ bidirectionalLayout centralName imports dependents maxConnections

    log "[AdjacencyMatrix] Matrix rendered"

-- | Clear the adjacency matrix
clearAdjacencyMatrix :: Effect Unit
clearAdjacencyMatrix = clearMatrixSvg_
