-- | Chord diagram for neighborhood module relationships
-- |
-- | Shows import/dependency relationships between a central module
-- | and its neighbors as a circular chord diagram.
module Engine.ChordDiagram
  ( renderNeighborhoodChord
  , clearChordDiagram
  ) where

import Prelude

import Data.Array (length, (!!), mapWithIndex, findIndex, filter, nub)
import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (pi, cos, sin)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Types (SimNode)
import PSD3.Internal.FFI (chordLayoutWithPadAngle_, chordGroups_, chordArray_, ribbonGenerator_, arcGenerator_, setRibbonRadius_, setArcInnerRadius_, setArcOuterRadius_, arcPath_, ribbonPath_)
import PSD3.Internal.Types (Datum_)
import Unsafe.Coerce (unsafeCoerce)

-- | FFI imports for D3 selection operations
foreign import clearChordSvg_ :: Effect Unit
foreign import renderChordDiagram_
  :: Number  -- width
  -> Number  -- height
  -> Array ChordArc  -- arcs
  -> Array ChordRibbon  -- ribbons
  -> Array ChordLabel  -- labels
  -> Effect Unit

-- | Data for a chord arc (outer segment)
type ChordArc =
  { path :: String
  , color :: String
  , moduleName :: String
  , index :: Int
  }

-- | Data for a chord ribbon (connection)
type ChordRibbon =
  { path :: String
  , color :: String
  , sourceModule :: String
  , targetModule :: String
  , sourceIndex :: Int
  , targetIndex :: Int
  }

-- | Data for a chord label
type ChordLabel =
  { text :: String
  , x :: Number
  , y :: Number
  , anchor :: String
  , rotation :: Number
  , index :: Int
  }

-- | Color palette for modules - distinguishing central vs neighbors
centralModuleColor :: String
centralModuleColor = "#4299e1"  -- Blue for central module

neighborImportColor :: String
neighborImportColor = "#48bb78"  -- Green for modules we import

neighborDependentColor :: String
neighborDependentColor = "#ed8936"  -- Orange for modules that depend on us

-- | Get color for a module based on its relationship to the central module
-- | importNames and dependentNames are module names resolved from IDs
getModuleColor :: String -> String -> Array String -> Array String -> String
getModuleColor moduleName centralName importNames dependentNames
  | moduleName == centralName = centralModuleColor
  | Array.elem moduleName importNames = neighborImportColor
  | Array.elem moduleName dependentNames = neighborDependentColor
  | otherwise = "#a0aec0"  -- Gray fallback

-- | Convert IDs to names using a node list
idsToNames :: Array Int -> Array SimNode -> Array String
idsToNames ids nodes =
  let idToName = Map.fromFoldable $ map (\n -> Tuple n.id n.name) nodes
  in Array.mapMaybe (\id -> Map.lookup id idToName) ids

-- | Accessor helpers for chord data
getSourceIndex :: Datum_ -> Int
getSourceIndex d = (unsafeCoerce d).source.index

getTargetIndex :: Datum_ -> Int
getTargetIndex d = (unsafeCoerce d).target.index

getGroupIndex :: Datum_ -> Int
getGroupIndex d = (unsafeCoerce d).index

-- | Get start/end angles from group datum
getGroupAngles :: Datum_ -> { startAngle :: Number, endAngle :: Number }
getGroupAngles d = unsafeCoerce d

-- | Build adjacency matrix from neighborhood nodes
-- | Returns (matrix, moduleNames in order)
buildNeighborhoodMatrix :: String -> Array SimNode -> { matrix :: Array (Array Number), names :: Array String }
buildNeighborhoodMatrix centralName nodes =
  let
    -- Get all unique module names, putting central module first
    allNames = nub $ [centralName] <> map _.name nodes

    -- Build a map from name to matrix index
    nameToIdx = Map.fromFoldable $ mapWithIndex (\i n -> Tuple n i) allNames

    -- Build a map from node ID to node name (for looking up targets/sources)
    idToName = Map.fromFoldable $ map (\n -> Tuple n.id n.name) nodes

    -- Initialize zero matrix
    zeroRow = map (const 0.0) allNames

    -- For each node, add its connections to the matrix
    -- Connection value = 1.0 for each import relationship
    addConnections :: Array (Array Number) -> SimNode -> Array (Array Number)
    addConnections matrix node =
      let
        nodeIdx = fromMaybe 0 $ Map.lookup node.name nameToIdx
        -- Add outgoing edges (node imports targets) - targets are IDs
        matrixWithOutgoing = foldlTargets node.targets nodeIdx matrix
        -- Add incoming edges (sources import node) - sources are IDs
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

    -- Helper to set a value in the matrix
    setMatrixValue :: Array (Array Number) -> Int -> Int -> Number -> Array (Array Number)
    setMatrixValue m row col val =
      case m !! row of
        Just rowArr ->
          let newRow = fromMaybe rowArr $ Array.updateAt col val rowArr
          in fromMaybe m $ Array.updateAt row newRow m
        Nothing -> m

    -- Start with zero matrix and add all connections
    initialMatrix = map (const zeroRow) allNames
    finalMatrix = Array.foldl addConnections initialMatrix nodes
  in
    { matrix: finalMatrix, names: allNames }

-- | Render a chord diagram for a neighborhood
renderNeighborhoodChord :: String -> Array SimNode -> Number -> Number -> Effect Unit
renderNeighborhoodChord centralName nodes containerWidth containerHeight = do
  log $ "[ChordDiagram] Rendering chord for " <> centralName <> " with " <> show (length nodes) <> " nodes"

  -- Clear any existing diagram
  clearChordSvg_

  -- Find the central node to get its imports/dependents (as IDs)
  let mCentralNode = Array.find (\n -> n.name == centralName) nodes
  let importIds = case mCentralNode of
                    Just cn -> cn.targets
                    Nothing -> []
  let dependentIds = case mCentralNode of
                       Just cn -> cn.sources
                       Nothing -> []
  -- Convert IDs to names for color lookups
  let imports = idsToNames importIds nodes
  let dependents = idsToNames dependentIds nodes

  -- Build adjacency matrix
  let { matrix, names } = buildNeighborhoodMatrix centralName nodes

  log $ "[ChordDiagram] Matrix has " <> show (length names) <> " modules"

  -- Skip if matrix is trivial
  if length names < 2 then do
    log "[ChordDiagram] Not enough modules for chord diagram"
    pure unit
  else do
    -- Create chord layout
    let chordData = chordLayoutWithPadAngle_ matrix 0.04
    let groups = chordGroups_ chordData
    let chords = chordArray_ chordData

    log $ "[ChordDiagram] Chord layout: " <> show (length groups) <> " groups, " <> show (length chords) <> " chords"

    -- Dimensions - use container size
    let w = containerWidth
    let h = containerHeight
    let outerR = (min w h) / 2.0 - 60.0  -- Leave room for labels
    let innerR = outerR - 12.0

    -- Create generators
    let ribbonGen = setRibbonRadius_ (ribbonGenerator_ unit) innerR
    let arcGen = setArcOuterRadius_ (setArcInnerRadius_ (arcGenerator_ unit) innerR) outerR

    -- Build arc data
    let arcs = mapWithIndex (\i groupDatum ->
          let
            idx = getGroupIndex groupDatum
            moduleName = fromMaybe "" (names !! idx)
            color = getModuleColor moduleName centralName imports dependents
          in
            { path: arcPath_ arcGen groupDatum
            , color
            , moduleName
            , index: idx
            }) groups

    -- Build ribbon data - color by source module
    let ribbons = mapWithIndex (\i chordDatum ->
          let
            srcIdx = getSourceIndex chordDatum
            tgtIdx = getTargetIndex chordDatum
            srcName = fromMaybe "" (names !! srcIdx)
            tgtName = fromMaybe "" (names !! tgtIdx)
            color = getModuleColor srcName centralName imports dependents
          in
            { path: ribbonPath_ ribbonGen chordDatum
            , color
            , sourceModule: srcName
            , targetModule: tgtName
            , sourceIndex: srcIdx
            , targetIndex: tgtIdx
            }) chords

    -- Build label data
    let labels = mapWithIndex (\i groupDatum ->
          let
            idx = getGroupIndex groupDatum
            moduleName = fromMaybe "" (names !! idx)
            angles = getGroupAngles groupDatum
            angle = (angles.startAngle + angles.endAngle) / 2.0
            labelR = outerR + 10.0
            labelX = labelR * cos (angle - pi / 2.0)
            labelY = labelR * sin (angle - pi / 2.0)
            -- Text anchor based on position
            anchor = if angle > pi then "end" else "start"
            -- Rotation for readability
            rotation = (angle * 180.0 / pi) - 90.0
          in
            { text: shortenModuleName moduleName
            , x: labelX
            , y: labelY
            , anchor
            , rotation
            , index: idx
            }) groups

    -- Render via FFI
    renderChordDiagram_ w h arcs ribbons labels

    log "[ChordDiagram] Chord diagram rendered"

-- | Shorten module name for display
shortenModuleName :: String -> String
shortenModuleName name =
  let parts = splitOnDot name
  in case Array.last parts of
       Just lastPart -> lastPart
       Nothing -> name

-- | Split string on dots
splitOnDot :: String -> Array String
splitOnDot = splitOnDotImpl

foreign import splitOnDotImpl :: String -> Array String

-- | Clear the chord diagram
clearChordDiagram :: Effect Unit
clearChordDiagram = clearChordSvg_
