-- | DataViz.Layout.Sankey.ComputeWithSteps
-- |
-- | Version of Sankey layout that captures intermediate states at each iteration.
-- | Used for debugging and visual comparison with D3's relaxation process.
module DataViz.Layout.Sankey.ComputeWithSteps
  ( computeLayoutWithSteps
  ) where

import Prelude

import Control.Monad.State (State, execState, get, modify_)
import Data.Array (catMaybes, filter, foldl, length, mapWithIndex, snoc, (!!))
import Data.Array as Array
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (pow)
import Data.Set as Set
import DataViz.Layout.Sankey.Types (Alignment(..), DependencyMap, LinkCSVRow, NodeID(..), LinkID(..), SankeyConfig, SankeyGraphModel, SankeyLink, SankeyNode, SankeyStep, defaultSankeyConfig, initialSankeyGraphModel, initialiseSankeyLink, initialiseSankeyNode)

-- Constants
epsilon :: Number
epsilon = 1.0e-6

-- | Compute Sankey layout with intermediate steps captured
computeLayoutWithSteps
  :: Array LinkCSVRow
  -> Number  -- width
  -> Number  -- height
  -> Int     -- maxIterations
  -> Array SankeyStep
computeLayoutWithSteps linkInputs width height maxIterations =
  let
    config = (defaultSankeyConfig width height) { iterations = maxIterations }
    -- Run the State computation to build and capture all steps
    finalModel = execState (computeSankeyLayoutWithSteps linkInputs maxIterations) $ initialSankeyGraphModel config
  in
    finalModel.capturedSteps

-- | Extended graph model that captures steps
type GraphModelWithSteps =
  { sankeyNodes :: Array SankeyNode
  , sankeyLinks :: Array SankeyLink
  , nodeNameToID :: M.Map String NodeID
  , nodeIDToName :: M.Map NodeID String
  , nodeOrder :: Array NodeID
  , deps :: DependencyMap
  , sped :: DependencyMap
  , nodeCount :: Int
  , linkCount :: Int
  , config :: SankeyConfig
  , capturedSteps :: Array SankeyStep
  }

-- | State computation that runs all layout steps and captures intermediate states
computeSankeyLayoutWithSteps :: Array LinkCSVRow -> Int -> State SankeyGraphModel Unit
computeSankeyLayoutWithSteps linkInputs maxIterations = do
  -- Step 1: Build graph from link inputs (creates nodes and links)
  for_ linkInputs processCSVLink
  initialiseSankeyNodes

  -- Step 2: Compute node values (max flow through each node)
  computeNodeValues

  -- Step 3: Compute node depths (left-to-right BFS)
  computeNodeDepths

  -- Step 4: Compute node heights (right-to-left BFS)
  computeNodeHeights

  -- Step 5: Compute node layers and x positions
  computeNodeLayers

  -- Step 6: Initialize y positions (stacking)
  initializeNodeBreadths

  -- Capture step 0: after initialization (before relaxation)
  captureStep 0 "Initial (no relaxation)"

  -- Step 7: Run relaxation iterations and capture each step
  runRelaxationWithCapture maxIterations

  -- Step 8: Compute link breadths (after all relaxation)
  computeLinkBreadths

  -- Step 9: Assign colors
  assignColors

-- | Capture current state as a step
captureStep :: Int -> String -> State SankeyGraphModel Unit
captureStep iteration label = do
  model <- get
  let step =
        { iteration
        , label
        , nodes: model.sankeyNodes
        , links: model.sankeyLinks
        }
  modify_ _ { capturedSteps = Array.snoc model.capturedSteps step }

-- | Run relaxation with capture after each iteration
runRelaxationWithCapture :: Int -> State SankeyGraphModel Unit
runRelaxationWithCapture maxIterations = do
  for_ (Array.range 0 (maxIterations - 1)) $ \i -> do
    model <- get
    let config = model.config
    let padding = calculateAdjustedPadding model.sankeyNodes config
    let alpha = pow 0.99 (toNumber i)
    let beta = max (1.0 - alpha) (toNumber (i + 1) / toNumber maxIterations)

    -- Relax right to left
    let rtlNodes = relaxByLayer model.sankeyNodes model.sankeyLinks padding false alpha beta
    modify_ _ { sankeyNodes = rtlNodes }

    -- Then left to right
    model' <- get
    let ltrNodes = relaxByLayer model'.sankeyNodes model'.sankeyLinks padding true alpha beta
    modify_ _ { sankeyNodes = ltrNodes }

    -- Capture this iteration
    captureStep (i + 1) ("After " <> show (i + 1) <> " relaxation iteration" <> if i > 0 then "s" else "")

-- ============================================================================
-- Helper Functions (copied from Compute.purs for self-containment)
-- ============================================================================

-- | utility to update a dependency map
unionInsert :: NodeID -> NodeID -> DependencyMap -> DependencyMap
unionInsert sid tid deps = M.insert sid targetSet deps
  where
  targetSet =
    case M.lookup sid deps of
      Nothing -> Set.singleton tid
      (Just targets) -> Set.insert tid targets

-- | Process each row of the CSV
processCSVLink :: LinkCSVRow -> State SankeyGraphModel Unit
processCSVLink link = do
  model <- get
  let
    s = M.lookup link.s model.nodeNameToID
    t = M.lookup link.t model.nodeNameToID
    { sid, tid, nodeCount, newNodes } =
      case s, t of
        (Just sid), (Just tid) -> { nodeCount: model.nodeCount, sid, tid, newNodes: [] }
        (Just sid), Nothing -> { nodeCount: model.nodeCount + 1, sid, tid: NodeID model.nodeCount, newNodes: [ NodeID model.nodeCount ] }
        Nothing, (Just tid) -> { nodeCount: model.nodeCount + 1, sid: NodeID model.nodeCount, tid: tid, newNodes: [ NodeID model.nodeCount ] }
        Nothing, Nothing -> { nodeCount: model.nodeCount + 2, sid: NodeID model.nodeCount, tid: NodeID (model.nodeCount + 1), newNodes: [ NodeID model.nodeCount, NodeID (model.nodeCount + 1) ] }
  modify_ _
    { linkCount = model.linkCount + 1
    , nodeCount = nodeCount
    , nodeNameToID = M.insert link.s sid $ M.insert link.t tid model.nodeNameToID
    , nodeIDToName = M.insert sid link.s $ M.insert tid link.t model.nodeIDToName
    , nodeOrder = model.nodeOrder <> newNodes
    , deps = unionInsert sid tid model.deps
    , sped = unionInsert tid sid model.sped
    , sankeyLinks = snoc model.sankeyLinks $ initialiseSankeyLink { source: sid, target: tid, value: link.v, id: LinkID model.linkCount }
    }

initialiseSankeyNodes :: State SankeyGraphModel Unit
initialiseSankeyNodes = do
  model <- get
  let sankeyNodes = catMaybes $ (initialiseSankeyNode model) <$> model.nodeOrder
  modify_ _ { sankeyNodes = sankeyNodes }

computeNodeValues :: State SankeyGraphModel Unit
computeNodeValues = do
  model <- get
  let updatedNodes = (setNodeValue model.sankeyLinks) <$> model.sankeyNodes
  modify_ _ { sankeyNodes = updatedNodes }
  where
  setNodeValue :: Array SankeyLink -> SankeyNode -> SankeyNode
  setNodeValue links n = n { value = max totalIn totalOut }
    where
    totalIn = foldl (\sum link -> sum + link.value) 0.0 $ filter (\l -> l.targetIndex == n.index) links
    totalOut = foldl (\sum link -> sum + link.value) 0.0 $ filter (\l -> l.sourceIndex == n.index) links

computeNodeDepths :: State SankeyGraphModel Unit
computeNodeDepths = do
  model <- get
  let n = length model.sankeyNodes
  let sourceNodes = filter (\node -> Set.isEmpty node.targetLinks) model.sankeyNodes
  let initialCurrent = Set.fromFoldable $ map (_.index) sourceNodes
  let result = bfsDepth model.sankeyNodes 0 initialCurrent n
  modify_ _ { sankeyNodes = result }
  where
  bfsDepth :: Array SankeyNode -> Int -> Set.Set NodeID -> Int -> Array SankeyNode
  bfsDepth nodes depth current maxDepth
    | Set.isEmpty current = nodes
    | depth > maxDepth = nodes
    | otherwise =
        let
          nodesWithDepth = map (\node -> if Set.member node.index current then node { depth = depth } else node) nodes
          next = foldl (\acc node -> if Set.member node.index current then Set.union acc node.sourceLinks else acc) Set.empty nodes
        in bfsDepth nodesWithDepth (depth + 1) next maxDepth

computeNodeHeights :: State SankeyGraphModel Unit
computeNodeHeights = do
  model <- get
  let n = length model.sankeyNodes
  let sinkNodes = filter (\node -> Set.isEmpty node.sourceLinks) model.sankeyNodes
  let initialCurrent = Set.fromFoldable $ map (_.index) sinkNodes
  let result = bfsHeight model.sankeyNodes 0 initialCurrent n
  modify_ _ { sankeyNodes = result }
  where
  bfsHeight :: Array SankeyNode -> Int -> Set.Set NodeID -> Int -> Array SankeyNode
  bfsHeight nodes height current maxHeight
    | Set.isEmpty current = nodes
    | height > maxHeight = nodes
    | otherwise =
        let
          nodesWithHeight = map (\node -> if Set.member node.index current then node { nodeHeight = height } else node) nodes
          next = foldl (\acc node -> if Set.member node.index current then Set.union acc node.targetLinks else acc) Set.empty nodes
        in bfsHeight nodesWithHeight (height + 1) next maxHeight

computeNodeLayers :: State SankeyGraphModel Unit
computeNodeLayers = do
  model <- get
  let config = model.config
  let extent = config.extent
  let maxDepth = foldl (\acc node -> max acc node.depth) 0 model.sankeyNodes
  let numLayers = maxDepth + 1
  let kx = (extent.x1 - extent.x0 - config.nodeWidth) / max 1.0 (toNumber (numLayers - 1))

  let nodesWithLayer = map
        (\node ->
          let layer = alignNodeToLayer config.alignment node numLayers
              x0Pos = extent.x0 + toNumber layer * kx
          in node { layer = layer, x0 = x0Pos, x1 = x0Pos + config.nodeWidth }
        )
        model.sankeyNodes

  modify_ _ { sankeyNodes = nodesWithLayer }

alignNodeToLayer :: Alignment -> SankeyNode -> Int -> Int
alignNodeToLayer alignment node numLayers =
  case alignment of
    Left -> node.depth
    Right -> max 0 (numLayers - 1 - node.nodeHeight)
    Justify ->
      if Set.isEmpty node.sourceLinks then numLayers - 1
      else node.depth
    Center ->
      if not (Set.isEmpty node.targetLinks) then node.depth
      else 0

initializeNodeBreadths :: State SankeyGraphModel Unit
initializeNodeBreadths = do
  model <- get
  let config = model.config
  let extent = config.extent
  let padding = calculateAdjustedPadding model.sankeyNodes config
  let ky = calculateGlobalKy model.sankeyNodes extent.y0 extent.y1 padding

  -- Group by layer
  let maxLayer = foldl (\acc node -> max acc node.layer) 0 model.sankeyNodes
  let layers = map (\layerIdx -> filter (\n -> n.layer == layerIdx) model.sankeyNodes) (Array.range 0 maxLayer)

  -- Position each layer
  let positionedLayers = map (positionLayer ky padding extent.y0 extent.y1) layers
  let positioned = Array.concat positionedLayers

  -- Set link widths
  let linksWithWidth = map (\link -> link { width = link.value * ky }) model.sankeyLinks

  modify_ _ { sankeyNodes = positioned, sankeyLinks = linksWithWidth }

calculateAdjustedPadding :: Array SankeyNode -> SankeyConfig -> Number
calculateAdjustedPadding nodes config =
  let
    totalHeight = config.extent.y1 - config.extent.y0
    maxLayer = foldl (\acc node -> max acc node.layer) 0 nodes
    layers = map (\layerIdx -> filter (\n -> n.layer == layerIdx) nodes) (Array.range 0 maxLayer)
    maxNodesInLayer = foldl (\acc layer -> max acc (length layer)) 0 layers
  in
    if maxNodesInLayer > 1
    then min config.nodePadding (totalHeight / (toNumber maxNodesInLayer - 1.0))
    else config.nodePadding

calculateGlobalKy :: Array SankeyNode -> Number -> Number -> Number -> Number
calculateGlobalKy nodes y0 y1 padding =
  let
    maxLayer = foldl (\acc node -> max acc node.layer) 0 nodes
    layers = map (\layerIdx -> filter (\node -> node.layer == layerIdx) nodes) (Array.range 0 maxLayer)
    scaleForLayer layerNodes =
      let
        totalValue = foldl (\sum node -> sum + node.value) 0.0 layerNodes
        numNodes = toNumber (length layerNodes)
        availableHeight = (y1 - y0) - (numNodes - 1.0) * padding
      in if totalValue > 0.0 then availableHeight / totalValue else 1.0
    scales = map scaleForLayer layers
    nonEmptyScales = filter (\s -> s > 0.0) scales
  in
    case Array.head nonEmptyScales of
      Just firstScale -> foldl min firstScale nonEmptyScales
      Nothing -> 1.0

positionLayer :: Number -> Number -> Number -> Number -> Array SankeyNode -> Array SankeyNode
positionLayer ky padding yStart yEnd layerNodes =
  let
    stacked = foldl
      (\acc node ->
        let
          currentY = acc.y
          nodeHeight = node.value * ky
          positioned = node { y0 = currentY, y1 = currentY + nodeHeight }
        in { nodes: Array.snoc acc.nodes positioned, y: currentY + nodeHeight + padding }
      )
      { nodes: [], y: yStart }
      layerNodes

    -- Add extra spacing adjustment (D3 sankey.js:223-228)
    lastY = stacked.y
    extraSpacing = (yEnd - lastY + padding) / (toNumber (length layerNodes) + 1.0)
    adjusted = mapWithIndex
      (\i node ->
        let shift = extraSpacing * (toNumber (i + 1))
        in node { y0 = node.y0 + shift, y1 = node.y1 + shift }
      )
      stacked.nodes
  in adjusted

-- | Relax all nodes layer by layer
relaxByLayer :: Array SankeyNode -> Array SankeyLink -> Number -> Boolean -> Number -> Number -> Array SankeyNode
relaxByLayer currentNodes allLinks padding ascending alpha beta =
  let
    maxLayer = foldl (\acc node -> max acc node.layer) 0 currentNodes
    layerIndices = if ascending then Array.range 0 maxLayer else Array.reverse (Array.range 0 maxLayer)
  in foldl (\nodes layerIdx -> relaxLayer nodes allLinks layerIdx padding ascending alpha beta) currentNodes layerIndices

-- | Relax a single layer
relaxLayer :: Array SankeyNode -> Array SankeyLink -> Int -> Number -> Boolean -> Number -> Number -> Array SankeyNode
relaxLayer currentNodes allLinks layerIdx padding ascending alpha beta =
  let
    layerNodeIndices = map _.index $ filter (\n -> n.layer == layerIdx) currentNodes

    nodesAfterRelax = foldl
      (\nodes nodeIdx ->
        case Array.find (\n -> n.index == nodeIdx) nodes of
          Nothing -> nodes
          Just node ->
            let
              weightedY = if ascending
                then calculateWeightedFromSources node nodes allLinks
                else calculateWeightedFromTargets node nodes allLinks
              dy = (weightedY - node.y0) * alpha
              newY0 = node.y0 + dy
              newY1 = node.y1 + dy
              updatedNode = node { y0 = newY0, y1 = newY1 }
            in map (\n -> if n.index == nodeIdx then updatedNode else n) nodes
      )
      currentNodes
      layerNodeIndices

    layerNodesUpdated = filter (\n -> n.layer == layerIdx) nodesAfterRelax
    sorted = Array.sortBy (\a b -> compare a.y0 b.y0) layerNodesUpdated
    sortedWithTarget = map (\node -> { node, targetY: node.y0 }) sorted
    resolved = resolveCollisions sortedWithTarget padding beta
  in updateNodes nodesAfterRelax resolved

calculateWeightedFromTargets :: SankeyNode -> Array SankeyNode -> Array SankeyLink -> Number
calculateWeightedFromTargets node allNodes allLinks =
  let
    outgoingLinks = filter (\l -> l.sourceIndex == node.index) allLinks
    result = foldl
      (\acc link ->
        case Array.find (\n -> n.index == link.targetIndex) allNodes of
          Just targetNode ->
            let
              layerDist = toNumber (targetNode.layer - node.layer)
              v = link.value * max 1.0 layerDist
              idealY = (targetNode.y0 + targetNode.y1) / 2.0 - (node.y1 - node.y0) / 2.0
            in { sum: acc.sum + idealY * v, totalWeight: acc.totalWeight + v }
          Nothing -> acc
      )
      { sum: 0.0, totalWeight: 0.0 }
      outgoingLinks
  in if result.totalWeight > 0.0 then result.sum / result.totalWeight else node.y0

calculateWeightedFromSources :: SankeyNode -> Array SankeyNode -> Array SankeyLink -> Number
calculateWeightedFromSources node allNodes allLinks =
  let
    incomingLinks = filter (\l -> l.targetIndex == node.index) allLinks
    result = foldl
      (\acc link ->
        case Array.find (\n -> n.index == link.sourceIndex) allNodes of
          Just sourceNode ->
            let
              layerDist = toNumber (node.layer - sourceNode.layer)
              v = link.value * max 1.0 layerDist
              idealY = (sourceNode.y0 + sourceNode.y1) / 2.0 - (node.y1 - node.y0) / 2.0
            in { sum: acc.sum + idealY * v, totalWeight: acc.totalWeight + v }
          Nothing -> acc
      )
      { sum: 0.0, totalWeight: 0.0 }
      incomingLinks
  in if result.totalWeight > 0.0 then result.sum / result.totalWeight else node.y0

resolveCollisions :: Array { node :: SankeyNode, targetY :: Number } -> Number -> Number -> Array { node :: SankeyNode, targetY :: Number }
resolveCollisions sorted padding beta =
  if Array.null sorted then sorted
  else
    let
      n = Array.length sorted
      middleIdx = n / 2

      middleNode = sorted !! middleIdx

      startYUp = case middleNode of
        Just m -> m.node.y0 - padding
        Nothing -> 0.0

      pushedUp = resolveCollisionsBottomToTop sorted (middleIdx - 1) startYUp padding beta

      startYDown = case middleNode of
        Just m -> m.node.y1 + padding
        Nothing -> 0.0

      pushedUpAndDown = resolveCollisionsTopToBottom pushedUp (middleIdx + 1) startYDown padding beta
    in pushedUpAndDown

resolveCollisionsBottomToTop :: Array { node :: SankeyNode, targetY :: Number } -> Int -> Number -> Number -> Number -> Array { node :: SankeyNode, targetY :: Number }
resolveCollisionsBottomToTop nodes startIdx targetY padding beta =
  if startIdx < 0 then nodes
  else
    let
      go :: Int -> Number -> Array { node :: SankeyNode, targetY :: Number } -> Array { node :: SankeyNode, targetY :: Number }
      go idx y currentNodes =
        if idx < 0 then currentNodes
        else case currentNodes !! idx of
          Nothing -> currentNodes
          Just item ->
            let
              dy = (item.node.y1 - y) * beta
              updated =
                if dy > epsilon then
                  let newNode = item.node { y0 = item.node.y0 - dy, y1 = item.node.y1 - dy }
                  in fromMaybe currentNodes (Array.updateAt idx { node: newNode, targetY: item.targetY } currentNodes)
                else currentNodes
              newY = case updated !! idx of
                Just n -> n.node.y0 - padding
                Nothing -> y
            in go (idx - 1) newY updated
    in go startIdx targetY nodes

resolveCollisionsTopToBottom :: Array { node :: SankeyNode, targetY :: Number } -> Int -> Number -> Number -> Number -> Array { node :: SankeyNode, targetY :: Number }
resolveCollisionsTopToBottom nodes startIdx targetY padding beta =
  let
    n = Array.length nodes
    go :: Int -> Number -> Array { node :: SankeyNode, targetY :: Number } -> Array { node :: SankeyNode, targetY :: Number }
    go idx y currentNodes =
      if idx >= n then currentNodes
      else case currentNodes !! idx of
        Nothing -> currentNodes
        Just item ->
          let
            dy = (y - item.node.y0) * beta
            updated =
              if dy > epsilon then
                let newNode = item.node { y0 = item.node.y0 + dy, y1 = item.node.y1 + dy }
                in fromMaybe currentNodes (Array.updateAt idx { node: newNode, targetY: item.targetY } currentNodes)
              else currentNodes
            newY = case updated !! idx of
              Just n' -> n'.node.y1 + padding
              Nothing -> y
          in go (idx + 1) newY updated
  in go startIdx targetY nodes

updateNodes :: Array SankeyNode -> Array { node :: SankeyNode, targetY :: Number } -> Array SankeyNode
updateNodes allNodes updated =
  map
    (\node ->
      case Array.find (\u -> u.node.index == node.index) updated of
        Just u -> u.node
        Nothing -> node
    )
    allNodes

computeLinkBreadths :: State SankeyGraphModel Unit
computeLinkBreadths = do
  model <- get
  let linksWithY = calculateLinkYPositions model.sankeyNodes model.sankeyLinks
  modify_ _ { sankeyLinks = linksWithY }

calculateLinkYPositions :: Array SankeyNode -> Array SankeyLink -> Array SankeyLink
calculateLinkYPositions nodes links =
  let
    linksWithY0 = foldl
      (\currentLinks node ->
        let
          outgoingLinks = filter (\link -> link.sourceIndex == node.index) currentLinks
          sorted = Array.sortBy
            (\a b ->
              case Array.find (\n -> n.index == a.targetIndex) nodes, Array.find (\n -> n.index == b.targetIndex) nodes of
                Just na, Just nb -> compare na.y0 nb.y0 <> compare a.index b.index
                _, _ -> compare a.index b.index
            )
            outgoingLinks
          withY0 = foldl
            (\acc link ->
              let
                y = acc.y + link.width / 2.0
                updated = link { y0 = y }
              in { y: acc.y + link.width, links: Array.snoc acc.links updated }
            )
            { y: node.y0, links: [] }
            sorted
        in map (\link -> case Array.find (\l -> l.index == link.index) withY0.links of
                 Just updatedLink -> updatedLink
                 Nothing -> link) currentLinks
      )
      links
      nodes

    linksWithY1 = foldl
      (\currentLinks node ->
        let
          incomingLinks = filter (\link -> link.targetIndex == node.index) currentLinks
          sorted = Array.sortBy
            (\a b ->
              case Array.find (\n -> n.index == a.sourceIndex) nodes, Array.find (\n -> n.index == b.sourceIndex) nodes of
                Just na, Just nb -> compare na.y0 nb.y0 <> compare a.index b.index
                _, _ -> compare a.index b.index
            )
            incomingLinks
          withY1 = foldl
            (\acc link ->
              let
                y = acc.y + link.width / 2.0
                updated = link { y1 = y }
              in { y: acc.y + link.width, links: Array.snoc acc.links updated }
            )
            { y: node.y0, links: [] }
            sorted
        in map (\link -> case Array.find (\l -> l.index == link.index) withY1.links of
                 Just updatedLink -> updatedLink
                 Nothing -> link) currentLinks
      )
      linksWithY0
      nodes
  in linksWithY1

assignColors :: State SankeyGraphModel Unit
assignColors = do
  model <- get
  let
    colors = [ "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf" ]
    nodesWithColor = mapWithIndex
      (\i node -> node { color = fromMaybe "#999" (colors !! (i `mod` length colors)) })
      model.sankeyNodes
  modify_ _ { sankeyNodes = nodesWithColor }
