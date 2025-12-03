-- | D3.Layout.Sankey.Compute
-- |
-- | Pure PureScript implementation of Sankey diagram layout computation.
-- | Uses State monad to thread graph model through all layout functions.
-- | Starts with Array SankeyLinkInput and generates nodes dynamically.
-- |
-- | Reference: https://github.com/d3/d3-sankey (ISC License)
module D3.Layout.Sankey.Compute
  ( epsilon
  , computeLayout
  , computeLayoutWithConfig
  ) where

import Prelude

import Control.Monad.State (State, execState, get, modify_)
import Data.Array (catMaybes, filter, find, foldl, foldr, length, mapWithIndex, snoc, (!!))
import Data.Array as Array
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Map as M
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number (pow)
import Data.Set as Set
import Effect.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import D3.Layout.Sankey.Types (Alignment(..), DependencyMap, LinkCSVRow, LinkColorMode(..), LinkID(..), NodeID(..), SankeyConfig, SankeyGraphModel, SankeyLayoutResult, SankeyLink, SankeyNode, defaultSankeyConfig, initialSankeyGraphModel, initialiseSankeyLink, initialiseSankeyNode)

-- Constants
epsilon :: Number
epsilon = 1.0e-6

-- ============================================================================
-- Main Entry Points
-- ============================================================================

-- | Main entry point: Compute Sankey layout with default configuration
-- | Takes array of link inputs with named nodes (from CSV parsing)
computeLayout
  :: Array LinkCSVRow
  -> Number -- SVG width
  -> Number -- SVG height
  -> SankeyLayoutResult
computeLayout linkInputs width height =
  computeLayoutWithConfig linkInputs $ defaultSankeyConfig width height

-- | Compute Sankey layout with custom configuration
-- | This is the main algorithm using State monad to thread the graph model
computeLayoutWithConfig
  :: Array LinkCSVRow
  -> SankeyConfig
  -> SankeyLayoutResult
computeLayoutWithConfig linkInputs config =
  let
    -- Run the State computation to build and compute layout
    finalModel = execState computeSankeyLayout $ initialSankeyGraphModel config
  in
    { nodes: finalModel.sankeyNodes
    , links: finalModel.sankeyLinks
    }
  where
  -- State computation that runs all layout steps
  computeSankeyLayout :: State SankeyGraphModel Unit
  computeSankeyLayout = do
    -- Step 1: Build graph from link inputs (creates nodes and links)
    for_ linkInputs processCSVLink
    initialiseSankeyNodes
    logStep "After initialiseSankeyNodes (nodeLinks)"

    -- Step 2: Compute node links (populate sourceLinks/targetLinks arrays)

    -- Step 3: Compute node values (max flow through each node)
    computeNodeValues
    logStep "After computeNodeValues"

    -- Step 4: Compute node depths (left-to-right BFS)
    computeNodeDepths
    logStep "After computeNodeDepths"

    -- Step 5: Compute node heights (right-to-left BFS)
    computeNodeHeights
    logStep "After computeNodeHeights"

    -- Step 6: Compute node breadths (vertical positioning with relaxation)
    computeNodeBreadths
    logStep "After computeNodeBreadths (FINAL)"

    -- Step 7: Compute link breadths (link y0/y1 positions)
    computeLinkBreadths

    -- Step 8: Assign colors
    assignColors

  -- Helper to log current node state
  logStep :: String -> State SankeyGraphModel Unit
  logStep stepName = do
    model <- get
    let
      _ = unsafePerformEffect $ do
        log $ "\n=== " <> stepName <> " ==="
        for_ (Array.sortBy (\a b -> compare a.name b.name) model.sankeyNodes) $ \n -> do
          log $ "  " <> n.name
            <> " | layer=" <> show n.layer
            <> " | depth=" <> show n.depth
            <> " | height=" <> show n.nodeHeight
            <> " | value=" <> show n.value
            <> " | y0=" <> show n.y0
            <> " | y1=" <> show n.y1
    pure unit

-- ============================================================================
-- Step 1: Build Graph from Link Inputs
-- ============================================================================

-- | utility to update a dependency map, adding target into set if pre-existing source id
unionInsert :: NodeID -> NodeID -> DependencyMap -> DependencyMap
unionInsert sid tid deps = M.insert sid targetSet deps
  where
  targetSet =
    case M.lookup sid deps of
      Nothing -> Set.singleton tid -- source is new, tid is first target
      (Just targets) -> Set.insert tid targets -- source exists, ensure tid is in target set

-- | Process each row of the CSV to get the information needed to build a Sankey
processCSVLink :: LinkCSVRow -> State SankeyGraphModel Unit
processCSVLink link = do
  model <- get

  let
    -- assume no cycles, s != t. We could drop cyclic links or error out but not checking at this time.
    s = M.lookup link.s model.nodeNameToID
    t = M.lookup link.t model.nodeNameToID

    -- Track which nodes are new (for nodeOrder - must match D3's Set insertion order: source first, then target)
    { sid, tid, nodeCount, newNodes } =
      case s, t of -- have we seen either or both of these nodes before?
        (Just sid), (Just tid) -> -- neither new, simple
          { nodeCount: model.nodeCount, sid, tid, newNodes: [] }
        -- one new, count increments by one
        (Just sid), Nothing ->
          { nodeCount: model.nodeCount + 1, sid, tid: NodeID model.nodeCount, newNodes: [NodeID model.nodeCount] }
        Nothing, (Just tid) ->
          { nodeCount: model.nodeCount + 1, sid: NodeID model.nodeCount, tid: tid, newNodes: [NodeID model.nodeCount] }
        -- both new, count increments by two (source added first, then target - matches D3's Set.add order)
        Nothing, Nothing ->
          { nodeCount: model.nodeCount + 2, sid: NodeID model.nodeCount, tid: NodeID (model.nodeCount + 1), newNodes: [NodeID model.nodeCount, NodeID (model.nodeCount + 1)] }

  modify_ _
    { linkCount = model.linkCount + 1
    , nodeCount = nodeCount
    , nodeNameToID = M.insert link.s sid $ M.insert link.t tid model.nodeNameToID
    , nodeIDToName = M.insert sid link.s $ M.insert tid link.t model.nodeIDToName
    , nodeOrder = model.nodeOrder <> newNodes  -- Append new nodes in encounter order
    , deps = unionInsert sid tid model.deps
    , sped = unionInsert tid sid model.sped
    , sankeyLinks =
        snoc model.sankeyLinks $ initialiseSankeyLink { source: sid, target: tid, value: link.v, id: LinkID model.linkCount }
    }

-- | Once all the links have been made and all the source and target maps are filled we can create the sankeyNodes
-- | Uses nodeOrder to preserve encounter order (matching D3's JavaScript Set insertion order)
initialiseSankeyNodes :: State SankeyGraphModel Unit
initialiseSankeyNodes = do
  model <- get
  let
    -- Use nodeOrder (encounter order) instead of Map.keys (sorted order)
    -- This matches D3's JavaScript Set which preserves insertion order
    sankeyNodes = catMaybes $ (initialiseSankeyNode model) <$> model.nodeOrder
  modify_ _ { sankeyNodes = sankeyNodes }

-- ============================================================================
-- Step 2: computeNodeLinks - Build sourceLinks/targetLinks arrays
-- ============================================================================
-- | Done in first pass in PureScript solution

-- ============================================================================
-- Step 3: computeNodeValues - Calculate node.value (max flow)
-- ============================================================================

-- | For each node, compute value as max of total outgoing and incoming flow (link values)
computeNodeValues :: State SankeyGraphModel Unit
computeNodeValues = do
  model <- get
  let updatedNodes = (setNodeValue model.sankeyLinks) <$> model.sankeyNodes
  modify_ _ { sankeyNodes = updatedNodes }
  where
  setNodeValue :: Array SankeyLink -> SankeyNode -> SankeyNode
  setNodeValue links n = n { value = max totalIn totalOut }
    where
    totalIn = foldl (\sum link -> sum + link.value) 0.0 $
      filter (\l -> l.targetIndex == n.index) links
    totalOut = foldl (\sum link -> sum + link.value) 0.0 $
      filter (\l -> l.sourceIndex == n.index) links

-- ============================================================================
-- Step 4: computeNodeDepths - Left-to-right BFS (depth)
-- ============================================================================

-- | Compute depth for each node using breadth-first search from left
-- | This gives each node the depth of its longest path from any source
-- | D3 algorithm: start with SOURCE nodes (no incoming links), assign depth, then process targets
computeNodeDepths :: State SankeyGraphModel Unit
computeNodeDepths = do
  model <- get
  let n = length model.sankeyNodes
  -- Start with source nodes only (nodes with no incoming links)
  let sourceNodes = filter (\node -> Set.isEmpty node.targetLinks) model.sankeyNodes
  let initialCurrent = Set.fromFoldable $ map (_.index) sourceNodes
  let result = bfsDepth model.sankeyNodes 0 initialCurrent n
  modify_ _ { sankeyNodes = result }
  where
  bfsDepth :: Array SankeyNode -> Int -> Set.Set NodeID -> Int -> Array SankeyNode
  bfsDepth nodes depth current maxDepth
    | Set.isEmpty current = nodes
    | depth > maxDepth = nodes -- Circular link detection
    | otherwise =
        let
          -- Update all nodes in current set to have this depth
          nodesWithDepth = map
            ( \node ->
                if Set.member node.index current then node { depth = depth }
                else node
            )
            nodes

          -- Collect all targets from nodes in current set (targets = sourceLinks)
          next = foldl
            ( \acc node ->
                if Set.member node.index current then Set.union acc node.sourceLinks -- sourceLinks is Set NodeID
                else acc
            )
            Set.empty
            nodes
        in
          bfsDepth nodesWithDepth (depth + 1) next maxDepth

-- ============================================================================
-- Step 5: computeNodeHeights - Right-to-left BFS (height)
-- ============================================================================

-- | Compute height for each node using breadth-first search from right
-- | Mirror of computeNodeDepths but going backwards using targetLinks
-- | Start with SINK nodes (no outgoing links), then process sources
computeNodeHeights :: State SankeyGraphModel Unit
computeNodeHeights = do
  model <- get
  let n = length model.sankeyNodes
  -- Start with sink nodes only (nodes with no outgoing links)
  let sinkNodes = filter (\node -> Set.isEmpty node.sourceLinks) model.sankeyNodes
  let initialCurrent = Set.fromFoldable $ map (_.index) sinkNodes
  let result = bfsHeight model.sankeyNodes 0 initialCurrent n
  modify_ _ { sankeyNodes = result }
  where
  bfsHeight :: Array SankeyNode -> Int -> Set.Set NodeID -> Int -> Array SankeyNode
  bfsHeight nodes height current maxHeight
    | Set.isEmpty current = nodes
    | height > maxHeight = nodes -- Circular link detection
    | otherwise =
        let
          -- Update all nodes in current set to have this height
          nodesWithHeight = map
            ( \node ->
                if Set.member node.index current then node { nodeHeight = height }
                else node
            )
            nodes

          -- Collect all sources from nodes in current set (sources = targetLinks)
          next = foldl
            ( \acc node ->
                if Set.member node.index current then Set.union acc node.targetLinks -- targetLinks is Set NodeID
                else acc
            )
            Set.empty
            nodes
        in
          bfsHeight nodesWithHeight (height + 1) next maxHeight

-- ============================================================================
-- Step 6: computeNodeBreadths - Vertical positioning with relaxation
-- ============================================================================

-- | Calculate the global scale factor (ky) - pixels per unit of value
-- | Uses the minimum scale across all layers to ensure all nodes fit
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
      in
        if totalValue > 0.0 then availableHeight / totalValue else 1.0

    scales = map scaleForLayer layers
    nonEmptyScales = filter (\s -> s > 0.0) scales
  in
    -- Use minimum scale to ensure all nodes fit
    case Array.head nonEmptyScales of
      Just firstScale -> foldl min firstScale nonEmptyScales
      Nothing -> 1.0

-- | Compute node layers, x positions, and y positions with relaxation
-- | This is the main positioning algorithm that makes the Sankey look good
computeNodeBreadths :: State SankeyGraphModel Unit
computeNodeBreadths = do
  model <- get
  let config = model.config
  let extent = config.extent

  -- Step 6a: Assign nodes to layers and compute x positions
  let nodesWithLayers = computeNodeLayers model.sankeyNodes config extent.x0 extent.x1

  -- Calculate adjusted padding BEFORE initializeNodeBreadths
  -- D3 sankey.js:219: py = Math.min(py, (y1 - y0) / (d3.max(columns, c => c.length) - 1))
  let totalHeight = extent.y1 - extent.y0
  let maxLayer = foldl (\acc node -> max acc node.layer) 0 nodesWithLayers
  let layers = map (\layerIdx -> filter (\node -> node.layer == layerIdx) nodesWithLayers) (Array.range 0 maxLayer)
  let maxNodesInLayer = foldl (\acc layer -> max acc (length layer)) 0 layers
  let adjustedPadding = if maxNodesInLayer > 1
        then min config.nodePadding (totalHeight / (toNumber maxNodesInLayer - 1.0))
        else config.nodePadding

  -- Step 6b: Initialize y positions by stacking nodes in each layer
  let nodesWithY = initializeNodeBreadths nodesWithLayers model.sankeyLinks config adjustedPadding extent.y0 extent.y1

  -- CRITICAL: Set link widths NOW, before relaxation (D3 does this in initializeNodeBreadths)
  -- Without this, targetTop/sourceTop use link.width=0 and calculations are wrong
  let ky = calculateGlobalKy nodesWithY extent.y0 extent.y1 adjustedPadding
  let linksWithWidth = map (\link -> link { width = link.value * ky }) model.sankeyLinks

  -- DEBUG: Log positions AFTER initializeNodeBreadths (before relaxation)
  let
    _ = unsafePerformEffect $ do
      log $ "\n=== After initializeNodeBreadths (ky=" <> show ky <> ", adjustedPadding=" <> show adjustedPadding <> ") ==="
      -- Group by layer for easier comparison with D3
      let maxLayer' = foldl (\acc node -> max acc node.layer) 0 nodesWithY
      for_ (Array.range 0 maxLayer') $ \layerIdx -> do
        let layerNodes = Array.sortBy (\a b -> compare a.y0 b.y0) $ filter (\n -> n.layer == layerIdx) nodesWithY
        log $ "  Layer " <> show layerIdx <> ":"
        for_ layerNodes $ \n ->
          log $ "    " <> n.name <> " | y0=" <> show n.y0 <> " | y1=" <> show n.y1 <> " | value=" <> show n.value

  -- Step 6c: Run relaxation iterations to minimize crossings
  -- D3 doesn't pre-sort, it relies on relaxation + collision resolution to establish order
  -- IMPORTANT: Use linksWithWidth so targetTop/sourceTop have correct link widths
  -- Pass adjustedPadding to ensure consistent spacing throughout the algorithm
  let relaxedNodes = relaxation nodesWithY linksWithWidth config adjustedPadding extent.y0 extent.y1

  -- DEBUG: Log positions AFTER relaxation for key nodes
  let
    _ = unsafePerformEffect $ do
      case find (\n -> n.name == "Nuclear") relaxedNodes of
        Just n -> log $ "AFTER relaxation - Nuclear: y0=" <> show n.y0 <> ", y1=" <> show n.y1
        Nothing -> pure unit
      case find (\n -> n.name == "Thermal generation") relaxedNodes of
        Just n -> log $ "AFTER relaxation - Thermal generation: y0=" <> show n.y0 <> ", y1=" <> show n.y1
        Nothing -> pure unit
      case find (\n -> n.name == "Losses") relaxedNodes of
        Just n -> log $ "AFTER relaxation - Losses: y0=" <> show n.y0 <> ", y1=" <> show n.y1
        Nothing -> pure unit

  -- Store both nodes AND links (with widths) so computeLinkBreadths can use them
  modify_ _ { sankeyNodes = relaxedNodes, sankeyLinks = linksWithWidth }

-- | Pre-sort nodes by topology to establish initial order before relaxation
-- | Sorts nodes within each layer by weighted average of their target y-positions
reorderNodesByTopology :: Array SankeyNode -> Array SankeyLink -> Number -> Number -> Array SankeyNode -- TODO not used?
reorderNodesByTopology nodes links padding yStart =
  let
    maxLayer = foldl (\acc node -> max acc node.layer) 0 nodes

    -- Group nodes by layer
    layers = map
      ( \layerIdx ->
          filter (\n -> n.layer == layerIdx) nodes
      )
      (Array.range 0 maxLayer)

    -- Sort each layer by weighted y-position of targets
    sortedLayers = map (sortLayerByTargets nodes links) layers

    -- Re-position nodes vertically with new order
    repositionedLayers = map (repositionLayer padding yStart) sortedLayers

    repositioned = Array.concat repositionedLayers
  in
    repositioned
  where

  -- Sort nodes in a layer by weighted average of target y-positions
  sortLayerByTargets :: Array SankeyNode -> Array SankeyLink -> Array SankeyNode -> Array SankeyNode
  sortLayerByTargets allNodes allLinks layerNodes =
    Array.sortBy
      ( \a b ->
          let
            aCenter = weightedTargetCenter a allNodes allLinks
            bCenter = weightedTargetCenter b allNodes allLinks
          in
            compare aCenter bCenter
      )
      layerNodes

  -- Calculate weighted center of target nodes (for sorting)
  weightedTargetCenter :: SankeyNode -> Array SankeyNode -> Array SankeyLink -> Number
  weightedTargetCenter node allNodes allLinks =
    let
      -- Find outgoing links and weight by value
      targetSum = foldl
        ( \acc link ->
            if link.sourceIndex == node.index then
              case find (\n -> n.index == link.targetIndex) allNodes of
                Just targetNode ->
                  let
                    center = (targetNode.y0 + targetNode.y1) / 2.0
                  in
                    { sum: acc.sum + center * link.value, weight: acc.weight + link.value }
                Nothing -> acc
            else acc
        )
        { sum: 0.0, weight: 0.0 }
        allLinks
    in
      -- For source nodes (no targets), use current position
      if targetSum.weight > 0.0 then targetSum.sum / targetSum.weight
      else (node.y0 + node.y1) / 2.0

  -- Re-position nodes in layer after sorting
  repositionLayer :: Number -> Number -> Array SankeyNode -> Array SankeyNode
  repositionLayer padding' yStart' layerNodes =
    let
      positioned = foldl
        ( \acc node ->
            let
              height = node.y1 - node.y0
              newNode = node { y0 = acc.y, y1 = acc.y + height }
            in
              { y: acc.y + height + padding'
              , nodes: Array.snoc acc.nodes newNode
              }
        )
        { y: yStart', nodes: [] }
        layerNodes
    in
      positioned.nodes

-- | Step 6a: Compute node layers and x positions based on alignment strategy
computeNodeLayers :: Array SankeyNode -> SankeyConfig -> Number -> Number -> Array SankeyNode
computeNodeLayers nodes config x0 x1 =
  let
    maxDepth = foldl (\acc node -> max acc node.depth) 0 nodes
    numLayers = maxDepth + 1
    kx = (x1 - x0 - config.nodeWidth) / max 1.0 (toNumber (numLayers - 1))

    -- Apply alignment function to determine layer for each node
    nodesWithLayer = map
      ( \node ->
          let
            layer = alignNodeToLayer config.alignment node numLayers
          in
            node { layer = layer }
      )
      nodes

    -- Set x0 and x1 based on layer
    nodesWithX = map
      ( \node ->
          let
            x0Pos = x0 + toNumber node.layer * kx
          in
            node { x0 = x0Pos, x1 = x0Pos + config.nodeWidth }
      )
      nodesWithLayer
  in
    nodesWithX

-- | Helper: Apply alignment strategy to determine which layer a node belongs to
alignNodeToLayer :: Alignment -> SankeyNode -> Int -> Int
alignNodeToLayer alignment node numLayers =
  case alignment of
    Left -> node.depth
    Right -> max 0 (numLayers - 1 - node.nodeHeight)
    Justify ->
      if Set.isEmpty node.sourceLinks -- sink node (no outgoing links)
      then numLayers - 1
      else node.depth
    Center ->
      if not (Set.isEmpty node.targetLinks) -- has incoming links
      then node.depth
      else 0 -- source node

-- | Step 6b: Initialize vertical (y) positions by stacking nodes in each layer
-- | Takes pre-calculated adjustedPadding from caller
initializeNodeBreadths :: Array SankeyNode -> Array SankeyLink -> SankeyConfig -> Number -> Number -> Number -> Array SankeyNode
initializeNodeBreadths nodes links config padding y0 y1 =
  let
    -- Group nodes by layer
    maxLayer = foldl (\acc node -> max acc node.layer) 0 nodes
    layers = map
      ( \layerIdx ->
          filter (\node -> node.layer == layerIdx) nodes
      )
      (Array.range 0 maxLayer)

    -- Calculate ky: vertical scale factor (pixels per unit of value)
    totalHeight = y1 - y0
    ky = calculateKy layers totalHeight padding

    -- DEBUG: This will help us verify the new code is running
    _ = unsafePerformEffect $ log $ "DEBUG initializeNodeBreadths: totalHeight=" <> show totalHeight <> ", ky=" <> show ky <> ", numLayers=" <> show (Array.length layers) <> ", padding=" <> show padding

    -- D3 doesn't sort initially - nodes stay in input order
    -- Relaxation and collision resolution will establish the final order

    -- Position nodes in each layer vertically (using adjusted padding from caller)
    positionedLayers = map (positionLayer ky padding y0) layers

    -- Flatten back to single array
    positioned = Array.concat positionedLayers
  in
    positioned
  where
  -- Calculate the scale factor (ky) - how many pixels per unit of value
  calculateKy :: Array (Array SankeyNode) -> Number -> Number -> Number
  calculateKy layers height padding =
    let
      scaleForLayer :: Array SankeyNode -> Number
      scaleForLayer layerNodes =
        let
          totalValue = foldl (\sum node -> sum + node.value) 0.0 layerNodes
          numNodes = toNumber (length layerNodes)
          availableHeight = height - (numNodes - 1.0) * padding
          scale = if totalValue > 0.0 then availableHeight / totalValue else 1.0
          _ = unsafePerformEffect $ log $ "  Layer: " <> show (length layerNodes) <> " nodes, totalValue=" <> show totalValue <> ", availableHeight=" <> show availableHeight <> ", scale=" <> show scale
        in
          scale

      _ = unsafePerformEffect $ log $ "calculateKy: height=" <> show height <> ", padding=" <> show padding
      scales = map scaleForLayer layers
      nonEmptyScales = filter (\s -> s > 0.0) scales
      result = case Array.head nonEmptyScales of
        Just firstScale -> foldl min firstScale nonEmptyScales
        Nothing -> 1.0
      _ = unsafePerformEffect $ log $ "calculateKy result (min of all scales): " <> show result
    in
      result

  -- Position all nodes in a single layer
  positionLayer :: Number -> Number -> Number -> Array SankeyNode -> Array SankeyNode
  positionLayer ky padding yStart layerNodes =
    let
      -- Phase 1: Stack nodes vertically with padding
      stacked = foldl
        ( \acc node ->
            let
              currentY = acc.y
              nodeHeight = node.value * ky
              positioned = node { y0 = currentY, y1 = currentY + nodeHeight }
            in
              { nodes: Array.snoc acc.nodes positioned
              , y: currentY + nodeHeight + padding
              , finalY: acc.finalY -- Pass through
              }
        )
        { nodes: [], y: yStart, finalY: y1 }
        layerNodes

      -- Phase 2: Add extra spacing adjustment (D3 sankey.js:223-228)
      -- This distributes unused vertical space evenly across nodes
      lastY = stacked.y
      extraSpacing = (y1 - lastY + padding) / (toNumber (length layerNodes) + 1.0)

      _ = unsafePerformEffect $ log $ "  Layer extra spacing: " <> show extraSpacing <> " (lastY=" <> show lastY <> ", y1=" <> show y1 <> ")"

      adjusted = mapWithIndex
        ( \i node ->
            let
              shift = extraSpacing * (toNumber (i + 1))
            in
              node { y0 = node.y0 + shift, y1 = node.y1 + shift }
        )
        stacked.nodes
    in
      adjusted

-- | Step 6c: Relaxation - iteratively adjust node positions to minimize crossings
-- Uses exponential decay of alpha to dampen adjustments over iterations (like D3)
relaxation :: Array SankeyNode -> Array SankeyLink -> SankeyConfig -> Number -> Number -> Number -> Array SankeyNode
relaxation nodes links config padding y0 y1 =
  let
    iterations = config.iterations

    -- Run iterations with decreasing alpha
    -- D3 uses exponential decay: alpha = Math.pow(0.99, i)
    relaxed = foldl
      ( \currentNodes i ->
          let
            -- Exponential decay matching D3 exactly
            alpha = pow 0.99 (toNumber i)
            -- Beta parameter for collision resolution (increases over iterations)
            beta = max (1.0 - alpha) (toNumber (i + 1) / toNumber iterations)
            -- Relax right to left (by decreasing layer)
            rtl = relaxByLayer currentNodes links padding false alpha beta
            -- Then left to right (by increasing layer)
            ltr = relaxByLayer rtl links padding true alpha beta
            -- Log after each iteration (only for first 6 iterations to match D3 debug)
            _ = unsafePerformEffect $
                  if i < 6 then do
                    log $ "\n=== After iteration " <> show i <> " (alpha=" <> show alpha <> ", beta=" <> show beta <> ") ==="
                    for_ (Array.sortBy (\a b -> compare a.layer b.layer) ltr) $ \n ->
                      log $ "  " <> n.name <> " | layer=" <> show n.layer <> " | y0=" <> show n.y0 <> " | y1=" <> show n.y1
                  else pure unit
          in
            ltr
      )
      nodes
      (Array.range 0 (iterations - 1))
  in
    relaxed
  where
  -- Relax all nodes layer by layer, with alpha controlling adjustment strength
  relaxByLayer :: Array SankeyNode -> Array SankeyLink -> Number -> Boolean -> Number -> Number -> Array SankeyNode
  relaxByLayer currentNodes allLinks padding ascending alpha beta =
    let
      maxLayer = foldl (\acc node -> max acc node.layer) 0 currentNodes
      layerIndices =
        if ascending then Array.range 0 maxLayer
        else Array.reverse (Array.range 0 maxLayer)

      -- Process each layer in order
      relaxed = foldl
        ( \nodes layerIdx ->
            relaxLayer nodes allLinks layerIdx padding ascending alpha beta
        )
        currentNodes
        layerIndices
    in
      relaxed

  -- Relax a single layer, with alpha controlling how much to move toward weighted center
  -- ascending tells us if we're going left-to-right (true) or right-to-left (false)
  -- KEY: D3 processes nodes ONE AT A TIME, updating positions immediately
  relaxLayer :: Array SankeyNode -> Array SankeyLink -> Int -> Number -> Boolean -> Number -> Number -> Array SankeyNode
  relaxLayer currentNodes allLinks layerIdx padding ascending alpha beta =
    let
      -- Get node indices in this layer
      layerNodeIndices = map _.index $ filter (\n -> n.layer == layerIdx) currentNodes

      -- Process each node one at a time, updating the array immediately (D3's approach)
      nodesAfterRelax = foldl
        ( \nodes nodeIdx ->
            case find (\n -> n.index == nodeIdx) nodes of
              Nothing -> nodes
              Just node ->
                let
                  -- Calculate weighted position based on CURRENT positions of all nodes
                  -- D3: relaxLeftToRight (ascending) looks at INCOMING links, uses targetTop
                  -- D3: relaxRightToLeft (descending) looks at OUTGOING links, uses sourceTop
                  weightedY =
                    if ascending then calculateWeightedFromSources node nodes allLinks padding
                    else calculateWeightedFromTargets node nodes allLinks padding
                  -- D3 formula: dy = (y / w - node.y0) * alpha
                  dy = (weightedY - node.y0) * alpha
                  newY0 = node.y0 + dy
                  newY1 = node.y1 + dy
                  updatedNode = node { y0 = newY0, y1 = newY1 }
                in
                  -- Update this node immediately so next node sees the change
                  map (\n -> if n.index == nodeIdx then updatedNode else n) nodes
        )
        currentNodes
        layerNodeIndices

      -- Get the updated layer nodes for sorting and collision resolution
      layerNodesUpdated = filter (\n -> n.layer == layerIdx) nodesAfterRelax

      -- Sort by y0 position (D3's ascendingBreadth)
      sorted = Array.sortBy (\a b -> compare a.y0 b.y0) layerNodesUpdated

      -- Wrap for collision resolution
      sortedWithTarget = map (\node -> { node, targetY: node.y0 }) sorted

      -- Resolve overlaps - D3 uses beta for dampening
      resolved = resolveCollisions sortedWithTarget padding y0 y1 beta
    in
      -- Update the main nodes array with resolved positions
      updateNodes nodesAfterRelax resolved

  -- Calculate weighted y-position based on OUTGOING links (targets)
  -- Used in relaxRightToLeft - D3 uses sourceTop for this pass
  -- D3 sankey.js:268-289
  calculateWeightedFromTargets :: SankeyNode -> Array SankeyNode -> Array SankeyLink -> Number -> Number
  calculateWeightedFromTargets node allNodes allLinks padding =
    let
      -- Get outgoing links sorted by target y position (with tie-breaker by link index)
      outgoingLinks = filter (\l -> l.sourceIndex == node.index) allLinks
      sortedLinks = Array.sortBy
        ( \a b ->
            case find (\n -> n.index == a.targetIndex) allNodes, find (\n -> n.index == b.targetIndex) allNodes of
              Just na, Just nb -> compare na.y0 nb.y0 <> compare a.index b.index
              _, _ -> compare a.index b.index
        )
        outgoingLinks

      -- Calculate weighted sum using sourceTop positioning
      -- For each outgoing link, calculate ideal position based on where target is
      result = foldl
        ( \acc link ->
            case find (\n -> n.index == link.targetIndex) allNodes of
              Just targetNode ->
                let
                  -- D3's sourceTop: where should source be based on target position
                  layerDist = toNumber (targetNode.layer - node.layer)
                  v = link.value * max 1.0 layerDist
                  -- sourceTop(source, target) - calculate ideal y for source based on target
                  idealY = sourceTop node targetNode sortedLinks allNodes allLinks padding
                in
                  { sum: acc.sum + idealY * v, totalWeight: acc.totalWeight + v }
              Nothing -> acc
        )
        { sum: 0.0, totalWeight: 0.0 }
        sortedLinks
    in
      if result.totalWeight > 0.0 then result.sum / result.totalWeight
      else node.y0

  -- Calculate weighted y-position based on INCOMING links (sources)
  -- Used in relaxLeftToRight - D3 uses targetTop for this pass
  -- D3 sankey.js:245-266
  calculateWeightedFromSources :: SankeyNode -> Array SankeyNode -> Array SankeyLink -> Number -> Number
  calculateWeightedFromSources node allNodes allLinks padding =
    let
      -- Get incoming links sorted by source y position (with tie-breaker by link index)
      incomingLinks = filter (\l -> l.targetIndex == node.index) allLinks
      sortedLinks = Array.sortBy
        ( \a b ->
            case find (\n -> n.index == a.sourceIndex) allNodes, find (\n -> n.index == b.sourceIndex) allNodes of
              Just na, Just nb -> compare na.y0 nb.y0 <> compare a.index b.index
              _, _ -> compare a.index b.index
        )
        incomingLinks

      -- Calculate weighted sum using targetTop positioning
      -- For each incoming link, calculate ideal position based on where source is
      result = foldl
        ( \acc link ->
            case find (\n -> n.index == link.sourceIndex) allNodes of
              Just sourceNode ->
                let
                  layerDist = toNumber (node.layer - sourceNode.layer)
                  v = link.value * max 1.0 layerDist
                  -- targetTop(source, target) - calculate ideal y for target based on source
                  idealY = targetTop sourceNode node sortedLinks allNodes allLinks padding
                in
                  { sum: acc.sum + idealY * v, totalWeight: acc.totalWeight + v }
              Nothing -> acc
        )
        { sum: 0.0, totalWeight: 0.0 }
        sortedLinks
    in
      if result.totalWeight > 0.0 then result.sum / result.totalWeight
      else node.y0

  -- D3's targetTop: Returns the target.y0 that would produce an ideal link from source to target
  -- Accounts for link stacking order at both source and target nodes
  targetTop :: SankeyNode -> SankeyNode -> Array SankeyLink -> Array SankeyNode -> Array SankeyLink -> Number -> Number
  targetTop source target _sortedSourceLinks allNodes allLinks py =
    let
      -- Get all outgoing links from source, sorted by target y (with tie-breaker by link index)
      sourceOutgoing = Array.sortBy
        ( \a b ->
            case find (\n -> n.index == a.targetIndex) allNodes, find (\n -> n.index == b.targetIndex) allNodes of
              Just na, Just nb -> compare na.y0 nb.y0 <> compare a.index b.index
              _, _ -> compare a.index b.index
        )
        (filter (\l -> l.sourceIndex == source.index) allLinks)

      -- Get all incoming links to target, sorted by source y (with tie-breaker by link index)
      targetIncoming = Array.sortBy
        ( \a b ->
            case find (\n -> n.index == a.sourceIndex) allNodes, find (\n -> n.index == b.sourceIndex) allNodes of
              Just na, Just nb -> compare na.y0 nb.y0 <> compare a.index b.index
              _, _ -> compare a.index b.index
        )
        (filter (\l -> l.targetIndex == target.index) allLinks)

      numSourceLinks = toNumber (Array.length sourceOutgoing)

      -- Start position: source.y0 offset for centering links
      startY = source.y0 - (numSourceLinks - 1.0) * py / 2.0

      -- Sum widths of links before this one at source
      yAtSource = foldl
        ( \acc link ->
            if acc.found then acc  -- Already found target, skip remaining
            else if link.targetIndex == target.index then acc { found = true }  -- Found it, mark and stop
            else { y: acc.y + link.width + py, found: false }  -- Not found yet, keep adding
        )
        { y: startY, found: false }
        sourceOutgoing

      -- Subtract widths of links before this one at target
      yFinal = foldl
        ( \acc link ->
            if acc.found then acc  -- Already found source, skip remaining
            else if link.sourceIndex == source.index then acc { found = true }  -- Found it, mark and stop
            else { y: acc.y - link.width, found: false }  -- Not found yet, keep subtracting
        )
        { y: yAtSource.y, found: false }
        targetIncoming
    in
      yFinal.y

  -- D3's sourceTop: Returns the source.y0 that would produce an ideal link from source to target
  sourceTop :: SankeyNode -> SankeyNode -> Array SankeyLink -> Array SankeyNode -> Array SankeyLink -> Number -> Number
  sourceTop source target _sortedTargetLinks allNodes allLinks py =
    let
      -- Get all incoming links to target, sorted by source y (with tie-breaker by link index)
      targetIncoming = Array.sortBy
        ( \a b ->
            case find (\n -> n.index == a.sourceIndex) allNodes, find (\n -> n.index == b.sourceIndex) allNodes of
              Just na, Just nb -> compare na.y0 nb.y0 <> compare a.index b.index
              _, _ -> compare a.index b.index
        )
        (filter (\l -> l.targetIndex == target.index) allLinks)

      -- Get all outgoing links from source, sorted by target y (with tie-breaker by link index)
      sourceOutgoing = Array.sortBy
        ( \a b ->
            case find (\n -> n.index == a.targetIndex) allNodes, find (\n -> n.index == b.targetIndex) allNodes of
              Just na, Just nb -> compare na.y0 nb.y0 <> compare a.index b.index
              _, _ -> compare a.index b.index
        )
        (filter (\l -> l.sourceIndex == source.index) allLinks)

      numTargetLinks = toNumber (Array.length targetIncoming)

      -- Start position: target.y0 offset for centering links
      startY = target.y0 - (numTargetLinks - 1.0) * py / 2.0

      -- Sum widths of links before this one at target
      yAtTarget = foldl
        ( \acc link ->
            if acc.found then acc  -- Already found source, skip remaining
            else if link.sourceIndex == source.index then acc { found = true }  -- Found it, mark and stop
            else { y: acc.y + link.width + py, found: false }  -- Not found yet, keep adding
        )
        { y: startY, found: false }
        targetIncoming

      -- Subtract widths of links before this one at source
      yFinal = foldl
        ( \acc link ->
            if acc.found then acc  -- Already found target, skip remaining
            else if link.targetIndex == target.index then acc { found = true }  -- Found it, mark and stop
            else { y: acc.y - link.width, found: false }  -- Not found yet, keep subtracting
        )
        { y: yAtTarget.y, found: false }
        sourceOutgoing
    in
      yFinal.y

  -- Resolve collisions by pushing overlapping nodes apart
  -- D3 approach: start from MIDDLE, push outward in both directions
  -- This preserves the relative positions better than starting from edges
  -- beta parameter dampens collision resolution (starts low, increases over iterations)
  resolveCollisions :: Array { node :: SankeyNode, targetY :: Number } -> Number -> Number -> Number -> Number -> Array { node :: SankeyNode, targetY :: Number }
  resolveCollisions sorted padding yMin yMax beta =
    if Array.null sorted then sorted
    else
      let
        n = Array.length sorted
        middleIdx = n / 2  -- Integer division via /

        -- Get the middle node as the anchor point
        middleNode = sorted !! middleIdx

        -- Phase 1: Push nodes UP from middle-1 down to 0 (resolveCollisionsBottomToTop)
        -- Starting y is the top of the middle node minus padding
        startYUp = case middleNode of
          Just m -> m.node.y0 - padding
          Nothing -> yMax

        pushedUp = resolveCollisionsBottomToTop sorted (middleIdx - 1) startYUp padding beta

        -- Phase 2: Push nodes DOWN from middle+1 to end (resolveCollisionsTopToBottom)
        -- Starting y is the bottom of the middle node plus padding
        startYDown = case middleNode of
          Just m -> m.node.y1 + padding
          Nothing -> yMin

        pushedUpAndDown = resolveCollisionsTopToBottom pushedUp (middleIdx + 1) startYDown padding beta

        -- Phase 3: Clamp to bounds - push up from bottom edge
        clampedBottom = resolveCollisionsBottomToTop pushedUpAndDown (n - 1) yMax padding beta

        -- Phase 4: Clamp to bounds - push down from top edge
        clampedTop = resolveCollisionsTopToBottom clampedBottom 0 yMin padding beta
      in
        clampedTop

  -- Push overlapping nodes UP (from index i down to 0)
  -- beta dampens the displacement (starts low, increases over iterations)
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
                -- Calculate how much to push up (D3 multiplies by alpha/beta)
                dy = (item.node.y1 - y) * beta
                updated = if dy > epsilon
                  then
                    let newNode = item.node { y0 = item.node.y0 - dy, y1 = item.node.y1 - dy }
                    in fromMaybe currentNodes (Array.updateAt idx { node: newNode, targetY: item.targetY } currentNodes)
                  else currentNodes
                newY = case updated !! idx of
                  Just n -> n.node.y0 - padding
                  Nothing -> y
              in
                go (idx - 1) newY updated
      in
        go startIdx targetY nodes

  -- Push overlapping nodes DOWN (from index i up to end)
  -- beta dampens the displacement (starts low, increases over iterations)
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
              -- Calculate how much to push down (D3 multiplies by alpha/beta)
              dy = (y - item.node.y0) * beta
              updated = if dy > epsilon
                then
                  let newNode = item.node { y0 = item.node.y0 + dy, y1 = item.node.y1 + dy }
                  in fromMaybe currentNodes (Array.updateAt idx { node: newNode, targetY: item.targetY } currentNodes)
                else currentNodes
              newY = case updated !! idx of
                Just n' -> n'.node.y1 + padding
                Nothing -> y
            in
              go (idx + 1) newY updated
    in
      go startIdx targetY nodes

  -- Update nodes array with new positions
  updateNodes :: Array SankeyNode -> Array { node :: SankeyNode, targetY :: Number } -> Array SankeyNode
  updateNodes allNodes updated =
    map
      ( \node ->
          case find (\u -> u.node.index == node.index) updated of
            Just u -> u.node
            Nothing -> node
      )
      allNodes

-- ============================================================================
-- Step 7: computeLinkBreadths - Link y positions
-- ============================================================================

-- | Compute y0 and y1 positions for each link and set link widths
-- | This determines where each link connects to its source and target nodes
computeLinkBreadths :: State SankeyGraphModel Unit
computeLinkBreadths = do
  model <- get
  let config = model.config

  -- First, set link widths based on values (need ky from nodes)
  let ky = calculateGlobalKy model.sankeyNodes config.extent.y0 config.extent.y1 config.nodePadding
  let linksWithWidth = map (\link -> link { width = link.value * ky }) model.sankeyLinks

  -- Then calculate y positions for each link
  let linksWithY = calculateLinkYPositions model.sankeyNodes linksWithWidth

  -- DEBUG: Log link breadths for key links
  let _ = unsafePerformEffect do
        log "\n=== After computeLinkBreadths ==="
        for_ linksWithY $ \link -> do
          case find (\n -> n.index == link.sourceIndex) model.sankeyNodes,
               find (\n -> n.index == link.targetIndex) model.sankeyNodes of
            Just srcNode, Just tgtNode ->
              log $ "  " <> srcNode.name <> " -> " <> tgtNode.name <> " | y0=" <> show link.y0 <> " | y1=" <> show link.y1 <> " | width=" <> show link.width
            _, _ -> pure unit

  modify_ _ { sankeyLinks = linksWithY }
  where
  -- Calculate global ky (same as in initializeNodeBreadths)
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
        in
          if totalValue > 0.0 then availableHeight / totalValue else 1.0

      scales = map scaleForLayer layers
      nonEmptyScales = filter (\s -> s > 0.0) scales
    in
      -- Use minimum scale to ensure all nodes fit (don't start with 1.0!)
      case Array.head nonEmptyScales of
        Just firstScale -> foldl min firstScale nonEmptyScales
        Nothing -> 1.0 -- Fallback if no valid scales

  -- Calculate y0 and y1 for each link based on node positions
  calculateLinkYPositions :: Array SankeyNode -> Array SankeyLink -> Array SankeyLink
  calculateLinkYPositions nodes links =
    let
      -- Step 1: Set y0 for each link based on its source node
      linksWithY0 = foldl
        ( \currentLinks node ->
            let
              -- Get outgoing links from this node
              outgoingLinks = filter (\link -> link.sourceIndex == node.index) currentLinks
              -- Sort by target node's y position to minimize crossings
              -- Tie-breaker: use link index (matches D3's ascendingTargetBreadth)
              sorted = Array.sortBy
                ( \a b ->
                    case find (\n -> n.index == a.targetIndex) nodes, find (\n -> n.index == b.targetIndex) nodes of
                      Just na, Just nb -> compare na.y0 nb.y0 <> compare a.index b.index
                      _, _ -> compare a.index b.index
                )
                outgoingLinks
              -- Calculate cumulative y positions
              withY0 = foldl
                ( \acc link ->
                    let
                      y = acc.y + link.width / 2.0 -- center of the link ribbon
                      updated = link { y0 = y }
                    in
                      { y: acc.y + link.width
                      , links: Array.snoc acc.links updated
                      }
                )
                { y: node.y0, links: [] }
                sorted
            in
              -- Update these links in the main array
              map
                ( \link ->
                    case find (\l -> l.index == link.index) withY0.links of
                      Just updatedLink -> updatedLink
                      Nothing -> link
                )
                currentLinks
        )
        links
        nodes

      -- Step 2: Set y1 for each link based on its target node
      linksWithY1 = foldl
        ( \currentLinks node ->
            let
              -- Get incoming links to this node
              incomingLinks = filter (\link -> link.targetIndex == node.index) currentLinks
              -- Sort by source node's y position to minimize crossings
              -- Tie-breaker: use link index (matches D3's ascendingSourceBreadth)
              sorted = Array.sortBy
                ( \a b ->
                    case find (\n -> n.index == a.sourceIndex) nodes, find (\n -> n.index == b.sourceIndex) nodes of
                      Just na, Just nb -> compare na.y0 nb.y0 <> compare a.index b.index
                      _, _ -> compare a.index b.index
                )
                incomingLinks
              -- Calculate cumulative y positions
              withY1 = foldl
                ( \acc link ->
                    let
                      y = acc.y + link.width / 2.0 -- center of the link ribbon
                      updated = link { y1 = y }
                    in
                      { y: acc.y + link.width
                      , links: Array.snoc acc.links updated
                      }
                )
                { y: node.y0, links: [] }
                sorted
            in
              -- Update these links in the main array
              map
                ( \link ->
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

-- ============================================================================
-- Step 8: Assign Colors
-- ============================================================================

-- | Assign colors to nodes and links based on color mode
assignColors :: State SankeyGraphModel Unit
assignColors = do
  model <- get
  let config = model.config

  -- Assign colors to nodes using a simple color palette
  let
    colors =
      [ "#1f77b4"
      , "#ff7f0e"
      , "#2ca02c"
      , "#d62728"
      , "#9467bd"
      , "#8c564b"
      , "#e377c2"
      , "#7f7f7f"
      , "#bcbd22"
      , "#17becf"
      ]
    nodesWithColor = mapWithIndex
      ( \i node ->
          node { color = fromMaybe "#999" (colors !! (i `mod` length colors)) }
      )
      model.sankeyNodes

  -- Helper function to assign link color based on mode
  let
    assignLinkColor :: Array SankeyNode -> LinkColorMode -> SankeyLink -> SankeyLink
    assignLinkColor nodes mode link =
      case mode of
        SourceColor ->
          let
            sourceNode = find (\n -> n.index == link.sourceIndex) nodes
          in
            link { color = maybe "#999" (\n -> n.color) sourceNode }
        TargetColor ->
          let
            targetNode = find (\n -> n.index == link.targetIndex) nodes
          in
            link { color = maybe "#999" (\n -> n.color) targetNode }
        SourceTargetGradient ->
          link { color = "#999" } -- TODO: Could implement gradient IDs for SVG
        StaticColor c ->
          link { color = c }

  -- Assign colors to links based on color mode
  let linksWithColor = map (assignLinkColor nodesWithColor config.linkColorMode) model.sankeyLinks

  modify_ _ { sankeyNodes = nodesWithColor, sankeyLinks = linksWithColor }
