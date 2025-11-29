-- | PSD3.Layout.Sankey.Compute
-- |
-- | Pure PureScript implementation of Sankey diagram layout computation.
-- | Uses State monad to thread graph model through all layout functions.
-- | Starts with Array SankeyLinkInput and generates nodes dynamically.
-- |
-- | Reference: https://github.com/d3/d3-sankey (ISC License)
module PSD3.Layout.Sankey.Compute
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
import PSD3.Layout.Sankey.Types (Alignment(..), DependencyMap, LinkCSVRow, LinkColorMode(..), LinkID(..), NodeID(..), SankeyConfig, SankeyGraphModel, SankeyLayoutResult, SankeyLink, SankeyNode, defaultSankeyConfig, initialSankeyGraphModel, initialiseSankeyLink, initialiseSankeyNode)

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

    -- Step 2: Compute node links (populate sourceLinks/targetLinks arrays)

    -- Step 3: Compute node values (max flow through each node)
    computeNodeValues

    -- Step 4: Compute node depths (left-to-right BFS)
    computeNodeDepths

    -- Step 5: Compute node heights (right-to-left BFS)
    computeNodeHeights

    -- Step 6: Compute node breadths (vertical positioning with relaxation)
    computeNodeBreadths

    -- Step 7: Compute link breadths (link y0/y1 positions)
    computeLinkBreadths

    -- Step 8: Assign colors
    assignColors

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

    { sid, tid, nodeCount } =
      case s, t of -- have we seen either or both of these nodes before?
        (Just sid), (Just tid) -> -- neither new, simple
          { nodeCount: model.nodeCount, sid, tid }
        -- one new, count increments by one
        (Just sid), Nothing -> -- 
          { nodeCount: model.nodeCount + 1, sid, tid: NodeID model.nodeCount }
        Nothing, (Just tid) -> -- 
          { nodeCount: model.nodeCount + 1, sid: NodeID model.nodeCount, tid: tid }
        -- both new, count increments by two
        Nothing, Nothing ->
          { nodeCount: model.nodeCount + 2, sid: NodeID model.nodeCount, tid: NodeID (model.nodeCount + 1) }

  modify_ _
    { linkCount = model.linkCount + 1
    , nodeCount = nodeCount
    , nodeNameToID = M.insert link.s sid $ M.insert link.t tid model.nodeNameToID
    , nodeIDToName = M.insert sid link.s $ M.insert tid link.t model.nodeIDToName
    , deps = unionInsert sid tid model.deps
    , sped = unionInsert tid sid model.sped
    , sankeyLinks =
        snoc model.sankeyLinks $ initialiseSankeyLink { source: sid, target: tid, value: link.v, id: LinkID model.linkCount }
    }

-- | Once all the links have been made and all the source and target maps are filled we can create the sankeyNodes
initialiseSankeyNodes :: State SankeyGraphModel Unit
initialiseSankeyNodes = do
  model <- get
  let
    nodeNames = Set.toUnfoldable $ Map.keys model.nodeIDToName
    sankeyNodes = catMaybes $ (initialiseSankeyNode model) <$> nodeNames
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

-- | Compute node layers, x positions, and y positions with relaxation
-- | This is the main positioning algorithm that makes the Sankey look good
computeNodeBreadths :: State SankeyGraphModel Unit
computeNodeBreadths = do
  model <- get
  let config = model.config
  let extent = config.extent

  -- Step 6a: Assign nodes to layers and compute x positions
  let nodesWithLayers = computeNodeLayers model.sankeyNodes config extent.x0 extent.x1

  -- Step 6b: Initialize y positions by stacking nodes in each layer
  let nodesWithY = initializeNodeBreadths nodesWithLayers model.sankeyLinks config extent.y0 extent.y1

  -- DEBUG: Log positions BEFORE relaxation for key nodes
  let
    _ = unsafePerformEffect $ do
      case find (\n -> n.name == "Nuclear") nodesWithY of
        Just n -> log $ "BEFORE relaxation - Nuclear: y0=" <> show n.y0 <> ", y1=" <> show n.y1
        Nothing -> pure unit
      case find (\n -> n.name == "Thermal generation") nodesWithY of
        Just n -> log $ "BEFORE relaxation - Thermal generation: y0=" <> show n.y0 <> ", y1=" <> show n.y1
        Nothing -> pure unit
      case find (\n -> n.name == "Losses") nodesWithY of
        Just n -> log $ "BEFORE relaxation - Losses: y0=" <> show n.y0 <> ", y1=" <> show n.y1
        Nothing -> pure unit

  -- Step 6c: Run relaxation iterations to minimize crossings
  -- D3 doesn't pre-sort, it relies on relaxation + collision resolution to establish order
  let relaxedNodes = relaxation nodesWithY model.sankeyLinks config extent.y0 extent.y1

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

  modify_ _ { sankeyNodes = relaxedNodes }

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
initializeNodeBreadths :: Array SankeyNode -> Array SankeyLink -> SankeyConfig -> Number -> Number -> Array SankeyNode
initializeNodeBreadths nodes links config y0 y1 =
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
    ky = calculateKy layers totalHeight config.nodePadding

    -- DEBUG: This will help us verify the new code is running
    _ = unsafePerformEffect $ log $ "DEBUG initializeNodeBreadths: totalHeight=" <> show totalHeight <> ", ky=" <> show ky <> ", numLayers=" <> show (Array.length layers)

    -- D3 doesn't sort initially - nodes stay in input order
    -- Relaxation and collision resolution will establish the final order

    -- Position nodes in each layer vertically
    positionedLayers = map (positionLayer ky config.nodePadding y0) layers

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
relaxation :: Array SankeyNode -> Array SankeyLink -> SankeyConfig -> Number -> Number -> Array SankeyNode
relaxation nodes links config y0 y1 =
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
            _ = unsafePerformEffect $ log $ "Relaxation iteration " <> show i <> ", alpha=" <> show alpha <> ", beta=" <> show beta
            -- Relax right to left (by decreasing layer)
            rtl = relaxByLayer currentNodes links config.nodePadding false alpha beta
            -- Then left to right (by increasing layer)
            ltr = relaxByLayer rtl links config.nodePadding true alpha beta
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
  relaxLayer :: Array SankeyNode -> Array SankeyLink -> Int -> Number -> Boolean -> Number -> Number -> Array SankeyNode
  relaxLayer currentNodes allLinks layerIdx padding ascending alpha beta =
    let
      -- Get nodes in this layer
      layerNodes = filter (\n -> n.layer == layerIdx) currentNodes

      -- Calculate target y position for each node based on connected links
      -- D3 separates relaxRightToLeft (uses outgoing/sourceLinks) from relaxLeftToRight (uses incoming/targetLinks)
      nodesWithTarget = map
        ( \node ->
            let
              weightedY =
                if ascending then calculateWeightedFromSources node currentNodes allLinks -- left-to-right: look at sources
                else calculateWeightedFromTargets node currentNodes allLinks -- right-to-left: look at targets
              -- D3 formula: dy = (y / w - node.y0) * alpha
              dy = (weightedY - node.y0) * alpha
              newY0 = node.y0 + dy
              newY1 = node.y1 + dy
            in
              { node: node { y0 = newY0, y1 = newY1 }, targetY: newY0 }
        )
        layerNodes

      -- Sort by target position, but use value as tiebreaker to maintain stable order
      sorted = Array.sortBy
        ( \a b ->
            let
              posCompare = compare a.targetY b.targetY
            in
              if posCompare == EQ then compare b.node.value a.node.value -- Larger value first
              else posCompare
        )
        nodesWithTarget

      -- Resolve overlaps - push down then push back up
      resolved = resolveCollisions sorted padding y0 y1
    in
      -- Update the main nodes array with new positions
      updateNodes currentNodes resolved

  -- Calculate weighted y-position based on OUTGOING links (targets)
  -- Used in relaxRightToLeft - D3 sankey.js:268-289
  calculateWeightedFromTargets :: SankeyNode -> Array SankeyNode -> Array SankeyLink -> Number
  calculateWeightedFromTargets node allNodes allLinks =
    let
      -- Find outgoing links and weight by value * layer distance
      result = foldl
        ( \acc link ->
            if link.sourceIndex == node.index then
              case find (\n -> n.index == link.targetIndex) allNodes of
                Just targetNode ->
                  let
                    -- D3 KEY INSIGHT: weight = value * (target.layer - source.layer)
                    layerDist = toNumber (targetNode.layer - node.layer)
                    v = link.value * layerDist
                    center = (targetNode.y0 + targetNode.y1) / 2.0
                  in
                    { sum: acc.sum + center * v, totalWeight: acc.totalWeight + v }
                Nothing -> acc
            else acc
        )
        { sum: 0.0, totalWeight: 0.0 }
        allLinks
    in
      if result.totalWeight > 0.0 then result.sum / result.totalWeight
      else node.y0 -- No outgoing links, stay at current position

  -- Calculate weighted y-position based on INCOMING links (sources)
  -- Used in relaxLeftToRight - D3 sankey.js:245-266
  calculateWeightedFromSources :: SankeyNode -> Array SankeyNode -> Array SankeyLink -> Number
  calculateWeightedFromSources node allNodes allLinks =
    let
      -- Find incoming links and weight by value * layer distance
      result = foldl
        ( \acc link ->
            if link.targetIndex == node.index then
              case find (\n -> n.index == link.sourceIndex) allNodes of
                Just sourceNode ->
                  let
                    -- D3 KEY INSIGHT: weight = value * (target.layer - source.layer)
                    layerDist = toNumber (node.layer - sourceNode.layer)
                    v = link.value * layerDist
                    center = (sourceNode.y0 + sourceNode.y1) / 2.0
                  in
                    { sum: acc.sum + center * v, totalWeight: acc.totalWeight + v }
                Nothing -> acc
            else acc
        )
        { sum: 0.0, totalWeight: 0.0 }
        allLinks
    in
      if result.totalWeight > 0.0 then result.sum / result.totalWeight
      else node.y0 -- No incoming links, stay at current position

  -- Resolve collisions by pushing overlapping nodes apart
  -- D3 approach: start from top, push down to avoid overlaps, then push back up if exceeded bounds
  resolveCollisions :: Array { node :: SankeyNode, targetY :: Number } -> Number -> Number -> Number -> Array { node :: SankeyNode, targetY :: Number }
  resolveCollisions sorted padding yMin yMax =
    if Array.null sorted then sorted
    else
      let
        nodeHeight n = n.node.y1 - n.node.y0

        -- Pass 1: Push nodes down to avoid overlaps, starting from yMin (top)
        -- Try to position each node at its targetY, but push down if it would overlap
        pushedDown = foldl
          ( \acc item ->
              let
                prevY = acc.y
                height = nodeHeight item
                -- Desired position: center the node at targetY
                desiredY0 = item.targetY - height / 2.0
                -- Actual position: at least prevY to avoid overlap
                actualY0 = max prevY desiredY0
                actualY1 = actualY0 + height
                updatedNode = item.node { y0 = actualY0, y1 = actualY1 }
              in
                { y: actualY1 + padding
                , nodes: Array.snoc acc.nodes { node: updatedNode, targetY: item.targetY }
                }
          )
          { y: yMin, nodes: [] }
          sorted

        -- Pass 2: Push nodes back up if we exceeded yMax
        pushedUp = foldr
          ( \item acc ->
              let
                nextY = acc.y
                height = nodeHeight item
                -- Make sure we don't exceed yMax
                desiredY1 = min item.node.y1 (nextY - padding)
                actualY1 = desiredY1
                actualY0 = actualY1 - height
                updatedNode = item.node { y0 = actualY0, y1 = actualY1 }
              in
                { y: actualY0
                , nodes: Array.cons { node: updatedNode, targetY: item.targetY } acc.nodes
                }
          )
          { y: yMax, nodes: [] }
          pushedDown.nodes
      in
        pushedUp.nodes

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
              sorted = Array.sortBy
                ( \a b ->
                    case find (\n -> n.index == a.targetIndex) nodes, find (\n -> n.index == b.targetIndex) nodes of
                      Just na, Just nb -> compare na.y0 nb.y0
                      _, _ -> EQ
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
              sorted = Array.sortBy
                ( \a b ->
                    case find (\n -> n.index == a.sourceIndex) nodes, find (\n -> n.index == b.sourceIndex) nodes of
                      Just na, Just nb -> compare na.y0 nb.y0
                      _, _ -> EQ
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
