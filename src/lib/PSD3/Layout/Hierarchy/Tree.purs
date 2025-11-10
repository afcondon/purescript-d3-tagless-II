module PSD3.Layout.Hierarchy.Tree
  ( TreeNode(..)
  , TreeConfig
  , SeparationFn
  , defaultTreeConfig
  , tree
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Int (toNumber)
import Data.Foldable (foldl)
import Data.Tuple (Tuple(..))
import Data.Map (Map)
import Data.Map as Map
import PSD3.Layout.Hierarchy.Types (HierarchyNode(..))

-- | Tree layout node with x, y positions
newtype TreeNode a = TreeNode
  { data_ :: a
  , depth :: Int
  , x :: Number
  , y :: Number
  , children :: Array (TreeNode a)
  }

derive instance Functor TreeNode

-- | Configuration for tree layout
type TreeConfig =
  { size :: { width :: Number, height :: Number }
  , separation :: SeparationFn
  , nodeSize :: Maybe { width :: Number, height :: Number }
  }

type SeparationFn = forall a. TreeNode a -> TreeNode a -> Number

-- | Default separation: 1 for siblings, 2 for non-siblings
defaultSeparation :: forall a. TreeNode a -> TreeNode a -> Number
defaultSeparation _ _ = 1.0

-- | Default tree configuration
defaultTreeConfig :: TreeConfig
defaultTreeConfig =
  { size: { width: 1.0, height: 1.0 }
  , separation: defaultSeparation
  , nodeSize: Nothing
  }

-- Internal tree node used during layout computation
-- Following D3's TreeNode structure from tree.js
type InternalNode a =
  { node :: TreeNode a          -- Original tree node (will be updated with final positions)
  , parent :: Maybe Int         -- Parent ID
  , children :: Array Int       -- Child IDs
  , nodeId :: Int              -- This node's ID
  , z :: Number                 -- prelim (preliminary x-coordinate)
  , m :: Number                 -- mod (modifier)
  , a :: Int                    -- ancestor (for apportion)
  , t :: Maybe Int              -- thread
  , c :: Number                 -- change
  , s :: Number                 -- shift
  , i :: Int                    -- index among siblings
  }

-- | Main tree layout function
-- Implements the Reingold-Tilford "tidy tree" algorithm
tree :: forall a. TreeConfig -> HierarchyNode a -> TreeNode a
tree config root =
  let
    -- Step 1: Convert hierarchy to tree nodes
    treeRoot = hierarchyToTreeNode root

    -- Step 2: Build internal representation with indexed nodes
    { nodes: internalNodes, root: rootId } = buildInternalTree treeRoot

    -- Step 3: First walk (post-order) - compute preliminary positions
    afterFirstWalk = firstWalkAll config internalNodes rootId

    -- Step 4: Adjust root so tree is centered at x=0
    rootNode = unsafeGetNode afterFirstWalk rootId
    adjustedNodes = adjustNode afterFirstWalk rootId { m: -rootNode.z }

    -- Step 5: Second walk (pre-order) - compute final positions
    finalNodes = secondWalkAll config adjustedNodes rootId

    -- Step 6: Convert back to TreeNode and scale to fit size
    resultTree = extractTreeNode finalNodes rootId
    scaledTree = scaleToFit config resultTree

  in scaledTree

-- | Convert HierarchyNode to TreeNode (no layout yet)
hierarchyToTreeNode :: forall a. HierarchyNode a -> TreeNode a
hierarchyToTreeNode (HNode h) =
  TreeNode
    { data_: h.data_
    , depth: h.depth
    , x: 0.0
    , y: 0.0
    , children: map hierarchyToTreeNode h.children
    }

-- | Build internal tree structure with node IDs
-- Use Map for reliable node storage
buildInternalTree :: forall a. TreeNode a -> { nodes :: Map Int (InternalNode a), root :: Int }
buildInternalTree treeRoot =
  let
    -- Build all nodes in a single pass, using Map for storage
    result = buildNodesPass Nothing 0 treeRoot { nodes: Map.empty, nextId: 0 }
  in
    { nodes: result.nodes, root: 0 }
  where
    -- Build nodes recursively, immediately setting children
    buildNodesPass parent siblingIndex (TreeNode node) state =
      let
        myId = state.nextId

        -- Process children first
        childrenWithIndex = Array.mapWithIndex Tuple node.children
        { childIds, state: childrenResult } = foldl
          (\acc (Tuple sibIdx child) ->
            let childState = acc.state
                childId = childState.nextId
                childResult = buildNodesPass (Just myId) sibIdx child childState
            in { childIds: Array.snoc acc.childIds childId, state: childResult }
          )
          { childIds: [], state: { nodes: state.nodes, nextId: state.nextId + 1 } }
          childrenWithIndex

        -- Create internal node with children already set
        internal =
          { node: TreeNode node
          , parent
          , children: childIds
          , nodeId: myId
          , z: 0.0
          , m: 0.0
          , a: myId
          , t: Nothing
          , c: 0.0
          , s: 0.0
          , i: siblingIndex
          }

        -- Add this node to the map
        nodesWithThis = Map.insert myId internal childrenResult.nodes

      in
        { nodes: nodesWithThis, nextId: childrenResult.nextId }

-- Helper: unsafe crash
foreign import unsafeCrashWith :: forall a. String -> a

-- Helper: get node by ID from Map
unsafeGetNode :: forall a. Map Int (InternalNode a) -> Int -> InternalNode a
unsafeGetNode nodes id = case Map.lookup id nodes of
  Just node -> node
  Nothing -> unsafeCrashWith $ "Node ID " <> show id <> " not found in map"

-- Helper: update node in map
updateNode :: forall a. Map Int (InternalNode a) -> Int -> InternalNode a -> Map Int (InternalNode a)
updateNode nodes id newNode =
  Map.insert id newNode nodes

-- Helper: adjust a node's m value (for root centering)
adjustNode :: forall a. Map Int (InternalNode a) -> Int -> { m :: Number } -> Map Int (InternalNode a)
adjustNode nodes id update =
  let node = unsafeGetNode nodes id
      updated = node { m = update.m }
  in updateNode nodes id updated

-- First walk: compute preliminary positions (post-order)
firstWalkAll :: forall a. TreeConfig -> Map Int (InternalNode a) -> Int -> Map Int (InternalNode a)
firstWalkAll config nodes rootId =
  eachAfter nodes rootId (\ns nid -> firstWalk config ns nid)

-- Post-order traversal
eachAfter :: forall a. Map Int (InternalNode a) -> Int -> (Map Int (InternalNode a) -> Int -> Map Int (InternalNode a)) -> Map Int (InternalNode a)
eachAfter nodes nodeId fn =
  let
      node = unsafeGetNode nodes nodeId
      -- Process children first
      nodesAfterChildren = foldl (\ns childId -> eachAfter ns childId fn) nodes node.children
      -- Then process this node
  in fn nodesAfterChildren nodeId

-- First walk for single node
firstWalk :: forall a. TreeConfig -> Map Int (InternalNode a) -> Int -> Map Int (InternalNode a)
firstWalk config nodes nodeId =
  let
    v = unsafeGetNode nodes nodeId
    children = v.children
    parentNode = case v.parent of
      Nothing -> v  -- Root's parent is itself (dummy)
      Just pid -> unsafeGetNode nodes pid
    siblings = case v.parent of
      Nothing -> [nodeId]  -- Root has no siblings
      Just _ -> parentNode.children
    w = if v.i > 0 then Array.index siblings (v.i - 1) else Nothing

  in
    if Array.length children > 0 then
      -- Internal node
      let
        -- Execute shifts
        nodes1 = executeShifts nodes nodeId
        v1 = unsafeGetNode nodes1 nodeId

        -- Compute midpoint
        firstChildId = fromMaybe 0 (Array.index children 0)
        lastChildId = fromMaybe 0 (Array.index children (Array.length children - 1))
        firstChild = unsafeGetNode nodes1 firstChildId
        lastChild = unsafeGetNode nodes1 lastChildId
        midpoint = (firstChild.z + lastChild.z) / 2.0

        -- Set preliminary position
        nodes2 = case w of
          Just wId ->
            let wNode = unsafeGetNode nodes1 wId
                sep = config.separation v1.node wNode.node
                newZ = wNode.z + sep
                newM = newZ - midpoint
                updated = v1 { z = newZ, m = newM }
            in updateNode nodes1 nodeId updated
          Nothing ->
            let updated = v1 { z = midpoint }
            in updateNode nodes1 nodeId updated

        -- Apportion
        ancestor = fromMaybe nodeId (Array.index siblings 0)
        nodes3 = apportion config nodes2 nodeId w ancestor

        -- Update parent's ancestor
        v3 = unsafeGetNode nodes3 nodeId
        nodes4 = case v3.parent of
          Just pid ->
            let pNode = unsafeGetNode nodes3 pid
                pUpdated = pNode { a = v3.a }
            in updateNode nodes3 pid pUpdated
          Nothing -> nodes3

      in nodes4
    else
      -- Leaf node
      case w of
        Just wId ->
          let wNode = unsafeGetNode nodes wId
              sep = config.separation v.node wNode.node
              newZ = wNode.z + sep
              updated = v { z = newZ }
              nodes1 = updateNode nodes nodeId updated

              -- Apportion
              ancestor = fromMaybe nodeId (Array.index siblings 0)
              nodes2 = apportion config nodes1 nodeId w ancestor

              -- Update parent's ancestor
              v2 = unsafeGetNode nodes2 nodeId
              nodes3 = case v2.parent of
                Just pid ->
                  let pNode = unsafeGetNode nodes2 pid
                      pUpdated = pNode { a = v2.a }
                  in updateNode nodes2 pid pUpdated
                Nothing -> nodes2
          in nodes3
        Nothing ->
          -- First child, z stays 0
          let ancestor = fromMaybe nodeId (Array.index siblings 0)
              nodes1 = apportion config nodes nodeId Nothing ancestor
              v1 = unsafeGetNode nodes1 nodeId
              nodes2 = case v1.parent of
                Just pid ->
                  let pNode = unsafeGetNode nodes1 pid
                      pUpdated = pNode { a = v1.a }
                  in updateNode nodes1 pid pUpdated
                Nothing -> nodes1
          in nodes2

-- Execute shifts (from right to left)
executeShifts :: forall a. Map Int (InternalNode a) -> Int -> Map Int (InternalNode a)
executeShifts nodes nodeId =
  let v = unsafeGetNode nodes nodeId
      children = v.children
      go shift change i ns
        | i < 0 = ns
        | otherwise =
            case Array.index children i of
              Nothing -> ns  -- Shouldn't happen
              Just childId ->
                let w = unsafeGetNode ns childId
                    newZ = w.z + shift
                    newM = w.m + shift
                    newShift = shift + w.s + (change + w.c)
                    newChange = change + w.c
                    updated = w { z = newZ, m = newM }
                    ns1 = updateNode ns childId updated
                in go newShift newChange (i - 1) ns1
  in go 0.0 0.0 (Array.length children - 1) nodes

-- Apportion: resolve conflicts between subtrees
apportion :: forall a. TreeConfig -> Map Int (InternalNode a) -> Int -> Maybe Int -> Int -> Map Int (InternalNode a)
apportion config nodes vId wMaybe ancestor =
  case wMaybe of
    Nothing -> nodes
    Just wId ->
      let
        v = unsafeGetNode nodes vId
        parentNode = case v.parent of
          Just pid -> unsafeGetNode nodes pid
          Nothing -> v
        vom = fromMaybe 0 (Array.index parentNode.children 0)

        result = walkContours nodes vId vId wId vom vId v.m v.m (unsafeGetNode nodes wId).m (unsafeGetNode nodes vom).m ancestor

        -- Set threads if needed
        nodes1 = result.nodes
        vim = result.vim
        vip = result.vip
        vop = result.vop
        vom' = result.vom
        sim = result.sim
        sip = result.sip
        sop = result.sop
        som = result.som

        nodes2 = case vim of
          Just vimId ->
            let vimNode = unsafeGetNode nodes1 vimId
                vopNode = unsafeGetNode nodes1 vop
            in case nextRight nodes1 vop of
                 Nothing ->
                   let vopUpdated = vopNode { t = Just vimId, m = vopNode.m + sim - sop }
                   in updateNode nodes1 vop vopUpdated
                 Just _ -> nodes1
          Nothing -> nodes1

        nodes3 = case vip of
          Just vipId ->
            let vomNode' = unsafeGetNode nodes2 vom'
            in case nextLeft nodes2 vom' of
                 Nothing ->
                   let vomUpdated = vomNode' { t = Just vipId, m = vomNode'.m + sip - som }
                       nodes' = updateNode nodes2 vom' vomUpdated
                       vNode = unsafeGetNode nodes' vId
                       vUpdated = vNode { a = vId }
                   in updateNode nodes' vId vUpdated
                 Just _ -> nodes2
          Nothing -> nodes2

      in nodes3
  where
    walkContours ns vip vop vim vom ancestor' sip sop sim som currentAncestor =
      let
        vimNext = nextRight ns vim
        vipNext = nextLeft ns vip
      in case vimNext, vipNext of
           Just vimId, Just vipId ->
             let
               vomNext = nextLeft ns vom
               vopNext = nextRight ns vop
               vom' = fromMaybe vom vomNext
               vop' = fromMaybe vop vopNext

               -- Update vop ancestor
               vopNode = unsafeGetNode ns vop'
               vopUpdated = vopNode { a = ancestor' }
               ns1 = updateNode ns vop' vopUpdated

               -- Compute shift
               vimNode = unsafeGetNode ns1 vimId
               vipNode = unsafeGetNode ns1 vipId
               sep = config.separation vimNode.node vipNode.node
               shift = vimNode.z + sim - vipNode.z - sip + sep

               ns2 = if shift > 0.0 then
                       let anc = nextAncestor ns1 vimId ancestor' currentAncestor
                       in moveSubtree ns1 anc ancestor' shift
                     else ns1

               -- Update sums
               sip' = if shift > 0.0 then sip + shift else sip
               sop' = if shift > 0.0 then sop + shift else sop
               sim' = sim + vimNode.m
               sip'' = sip' + vipNode.m
               som' = som + (unsafeGetNode ns2 vom').m
               sop'' = sop' + (unsafeGetNode ns2 vop').m

             in walkContours ns2 vipId vop' vimId vom' ancestor' sip'' sop'' sim' som' currentAncestor
           _, _ ->
             { nodes: ns
             , vim: vimNext
             , vip: vipNext
             , vop
             , vom
             , sim
             , sip
             , sop
             , som
             }

-- Helper: next node on left contour
nextLeft :: forall a. Map Int (InternalNode a) -> Int -> Maybe Int
nextLeft nodes nodeId =
  let node = unsafeGetNode nodes nodeId
  in if Array.length node.children > 0
       then Array.index node.children 0
       else node.t

-- Helper: next node on right contour
nextRight :: forall a. Map Int (InternalNode a) -> Int -> Maybe Int
nextRight nodes nodeId =
  let node = unsafeGetNode nodes nodeId
      lastIdx = Array.length node.children - 1
  in if lastIdx >= 0
       then Array.index node.children lastIdx
       else node.t

-- Helper: next ancestor
nextAncestor :: forall a. Map Int (InternalNode a) -> Int -> Int -> Int -> Int
nextAncestor nodes vimId vId defaultAncestor =
  let vim = unsafeGetNode nodes vimId
      vimAncestor = unsafeGetNode nodes vim.a
      v = unsafeGetNode nodes vId
  in case vimAncestor.parent, v.parent of
       Just vimP, Just vP -> if vimP == vP then vim.a else defaultAncestor
       _, _ -> defaultAncestor

-- Move subtree
moveSubtree :: forall a. Map Int (InternalNode a) -> Int -> Int -> Number -> Map Int (InternalNode a)
moveSubtree nodes wmId wpId shift =
  let
    wm = unsafeGetNode nodes wmId
    wp = unsafeGetNode nodes wpId
    change = shift / toNumber (wp.i - wm.i)
    wpUpdated = wp
      { c = wp.c - change
      , s = wp.s + shift
      , z = wp.z + shift
      , m = wp.m + shift
      }
    wmUpdated = wm { c = wm.c + change }
    nodes1 = updateNode nodes wpId wpUpdated
  in updateNode nodes1 wmId wmUpdated

-- Second walk: compute final positions (pre-order)
secondWalkAll :: forall a. TreeConfig -> Map Int (InternalNode a) -> Int -> Map Int (InternalNode a)
secondWalkAll config nodes rootId =
  eachBefore nodes rootId (\ns nid -> secondWalk config ns nid)

-- Pre-order traversal
eachBefore :: forall a. Map Int (InternalNode a) -> Int -> (Map Int (InternalNode a) -> Int -> Map Int (InternalNode a)) -> Map Int (InternalNode a)
eachBefore nodes nodeId fn =
  let
    -- Process this node first
    nodes1 = fn nodes nodeId
    node = unsafeGetNode nodes1 nodeId
    -- Then process children
  in foldl (\ns childId -> eachBefore ns childId fn) nodes1 node.children

-- Second walk for single node
secondWalk :: forall a. TreeConfig -> Map Int (InternalNode a) -> Int -> Map Int (InternalNode a)
secondWalk config nodes nodeId =
  let
    v = unsafeGetNode nodes nodeId
    parentM = case v.parent of
      Nothing -> v.m  -- Root uses its own m value
      Just pid -> (unsafeGetNode nodes pid).m

    TreeNode tnode = v.node
    finalX = v.z + parentM
    -- Get max depth from the tree (fold over Map values)
    maxD = toNumber $ foldl (\acc n -> let TreeNode tn = n.node in max acc tn.depth) 0 (Map.values nodes)
    finalY = toNumber tnode.depth * (config.size.height / if maxD == 0.0 then 1.0 else maxD)

    updatedTreeNode = TreeNode (tnode { x = finalX, y = finalY })
    newM = v.m + parentM
    updated = v { node = updatedTreeNode, m = newM }
  in updateNode nodes nodeId updated

-- Extract TreeNode from internal representation
extractTreeNode :: forall a. Map Int (InternalNode a) -> Int -> TreeNode a
extractTreeNode nodes nodeId =
  let internal = unsafeGetNode nodes nodeId
      TreeNode node = internal.node
      childTrees = map (extractTreeNode nodes) internal.children
  in TreeNode (node { children = childTrees })

-- Find tree extents
findExtents :: forall a. TreeNode a -> TreeNode a -> TreeNode a -> TreeNode a -> { left :: TreeNode a, right :: TreeNode a, bottom :: TreeNode a }
findExtents left right bottom node@(TreeNode n) =
  let
    TreeNode l = left
    TreeNode r = right
    TreeNode b = bottom

    newLeft = if n.x < l.x then node else left
    newRight = if n.x > r.x then node else right
    newBottom = if n.depth > b.depth then node else bottom

  in foldl
       (\acc child -> findExtents acc.left acc.right acc.bottom child)
       { left: newLeft, right: newRight, bottom: newBottom }
       n.children

-- Scale a node
scaleNode :: forall a. Number -> Number -> Number -> TreeNode a -> TreeNode a
scaleNode tx kx ky (TreeNode node) =
  TreeNode
    { data_: node.data_
    , depth: node.depth
    , x: (node.x + tx) * kx
    , y: toNumber node.depth * ky
    , children: map (scaleNode tx kx ky) node.children
    }

-- Scale tree to fit size
scaleToFit :: forall a. TreeConfig -> TreeNode a -> TreeNode a
scaleToFit config tree =
  case config.nodeSize of
    Just _ -> tree  -- TODO: handle nodeSize
    Nothing ->
      let
        -- Find extents
        extents = findExtents tree tree tree tree

        TreeNode leftNode = extents.left
        TreeNode rightNode = extents.right
        TreeNode bottomNode = extents.bottom

        -- Compute scale factors
        s = if leftNode.x == rightNode.x
              then 1.0
              else config.separation extents.left extents.right / 2.0
        tx = s - leftNode.x
        kx = config.size.width / (rightNode.x + s + tx)
        ky = config.size.height / (if bottomNode.depth == 0 then 1.0 else toNumber bottomNode.depth)

      in scaleNode tx kx ky tree
