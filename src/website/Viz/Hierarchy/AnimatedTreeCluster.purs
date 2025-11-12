module D3.Viz.AnimatedTreeCluster where

-- | Animated transition between Tree and Cluster layouts
-- | Demonstrates pure PureScript hierarchy layout algorithms with smooth transitions
-- | Creates SVG once, reuses hierarchy, and transitions node positions

import Prelude

import PSD3.Internal.Attributes.Sugar (classed, fill, fillOpacity, fontSize, strokeColor, strokeWidth, text, viewBox, cx, cy, radius, x, y, textAnchor, d, transitionWithDuration, to, transform)
import PSD3.Internal.Types (D3Selection_, Element(..), Selector, Datum_, Index_)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach, openSelection, updateJoin, mergeSelections, setAttributes, simpleJoin)
import PSD3.Layout.Hierarchy.Core (hierarchy)
import PSD3.Layout.Hierarchy.Core as Hierarchy
import PSD3.Layout.Hierarchy.Types (HierarchyNode(..), ValuedNode(..))
import PSD3.Layout.Hierarchy.Tree (tree, defaultTreeConfig, TreeNode(..))
import PSD3.Layout.Hierarchy.Cluster (cluster, defaultClusterConfig, ClusterNode(..))
import PSD3.Layout.Hierarchy.Projection (verticalX, verticalY, clusterVerticalX, clusterVerticalY, verticalLinkPath)
import D3.Viz.FlareData (HierData, getName, getValue)
import D3.Viz.FlareData (getChildren) as FlareData
import Data.Array (length, (!!))
import Data.Array as Array
import Data.Traversable (traverse_, foldl)
import Data.Ord (max)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Unsafe.Coerce (unsafeCoerce)

-- Layout type
data LayoutType = TreeLayout | ClusterLayout

derive instance Eq LayoutType

instance Show LayoutType where
  show TreeLayout = "Tree"
  show ClusterLayout = "Cluster"

-- Toggle layout type
toggleLayout :: LayoutType -> LayoutType
toggleLayout TreeLayout = ClusterLayout
toggleLayout ClusterLayout = TreeLayout

-- Convert ValuedNode back to HierarchyNode (for cluster layout)
valuedToHierarchy :: forall a. ValuedNode a -> HierarchyNode a
valuedToHierarchy (VNode vn) =
  HNode
    { data_: vn.data_
    , depth: vn.depth
    , height: vn.height
    , parent: Nothing
    , children: map valuedToHierarchy vn.children
    }

-- Sort ValuedNode by height and value (for cluster layout)
sortValuedByHeightAndValue :: forall a. ValuedNode a -> ValuedNode a
sortValuedByHeightAndValue (VNode vn) =
  if length vn.children == 0
  then VNode vn
  else
    let
      sortedGrandchildren = map sortValuedByHeightAndValue vn.children
      sortedChildren = Array.sortBy comparator sortedGrandchildren
    in
      VNode (vn { children = sortedChildren })
  where
    comparator (VNode a) (VNode b) =
      case compare b.height a.height of
        EQ -> compare b.value a.value
        other -> other

-- Get all nodes from tree layout
getAllTreeNodes :: forall a. TreeNode a -> Array (TreeNode a)
getAllTreeNodes node@(TreeNode n) =
  if length n.children == 0
  then [node]
  else [node] <> (n.children >>= getAllTreeNodes)

-- Get all nodes from cluster layout
getAllClusterNodes :: forall a. ClusterNode a -> Array (ClusterNode a)
getAllClusterNodes node@(ClusterNode n) =
  if length n.children == 0
  then [node]
  else [node] <> (n.children >>= getAllClusterNodes)

-- Key function: use node name from data as unique identifier
-- TreeNode and ClusterNode have the same structure, so this works for both
nodeKey :: Datum_ -> Index_
nodeKey d =
  let TreeNode node = unsafeCoerce d
  in unsafeCoerce $ getName node.data_

-- Get link key (source -> target path)
-- Links contain TreeNode or ClusterNode, need to unwrap
linkKey :: Datum_ -> Index_
linkKey d =
  let link = unsafeCoerce d :: { source :: TreeNode HierData, target :: TreeNode HierData }
      TreeNode source = link.source
      TreeNode target = link.target
  in unsafeCoerce $ getName source.data_ <> " -> " <> getName target.data_

-- Create links array from nodes
makeLinks :: forall a node. (node -> Array node) -> (node -> a) -> node -> Array { source :: a, target :: a }
makeLinks getChildren' getData' root =
  let
    go parent = do
      child <- getChildren' parent
      let parentData = getData' parent
      let childData = getData' child
      [{ source: parentData, target: childData }] <> go child
  in go root

-- Initial draw - creates SVG structure and draws tree layout
draw :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  HierData -> Selector (D3Selection_ Unit) -> m { hierarchyRoot :: HierarchyNode HierData, links :: D3Selection_ Datum_, nodes :: D3Selection_ Datum_ }
draw flareData selector = do
  liftEffect $ log "AnimatedTreeCluster: Initial draw (Tree layout)"

  let chartWidth = 1600.0
  let chartHeight = 1200.0

  -- Create hierarchy ONCE - we'll reuse this
  let hierarchyRoot = hierarchy flareData FlareData.getChildren

  -- Apply tree layout
  let config = defaultTreeConfig { size = { width: chartWidth, height: chartHeight } }
  let treeLayout = tree config hierarchyRoot

  -- Get nodes for visualization
  let nodes = getAllTreeNodes treeLayout
  let nodesData = unsafeCoerce nodes :: Array Datum_

  -- Create links
  let links = makeLinks (\(TreeNode n) -> n.children) identity treeLayout
  let linksData = unsafeCoerce links :: Array Datum_

  liftEffect $ log $ "Initial: " <> show (length nodes) <> " nodes, " <> show (length links) <> " links"

  -- Create SVG structure
  root' <- attach selector :: m (D3Selection_ Unit)
  svg <- appendTo root' Svg
    [ viewBox 0.0 0.0 chartWidth chartHeight
    , classed "animated-tree-cluster"
    ]

  -- Add title
  _ <- appendTo svg Text
    [ x 10.0
    , y 20.0
    , fill "#333"
    , fontSize 16.0
    , text "Tree Layout (Reingold-Tilford)"
    , classed "title"
    ]

  -- Create groups for links and nodes
  linksGroup <- appendTo svg Group [ classed "links" ]
  nodesGroup <- appendTo svg Group [ classed "nodes" ]

  -- Draw initial links (no transitions)
  linkSelection <- simpleJoin linksGroup Path linksData linkKey

  let linkPathFn :: Datum_ -> String
      linkPathFn datum =
        -- Links contain TreeNode, need to unwrap the newtype
        let link = unsafeCoerce datum :: { source :: TreeNode HierData, target :: TreeNode HierData }
            TreeNode source = link.source
            TreeNode target = link.target
        in verticalLinkPath source.x source.y target.x target.y

  setAttributes linkSelection
    [ d linkPathFn
    , fill "none"
    , strokeColor "#555"
    , strokeWidth 1.5
    , fillOpacity 0.4
    ]

  -- Draw initial nodes (no transitions)
  -- Create a group for each node (matches D3 pattern)
  nodeGroupsSelection <- simpleJoin nodesGroup Group nodesData nodeKey

  let translateFn :: Datum_ -> String
      translateFn d =
        -- Node is TreeNode, need to unwrap the newtype
        let TreeNode node = unsafeCoerce d
        in "translate(" <> show node.x <> "," <> show node.y <> ")"

  setAttributes nodeGroupsSelection
    [ transform [\d -> translateFn d]
    ]

  -- Add circles to each node group
  let radiusFn :: Datum_ -> Number
      radiusFn d =
        let TreeNode node = unsafeCoerce d
        in if length node.children == 0 then 3.0 else 4.0

  _ <- appendTo nodeGroupsSelection Circle
    [ radius radiusFn
    , fill "#999"
    , strokeColor "#555"
    , strokeWidth 1.5
    ]

  -- Return the data-bound selections, not the parent containers!
  pure { hierarchyRoot, links: linkSelection, nodes: nodeGroupsSelection }

-- Update - transitions to the other layout
updateLayout :: forall m d1 d2.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  LayoutType ->
  HierarchyNode HierData ->
  D3Selection_ d1 ->
  D3Selection_ d2 ->
  m LayoutType
updateLayout currentLayout hierarchyRoot linksGroup nodesGroup = do
  let newLayout = toggleLayout currentLayout
  liftEffect $ log $ "AnimatedTreeCluster: Transitioning from " <> show currentLayout <> " to " <> show newLayout

  let chartWidth = 1600.0
  let chartHeight = 1200.0

  -- Apply the appropriate layout to the SAME hierarchy
  case newLayout of
    TreeLayout -> do
      let config = defaultTreeConfig { size = { width: chartWidth, height: chartHeight } }
      let treeLayout = tree config hierarchyRoot
      let nodes = getAllTreeNodes treeLayout
      let nodesData = unsafeCoerce nodes :: Array Datum_
      let links = makeLinks (\(TreeNode n) -> n.children) identity treeLayout
      let linksData = unsafeCoerce links :: Array Datum_

      updateVisualization nodesData linksData linksGroup nodesGroup

    ClusterLayout -> do
      -- For cluster, we need to sort by value
      let valued = Hierarchy.sum hierarchyRoot getValue
      let sortedValued = sortValuedByHeightAndValue valued
      let sorted = valuedToHierarchy sortedValued

      let config = defaultClusterConfig { size = { width: chartWidth, height: chartHeight } }
      let clusterLayout = cluster config sorted
      let nodes = getAllClusterNodes clusterLayout
      let nodesData = unsafeCoerce nodes :: Array Datum_
      let links = makeLinks (\(ClusterNode n) -> n.children) identity clusterLayout
      let linksData = unsafeCoerce links :: Array Datum_

      updateVisualization nodesData linksData linksGroup nodesGroup

  pure newLayout

-- Helper to update visualization with transitions
-- Takes the existing data-bound selections and updates them with new data
updateVisualization :: forall m d1 d2.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  Array Datum_ ->
  Array Datum_ ->
  D3Selection_ d1 ->  -- Existing link selection with old data
  D3Selection_ d2 ->  -- Existing node selection with old data
  m Unit
updateVisualization nodesData linksData linksSelection nodesSelection = do
  let transition = transitionWithDuration $ Milliseconds 750.0

  -- Update links with transitions
  -- No openSelection needed - linksSelection already has the path elements
  linksJoin <- updateJoin linksSelection Path linksData linkKey
  linksAll <- mergeSelections linksJoin.enter linksJoin.update

  setAttributes linksAll
    [ fill "none"
    , strokeColor "#555"
    , strokeWidth 1.5
    , fillOpacity 0.4
    ]

  let linkPathFn :: Datum_ -> String
      linkPathFn datum =
        -- Links contain TreeNode/ClusterNode, need to unwrap
        let link = unsafeCoerce datum :: { source :: TreeNode HierData, target :: TreeNode HierData }
            TreeNode source = link.source
            TreeNode target = link.target
        in verticalLinkPath source.x source.y target.x target.y

  setAttributes linksAll $ transition `to` [ d linkPathFn ]

  -- Update nodes with transitions
  -- No openSelection needed - nodesSelection already has the group elements
  nodesJoin <- updateJoin nodesSelection Group nodesData nodeKey
  nodeGroupsAll <- mergeSelections nodesJoin.enter nodesJoin.update

  let translateFn :: Datum_ -> String
      translateFn d =
        -- Node is TreeNode/ClusterNode, need to unwrap
        let TreeNode node = unsafeCoerce d
        in "translate(" <> show node.x <> "," <> show node.y <> ")"

  -- Transition the transform attribute of the groups
  setAttributes nodeGroupsAll $ transition `to` [ transform [\d -> translateFn d] ]

  pure unit
