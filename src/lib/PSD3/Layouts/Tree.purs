module PSD3.Layouts.Tree
  ( TreeLayoutConfig(..)
  , TreeOrientation(..)
  , applyTreeLayout
  , extractTreePositions
  ) where

import Prelude

import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Number (pi)
import Data.Tuple (Tuple(..))
import Data.Tree (Tree)
import Foreign (unsafeToForeign)
import PSD3.Data.Node (NodeID, D3_TreeNode)
import PSD3.Data.Tree (TreeJson_, TreeLayoutFn_, treeToD3Tree)
import PSD3.Data.Tree as DataTree
import PSD3.Internal.FFI (PackLayout_, TreemapLayout_, PartitionLayout_)
import PSD3.Internal.FFI (descendants_, hierarchyFromJSON_, keyIsID_, runLayoutFn_)
import PSD3.Internal.FFI (treeSetSize_, treeSetNodeSize_, treeSetSeparation_)
import PSD3.Internal.FFI (packLayout_, packSetSize_, packSetPadding_, runPackLayout_)
import PSD3.Internal.FFI (treemapLayout_, treemapSetSize_, treemapSetPadding_, runTreemapLayout_)
import PSD3.Internal.FFI (partitionLayout_, runPartitionLayout_)
import PSD3.Internal.Hierarchical (radialSeparation)

-- FFI imports for D3 tree layout algorithms
foreign import d3Tree_ :: Unit -> TreeLayoutFn_
foreign import d3Cluster_ :: Unit -> TreeLayoutFn_

-- | Orientation for tree and dendrogram layouts
data TreeOrientation
  = Radial       -- Emanates from center in polar coordinates
  | Horizontal   -- Left-to-right layout
  | Vertical     -- Top-to-bottom layout

derive instance eqTreeOrientation :: Eq TreeOrientation
derive instance ordTreeOrientation :: Ord TreeOrientation

instance showTreeOrientation :: Show TreeOrientation where
  show Radial = "Radial"
  show Horizontal = "Horizontal"
  show Vertical = "Vertical"

-- | Unified configuration for all tree layout types
-- |
-- | This ADT allows you to specify any D3 hierarchy layout with a single type.
-- | Each constructor contains the parameters specific to that layout type.
data TreeLayoutConfig
  = TidyTree
      { orientation :: TreeOrientation
      , width :: Number
      , height :: Number
      , nodeSize :: Maybe (Array Number)
      }
  | Dendrogram
      { orientation :: TreeOrientation
      , width :: Number
      , height :: Number
      , nodeSize :: Maybe (Array Number)
      }
  | Pack
      { width :: Number
      , height :: Number
      , padding :: Number
      }
  | Treemap
      { width :: Number
      , height :: Number
      , padding :: Number
      }
  | Partition
      { width :: Number
      , height :: Number
      }

-- | Apply a tree layout to hierarchical data
-- |
-- | This is the main entry point for tree layouts. It takes a PureScript Tree
-- | and a layout configuration, and returns a D3 tree node with positions computed.
-- |
-- | Example:
-- | ```purescript
-- | let config = TidyTree {
-- |   orientation: Radial,
-- |   width: 800.0,
-- |   height: 800.0,
-- |   nodeSize: Nothing
-- | }
-- | let laidOutTree = applyTreeLayout myTree config
-- | ```
applyTreeLayout :: forall d r. Tree d -> TreeLayoutConfig -> D3_TreeNode r
applyTreeLayout tree config = case config of
  TidyTree opts -> applyTidyTree tree opts
  Dendrogram opts -> applyDendrogram tree opts
  Pack opts -> applyPack tree opts
  Treemap opts -> applyTreemap tree opts
  Partition opts -> applyPartition tree opts

-- Internal: Apply TidyTree layout
applyTidyTree :: forall d r.
  Tree d ->
  { orientation :: TreeOrientation
  , width :: Number
  , height :: Number
  , nodeSize :: Maybe (Array Number)
  } ->
  D3_TreeNode r
applyTidyTree tree opts =
  let
    treeJson = treeToD3Tree tree
    root = hierarchyFromJSON_ treeJson
    layoutFn = case opts.orientation of
      Radial ->
        let baseLayout = d3Tree_ unit
            withSize = baseLayout `treeSetSize_` [2.0 * pi, opts.width / 2.0 - 100.0]
        in withSize `treeSetSeparation_` radialSeparation
      Horizontal ->
        let baseLayout = d3Tree_ unit
        in case opts.nodeSize of
          Just size -> baseLayout `treeSetNodeSize_` size
          Nothing -> baseLayout `treeSetSize_` [opts.height, opts.width]
      Vertical ->
        let baseLayout = d3Tree_ unit
        in case opts.nodeSize of
          Just size -> baseLayout `treeSetNodeSize_` size
          Nothing -> baseLayout `treeSetSize_` [opts.width, opts.height]
  in
    layoutFn `runLayoutFn_` root

-- Internal: Apply Dendrogram (cluster) layout
applyDendrogram :: forall d r.
  Tree d ->
  { orientation :: TreeOrientation
  , width :: Number
  , height :: Number
  , nodeSize :: Maybe (Array Number)
  } ->
  D3_TreeNode r
applyDendrogram tree opts =
  let
    treeJson = treeToD3Tree tree
    root = hierarchyFromJSON_ treeJson
    layoutFn = case opts.orientation of
      Radial ->
        let baseLayout = d3Cluster_ unit
            withSize = baseLayout `treeSetSize_` [2.0 * pi, opts.width / 2.0 - 100.0]
        in withSize `treeSetSeparation_` radialSeparation
      Horizontal ->
        let baseLayout = d3Cluster_ unit
        in case opts.nodeSize of
          Just size -> baseLayout `treeSetNodeSize_` size
          Nothing -> baseLayout `treeSetSize_` [opts.height, opts.width]
      Vertical ->
        let baseLayout = d3Cluster_ unit
        in case opts.nodeSize of
          Just size -> baseLayout `treeSetNodeSize_` size
          Nothing -> baseLayout `treeSetSize_` [opts.width, opts.height]
  in
    layoutFn `runLayoutFn_` root

-- Internal: Apply Pack layout
applyPack :: forall d r.
  Tree d ->
  { width :: Number, height :: Number, padding :: Number } ->
  D3_TreeNode r
applyPack tree opts =
  let
    treeJson = treeToD3Tree tree
    root = hierarchyFromJSON_ treeJson
    layout = packLayout_ unit
              # (\l -> packSetSize_ l opts.width opts.height)
              # (\l -> packSetPadding_ l opts.padding)
  in
    runPackLayout_ layout root

-- Internal: Apply Treemap layout
applyTreemap :: forall d r.
  Tree d ->
  { width :: Number, height :: Number, padding :: Number } ->
  D3_TreeNode r
applyTreemap tree opts =
  let
    treeJson = treeToD3Tree tree
    root = hierarchyFromJSON_ treeJson
    layout = treemapLayout_ unit
              # (\l -> treemapSetSize_ l opts.width opts.height)
              # (\l -> treemapSetPadding_ l opts.padding)
  in
    runTreemapLayout_ layout root

-- Internal: Apply Partition layout
applyPartition :: forall d r.
  Tree d ->
  { width :: Number, height :: Number } ->
  D3_TreeNode r
applyPartition tree _ =
  let
    treeJson = treeToD3Tree tree
    root = hierarchyFromJSON_ treeJson
    layout = partitionLayout_ unit
    -- Note: partition layout doesn't use explicit size setting like other layouts
    -- The size is determined by the parent container
  in
    runPartitionLayout_ layout root

-- | Extract positions from a laid-out tree
-- |
-- | After applying a layout, this function extracts the computed positions
-- | into a Map keyed by node ID. This is useful for applying tree positions
-- | to simulation nodes or other visualization elements.
-- |
-- | Note: This assumes nodes have an 'id' field. The extraction uses D3's
-- | descendants traversal and unsafe coercion to extract the ID field.
-- |
-- | Example:
-- | ```purescript
-- | let laidOutTree = applyTreeLayout myTree config
-- | let positions = extractTreePositions laidOutTree
-- | -- Now use positions Map to position your visualization elements
-- | ```
extractTreePositions :: forall r.
  D3_TreeNode r ->
  Map NodeID { x :: Number, y :: Number, depth :: Int }
extractTreePositions root =
  let
    nodes = descendants_ root
    -- Extract position data from each node
    -- We use unsafeToForeign to access the x, y, depth properties
    -- that D3 adds to the tree nodes
    extractPosition node =
      let foreign_ = unsafeToForeign node
      in { id: extractID foreign_
         , x: extractX foreign_
         , y: extractY foreign_
         , depth: extractDepth foreign_
         }
    extracted = extractPosition <$> nodes
  in
    M.fromFoldable $ (\p -> Tuple p.id { x: p.x, y: p.y, depth: p.depth }) <$> extracted

-- FFI helpers for extracting layout properties
foreign import extractID :: forall a. a -> NodeID
foreign import extractX :: forall a. a -> Number
foreign import extractY :: forall a. a -> Number
foreign import extractDepth :: forall a. a -> Int
