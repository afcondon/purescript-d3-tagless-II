module PSD3.Data.Tree
  ( TreeJson_
  , TreeLayoutFn_
  , TreeType(..)
  , TreeModel
  , TreeLayout(..)
  , makeD3TreeJSONFromTreeID
  , treeToD3Tree
  , arrayToTree
  ) where

import Prelude

import Data.Array (filter)
import Data.Array as A
import Data.Either (Either(..))
import Data.List (List(..))
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Tree (Tree(..))
import PSD3.Data.Node (NodeID)

foreign import data TreeJson_ :: Type
foreign import emptyTreeJson_ :: TreeJson_

data TreeType   = TidyTree | Dendrogram
derive instance eqTreeType :: Eq TreeType
instance showTreeType :: Show TreeType where
  show TidyTree = "Tidy Tree"
  show Dendrogram = "Dendrogram"

data TreeLayout = Radial | Horizontal | Vertical
derive instance eqTreeLayout :: Eq TreeLayout
derive instance ordTreeLayout :: Ord TreeLayout
instance showTreeLayout :: Show TreeLayout where
  show Radial = "Radial"
  show Horizontal = "Horizontal"
  show Vertical = "Vertical"

type TreeModel = {
      json         :: TreeJson_                      -- data from file
    , treeType     :: TreeType
    , treeLayout   :: TreeLayout
    , treeLayoutFn :: TreeLayoutFn_
    , svgConfig    :: { width :: Number, height :: Number }
}

foreign import data TreeLayoutFn_ :: Type

-- | Convert a Tree of NodeIDs to D3 TreeJson format using a lookup map
-- |
-- | This function is useful when you have a Tree structure where nodes
-- | contain only IDs, and you need to look up the full node data from a Map.
-- |
-- | Example:
-- | ```purescript
-- | let tree = Node "root" [Node "child1" [], Node "child2" []]
-- | let dataMap = M.fromFoldable [
-- |   Tuple "root" {name: "Root", value: 100},
-- |   Tuple "child1" {name: "Child 1", value: 50}
-- | ]
-- | let d3Tree = makeD3TreeJSONFromTreeID tree dataMap
-- | ```
-- |
-- | Note: If a node ID is not found in the map, an empty tree node is created.
makeD3TreeJSONFromTreeID :: forall d. Tree NodeID -> M.Map NodeID d -> TreeJson_
makeD3TreeJSONFromTreeID root nodesMap = go root
  where
    go (Node id children) =
      case M.lookup id nodesMap of
        Nothing -> emptyTreeJson_
        Just obj -> case children of
          Nil -> idTreeLeaf_ obj
          _ -> idTreeParent_ obj (go <$> (A.fromFoldable children))

-- | Convert a PureScript Tree directly to D3 TreeJson format
-- |
-- | This is the simplest conversion - each node already contains its data,
-- | no lookup required.
-- |
-- | Example:
-- | ```purescript
-- | let tree = Node {name: "Root", value: 100}
-- |              [ Node {name: "Child 1", value: 50} []
-- |              , Node {name: "Child 2", value: 30} []
-- |              ]
-- | let d3Tree = treeToD3Tree tree
-- | ```
treeToD3Tree :: forall d. Tree d -> TreeJson_
treeToD3Tree (Node data_ children) = case children of
  Nil -> idTreeLeaf_ data_
  _ -> idTreeParent_ data_ (treeToD3Tree <$> (A.fromFoldable children))

-- | Build a Tree from an array of nodes with parent pointers
-- |
-- | This function constructs a tree from a flat array where each node
-- | knows its parent ID. Useful for loading hierarchical data from databases
-- | or JSON where parent-child relationships are encoded as references.
-- |
-- | Example:
-- | ```purescript
-- | let nodes = [
-- |   {id: "1", parentId: Nothing, name: "Root"},
-- |   {id: "2", parentId: Just "1", name: "Child 1"},
-- |   {id: "3", parentId: Just "1", name: "Child 2"}
-- | ]
-- | let tree = arrayToTree {
-- |   nodes: nodes,
-- |   getId: _.id,
-- |   getParentId: _.parentId
-- | }
-- | ```
-- |
-- | Returns Left with error message if:
-- | - No root node found (node with Nothing parent)
-- | - Multiple root nodes found
-- | - Circular references detected
arrayToTree :: forall d.
  { nodes :: Array d
  , getId :: d -> NodeID
  , getParentId :: d -> Maybe NodeID
  } ->
  Either String (Tree d)
arrayToTree config = do
  -- Find root node (node with no parent)
  let roots = filter (\n -> config.getParentId n == Nothing) config.nodes

  case A.uncons roots of
    Nothing -> Left "No root node found (no node with parent = Nothing)"
    Just { head: root, tail } ->
      if A.length tail > 0
        then Left $ "Multiple root nodes found: " <> show (A.length roots)
        else Right $ buildSubtree root
  where
    buildSubtree :: d -> Tree d
    buildSubtree node =
      let
        nodeId = config.getId node
        -- Find all direct children of this node
        children = filter (\n -> config.getParentId n == Just nodeId) config.nodes
        -- Recursively build subtrees
        childTrees = buildSubtree <$> children
      in
        Node node (L.fromFoldable childTrees)

-- FFI imports
foreign import idTreeLeaf_   :: forall d. d -> TreeJson_
foreign import idTreeParent_ :: forall d. d -> Array TreeJson_ -> TreeJson_

