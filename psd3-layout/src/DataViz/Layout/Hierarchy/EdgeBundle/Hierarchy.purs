-- | DataViz.Layout.Hierarchy.EdgeBundle.Hierarchy
-- |
-- | Build a tree hierarchy from flat dot-notation names.
-- | This implements the same algorithm as D3's Observable example:
-- | names like "flare.animate.Easing" become nested tree nodes.
module DataViz.Layout.Hierarchy.EdgeBundle.Hierarchy
  ( buildHierarchy
  , TreeNode(..)
  , getTreeNodeName
  , getTreeNodeChildren
  , getTreeNodeData
  , isLeaf
  , leaves
  , descendants
  , findNode
  , getFullName
  , getAncestors
  , pathBetween
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.Tuple (Tuple(..))

-- | A node in the hierarchy tree
-- | Generic over the leaf data type
data TreeNode a = TreeNode
  { name :: String -- Short name (e.g. "Easing")
  , fullName :: String -- Full path (e.g. "flare.animate.Easing")
  , children :: Array (TreeNode a) -- Child nodes (empty for leaves)
  , data_ :: Maybe a -- User data (only for leaves)
  , depth :: Int -- Distance from root
  , height :: Int -- Distance to deepest leaf
  }

instance showTreeNode :: Show a => Show (TreeNode a) where
  show (TreeNode n) = "TreeNode { name: " <> n.name <> ", fullName: " <> n.fullName <> " }"

-- | Get the short name of a node
getTreeNodeName :: forall a. TreeNode a -> String
getTreeNodeName (TreeNode n) = n.name

-- | Get children of a node
getTreeNodeChildren :: forall a. TreeNode a -> Array (TreeNode a)
getTreeNodeChildren (TreeNode n) = n.children

-- | Get the user data (if this is a leaf)
getTreeNodeData :: forall a. TreeNode a -> Maybe a
getTreeNodeData (TreeNode n) = n.data_

-- | Get the full name of a node
getFullName :: forall a. TreeNode a -> String
getFullName (TreeNode n) = n.fullName

-- | Check if a node is a leaf (no children)
isLeaf :: forall a. TreeNode a -> Boolean
isLeaf (TreeNode n) = Array.null n.children

-- | Get all leaf nodes
leaves :: forall a. TreeNode a -> Array (TreeNode a)
leaves root = Array.filter isLeaf (descendants root)

-- | Get all descendant nodes (pre-order traversal, including self)
descendants :: forall a. TreeNode a -> Array (TreeNode a)
descendants node@(TreeNode n) =
  [ node ] <> Array.concatMap descendants n.children

-- | Find a node by its full name
findNode :: forall a. String -> TreeNode a -> Maybe (TreeNode a)
findNode targetName root@(TreeNode n)
  | n.fullName == targetName = Just root
  | otherwise =
      Array.findMap (findNode targetName) n.children

-- | Get all ancestors of a node (from root to node, inclusive)
-- | This traverses down from root to find the path
getAncestors :: forall a. TreeNode a -> TreeNode a -> Array (TreeNode a)
getAncestors root target = fromMaybe [] (findPath root target)
  where
  findPath :: TreeNode a -> TreeNode a -> Maybe (Array (TreeNode a))
  findPath node@(TreeNode n) (TreeNode t)
    | n.fullName == t.fullName = Just [ node ]
    | otherwise =
        Array.findMap
          ( \child ->
              map (Array.cons node) (findPath child (TreeNode t))
          )
          n.children

-- | Find the path between two nodes through their lowest common ancestor
-- | Returns: path from source up to LCA, then down to target
-- | D3's node.path(target) implementation
pathBetween :: forall a. TreeNode a -> TreeNode a -> TreeNode a -> Array (TreeNode a)
pathBetween root source target =
  let
    -- Get ancestors from root to each node
    sourceAncestors = getAncestors root source
    targetAncestors = getAncestors root target

    -- Find lowest common ancestor (last shared ancestor)
    lca = findLCA sourceAncestors targetAncestors

    -- Path from source to LCA (reverse of source ancestors up to LCA)
    sourceToLCA = case lca of
      Nothing -> sourceAncestors
      Just lcaNode ->
        let
          lcaName = getFullName lcaNode
        in
          Array.reverse $ Array.takeWhile (\n -> getFullName n /= lcaName) (Array.reverse sourceAncestors)

    -- Path from LCA to target (target ancestors from LCA down)
    lcaToTarget = case lca of
      Nothing -> targetAncestors
      Just lcaNode ->
        let
          lcaName = getFullName lcaNode
        in
          Array.dropWhile (\n -> getFullName n /= lcaName) targetAncestors
  in
    -- Combine: source → LCA → target (LCA appears once in the middle)
    sourceToLCA <> lcaToTarget

  where
  findLCA :: Array (TreeNode a) -> Array (TreeNode a) -> Maybe (TreeNode a)
  findLCA as bs =
    let
      -- Build set of ancestor names for efficient lookup
      bNames = map getFullName bs
      -- Find the deepest ancestor that's in both paths
      common = Array.filter (\a -> Array.elem (getFullName a) bNames) as
    in
      Array.last common -- Last = deepest

-- | Build a hierarchy from flat imported nodes
-- | Takes an array of nodes with dot-notation names and builds a tree
-- |
-- | Algorithm:
-- | 1. For each node, split name by "." to get path components
-- | 2. Create/find intermediate nodes for each path component
-- | 3. Attach leaf data at the final node
buildHierarchy
  :: forall a
   . { getName :: a -> String }
  -> Array a
  -> TreeNode a
buildHierarchy config nodes =
  let
    -- Start with empty root
    emptyRoot = TreeNode
      { name: ""
      , fullName: ""
      , children: []
      , data_: Nothing
      , depth: 0
      , height: 0
      }

    -- Insert each node into the tree
    treeWithNodes = foldl (insertNode config.getName) emptyRoot nodes

    -- Compute heights bottom-up
    withHeights = computeHeights treeWithNodes
  in
    withHeights

-- | Insert a node into the tree at the appropriate path
insertNode
  :: forall a
   . (a -> String)
  -> -- Get name from data
  TreeNode a
  -> -- Current tree
  a
  -> -- Data to insert
  TreeNode a
insertNode getName root nodeData =
  let
    fullName = getName nodeData
    parts = String.split (String.Pattern ".") fullName
  in
    insertAtPath parts fullName nodeData "" root

-- | Recursively insert at a path, creating intermediate nodes as needed
insertAtPath
  :: forall a
   . Array String
  -> -- Remaining path parts
  String
  -> -- Full original name
  a
  -> -- Data to insert (at leaf)
  String
  -> -- Current path prefix
  TreeNode a
  -> -- Current node
  TreeNode a
insertAtPath parts fullName nodeData prefix (TreeNode node) =
  case Array.uncons parts of
    Nothing ->
      -- No more parts - we're done (shouldn't happen with valid input)
      TreeNode node

    Just { head: part, tail: rest } ->
      let
        newPrefix = if prefix == "" then part else prefix <> "." <> part
        newDepth = node.depth + 1
      in
        if Array.null rest then
          -- This is the leaf - add with data
          let
            newLeaf = TreeNode
              { name: part
              , fullName: newPrefix
              , children: []
              , data_: Just nodeData
              , depth: newDepth
              , height: 0
              }
          in
            TreeNode (node { children = addOrUpdateChild part newLeaf node.children })
        else
          -- Intermediate node - find or create, then recurse
          let
            existingChild = Array.find (\(TreeNode c) -> c.name == part) node.children
            childNode = case existingChild of
              Just existing -> existing
              Nothing -> TreeNode
                { name: part
                , fullName: newPrefix
                , children: []
                , data_: Nothing
                , depth: newDepth
                , height: 0
                }

            -- Recursively insert into child
            updatedChild = insertAtPath rest fullName nodeData newPrefix childNode
          in
            TreeNode (node { children = addOrUpdateChild part updatedChild node.children })

-- | Add a child or update existing one with same name
addOrUpdateChild :: forall a. String -> TreeNode a -> Array (TreeNode a) -> Array (TreeNode a)
addOrUpdateChild name newChild children =
  let
    exists = Array.any (\(TreeNode c) -> c.name == name) children
  in
    if exists then
      map
        ( \child@(TreeNode c) ->
            if c.name == name then newChild else child
        )
        children
    else
      Array.snoc children newChild

-- | Compute heights bottom-up
computeHeights :: forall a. TreeNode a -> TreeNode a
computeHeights (TreeNode node) =
  if Array.null node.children then
    TreeNode (node { height = 0 })
  else
    let
      childrenWithHeights = map computeHeights node.children
      maxChildHeight = foldl (\acc (TreeNode c) -> max acc c.height) 0 childrenWithHeights
    in
      TreeNode (node { children = childrenWithHeights, height = maxChildHeight + 1 })
