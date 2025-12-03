-- | DataViz.Layout.Hierarchy.EdgeBundle.Bilink
-- |
-- | Create bidirectional links between nodes based on imports.
-- | This implements D3's bilink pattern:
-- | - Each leaf node gets an "outgoing" array (imports it makes)
-- | - Each leaf node gets an "incoming" array (nodes that import it)
module DataViz.Layout.Hierarchy.EdgeBundle.Bilink
  ( BilinkedTree
  , BilinkedNode(..)
  , Link(..)
  , bilink
  , getLinks
  , getOutgoing
  , getIncoming
  , getBilinkedData
  , getBilinkedChildren
  , getBilinkedFullName
  , allBilinkedNodes
  , allBilinkedLeaves
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import DataViz.Layout.Hierarchy.EdgeBundle.Hierarchy (TreeNode(..), getFullName, getTreeNodeChildren, isLeaf, leaves)
import DataViz.Layout.Hierarchy.EdgeBundle.Types (ImportedNode)

-- | A link between two nodes
newtype Link = Link
  { source :: String -- Full name of source node
  , target :: String -- Full name of target node
  }

derive instance eqLink :: Eq Link

instance showLink :: Show Link where
  show (Link l) = l.source <> " -> " <> l.target

-- | A node with bidirectional link information
data BilinkedNode a = BilinkedNode
  { name :: String
  , fullName :: String
  , children :: Array (BilinkedNode a)
  , data_ :: Maybe a
  , depth :: Int
  , height :: Int
  , outgoing :: Array Link -- Links FROM this node (imports)
  , incoming :: Array Link -- Links TO this node (imported by)
  }

instance showBilinkedNode :: Show a => Show (BilinkedNode a) where
  show (BilinkedNode n) = "BilinkedNode { fullName: " <> n.fullName
    <> ", outgoing: "
    <> show (Array.length n.outgoing)
    <> ", incoming: "
    <> show (Array.length n.incoming)
    <> " }"

-- | Type alias for the complete bilinked tree
type BilinkedTree a = BilinkedNode a

-- | Get outgoing links from a node
getOutgoing :: forall a. BilinkedNode a -> Array Link
getOutgoing (BilinkedNode n) = n.outgoing

-- | Get incoming links to a node
getIncoming :: forall a. BilinkedNode a -> Array Link
getIncoming (BilinkedNode n) = n.incoming

-- | Get the data from a bilinked node
getBilinkedData :: forall a. BilinkedNode a -> Maybe a
getBilinkedData (BilinkedNode n) = n.data_

-- | Get children of a bilinked node
getBilinkedChildren :: forall a. BilinkedNode a -> Array (BilinkedNode a)
getBilinkedChildren (BilinkedNode n) = n.children

-- | Get full name of a bilinked node
getBilinkedFullName :: forall a. BilinkedNode a -> String
getBilinkedFullName (BilinkedNode n) = n.fullName

-- | Get all links in the tree
getLinks :: forall a. BilinkedTree a -> Array Link
getLinks root =
  let
    allNodes = allBilinkedNodes root
  in
    Array.concatMap getOutgoing allNodes

-- | Get all nodes in the tree (pre-order)
allBilinkedNodes :: forall a. BilinkedNode a -> Array (BilinkedNode a)
allBilinkedNodes node@(BilinkedNode n) =
  [ node ] <> Array.concatMap allBilinkedNodes n.children

-- | Create bidirectional links from a hierarchy and import data
-- |
-- | Algorithm:
-- | 1. Build a map from fullName -> leaf node
-- | 2. For each leaf with imports, create outgoing links
-- | 3. For each outgoing link, add corresponding incoming link to target
-- |
-- | Takes:
-- | - A tree built from buildHierarchy
-- | - A function to get imports from the leaf data
bilink
  :: forall a
   . (a -> Array String)
  -> -- Get imports from node data
  TreeNode a
  -> -- Hierarchy tree
  BilinkedTree a
bilink getImports tree =
  let
    -- First, convert tree to bilinked format (without links)
    initialBilinked = convertToBilinked tree

    -- Get all leaves for the name map
    leafNodes = allBilinkedLeaves initialBilinked
    nameMap = Map.fromFoldable $ map (\n -> Tuple (getBilinkedFullName n) n) leafNodes

    -- Collect all links
    allLinks = collectLinks getImports leafNodes

    -- Build maps of outgoing and incoming links per node
    outgoingMap = buildLinkMap (\(Link l) -> l.source) allLinks
    incomingMap = buildLinkMap (\(Link l) -> l.target) allLinks

    -- Apply links to tree
    withLinks = applyLinks outgoingMap incomingMap initialBilinked
  in
    withLinks

-- | Convert a TreeNode to BilinkedNode (without links yet)
convertToBilinked :: forall a. TreeNode a -> BilinkedNode a
convertToBilinked (TreeNode n) = BilinkedNode
  { name: n.name
  , fullName: n.fullName
  , children: map convertToBilinked n.children
  , data_: n.data_
  , depth: n.depth
  , height: n.height
  , outgoing: []
  , incoming: []
  }

-- | Get all leaf nodes from a bilinked tree
allBilinkedLeaves :: forall a. BilinkedNode a -> Array (BilinkedNode a)
allBilinkedLeaves node@(BilinkedNode n) =
  if Array.null n.children then
    [ node ]
  else
    Array.concatMap allBilinkedLeaves n.children

-- | Collect all links from leaf nodes with imports
collectLinks
  :: forall a
   . (a -> Array String)
  -> Array (BilinkedNode a)
  -> Array Link
collectLinks getImports leafNodes =
  Array.concatMap (nodeToLinks getImports) leafNodes

-- | Convert a single node's imports to links
nodeToLinks
  :: forall a
   . (a -> Array String)
  -> BilinkedNode a
  -> Array Link
nodeToLinks getImports (BilinkedNode n) =
  case n.data_ of
    Nothing -> []
    Just nodeData ->
      let
        imports = getImports nodeData
      in
        map (\target -> Link { source: n.fullName, target }) imports

-- | Build a map from node name to array of links
buildLinkMap :: (Link -> String) -> Array Link -> Map String (Array Link)
buildLinkMap getKey links =
  foldl
    ( \acc link ->
        let
          key = getKey link
          existing = fromMaybe [] (Map.lookup key acc)
        in
          Map.insert key (Array.snoc existing link) acc
    )
    Map.empty
    links

-- | Apply outgoing and incoming links to the tree
applyLinks
  :: forall a
   . Map String (Array Link)
  -> -- Outgoing links by source
  Map String (Array Link)
  -> -- Incoming links by target
  BilinkedNode a
  -> BilinkedNode a
applyLinks outgoingMap incomingMap (BilinkedNode n) =
  let
    -- Get links for this node
    outgoing = fromMaybe [] (Map.lookup n.fullName outgoingMap)
    incoming = fromMaybe [] (Map.lookup n.fullName incomingMap)

    -- Recursively apply to children
    childrenWithLinks = map (applyLinks outgoingMap incomingMap) n.children
  in
    BilinkedNode
      ( n
          { outgoing = outgoing
          , incoming = incoming
          , children = childrenWithLinks
          }
      )
