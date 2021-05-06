module D3.Data.Tree where

import Prelude

import Data.Nullable (Nullable)

-- these definitions have to be here to avoid cycle (and probably all type defs should in fact be here)
foreign import data TreeJson_           :: Type
foreign import data D3HierarchicalNode_ :: Type

data Tree a = Node a (Array (Tree a))

data TreeType   = TidyTree | Dendrogram
derive instance eqTreeType :: Eq TreeType
data TreeLayout = Radial | Horizontal | Vertical
derive instance eqTreeLayout :: Eq TreeLayout

-- TODO put in proxy fields here to carry the type allowing safe coerce of root etc
-- TODO need to define a model here that works for all hierarchic layouts, this has its origins in Radial tree only
-- d is the type of the datum and v is the type of computed value, ie for summing etc
-- type Model :: forall d v. d -> v -> Type
type TreeModel d v = {
      json       :: TreeJson_
    , root       :: D3HierarchicalNode d v
    , root_      :: D3HierarchicalNode_
    , treeType   :: TreeType
    , treeLayout :: TreeLayout
    , svgConfig  :: { width :: Number, height :: Number }
}

-- the PureScript rep of opaque type D3HierarchicalNode_
-- we can safely cast any D3HierarchicalNode_ to this if we know the types d and v
-- there might be some way, passing proxies around, to enforce that constraint?
newtype D3HierarchicalNode d v = D3HierarchicalNode { -- (newtype to avoid cycles in types)
    "data"   :: d -- the data that is passed in to the tree
  , depth    :: Int
  , height   :: Int
  , parent   :: Nullable (D3HierarchicalNode d v)
  , children :: Array (D3HierarchicalNode d v)
  , value    :: v -- set by some function passed to node.value() or by node.count()
  , x        :: Number
  , y        :: Number
}

