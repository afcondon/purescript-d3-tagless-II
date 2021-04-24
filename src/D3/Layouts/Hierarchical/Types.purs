module D3.Layouts.Hierarchical.Types where

import D3.Attributes.Instances (Datum)
import Data.Nullable (Nullable)

foreign import data TreeJson_           :: Type
foreign import data D3HierarchicalNode_ :: Type

data Tree a = Node a (Array (Tree a))

data TreeConfig = RadialTree        RadialTreeConfig
                | HorizontalTree    HorizontalTreeConfig
                | HorizontalCluster HorizontalClusterConfig

-- bundles up the things that would be "in scope" for a D3 script rendering a Radial tree
type RadialTreeConfig = { 
    size       :: Array Number
  , separation :: Datum -> Datum -> Int
  }

-- bundles up the things that would be "in scope" for a D3 script rendering a Horizontal tree
type HorizontalTreeConfig = {
    rootDx :: Number
  , rootDy :: Number
  , x0     :: Number
  , x1     :: Number
}

-- bundles up the things that would be "in scope" for a D3 script rendering a Horizontal tree
type HorizontalClusterConfig = {
    rootDx :: Number
  , rootDy :: Number
}

-- TODO put in proxy fields here to carry the type allowing safe coerce of root etc
-- TODO need to define a model here that works for all hierarchic layouts, this has its origins in Radial tree only
-- d is the type of the datum and v is the type of computed value, ie for summing etc
-- type Model :: forall d v. d -> v -> Type
type Model d v = {
      json       :: TreeJson_
    , root       :: D3HierarchicalNode d v
    , root_      :: D3HierarchicalNode_
    , treeConfig :: TreeConfig
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

-- accessors for fields of D3HierarchicalNode
foreign import hNodeDepth_  :: D3HierarchicalNode_ -> Number
foreign import hNodeHeight_ :: D3HierarchicalNode_ -> Number
foreign import hNodeX_      :: D3HierarchicalNode_ -> Number
foreign import hNodeY_      :: D3HierarchicalNode_ -> Number
