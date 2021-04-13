module D3.Layouts.Hierarchical.Types where

import D3.Attributes.Instances (Datum)
import Data.Nullable (Nullable)

data Tree a = Node a (Array (Tree a))

-- TODO allow custom separations etc, either thru config with nulls or API
data TreeConfig = RadialTree     { size :: Array Number, separation :: Datum -> Datum -> Int }
                | HorizontalTree { width :: Number, height :: Number }

foreign import data TreeConfig_ :: Type
foreign import data TreeJson_           :: Type
foreign import data D3HierarchicalNode_ :: Type

-- TODO need to define a model here that works for all hierarchic layouts, this has its origins in Radial tree only
type Model :: forall d v. d -> v -> Type
-- d is the type of the datum and v is the type of computed value, ie for summing etc
type Model d v = {
      json   :: TreeJson_
    , root   :: D3HierarchicalNode_
    , config :: TreeConfig
}

newtype D3HierarchicalNode a b = D3HierarchicalNode { -- the PureScript rep of opaque type D3HierarchicalNode_
    "data"   :: a -- the data that is passed in to the tree
  , depth    :: Int
  , height   :: Int
  , parent   :: Nullable (D3HierarchicalNode a b)
  , children :: Array (D3HierarchicalNode a b)
  , value    :: b -- set by some function passed to node.value() or by node.count()
  , x        :: Number
  , y        :: Number
}
