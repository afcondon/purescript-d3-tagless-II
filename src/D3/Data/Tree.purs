module D3.Data.Tree where

import Prelude

import D3.Data.Foreign (Datum_)
import Data.Array as A
import Data.List (List(..))
import Data.Nullable (Nullable)
import Data.Tree (Tree(..))
import Unsafe.Coerce (unsafeCoerce)

-- these definitions have to be here to avoid cycle (and probably all type defs should in fact be here)
foreign import data TreeJson_           :: Type
foreign import data D3HierarchicalNode_ :: Type

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
-- TODO make this a row type similar to the handling of GraphNode_ and GraphLink_, formalize this pattern
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

-- | Coercion function to recover the structure that was given to D3, it's an unsafeCoerce but the types
-- | give some protection
datumIsTreeNode :: forall d v. Datum_ -> D3HierarchicalNode d v
datumIsTreeNode = unsafeCoerce

-- | Coercion function to recover the "extra" data that lives within the generic structure that was given to D3, 
-- | it's an unsafeCoerce but the types give some protection
labelName :: Datum_ -> String
labelName d = node."data".name
  where (D3HierarchicalNode node) = datumIsTreeNode d


-- | this function is to be used when you have a Tree ID, ie the id is already present for D3
-- | assumes there's no other information in the tree, so you likely just want a tree that can be laid out
-- | in order to get the (x,y), height, depth etc that are initialized by a D3 tree layout
makeD3TreeJSONFromTreeID :: forall id. Tree id -> TreeJson_
makeD3TreeJSONFromTreeID = 
  case _ of
    (Node id Nil) -> idTreeLeaf_ id
    (Node id children) -> idTreeParent_ id (makeD3TreeJSONFromTreeID <$> (A.fromFoldable children))

foreign import idTreeLeaf_   :: forall id. id -> TreeJson_
foreign import idTreeParent_ :: forall id. id -> Array TreeJson_ -> TreeJson_
