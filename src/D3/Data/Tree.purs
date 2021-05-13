module D3.Data.Tree where

import Prelude

import D3.Data.Foreign (Datum_)
import D3.Node (D3_Hierarchy_Node(..), D3_Hierarchy_Node_XY, D3_Hierarchy_Node_)
import Data.Array as A
import Data.List (List(..))
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Tree (Tree(..))
import Unsafe.Coerce (unsafeCoerce)

-- these definitions have to be here to avoid cycle (and probably all type defs should in fact be here)
foreign import data TreeJson_ :: Type

data TreeType   = TidyTree | Dendrogram
derive instance eqTreeType :: Eq TreeType

data TreeLayout = Radial | Horizontal | Vertical
derive instance eqTreeLayout :: Eq TreeLayout

type TreeModel d = {
      json         :: TreeJson_                      -- data from file
    , root         :: D3_Hierarchy_Node_ d           -- tree after initialization by d3.hierarchy
    , root_        :: Maybe (D3_Hierarchy_Node_XY d) -- tree after passing thru a layout algorithm
    , treeType     :: TreeType
    , treeLayout   :: TreeLayout
    , treeLayoutFn :: TreeLayoutFn_
    , svgConfig    :: { width :: Number, height :: Number }
}

foreign import data TreeLayoutFn_ :: Type

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
