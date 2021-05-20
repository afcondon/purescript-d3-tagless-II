module D3.Data.Tree where

import D3.Node (NodeID)
import Data.Array as A
import Data.List (List(..))
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tree (Tree(..))
import Prelude (class Eq, (<$>), ($))

-- TODO can this move to Node.purs ??
-- these definitions have to be here to avoid cycle (and probably all type defs should in fact be here)
foreign import data TreeJson_ :: Type
foreign import emptyTreeJson_ :: TreeJson_

data TreeType   = TidyTree | Dendrogram
derive instance eqTreeType :: Eq TreeType

data TreeLayout = Radial | Horizontal | Vertical
derive instance eqTreeLayout :: Eq TreeLayout

type TreeModel = {
      json         :: TreeJson_                      -- data from file
    , treeType     :: TreeType
    , treeLayout   :: TreeLayout
    , treeLayoutFn :: TreeLayoutFn_
    , svgConfig    :: { width :: Number, height :: Number }
}

foreign import data TreeLayoutFn_ :: Type

-- | this function is to be used when you have a Tree ID, ie the id is already present for D3
-- | so you likely just want a tree that can be laid out
-- | in order to get the (x,y), height, depth etc that are initialized by a D3 tree layout
-- | it does copy the name over because actually that is going to be needed for sorting in order
-- | to make a tidy tree (radial in our spago example)
makeD3TreeJSONFromTreeID :: forall d. Tree NodeID -> M.Map NodeID d -> TreeJson_
makeD3TreeJSONFromTreeID root nodesMap = go root
  where 
    go (Node id children)      = 
      case M.lookup id nodesMap of
        Nothing    -> emptyTreeJson_ -- TODO think of a more principled way to handle this
        (Just obj) -> case children of
                        Nil -> idTreeLeaf_ obj
                        _   -> idTreeParent_ obj (go <$> (A.fromFoldable children))

foreign import idTreeLeaf_   :: forall d. d -> TreeJson_
foreign import idTreeParent_ :: forall d. d -> Array TreeJson_ -> TreeJson_

