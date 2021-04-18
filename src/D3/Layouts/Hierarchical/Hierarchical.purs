module D3.Layouts.Hierarchical(
    hasChildren_, hierarchy_, nodeSize_, readJSON_, links_, descendants_
  , module D3.Layouts.Hierarchical.Types
  , module D3.Layouts.Hierarchical.HorizontalTree
  , module D3.Layouts.Hierarchical.RadialTree
) where

import D3.Attributes.Instances (Datum)
import D3.Layouts.Hierarchical.HorizontalTree (horizontalLink, horizontalTreeX0X1, horizontalTreeX0X1_, initHorizontalTree, initHorizontalTree_, linkHorizontal_)
import D3.Layouts.Hierarchical.RadialTree (initRadialTree, radialLink)
import D3.Layouts.Hierarchical.Types (D3HierarchicalNode(..), D3HierarchicalNode_, HorizontalTreeConfig, Model, RadialTreeConfig, Tree(..), TreeConfig(..), TreeJson_, hNodeDepth_, hNodeHeight_, hNodeX_, hNodeY_)
import D3.Selection (D3Data_)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Prelude (($))

find :: D3HierarchicalNode_ -> (Datum -> Boolean) -> Maybe D3HierarchicalNode_
find tree filter = toMaybe $ find_ tree filter

-- | foreign functions needed for tree layouts
-- TODO structures here carried over from previous interpreter - review and refactor
-- do the decode on the Purescript side unless files are ginormous, this is just for prototyping
-- this is an opaque type behind which hides the data type of the Purescript tree that was converted
foreign import data RecursiveD3TreeNode :: Type
-- this is the Purescript Tree after processing in JS to remove empty child fields from leaves etc
-- need to ensure that this structure is encapsulated in libraries (ie by moving this code)
foreign import data D3Tree_             :: Type
foreign import data D3SortComparator_   :: Type -- a number such that n < 0 => a > b, n > 0 => b > a, n == 0 undef'd
foreign import data D3Hierarchical_     :: Type

foreign import readJSON_                :: String -> TreeJson_ -- TODO no error handling at all here RN

foreign import hierarchy_               :: TreeJson_ -> D3HierarchicalNode_
-- next some functions to make attributes, types are a bit sloppy here
-- TODO tighten this up
foreign import hasChildren_             :: Datum -> Boolean -- really only works on Datum when it's a D3HierarchicalNode_

-- the full API for hierarchical nodes:
-- TODO implement more of this as needed
foreign import nodeSize_ :: D3HierarchicalNode_ -> Array Number -> D3HierarchicalNode_ -- TODO "returns this tree layout" is that node or tree config?
-- foreign import ancestors_   :: D3HierarchicalNode_ -> D3Data_
foreign import descendants_ :: D3HierarchicalNode_ -> Array D3Data_
-- foreign import leaves_      :: D3HierarchicalNode_ -> Array D3HierarchicalNode_
foreign import find_        :: D3HierarchicalNode_ -> (Datum -> Boolean) -> Nullable D3HierarchicalNode_
-- foreign import path_        :: D3HierarchicalNode_ -> D3HierarchicalNode_ -> Array D3HierarchicalNode_
foreign import links_       :: D3HierarchicalNode_ -> Array D3Data_ -- TODO this is actually Array Nodes
-- foreign import sum_ :: D3HierarchicalNode_ -> (Datum -> Number) -> D3HierarchicalNode_ -- alters the tree!!!!
-- from docs:  <<if you only want leaf nodes to have internal value, then return zero for any node with children. 
-- For example, as an alternative to node.count:
--        root.sum(function(d) { return d.value ? 1 : 0; });
-- foreign import count_ :: D3HierarchicalNode_ -> D3HierarchicalNode_ -- NB alters the tree!!!
-- foreign import sort_ :: D3HierarchicalNode_ -> (D3HierarchicalNode_ -> D3HierarchicalNode_ -> D3SortComparator_)
-- foreign import each_ -- breadth first traversal
-- foreign import eachAfter_ 
-- foreign import eachBefore_
-- foreign import deepCopy_ -- copies (sub)tree but shares data with clone !!!
