module D3.Layouts.Hierarchical where

import D3.Attributes.Instances (Attribute(..), Datum, toAttr)
import D3.Selection (Chainable(..), D3Data_)
import Data.Array ((!!))
import Data.Maybe (Maybe, fromMaybe)
import Data.Nullable (Nullable, toMaybe)
import Data.Tuple (Tuple)
import Math (pi)
import Prelude (Unit, unit, ($), (*), (/))
import Unsafe.Coerce (unsafeCoerce)

data Tree a = Node a (Array (Tree a))

-- TODO allow custom separations etc, either thru config with nulls or API
data TreeConfig = RadialTree     { size :: Array Number, separation :: Datum -> Datum -> Int }
                | HorizontalTree { width :: Number, height :: Number }

foreign import data TreeConfig_ :: Type

radialTreeConfig :: Number -> TreeConfig
radialTreeConfig width = RadialTree
  { size      : [2.0 * pi, width / 2.0]
  , separation: radialSeparationJS_
  }

horizontalTreeConfig :: Number -> Number-> TreeConfig
horizontalTreeConfig width height = HorizontalTree { width, height }

type Model :: forall d v. d -> v -> Type
-- d is the type of the datum and v is the type of computed value, ie for summing etc
type Model d v = {
      json   :: TreeJson_
    , root   :: D3HierarchicalNode_
    , config :: TreeConfig
}

-- | code for all the specific trees - Radial, Sideways, TidyTree etc should go here
-- helpers for Radial tree
radialLink :: forall a b. (a -> Number) -> (b -> Number) -> Chainable
radialLink angleFn radius_Fn = do
  let radialFn = linkRadial_ (unsafeCoerce angleFn) (unsafeCoerce radius_Fn)
  AttrT $ Attribute "d" $ toAttr radialFn

horizontalLink :: Chainable
horizontalLink = AttrT $ Attribute "d" $ toAttr linkHorizontal_
  

d3InitTree :: forall d v. TreeConfig -> D3HierarchicalNode_ -> D3HierarchicalNode d v
d3InitTree (RadialTree config)     = do
  let root = initRadialTree_ (unsafeCoerce config)
  unsafeCoerce root -- force the result to conform to our record 

d3InitTree (HorizontalTree config) = do
  let root = initHorizontalTree_ (unsafeCoerce config)
  unsafeCoerce root

-- | foreign functions needed for tree layouts
-- TODO structures here carried over from previous interpreter - review and refactor
-- do the decode on the Purescript side unless files are ginormous, this is just for prototyping
-- this is an opaque type behind which hides the data type of the Purescript tree that was converted
foreign import data RecursiveD3TreeNode :: Type
-- this is the Purescript Tree after processing in JS to remove empty child fields from leaves etc
-- need to ensure that this structure is encapsulated in libraries (ie by moving this code)
foreign import data D3Tree_             :: Type
foreign import data D3SortComparator_   :: Type -- a number such that n < 0 => a > b, n > 0 => b > a, n == 0 undef'd
foreign import data D3HierarchicalNode_ :: Type
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
foreign import data D3Hierarchical_     :: Type
foreign import data TreeJson_           :: Type

find :: D3HierarchicalNode_ -> (Datum -> Boolean) -> Maybe D3HierarchicalNode_
find tree filter = toMaybe $ find_ tree filter


foreign import readJSON_         :: String -> TreeJson_ -- TODO no error handling at all here RN

foreign import hierarchy_          :: TreeJson_ -> D3HierarchicalNode_
foreign import initRadialTree_         :: TreeConfig_ -> D3HierarchicalNode_ -> D3HierarchicalNode_ -- effectful function on hierarchy
foreign import initHorizontalTree_     :: TreeConfig_ -> D3HierarchicalNode_ -> D3HierarchicalNode_ -- effectful function on hierarchy
-- next some functions to make attributes, types are a bit sloppy here
-- TODO tighten this up
foreign import hasChildren_        :: Datum -> Boolean -- really only works on Datum when it's a D3HierarchicalNode_
foreign import linkRadial_         :: (Datum -> Number) -> (Datum -> Number) -> (Datum -> String)
foreign import radialSeparationJS_ :: Datum -> Datum -> Int
foreign import linkHorizontal_     :: (Datum -> String)

-- the full API for hierarchical nodes:
-- TODO implement more of this as needed
foreign import nodeSize_ :: D3HierarchicalNode_ -> Array Number -> D3HierarchicalNode_ -- TODO "returns this tree layout" is that node or tree config?
-- foreign import ancestors_   :: D3HierarchicalNode_ -> D3Data_
foreign import descendants_ :: D3HierarchicalNode_ -> D3Data_
-- foreign import leaves_      :: D3HierarchicalNode_ -> Array D3HierarchicalNode_
foreign import find_        :: D3HierarchicalNode_ -> (Datum -> Boolean) -> Nullable D3HierarchicalNode_
-- foreign import path_        :: D3HierarchicalNode_ -> D3HierarchicalNode_ -> Array D3HierarchicalNode_
foreign import links_       :: D3HierarchicalNode_ -> D3Data_ -- TODO this is actually Array Nodes
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

foreign import horizontalTreeX0X1_ :: D3HierarchicalNode_ -> Array Number
horizontalTreeX0X1 :: D3HierarchicalNode_ -> { x0 :: Number, x1 :: Number }
horizontalTreeX0X1 d = do
  let result = horizontalTreeX0X1_ d
      x0     = fromMaybe 0.0 $ result !! 0
      x1     = fromMaybe 0.0 $ result !! 1
  { x0, x1 }
  