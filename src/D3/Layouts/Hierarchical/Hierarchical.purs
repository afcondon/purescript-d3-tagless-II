module D3.Layouts.Hierarchical(
    hasChildren_, hierarchyFromJSON_, readJSON_, links_, descendants_, makeModel
  , getTreeViaAJAX, initTree_, initCluster_, autoBox_
  , treeSetRoot_, treeSetNodeSize_, treeSetSeparation_, treeMinMax_, treeSetSize_
  , defaultSeparation, radialSeparation, positionXY, positionXYreflected
  , horizontalLink, radialLink, verticalLink, horizontalClusterLink, verticalClusterLink
  , D3TreeLike_
  , module D3.Layouts.Hierarchical.Types
) where

import Prelude

import Affjax (Error, URL)
import Affjax as AJAX
import Affjax.ResponseFormat as ResponseFormat
import D3.Attributes.Instances (Attribute(..), Datum, toAttr)
import D3.Layouts.Hierarchical.Types (D3HierarchicalNode(..), D3HierarchicalNode_, Model, Tree(..), TreeJson_, TreeLayout, TreeType, hNodeDepth_, hNodeHeight_, hNodeX_, hNodeY_)
import D3.Selection (Chainable(..), D3Data_)
import Data.Bifunctor (rmap)
import Data.Either (Either)
import Data.Function.Uncurried (Fn2, mkFn2)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Unsafe.Coerce (unsafeCoerce)

find :: D3HierarchicalNode_ -> (Datum -> Boolean) -> Maybe D3HierarchicalNode_
find tree filter = toMaybe $ find_ tree filter

getTreeViaAJAX :: URL -> Aff (Either Error TreeJson_)
getTreeViaAJAX url = do
  result <-AJAX.get ResponseFormat.string url
  pure $ rmap (\{body} -> readJSON_ body) result

makeModel :: forall d v.
  Bind Aff => 
  MonadEffect Aff => 
  TreeType -> 
  TreeLayout ->
  TreeJson_ -> 
  Aff (Model d v)
makeModel treeType treeLayout json = do
  let 
    root_      = hierarchyFromJSON_ json
    -- svgConfig  = { width: fst widthHeight, height: snd widthHeight }
    svgConfig  = { width: 650.0, height: 650.0 }
    root       = D3HierarchicalNode (unsafeCoerce root_)
  pure $ { json, root, root_, treeType, treeLayout, svgConfig }


-- | foreign functions needed for tree layouts
-- TODO structures here carried over from previous interpreter - review and refactor
-- do the decode on the Purescript side unless files are ginormous, this is just for prototyping
-- this is an opaque type behind which hides the data type of the Purescript tree that was converted
foreign import data RecursiveD3TreeNode :: Type
-- this is the Purescript Tree after processing in JS to remove empty child fields from leaves etc
-- need to ensure that this structure is encapsulated in libraries (ie by moving this code)
foreign import data D3TreeLike_         :: Type -- covers both trees and clusters
foreign import data D3SortComparator_   :: Type -- a number such that n < 0 => a > b, n > 0 => b > a, n == 0 undef'd
foreign import data D3Hierarchical_     :: Type

foreign import readJSON_                :: String -> TreeJson_ -- TODO no error handling at all here RN

foreign import hierarchyFromJSON_       :: TreeJson_ -> D3HierarchicalNode_
-- next some functions to make attributes, types are a bit sloppy here
-- TODO tighten this up
foreign import hasChildren_             :: Datum -> Boolean -- really only works on Datum when it's a D3HierarchicalNode_

-- the full API for hierarchical nodes:
foreign import descendants_     :: D3HierarchicalNode_ -> Array D3Data_
foreign import find_            :: D3HierarchicalNode_ -> (Datum -> Boolean) -> Nullable D3HierarchicalNode_
foreign import links_           :: D3HierarchicalNode_ -> Array D3Data_ -- TODO this is actually Array Nodes
-- TODO implement the following as well
-- foreign import ancestors_    :: D3HierarchicalNode_ -> D3Data_
-- foreign import leaves_       :: D3HierarchicalNode_ -> Array D3HierarchicalNode_
-- foreign import path_         :: D3HierarchicalNode_ -> D3HierarchicalNode_ -> Array D3HierarchicalNode_

-- TODO there's very likely some confusion here with foreign types D3TreeLike_ and D3HierarchicalNode_
foreign import initTree_        :: Unit -> D3TreeLike_
foreign import initCluster_     :: Unit -> D3TreeLike_
foreign import initRadial_      :: Unit -> D3TreeLike_
foreign import treeSetRoot_     :: D3TreeLike_ -> D3HierarchicalNode_ -> D3HierarchicalNode_
foreign import treeSetSize_     :: D3TreeLike_ -> Array Number -> D3TreeLike_
foreign import treeSetNodeSize_ :: D3TreeLike_ -> Array Number -> D3TreeLike_
foreign import treeMinMax_      :: D3HierarchicalNode_ -> { xMin :: Number, xMax :: Number, yMin :: Number, yMax :: Number }
foreign import treeSetSeparation_ :: D3TreeLike_ -> (Fn2 D3HierarchicalNode_ D3HierarchicalNode_ Number) -> D3TreeLike_
-- foreign import sum_          :: D3HierarchicalNode_ -> (Datum -> Number) -> D3HierarchicalNode_ -- alters the tree!!!!
-- from docs:  <<if you only want leaf nodes to have internal value, then return zero for any node with children. 
-- For example, as an alternative to node.count:
--        root.sum(function(d) { return d.value ? 1 : 0; });
-- foreign import count_ :: D3HierarchicalNode_ -> D3HierarchicalNode_ -- NB alters the tree!!!
-- foreign import sort_ :: D3HierarchicalNode_ -> (D3HierarchicalNode_ -> D3HierarchicalNode_ -> D3SortComparator_)
-- foreign import each_ -- breadth first traversal
-- foreign import eachAfter_ 
-- foreign import eachBefore_
-- foreign import deepCopy_ -- copies (sub)tree but shares data with clone !!!

-- not clear if we really want to write all these in PureScript, there is no Eq instance for parents etc
-- but it will at least serve as documentation
-- OTOH if it can be nicely written here, so much the better as custom separation and all _is_ necessary
defaultSeparation :: Fn2 D3HierarchicalNode_ D3HierarchicalNode_ Number
defaultSeparation = mkFn2 (\a b -> if (sharesParent a b) then 1.0 else 2.0)

radialSeparation :: Fn2 D3HierarchicalNode_ D3HierarchicalNode_ Number 
radialSeparation  = mkFn2 (\a b -> (if (sharesParent a b) then 1.0 else 2.0) / (hNodeDepth_ a))

foreign import sharesParent :: D3HierarchicalNode_ -> D3HierarchicalNode_ -> Boolean

foreign import linkHorizontal_     :: (Datum -> String) 
horizontalLink :: Chainable
horizontalLink = AttrT $ Attribute "d" $ toAttr linkHorizontal_

foreign import linkVertical_     :: (Datum -> String) 
verticalLink :: Chainable
verticalLink = AttrT $ Attribute "d" $ toAttr linkVertical_

foreign import linkClusterHorizontal_ :: Number -> (Datum -> String) 
horizontalClusterLink :: Number -> Chainable
horizontalClusterLink yOffset = AttrT $ Attribute "d" $ toAttr (linkClusterHorizontal_ yOffset)

foreign import linkClusterVertical_ :: Number -> (Datum -> String) 
verticalClusterLink :: Number -> Chainable
verticalClusterLink xOffset = AttrT $ Attribute "d" $ toAttr (linkClusterVertical_ xOffset)

foreign import linkRadial_         :: (Datum -> Number) -> (Datum -> Number) -> (Datum -> String)
radialLink :: forall a b. (a -> Number) -> (b -> Number) -> Chainable
radialLink angleFn radius_Fn = do
  let radialFn = linkRadial_ (unsafeCoerce angleFn) (unsafeCoerce radius_Fn)
  AttrT $ Attribute "d" $ toAttr radialFn

positionXYreflected :: forall d v. D3HierarchicalNode d v -> String
positionXYreflected (D3HierarchicalNode d) = "translate(" <> show d.y <> "," <> show d.x <>")"

positionXY :: forall d v. D3HierarchicalNode d v -> String
positionXY (D3HierarchicalNode d) = "translate(" <> show d.x <> "," <> show d.y <>")"

foreign import autoBox_ :: Datum -> Array Number