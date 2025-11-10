module PSD3.Internal.Hierarchical where

import PSD3.Data.Node

import Affjax.Web (Error, URL)
import Affjax.Web as AJAX
import Affjax.ResponseFormat as ResponseFormat
import PSD3.Internal.Attributes.Instances (AttributeSetter(..), toAttr)
import PSD3.Data.Tree (TreeJson_, TreeLayout, TreeModel, TreeType)
import PSD3.Internal.Types (Datum_)
import PSD3.Internal.FFI (find_, getLayout, hNodeDepth_, linkClusterHorizontal_, linkClusterVertical_, linkHorizontal2_, linkHorizontal_, linkRadial_, linkVertical_, sharesParent_)
import PSD3.Internal.Selection.Types (SelectionAttribute(..))
import Data.Bifunctor (rmap)
import Data.Either (Either)
import Data.Function.Uncurried (Fn2, mkFn2)
import Data.Maybe (Maybe)
import Data.Nullable (toMaybe)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Prelude (class Bind, bind, pure, ($), (/))
import Unsafe.Coerce (unsafeCoerce)

find :: forall d. D3_TreeNode d -> (Datum_ -> Boolean) -> Maybe (D3_TreeNode d)
find tree filter = toMaybe $ find_ tree filter

getTreeViaAJAX :: URL -> Aff (Either Error TreeJson_)
getTreeViaAJAX url = do
  result <- AJAX.get ResponseFormat.string url
  pure $ rmap (\{body} -> readJSON_ body) result  

makeModel :: Bind Aff => 
  MonadEffect Aff => 
  TreeType -> 
  TreeLayout ->
  TreeJson_ -> 
  Aff TreeModel
makeModel treeType treeLayout json = do
  let 
    -- svgConfig  = { width: fst widthHeight, height: snd widthHeight }
    treeLayoutFn = getLayout treeType -- REVIEW why not run this here and fill in root_ ?
    svgConfig    = { width: 650.0, height: 650.0 }
  pure $ { json, treeType, treeLayout, treeLayoutFn, svgConfig }

foreign import readJSON_                :: String -> TreeJson_ -- TODO no error handling at all here RN

-- not clear if we really want to write all these in PureScript, there is no Eq instance for parents etc
-- but it will at least serve as documentation
-- OTOH if it can be nicely written here, so much the better as custom separation and all _is_ necessary
defaultSeparation :: forall d. Fn2 (D3_TreeNode d) (D3_TreeNode d) Number
defaultSeparation = mkFn2 (\a b -> if (sharesParent_ a b) 
                                   then 1.0
                                   else 2.0)

radialSeparation :: forall r. Fn2 (D3_TreeNode r) (D3_TreeNode r) Number 
radialSeparation  = mkFn2 (\a b -> if (sharesParent_ a b) 
                                   then 1.0 
                                   else 2.0 / (hNodeDepth_ a))

horizontalLink :: forall d. SelectionAttribute d
horizontalLink = AttrT $ AttributeSetter "d" $ toAttr linkHorizontal_

-- version for when the x and y point are already swapped
-- should be default someday
horizontalLink' :: forall d. SelectionAttribute d
horizontalLink' = AttrT $ AttributeSetter "d" $ toAttr linkHorizontal2_

verticalLink :: forall d. SelectionAttribute d
verticalLink = AttrT $ AttributeSetter "d" $ toAttr (unsafeCoerce linkVertical_ :: d -> String)

horizontalClusterLink :: forall d. Number -> SelectionAttribute d
horizontalClusterLink yOffset = AttrT $ AttributeSetter "d" $ toAttr (linkClusterHorizontal_ yOffset)

verticalClusterLink :: forall d. Number -> SelectionAttribute d
verticalClusterLink xOffset = AttrT $ AttributeSetter "d" $ toAttr (unsafeCoerce (linkClusterVertical_ xOffset) :: d -> String)

radialLink :: forall d. (Datum_ -> Number) -> (Datum_ -> Number) -> SelectionAttribute d
radialLink angleFn radius_Fn = do
  let radialFn = linkRadial_ angleFn radius_Fn
  AttrT $ AttributeSetter "d" $ toAttr radialFn



