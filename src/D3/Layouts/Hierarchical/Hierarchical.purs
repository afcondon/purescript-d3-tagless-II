module D3.Layouts.Hierarchical where

import Affjax (Error, URL)
import Affjax as AJAX
import Affjax.ResponseFormat as ResponseFormat
import D3.Attributes.Instances (Attribute(..), toAttr)
import D3.Data.Types (Datum_, TreeJson_, TreeLayout, TreeModel, TreeType)
import D3.FFI (find_, getLayout, hNodeDepth_, hierarchyFromJSON_, linkClusterHorizontal_, linkClusterVertical_, linkHorizontal_, linkRadial_, linkVertical_, sharesParent_)
import D3.Node (D3_Hierarchy_Node(..), D3_Hierarchy_Node_, D3_Hierarchy_Node_XY)
import D3.Selection (Chainable(..))
import Data.Bifunctor (rmap)
import Data.Either (Either)
import Data.Function.Uncurried (Fn2, mkFn2)
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Prelude (class Bind, bind, pure, show, ($), (/), (<>))
import Unsafe.Coerce (unsafeCoerce)

find :: forall d. D3_Hierarchy_Node_ d -> (Datum_ -> Boolean) -> Maybe (D3_Hierarchy_Node_ d)
find tree filter = toMaybe $ find_ tree filter

getTreeViaAJAX :: URL -> Aff (Either Error TreeJson_)
getTreeViaAJAX url = do
  result <-AJAX.get ResponseFormat.string url
  pure $ rmap (\{body} -> readJSON_ body) result

makeModel :: forall d.
  Bind Aff => 
  MonadEffect Aff => 
  TreeType -> 
  TreeLayout ->
  TreeJson_ -> 
  Aff (TreeModel d)
makeModel treeType treeLayout json = do
  let 
    root         = hierarchyFromJSON_ json
    -- svgConfig  = { width: fst widthHeight, height: snd widthHeight }
    treeLayoutFn = getLayout treeType -- REVIEW why not run this here and fill in root_ ?
    svgConfig    = { width: 650.0, height: 650.0 }
  pure $ { json, root, root_: Nothing, treeType, treeLayout, treeLayoutFn, svgConfig }

foreign import readJSON_                :: String -> TreeJson_ -- TODO no error handling at all here RN

-- not clear if we really want to write all these in PureScript, there is no Eq instance for parents etc
-- but it will at least serve as documentation
-- OTOH if it can be nicely written here, so much the better as custom separation and all _is_ necessary
defaultSeparation :: forall d r.  Fn2 (D3_Hierarchy_Node d r) (D3_Hierarchy_Node d r) Number
defaultSeparation = mkFn2 (\a b -> if (sharesParent_ a b) 
                                   then 1.0
                                   else 2.0)

radialSeparation :: forall d r. Fn2 (D3_Hierarchy_Node d r) (D3_Hierarchy_Node d r) Number 
radialSeparation  = mkFn2 (\a b -> if (sharesParent_ a b) 
                                   then 1.0 
                                   else 2.0 / (hNodeDepth_ a))

horizontalLink :: Chainable
horizontalLink = AttrT $ ToAttribute "d" $ toAttr linkHorizontal_

verticalLink :: Chainable
verticalLink = AttrT $ ToAttribute "d" $ toAttr linkVertical_

horizontalClusterLink :: Number -> Chainable
horizontalClusterLink yOffset = AttrT $ ToAttribute "d" $ toAttr (linkClusterHorizontal_ yOffset)

verticalClusterLink :: Number -> Chainable
verticalClusterLink xOffset = AttrT $ ToAttribute "d" $ toAttr (linkClusterVertical_ xOffset)

radialLink :: forall a b. (a -> Number) -> (b -> Number) -> Chainable
radialLink angleFn radius_Fn = do
  let radialFn = linkRadial_ (unsafeCoerce angleFn) (unsafeCoerce radius_Fn)
  AttrT $ ToAttribute "d" $ toAttr radialFn

positionXYreflected :: forall d. D3_Hierarchy_Node_XY d -> String
positionXYreflected (D3_Hierarchy_Node d) = "translate(" <> show d.y <> "," <> show d.x <>")"

positionXY :: forall d. D3_Hierarchy_Node_XY d -> String
positionXY (D3_Hierarchy_Node d) = "translate(" <> show d.x <> "," <> show d.y <>")"

