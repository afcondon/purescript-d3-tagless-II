module D3.Layouts.Hierarchical where

import D3.Data.Types
import Prelude

import Affjax (Error, URL)
import Affjax as AJAX
import Affjax.ResponseFormat as ResponseFormat
import D3.Attributes.Instances (Attribute(..), toAttr)
import D3.FFI (find_, hNodeDepth_, hierarchyFromJSON_, linkClusterHorizontal_, linkClusterVertical_, linkHorizontal_, linkRadial_, linkVertical_, sharesParent_)
import D3.Selection (Chainable(..))
import Data.Bifunctor (rmap)
import Data.Either (Either)
import Data.Function.Uncurried (Fn2, mkFn2)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Unsafe.Coerce (unsafeCoerce)

find :: D3HierarchicalNode_ -> (Datum_ -> Boolean) -> Maybe D3HierarchicalNode_
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

foreign import readJSON_                :: String -> TreeJson_ -- TODO no error handling at all here RN

-- not clear if we really want to write all these in PureScript, there is no Eq instance for parents etc
-- but it will at least serve as documentation
-- OTOH if it can be nicely written here, so much the better as custom separation and all _is_ necessary
defaultSeparation :: Fn2 D3HierarchicalNode_ D3HierarchicalNode_ Number
defaultSeparation = mkFn2 (\a b -> if (sharesParent_ a b) then 1.0 else 2.0)

radialSeparation :: Fn2 D3HierarchicalNode_ D3HierarchicalNode_ Number 
radialSeparation  = mkFn2 (\a b -> (if (sharesParent_ a b) then 1.0 else 2.0) / (hNodeDepth_ a))

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

positionXYreflected :: forall d v. D3HierarchicalNode d v -> String
positionXYreflected (D3HierarchicalNode d) = "translate(" <> show d.y <> "," <> show d.x <>")"

positionXY :: forall d v. D3HierarchicalNode d v -> String
positionXY (D3HierarchicalNode d) = "translate(" <> show d.x <> "," <> show d.y <>")"

