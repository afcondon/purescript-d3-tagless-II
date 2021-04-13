module D3.Layouts.Hierarchical.RadialTree(
    radialTreeConfig
  , radialLink
  , initRadialTree_
) where

import D3.Attributes.Instances (Attribute(..), Datum, toAttr)
import D3.Layouts.Hierarchical.Types 
import D3.Selection (Chainable(..))
import Math (pi)
import Prelude (($), (*), (/))
import Unsafe.Coerce (unsafeCoerce)


radialTreeConfig :: Number -> TreeConfig
radialTreeConfig width = RadialTree
  { size      : [2.0 * pi, width / 2.0]
  , separation: radialSeparationJS_
  }

-- helpers for Radial tree
radialLink :: forall a b. (a -> Number) -> (b -> Number) -> Chainable
radialLink angleFn radius_Fn = do
  let radialFn = linkRadial_ (unsafeCoerce angleFn) (unsafeCoerce radius_Fn)
  AttrT $ Attribute "d" $ toAttr radialFn

foreign import initRadialTree_         :: TreeConfig_ -> D3HierarchicalNode_ -> D3HierarchicalNode_ -- effectful function on hierarchy
foreign import linkRadial_         :: (Datum -> Number) -> (Datum -> Number) -> (Datum -> String)
foreign import radialSeparationJS_ :: Datum -> Datum -> Int
