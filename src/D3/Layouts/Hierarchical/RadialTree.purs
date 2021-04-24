module D3.Layouts.Hierarchical.RadialTree(
    radialLink
  , initRadialTree
) where

import D3.Layouts.Hierarchical.Types

import D3.Attributes.Instances (Attribute(..), Datum, toAttr)
import D3.Selection (Chainable(..))
import Data.Tuple (Tuple(..))
import Math (pi)
import Prelude (($), (*), (/))
import Unsafe.Coerce (unsafeCoerce)

initRadialTree :: Tuple Number Number -> D3HierarchicalNode_ -> TreeConfig-- effectful function on hierarchy
initRadialTree (Tuple width _) root = do
  let config =  { size : [2.0 * pi, width / 2.0]
                , separation: radialSeparationJS_
                }
      tree = initRadialTree_ config root
  RadialTree config 

-- helpers for Radial tree
radialLink :: forall a b. (a -> Number) -> (b -> Number) -> Chainable
radialLink angleFn radius_Fn = do
  let radialFn = linkRadial_ (unsafeCoerce angleFn) (unsafeCoerce radius_Fn)
  AttrT $ Attribute "d" $ toAttr radialFn

foreign import initRadialTree_     :: RadialTreeConfig -> D3HierarchicalNode_ -> D3HierarchicalNode_ -- effectful function on hierarchy
foreign import linkRadial_         :: (Datum -> Number) -> (Datum -> Number) -> (Datum -> String)
foreign import radialSeparationJS_ :: Datum -> Datum -> Int
