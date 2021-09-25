module D3.Examples.Spago.Unsafe where

import Prelude

import D3.Data.Types (Datum_, Index_)
import D3.Examples.Spago.Files (SpagoDataRecord, SpagoLinkData, SpagoTreeObj)
import D3.Node (D3LinkDatum, D3LinkSwizzled(..), D3_SimulationNode(..), D3_TreeNode(..))
import Unsafe.Coerce (unsafeCoerce)

spagoNodeKeyFunction :: Datum_ -> Index_
spagoNodeKeyFunction d = index
  where
    index = unsafeCoerce $ (unboxD3SimNode d).id

coerceToIndex_ :: forall a. (Ord a) => a -> Index_
coerceToIndex_ = unsafeCoerce

unboxD3SimNode :: Datum_ -> SpagoDataRecord
unboxD3SimNode datum = d
  where (D3SimNode d) = unsafeCoerce datum
  
unboxD3SimLink :: Datum_ -> D3LinkDatum SpagoDataRecord SpagoLinkData
unboxD3SimLink datum = l
  where
    (D3LinkObj l) = unsafeCoerce datum

-- ======================================================================
-- unboxD3TreeNode :: Datum_ -> SpagoDataRecord -- + Children / Parent
-- ======================================================================
unboxD3TreeNode datum = t
  where
    (t' :: SpagoTreeObj )  = unsafeCoerce datum
    (D3TreeNode t) = t'
