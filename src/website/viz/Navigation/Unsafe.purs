module D3.Examples.Navigation.Unsafe where

import D3.Data.Types (Datum_)
import D3.Examples.Navigation.Model (NavigationGraphLinkObj, NavigationSimRecord)
import D3.Node (D3LinkSwizzled(..), D3_SimulationNode(..))
import Unsafe.Coerce (unsafeCoerce)

unboxD3SimNode :: Datum_ -> NavigationSimRecord
unboxD3SimNode datum = do
  let (D3SimNode d) = unsafeCoerce datum
  d

unboxD3SimLink :: Datum_ -> NavigationGraphLinkObj
unboxD3SimLink datum = do
  let (D3LinkObj l) = unsafeCoerce datum
  l
