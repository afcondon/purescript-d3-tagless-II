module D3.Viz.ForceNavigator.Unsafe where

import PSD3.Internal.Types (Datum_)
import D3.Viz.ForceNavigator.Model (NavigationGraphLinkObj, NavigationSimRecord)
import PSD3.Data.Node (D3LinkSwizzled(..), D3_SimulationNode(..))
import Unsafe.Coerce (unsafeCoerce)

unboxD3SimNode :: Datum_ -> NavigationSimRecord
unboxD3SimNode datum = do
  let (D3SimNode d) = unsafeCoerce datum
  d

unboxD3SimLink :: Datum_ -> NavigationGraphLinkObj
unboxD3SimLink datum = do
  let (D3LinkObj l) = unsafeCoerce datum
  l
