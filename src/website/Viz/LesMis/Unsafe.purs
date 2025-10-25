module D3.Viz.LesMis.Unsafe where


import PSD3.Internal.Types (Datum_)
import D3.Viz.LesMiserables.Model (LesMisGraphLinkObj, LesMisSimRecord)
import PSD3.Data.Node (D3LinkSwizzled(..), D3_SimulationNode(..))
import Unsafe.Coerce (unsafeCoerce)

unboxD3SimNode :: Datum_ -> LesMisSimRecord
unboxD3SimNode datum = do
  let (D3SimNode d) = unsafeCoerce datum
  d

unboxD3SimLink :: Datum_ -> LesMisGraphLinkObj
unboxD3SimLink datum = do
  let (D3LinkObj l) = unsafeCoerce datum
  l
