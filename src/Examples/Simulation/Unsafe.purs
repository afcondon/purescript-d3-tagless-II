module D3.Examples.LesMis.Unsafe where


import D3.Data.Types (Datum_)
import D3.Examples.LesMiserables.Types (LesMisGraphLinkObj, LesMisSimRecord)
import D3.Node (D3_Link(..), D3_SimulationNode(..))
import Unsafe.Coerce (unsafeCoerce)

unboxD3SimNode :: Datum_ -> LesMisSimRecord
unboxD3SimNode datum = do
  let (D3SimNode d) = unsafeCoerce datum
  d

unboxD3SimLink :: Datum_ -> LesMisGraphLinkObj
unboxD3SimLink datum = do
  let (D3_Link l) = unsafeCoerce datum
  l
