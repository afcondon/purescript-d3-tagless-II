module D3.Viz.Sankey.Unsafe where

import PSD3.Internal.Types (Datum_)
import PSD3.Layout.Sankey.Types (SankeyLink, SankeyNode)
import Unsafe.Coerce (unsafeCoerce)

-- Unsafe unboxing of Sankey data from D3's opaque Datum_ type
-- This is needed because D3 callbacks lose type information
-- But we know what we put in, so we can safely coerce it back

unboxSankeyNode :: Datum_ -> SankeyNode
unboxSankeyNode = unsafeCoerce

unboxSankeyLink :: Datum_ -> SankeyLink
unboxSankeyLink = unsafeCoerce
