module D3.Layouts.Sankey.Functions where

import Prelude

import D3.Data.Types (Datum_)
import D3.Layouts.Sankey.Types (SankeyLink_, SankeyNode_)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

-- | Apply the Sankey layout algorithm to raw data
-- | This calls the D3 Sankey generator which computes node positions and link paths
sankeySetData :: forall m nodeData linkData.
  MonadEffect m =>
  { nodes :: Array nodeData, links :: Array linkData } ->
  Number ->
  Number ->
  m { nodes :: Array SankeyNode_, links :: Array SankeyLink_ }
sankeySetData data_ width height = liftEffect $ sankeySetData_ data_ width height

-- FFI function that will call d3-sankey
foreign import sankeySetData_ :: forall nodeData linkData.
  { nodes :: Array nodeData, links :: Array linkData } ->
  Number ->
  Number ->
  Effect { nodes :: Array SankeyNode_, links :: Array SankeyLink_ }

-- | Generate SVG path data for a Sankey link
-- | This creates the characteristic curved path for flow diagrams
foreign import sankeyLinkPath_ :: Datum_ -> String
