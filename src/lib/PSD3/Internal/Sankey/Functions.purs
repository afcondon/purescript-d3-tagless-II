module PSD3.Internal.Sankey.Functions where

import Prelude

import PSD3.Internal.Types (Datum_)
import PSD3.Internal.Sankey.Types (SankeyConfig, SankeyLink_, SankeyNode_)
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

-- | Apply Sankey layout with custom configuration
sankeySetDataWithConfig :: forall m nodeData linkData.
  MonadEffect m =>
  { nodes :: Array nodeData, links :: Array linkData } ->
  Number ->
  Number ->
  SankeyConfig ->
  m { nodes :: Array SankeyNode_, links :: Array SankeyLink_ }
sankeySetDataWithConfig data_ width height config = liftEffect $ sankeySetDataWithConfig_ data_ width height config

foreign import sankeySetDataWithConfig_ :: forall nodeData linkData.
  { nodes :: Array nodeData, links :: Array linkData } ->
  Number ->
  Number ->
  SankeyConfig ->
  Effect { nodes :: Array SankeyNode_, links :: Array SankeyLink_ }

-- | Generate SVG path data for a Sankey link
-- | This creates the characteristic curved path for flow diagrams
foreign import sankeyLinkPath_ :: Datum_ -> String
