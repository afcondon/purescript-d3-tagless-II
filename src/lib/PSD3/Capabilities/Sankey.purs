module PSD3.Capabilities.Sankey where

import PSD3.Capabilities.Selection (class SelectionM)
import PSD3.Internal.Sankey.Types (SankeyConfig, SankeyLink_, SankeyNode_)
import Prelude (class Monad)

-- | SankeyM capability extends SelectionM with Sankey layout operations
-- | Unlike SimulationM which manages dynamic forces, SankeyM computes a static layout
class (Monad m, SelectionM selection m) <= SankeyM selection m | m -> selection where
  -- | Apply Sankey layout algorithm to raw data
  -- | Takes nodes and links, returns layout-computed nodes and links with positions
  setSankeyData :: forall nodeData linkData.
    { nodes :: Array nodeData, links :: Array linkData } ->
    Number -> -- width
    Number -> -- height
    m { nodes :: Array SankeyNode_, links :: Array SankeyLink_ }

  -- | Apply Sankey layout with custom configuration
  setSankeyDataWithConfig :: forall nodeData linkData.
    { nodes :: Array nodeData, links :: Array linkData } ->
    Number -> -- width
    Number -> -- height
    SankeyConfig ->
    m { nodes :: Array SankeyNode_, links :: Array SankeyLink_ }
