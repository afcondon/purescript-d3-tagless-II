module PSD3.ForceNavigator.Actions where

import D3.Viz.ForceNavigator.Model (NodeType)

-- | Simple actions for the force navigator
data Action
  = Initialize
  | Finalize
  | EventFromVizualization VizEvent

-- | Events that originate from D3 visualization (uses String IDs for semantic navigation)
data VizEvent = NodeClick NodeType String
