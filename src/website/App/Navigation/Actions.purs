module PSD3.Apps.Navigation.Actions where

import Prelude

-- | Events that come from the visualization (clicks on nodes)
data VizEvent
  = NodeClick String  -- Click on a node with this ID

derive instance eqVizEvent :: Eq VizEvent

-- | Actions that can be performed in the component
data Action
  = Initialize                    -- Initialize the visualization
  | Finalize                      -- Clean up
  | EventFromVizualization VizEvent  -- Event from D3 visualization
  | ToggleExpansion String        -- Toggle expansion of a section node
  | NavigateToExample String      -- Navigate to an example page
  | NavigateToUrl String Boolean  -- Navigate to URL (external flag)
