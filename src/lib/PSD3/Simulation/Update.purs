module PSD3.Simulation.Update where

import Prelude

import PSD3.Internal.Types (D3Selection_, Datum_, Element, Index_)
import PSD3.Internal.Attributes.Instances (Label)
import PSD3.Internal.Selection.Types (SelectionAttribute)
import PSD3.Internal.Simulation.Types (Step(..))
import PSD3.Capabilities.Selection (class SelectionM, mergeSelections, openSelection, updateJoin)
import PSD3.Capabilities.Simulation (class SimulationM2, SimulationUpdate, addTickFunction, update)
import PSD3.Data.Node (D3Link_Unswizzled, D3_SimulationNode)
import Data.Maybe (Maybe(..))
import Data.Set (Set)

-- | Declarative callbacks for rendering nodes and links
-- |
-- | The library calls these in the correct order to guarantee state integrity.
-- | Visualizations just provide "what to render", not "when to render it".
-- |
-- | ## Node Rendering Flow (guaranteed by library):
-- | 1. onNodeEnter: Populate enter selection (create new DOM elements)
-- |    - Input: Empty selection placeholder
-- |    - Output: Populated selection (with children if needed)
-- | 2. onNodeUpdate: Modify update selection (update existing DOM elements)
-- |    - Input: Selection of existing nodes
-- |    - Output: Unit (modifications via setAttributes)
-- | 3. onNodeExit: Clean up exit selection
-- |    - Usually just: setAttributes sel [ remove ]
-- | 4. Library merges enter+update for tick functions
-- |
-- | ## Link Rendering Flow (same pattern):
-- | 1. onLinkEnter: Create new links
-- | 2. onLinkUpdate: Update existing links
-- | 3. onLinkExit: Remove old links
-- | 4. Library merges for tick functions
-- |
-- | ## Tick Functions:
-- | - nodeTickAttrs: Attributes to apply on each simulation tick for nodes
-- | - linkTickAttrs: Attributes to apply on each simulation tick for links
-- | - Applied to merged selections automatically
type RenderCallbacks attrs sel m = {
  -- Node rendering callbacks
  onNodeEnter :: sel -> attrs -> m sel,   -- Populate enter selection, return it for merging
  onNodeUpdate :: sel -> attrs -> m Unit, -- Update existing nodes in-place
  onNodeExit :: sel -> m Unit,            -- Clean up exiting nodes

  -- Link rendering callbacks
  onLinkEnter :: sel -> attrs -> m sel,   -- Populate enter selection, return it for merging
  onLinkUpdate :: sel -> attrs -> m Unit, -- Update existing links in-place
  onLinkExit :: sel -> m Unit,            -- Clean up exiting links

  -- Tick function attributes (applied to merged selections)
  nodeTickAttrs :: attrs -> Array SelectionAttribute,
  linkTickAttrs :: Array SelectionAttribute
}

-- | Fully generic updateSimulation that guarantees correct ordering
-- |
-- | This function is the core of the library's "impossible to mess up" guarantee.
-- | It controls the entire update flow:
-- |
-- | 1. Call SimulationM2 update API (data merging, swizzling, force engagement)
-- | 2. Open DOM selections
-- | 3. Apply General Update Pattern to nodes (enter/update/exit/merge)
-- | 4. Apply General Update Pattern to links (enter/update/exit/merge)
-- | 5. Register tick functions with merged selections
-- |
-- | The visualization provides callbacks via `RenderCallbacks` but has no control
-- | over ordering or state management. The library guarantees integrity.
-- |
-- | ## Parameters:
-- | - selections: DOM group selections for nodes and links
-- | - nodeElement: String name of node element type (e.g., "circle", "g")
-- | - linkElement: String name of link element type (e.g., "line", "path")
-- | - updateConfig: SimulationM2 update configuration
-- | - keyFn: Key function for data binding
-- | - attrs: Visualization-specific attributes
-- | - callbacks: Render callbacks for enter/update/exit phases
-- |
-- | ## Example Usage:
-- | ```purescript
-- | genericUpdateSimulation
-- |   { nodes: Just nodesGroup, links: Just linksGroup }
-- |   "circle"  -- Node element
-- |   "line"    -- Link element
-- |   { nodes: filteredNodes, links: allLinks, ... }
-- |   keyIsID_
-- |   myAttributes
-- |   { onNodeEnter: \sel attrs -> do
-- |       setAttributes sel [ radius 5.0, fill attrs.color ]
-- |       pure sel
-- |   , onNodeUpdate: \sel attrs -> setAttributes sel [ fill attrs.color ]
-- |   , onNodeExit: \sel -> setAttributes sel [ remove ]
-- |   , ... same for links ...
-- |   , nodeTickAttrs: \attrs -> [ cx datum_.x, cy datum_.y ]
-- |   , linkTickAttrs: [ x1 (_.x <<< link_.source), ... ]
-- |   }
-- | ```
genericUpdateSimulation :: forall d attrs sel m.
  Monad m =>
  SelectionM sel m =>
  SimulationM2 sel m =>
  { nodes :: Maybe sel, links :: Maybe sel } ->
  Element ->                                         -- Node element type
  Element ->                                         -- Link element type
  SimulationUpdate d ->                              -- Update configuration
  (Datum_ -> Index_) ->                              -- Key function
  attrs ->                                           -- Visualization attributes
  RenderCallbacks attrs sel m ->                     -- Render callbacks
  m Unit
genericUpdateSimulation { nodes: Just nodesGroup, links: Just linksGroup }
                        nodeElement linkElement updateConfig keyFn attrs callbacks = do
  -- STEP 1: Call SimulationM2 update API
  -- Handles data merging, link swizzling, force engagement
  enhanced <- update updateConfig

  -- STEP 2: Open selections for DOM operations
  node <- openSelection nodesGroup (show nodeElement)
  link <- openSelection linksGroup (show linkElement)

  -- STEP 3: Apply General Update Pattern to nodes
  node' <- updateJoin node nodeElement enhanced.nodes keyFn

  -- Enter: Create new nodes
  nodeEnter <- callbacks.onNodeEnter node'.enter attrs

  -- Update: Modify existing nodes
  callbacks.onNodeUpdate node'.update attrs

  -- Exit: Remove old nodes
  callbacks.onNodeExit node'.exit

  -- Merge enter and update for tick functions
  mergedNodes <- mergeSelections nodeEnter node'.update

  -- STEP 4: Apply General Update Pattern to links
  link' <- updateJoin link linkElement enhanced.links keyFn

  -- Enter: Create new links
  linkEnter <- callbacks.onLinkEnter link'.enter attrs

  -- Update: Modify existing links
  callbacks.onLinkUpdate link'.update attrs

  -- Exit: Remove old links
  callbacks.onLinkExit link'.exit

  -- Merge enter and update for tick functions
  mergedLinks <- mergeSelections linkEnter link'.update

  -- STEP 5: Register tick functions with merged selections
  -- The library controls when these are called (every tick)
  addTickFunction "nodes" $ Step mergedNodes (callbacks.nodeTickAttrs attrs)
  addTickFunction "links" $ Step mergedLinks callbacks.linkTickAttrs

  pure unit

-- Fallback when selections are missing (do nothing)
genericUpdateSimulation _ _ _ _ _ _ _ = pure unit
