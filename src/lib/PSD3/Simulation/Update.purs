module PSD3.Simulation.Update where

import Prelude

import PSD3.Internal.Types (D3Selection_, Datum_, Element, Index_)
import PSD3.Internal.Attributes.Instances (Label)
import PSD3.Internal.Selection.Types (SelectionAttribute)
import PSD3.Internal.Simulation.Types (Step(..))
import PSD3.Capabilities.Selection (class SelectionM, mergeSelections, openSelection, updateJoin)
import PSD3.Capabilities.Simulation (class SimulationM2, SimulationUpdate, addTickFunction, update)
import PSD3.Data.Node (D3Link_Unswizzled, D3_SimulationNode, NodeID)
import PSD3.Internal.FFI (SimulationVariables, getIDsFromNodes_, getLinkIDs_, keyIsID_)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Set (Set)
import Data.Array (filter, elem)
import Data.Foldable (foldl)

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

-- | Fully declarative update configuration
-- |
-- | This is the "impossible to mess up" API design.
-- | Users provide FULL datasets and a predicate - the library handles ALL filtering.
-- |
-- | KEY INSIGHT: By requiring full datasets and only a node filter predicate,
-- | the library can AUTOMATICALLY filter links to ensure consistency.
-- | Users can never accidentally provide nodes without matching links.
-- |
-- | ## Parameters:
-- | - allNodes: Complete node dataset (library will filter it)
-- | - allLinks: Complete link dataset (library will filter it AUTOMATICALLY)
-- | - nodeFilter: Which nodes to show (single source of truth)
-- | - activeForces: Which forces to enable
-- | - config: Optional simulation parameters to update
-- |
-- | ## What the library does automatically:
-- | 1. Apply nodeFilter to allNodes
-- | 2. Run nodeInitializers on filtered nodes (e.g., tree layout, pinning)
-- | 3. Extract IDs from initialized nodes
-- | 4. Filter allLinks to ONLY include links between visible nodes (AUTOMATIC - prevents bug!)
-- | 5. Apply optional linkFilter for visual/semantic filtering (e.g., hide dev dependencies)
-- | 6. Update simulation with consistent data
-- |
-- | This makes the "nodes filtered but links not" bug IMPOSSIBLE.
-- | The linkFilter is an optional ADDITIONAL filter for visual purposes.
type DeclarativeUpdateConfig d =
  { allNodes :: Array (D3_SimulationNode d)                                -- FULL dataset (required)
  , allLinks :: Array D3Link_Unswizzled                                    -- FULL dataset (required)
  , nodeFilter :: D3_SimulationNode d -> Boolean                           -- Which nodes to show (required)
  , linkFilter :: Maybe (D3Link_Unswizzled -> Boolean)                     -- Optional visual filtering (applied AFTER automatic structural filtering)
  , nodeInitializers :: Array (Array (D3_SimulationNode d) -> Array (D3_SimulationNode d))  -- Functions to transform filtered nodes (e.g., tree layout, grid, pinning)
  , activeForces :: Maybe (Set Label)                                      -- Which forces to enable
  , config :: Maybe SimulationVariables                                    -- Simulation config
  }

-- | Fully generic updateSimulation with declarative, impossible-to-mess-up API
-- |
-- | This function is the core of the library's "impossible to mess up" guarantee.
-- | It controls the entire update flow:
-- |
-- | 1. Apply node filter to full dataset
-- | 2. Run node initializers on filtered nodes (tree layout, grid, pinning, etc.)
-- | 3. Extract node IDs from initialized nodes
-- | 4. AUTOMATICALLY filter links structurally to match visible nodes (prevents bug!)
-- | 5. Apply optional visual/semantic link filter (e.g., hide dev dependencies)
-- | 6. Build internal update config with filtered & initialized data
-- | 7. Call SimulationM2 update API (data merging, swizzling, force engagement)
-- | 8. Open DOM selections
-- | 9. Apply General Update Pattern to nodes (enter/update/exit/merge)
-- | 10. Apply General Update Pattern to links (enter/update/exit/merge)
-- | 11. Register tick functions with merged selections
-- |
-- | The visualization provides callbacks via `RenderCallbacks` but has no control
-- | over ordering, filtering consistency, or state management. The library guarantees integrity.
-- |
-- | ## Parameters:
-- | - selections: DOM group selections for nodes and links
-- | - nodeElement: Element type for nodes (e.g., Circle, Group)
-- | - linkElement: Element type for links (e.g., Line, Path)
-- | - config: Declarative configuration (full datasets + node filter predicate)
-- | - keyFn: Key function for data binding
-- | - attrs: Visualization-specific attributes
-- | - callbacks: Render callbacks for enter/update/exit phases
-- |
-- | ## Example Usage:
-- | ```purescript
-- | genericUpdateSimulation
-- |   { nodes: Just nodesGroup, links: Just linksGroup }
-- |   Circle  -- Node element
-- |   Line    -- Link element
-- |   { allNodes: model.nodes                      -- FULL dataset
-- |   , allLinks: model.links                      -- FULL dataset
-- |   , nodeFilter: \(D3SimNode n) ->              -- Single predicate
-- |       Set.member n.group visibleGroups
-- |   , activeForces: Just activeForces
-- |   , config: Nothing
-- |   }
-- |   keyIsID_
-- |   myAttributes
-- |   { onNodeEnter: \sel attrs -> ...
-- |   , onNodeUpdate: \sel attrs -> ...
-- |   , onNodeExit: \sel -> ...
-- |   , onLinkEnter: \sel attrs -> ...
-- |   , onLinkUpdate: \sel attrs -> ...
-- |   , onLinkExit: \sel -> ...
-- |   , nodeTickAttrs: \attrs -> [ cx datum_.x, cy datum_.y ]
-- |   , linkTickAttrs: [ x1 (_.x <<< link_.source), ... ]
-- |   }
-- | ```
-- |
-- | Note: The user NEVER manually filters links. The library does it automatically
-- | by extracting node IDs and filtering links to only connect visible nodes.
-- | This makes the "filtered nodes + all links" bug impossible.
genericUpdateSimulation :: forall d attrs sel m.
  Monad m =>
  SelectionM sel m =>
  SimulationM2 sel m =>
  { nodes :: Maybe sel, links :: Maybe sel } ->
  Element ->                                         -- Node element type
  Element ->                                         -- Link element type
  DeclarativeUpdateConfig d ->                       -- Declarative configuration
  (Datum_ -> Index_) ->                              -- Key function
  attrs ->                                           -- Visualization attributes
  RenderCallbacks attrs sel m ->                     -- Render callbacks
  m Unit
genericUpdateSimulation { nodes: Just nodesGroup, links: Just linksGroup }
                        nodeElement linkElement config keyFn attrs callbacks = do
  -- STEP 1: Apply node filter to full dataset
  let filteredNodes = filter config.nodeFilter config.allNodes

  -- STEP 2: Run node initializers on filtered nodes (e.g., tree layout, pinning)
  -- This happens AFTER filtering but BEFORE link filtering
  let initializedNodes = foldl (\nodes fn -> fn nodes) filteredNodes config.nodeInitializers

  -- STEP 3: Extract node IDs for automatic link filtering
  let nodeIDs = getIDsFromNodes_ initializedNodes keyFn

  -- STEP 4: AUTOMATICALLY filter links to only connect visible nodes
  -- This is the key insight: user provides full data + node predicate,
  -- library ensures links are consistent. User can't forget this step!
  let validLink link = do
        let linkIDs = getLinkIDs_ keyFn link :: { sourceID :: String, targetID :: String }
        (linkIDs.sourceID `elem` nodeIDs) && (linkIDs.targetID `elem` nodeIDs)
      structurallyFilteredLinks = filter validLink config.allLinks

  -- STEP 5: Apply optional visual/semantic link filter
  -- This runs AFTER automatic structural filtering, so safety is guaranteed
  -- Used for things like "hide dev dependencies" or "show only certain link types"
  let finalLinks = case config.linkFilter of
        Nothing -> structurallyFilteredLinks
        Just visualFilter -> filter visualFilter structurallyFilteredLinks

  -- STEP 6: Build internal SimulationUpdate for the update API
  -- Note: We pass pre-filtered and initialized data with no filter predicates
  -- (filtering and initialization already done by library)
  let internalUpdateConfig :: SimulationUpdate d
      internalUpdateConfig =
        { nodes: Just initializedNodes    -- Filtered and initialized by library
        , links: Just finalLinks          -- Structurally + visually filtered by library (AUTOMATIC!)
        , nodeFilter: Nothing             -- No predicate needed (already filtered)
        , linkFilter: Nothing             -- No predicate needed (already filtered)
        , activeForces: config.activeForces
        , config: config.config
        , keyFn: keyFn
        }

  -- STEP 7: Call SimulationM2 update API with consistent data
  -- Handles data merging, link swizzling, force engagement
  enhanced <- update internalUpdateConfig

  -- STEP 8: Open selections for DOM operations
  node <- openSelection nodesGroup (show nodeElement)
  link <- openSelection linksGroup (show linkElement)

  -- STEP 9: Apply General Update Pattern to nodes
  node' <- updateJoin node nodeElement enhanced.nodes keyFn

  -- Enter: Create new nodes
  nodeEnter <- callbacks.onNodeEnter node'.enter attrs

  -- Update: Modify existing nodes
  callbacks.onNodeUpdate node'.update attrs

  -- Exit: Remove old nodes
  callbacks.onNodeExit node'.exit

  -- Merge enter and update for tick functions
  mergedNodes <- mergeSelections nodeEnter node'.update

  -- STEP 10: Apply General Update Pattern to links
  link' <- updateJoin link linkElement enhanced.links keyFn

  -- Enter: Create new links
  linkEnter <- callbacks.onLinkEnter link'.enter attrs

  -- Update: Modify existing links
  callbacks.onLinkUpdate link'.update attrs

  -- Exit: Remove old links
  callbacks.onLinkExit link'.exit

  -- Merge enter and update for tick functions
  mergedLinks <- mergeSelections linkEnter link'.update

  -- STEP 11: Register tick functions with merged selections
  -- The library controls when these are called (every tick)
  addTickFunction "nodes" $ Step mergedNodes (callbacks.nodeTickAttrs attrs)
  addTickFunction "links" $ Step mergedLinks callbacks.linkTickAttrs

  pure unit

-- Fallback when selections are missing (do nothing)
genericUpdateSimulation _ _ _ _ _ _ _ = pure unit
