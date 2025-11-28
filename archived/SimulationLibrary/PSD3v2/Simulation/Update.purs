module PSD3v2.Simulation.Update where

import Prelude

import PSD3.Internal.Attributes.Instances (Label)
import PSD3v2.Attribute.Types (Attribute, class_)
import PSD3v2.Capabilities.Selection (class SelectionM, appendChild, joinData, joinDataWithKey, merge, select)
import PSD3v2.Capabilities.Simulation (class SimulationM2, SimulationUpdate, Step(..), addTickFunction, update)
import PSD3v2.Selection.Operations (elementTypeToString)
import PSD3v2.Selection.Types (ElementType(..), JoinResult(..), SBoundOwns, SEmpty, SExiting, SPending, Selection(..), SelectionImpl(..))
import Web.DOM.Element (Element)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Data.Array as Array
import Unsafe.Coerce (unsafeCoerce)
import PSD3.Data.Node (Link, SimulationNode, SwizzledLink)
import PSD3.Internal.FFI (SimulationVariables, getIDsFromNodes_, getLinkIDs_, swizzleLinks_, swizzledLinkKey_)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Array (elem, filter)
import Data.Foldable (foldl)

-- | Declarative callbacks for rendering nodes and links
-- |
-- | The library calls these in the correct order to guarantee state integrity.
-- | Visualizations just provide "what to render", not "when to render it".
-- |
-- | ## Node Rendering Flow (guaranteed by library):
-- | 1. onNodeEnter: Populate enter selection (create new DOM elements)
-- |    - Input: Empty (pending) selection placeholder
-- |    - Output: Populated (bound) selection (with children if needed)
-- | 2. onNodeUpdate: Modify update selection (update existing DOM elements)
-- |    - Input: Selection of existing nodes (bound)
-- |    - Output: Unit (modifications via setAttributes)
-- | 3. onNodeExit: Clean up exit selection
-- |    - Usually just: remove exit
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
-- |
-- | Now uses concrete SwizzledLink type instead of polymorphic swizzled parameter.
-- | The swizzled type is derivable: SwizzledLink dataRow linkRow.
type RenderCallbacks attrs sel m dataRow linkRow = {
  -- Node rendering callbacks
  -- Enter: Takes SPending, returns SBoundOwns after appending elements
  onNodeEnter :: sel SPending Element (SimulationNode dataRow) -> attrs -> m (sel SBoundOwns Element (SimulationNode dataRow)),
  -- Update: Takes SBound, modifies in place, returns Unit
  onNodeUpdate :: sel SBoundOwns Element (SimulationNode dataRow) -> attrs -> m Unit,
  -- Exit: Takes SExiting, removes elements, returns Unit
  onNodeExit :: sel SExiting Element (SimulationNode dataRow) -> m Unit,

  -- Link rendering callbacks (swizzled links after processing)
  onLinkEnter :: sel SPending Element (SwizzledLink dataRow linkRow) -> attrs -> m (sel SBoundOwns Element (SwizzledLink dataRow linkRow)),
  onLinkUpdate :: sel SBoundOwns Element (SwizzledLink dataRow linkRow) -> attrs -> m Unit,
  onLinkExit :: sel SExiting Element (SwizzledLink dataRow linkRow) -> m Unit,

  -- Tick function attributes (applied to merged selections)
  nodeTickAttrs :: attrs -> Array (Attribute (SimulationNode dataRow)),
  linkTickAttrs :: Array (Attribute (SwizzledLink dataRow linkRow))  -- Links are swizzled type after swizzling
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
type DeclarativeUpdateConfig d id linkRow =
  { allNodes :: Array (SimulationNode d)                                -- FULL dataset (required)
  , allLinks :: Array (Link id linkRow)                                 -- FULL dataset (required)
  , nodeFilter :: SimulationNode d -> Boolean                           -- Which nodes to show (required)
  , linkFilter :: Maybe (Link id linkRow -> Boolean)                    -- Optional visual filtering (applied AFTER automatic structural filtering)
  , nodeInitializers :: Array (Array (SimulationNode d) -> Array (SimulationNode d))  -- Functions to transform filtered nodes (e.g., tree layout, grid, pinning)
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
-- | 8. Join data to DOM using General Update Pattern
-- | 9. Apply render callbacks (enter/update/exit)
-- | 10. Merge enter and update selections
-- | 11. Register tick functions with merged selections
-- |
-- | The visualization provides callbacks via `RenderCallbacks` but has no control
-- | over ordering, filtering consistency, or state management. The library guarantees integrity.
-- |
-- | ## Parameters:
-- | - selections: DOM group selections for nodes and links (empty selections to bind data to)
-- | - nodeElement: Element type for nodes (e.g., Circle, Group)
-- | - linkElement: Element type for links (e.g., Line, Path)
-- | - config: Declarative configuration (full datasets + node filter predicate)
-- | - nodeKeyFn: Key function for node data binding
-- | - linkKeyFn: Key function for link data binding
-- | - attrs: Visualization-specific attributes
-- | - callbacks: Render callbacks for enter/update/exit phases
-- |
-- | ## Example Usage:
-- | ```purescript
-- | genericUpdateSimulation
-- |   { nodes: nodesGroup, links: linksGroup }
-- |   Circle  -- Node element
-- |   Line    -- Link element
-- |   { allNodes: model.nodes                      -- FULL dataset
-- |   , allLinks: model.links                      -- FULL dataset
-- |   , nodeFilter: \n -> Set.member n.group visibleGroups  -- Single predicate
-- |   , linkFilter: Nothing
-- |   , nodeInitializers: []
-- |   , activeForces: Just activeForces
-- |   , config: Nothing
-- |   }
-- |   (_.id)                                       -- Node key
-- |   keyIsID_                                     -- Link key
-- |   myAttributes
-- |   myRenderCallbacks
-- | ```
-- |
-- | Note: The user NEVER manually filters links. The library does it automatically
-- | by extracting node IDs and filtering links to only connect visible nodes.
-- | This makes the "filtered nodes + all links" bug impossible.
genericUpdateSimulation :: forall dataRow id linkRow attrs sel m linkKey.
  MonadEffect m =>
  SelectionM sel m =>
  SimulationM2 (sel SBoundOwns Element) m =>
  Ord (SimulationNode dataRow) =>                        -- PSD3v2 requires Ord for data joins
  Ord id =>                                              -- Node key must be Ord (for Sets) and Eq (for elem)
  Eq id =>                                               -- For elem check in link filtering
  Show id =>                                             -- For debug logging (TODO: remove after debugging)
  Ord linkKey =>                                         -- Links use key-based join (no Ord SwizzledLink needed!)
  { nodes :: sel SEmpty Element (SimulationNode dataRow), links :: sel SEmpty Element (SwizzledLink dataRow linkRow) } ->
  ElementType ->                                         -- Node element type
  ElementType ->                                         -- Link element type
  DeclarativeUpdateConfig dataRow id linkRow ->          -- Declarative configuration (id = node key = link source/target type)
  (SimulationNode dataRow -> id) ->                      -- Node key function (must return same type as link source/target)
  (SwizzledLink dataRow linkRow -> linkKey) ->           -- Link key function
  attrs ->                                               -- Visualization attributes
  RenderCallbacks attrs sel m dataRow linkRow ->         -- Render callbacks
  m Unit
genericUpdateSimulation { nodes: nodesGroup, links: linksGroup }
                        nodeElement linkElement config nodeKeyFn linkKeyFn attrs callbacks = do
  -- STEP 1: Apply node filter to full dataset
  let filteredNodes = filter config.nodeFilter config.allNodes

  -- STEP 2: Run node initializers on filtered nodes (e.g., tree layout, pinning)
  -- This happens AFTER filtering but BEFORE link filtering
  let initializedNodes = foldl (\nodes fn -> fn nodes) filteredNodes config.nodeInitializers

  -- STEP 3: Extract node IDs for automatic link filtering
  let nodeIDs = getIDsFromNodes_ initializedNodes nodeKeyFn

  -- Debug: show what nodeIDs we extracted
  _ <- liftEffect $ log $ "🔗 DEBUG: initialized nodes count: " <> show (Array.length initializedNodes)
  _ <- liftEffect $ log $ "🔗 DEBUG: nodeIDs array length: " <> show (Array.length nodeIDs)
  _ <- liftEffect $ log $ "🔗 DEBUG: first 5 nodeIDs: " <> show (Array.take 5 nodeIDs)

  -- STEP 4: AUTOMATICALLY filter links to only connect visible nodes
  -- This is the key insight: user provides full data + node predicate,
  -- library ensures links are consistent. User can't forget this step!

  -- Debug: test first link
  let firstLink = Array.head config.allLinks
  case firstLink of
    Nothing -> pure unit
    Just link -> do
      let linkIDs = getLinkIDs_ link
      _ <- liftEffect $ log $ "🔗 DEBUG: first link source: " <> show linkIDs.sourceID <> ", target: " <> show linkIDs.targetID
      _ <- liftEffect $ log $ "🔗 DEBUG: source in nodeIDs: " <> show (linkIDs.sourceID `elem` nodeIDs)
      _ <- liftEffect $ log $ "🔗 DEBUG: target in nodeIDs: " <> show (linkIDs.targetID `elem` nodeIDs)
      pure unit

  let validLink link = do
        let linkIDs = getLinkIDs_ link
        (linkIDs.sourceID `elem` nodeIDs) && (linkIDs.targetID `elem` nodeIDs)
      structurallyFilteredLinks = filter validLink config.allLinks

  -- Debug: log link filtering info
  _ <- liftEffect $ log $ "🔗 genericUpdateSimulation: " <> show (Array.length config.allLinks) <> " total links"
  _ <- liftEffect $ log $ "🔗 genericUpdateSimulation: " <> show (Array.length structurallyFilteredLinks) <> " links after structural filter"

  -- STEP 5: Apply optional visual/semantic link filter
  -- This runs AFTER automatic structural filtering, so safety is guaranteed
  -- Used for things like "hide dev dependencies" or "show only certain link types"
  let finalLinks = case config.linkFilter of
        Nothing -> structurallyFilteredLinks
        Just visualFilter -> filter visualFilter structurallyFilteredLinks

  _ <- liftEffect $ log $ "🔗 genericUpdateSimulation: " <> show (Array.length finalLinks) <> " links after visual filter"

  -- STEP 6: Build internal SimulationUpdate for the update API
  -- Note: We pass pre-filtered and initialized data with no filter predicates
  -- (filtering and initialization already done by library)
  let internalUpdateConfig :: SimulationUpdate dataRow id linkRow id
      internalUpdateConfig =
        { nodes: Just initializedNodes    -- Filtered and initialized by library
        , links: Just finalLinks          -- Structurally + visually filtered by library (AUTOMATIC!)
        , nodeFilter: Nothing             -- No predicate needed (already filtered)
        , linkFilter: Nothing             -- No predicate needed (already filtered)
        , activeForces: config.activeForces
        , config: config.config
        , keyFn: nodeKeyFn
        }

  -- STEP 7: Call SimulationM2 update API with consistent data
  -- Handles data merging, link swizzling (for force if enabled), force engagement
  enhanced <- update internalUpdateConfig

  -- STEP 7b: Swizzle links directly for DOM join
  -- We do this ourselves because update may return empty links when links force is disabled.
  -- This ensures visual links work even without a links force.
  let swizzledLinks = swizzleLinks_ finalLinks enhanced.nodes nodeKeyFn

  -- STEP 8-9: Join data to DOM and apply General Update Pattern to nodes
  -- In PSD3v2, joinData directly returns the JoinResult with enter/update/exit
  let nodeSelector = elementTypeToString nodeElement
  _ <- liftEffect $ log $ "🔍 genericUpdateSimulation: joining nodes with selector '" <> nodeSelector <> "'"
  _ <- liftEffect $ log $ "🔍 genericUpdateSimulation: " <> show (Array.length enhanced.nodes) <> " nodes in data"
  JoinResult nodeJoin <- joinData enhanced.nodes nodeSelector nodesGroup
  _ <- liftEffect $ log $ "🔍 genericUpdateSimulation: join result - enter: " <> show (Array.length $ getEnterData nodeJoin.enter) <> ", update: " <> show (Array.length $ getUpdateData nodeJoin.update) <> ", exit: " <> show (Array.length $ getExitData nodeJoin.exit)

  -- Enter: Create new nodes
  nodeEnter <- callbacks.onNodeEnter nodeJoin.enter attrs

  -- Update: Modify existing nodes
  callbacks.onNodeUpdate nodeJoin.update attrs

  -- Exit: Remove old nodes
  callbacks.onNodeExit nodeJoin.exit

  -- Merge enter and update for tick functions
  mergedNodes <- merge nodeEnter nodeJoin.update

  -- STEP 10: Apply General Update Pattern to links (using key function to avoid Ord D3Link_Swizzled)
  -- Use swizzledLinks (our own swizzle) instead of enhanced.links (from simulation)
  -- Use swizzledLinkKey_ which extracts source/target IDs from node objects
  _ <- liftEffect $ log $ "🔗 genericUpdateSimulation: " <> show (Array.length swizzledLinks) <> " links to join"
  JoinResult linkJoin <- joinDataWithKey swizzledLinks swizzledLinkKey_ (elementTypeToString linkElement) linksGroup
  _ <- liftEffect $ log $ "🔗 genericUpdateSimulation: link join result - enter: " <> show (Array.length $ getEnterData linkJoin.enter) <> ", update: " <> show (Array.length $ getUpdateData linkJoin.update) <> ", exit: " <> show (Array.length $ getExitData linkJoin.exit)

  -- Enter: Create new links
  linkEnter <- callbacks.onLinkEnter linkJoin.enter attrs

  -- Update: Modify existing links
  callbacks.onLinkUpdate linkJoin.update attrs

  -- Exit: Remove old links
  callbacks.onLinkExit linkJoin.exit

  -- Merge enter and update for tick functions
  mergedLinks <- merge linkEnter linkJoin.update

  -- STEP 11: Register tick functions with merged selections
  -- The library controls when these are called (every tick)
  addTickFunction "nodes" $ Step mergedNodes (callbacks.nodeTickAttrs attrs)
  addTickFunction "links" $ Step mergedLinks callbacks.linkTickAttrs

  pure unit

-- Helper functions to extract data counts for debugging
getEnterData :: forall sel parent datum. sel SPending parent datum -> Array datum
getEnterData sel = case (unsafeCoerce sel :: Selection SPending parent datum) of
  Selection (PendingSelection r) -> r.pendingData
  _ -> []

getUpdateData :: forall sel parent datum. sel SBoundOwns parent datum -> Array datum
getUpdateData sel = case (unsafeCoerce sel :: Selection SBoundOwns parent datum) of
  Selection (BoundSelection r) -> r.data
  _ -> []

getExitData :: forall sel parent datum. sel SExiting parent datum -> Array datum
getExitData sel = case (unsafeCoerce sel :: Selection SExiting parent datum) of
  Selection (ExitingSelection r) -> r.data
  _ -> []

-- | Type alias for simulation container groups
-- |
-- | Contains the nodes and links container groups needed by genericUpdateSimulation.
-- | These are always SEmpty selections - they hold DOM groups that will have data bound to them.
type SimulationGroups sel nodeRow linkRow =
  { nodes :: sel SEmpty Element (SimulationNode nodeRow)
  , links :: sel SEmpty Element (SwizzledLink nodeRow linkRow)
  }

-- | Create simulation container groups within a parent element
-- |
-- | Creates two SVG groups: g.links (rendered first, underneath) and g.nodes (rendered second, on top).
-- | Returns properly-typed handles for use with genericUpdateSimulation.
-- |
-- | Example:
-- | ```purescript
-- | inner <- appendChild Group [] svg
-- | groups <- setupSimulationGroups inner
-- | genericUpdateSimulation groups Group Path sceneConfig ...
-- | ```
setupSimulationGroups
  :: forall sel m parent parentDatum nodeRow linkRow
   . SelectionM sel m
  => sel SEmpty parent parentDatum
  -> m (SimulationGroups sel nodeRow linkRow)
setupSimulationGroups parent = do
  -- Links underneath nodes (render order matters in SVG)
  linksGroup <- appendChild Group [ class_ "links" ] parent
  nodesGroup <- appendChild Group [ class_ "nodes" ] parent
  pure
    { nodes: nodesGroup  -- Safe: SEmpty selections, just casting phantom datum type
    , links: linksGroup
    }

-- | Re-select existing simulation container groups
-- |
-- | Use this in update functions to get fresh SEmpty handles to existing DOM groups.
-- | The groups must have been created previously with setupSimulationGroups.
-- |
-- | Example:
-- | ```purescript
-- | -- In update handler
-- | groups <- selectSimulationGroups
-- | genericUpdateSimulation groups Group Path newSceneConfig ...
-- | ```
selectSimulationGroups
  :: forall sel m nodeRow linkRow
   . SelectionM sel m
  => m (SimulationGroups sel nodeRow linkRow)
selectSimulationGroups = do
  nodesGroup <- select "g.nodes"
  linksGroup <- select "g.links"
  pure { nodes: nodesGroup, links: linksGroup }
