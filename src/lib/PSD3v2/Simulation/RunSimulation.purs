module PSD3v2.Simulation.RunSimulation where

import Prelude

import Control.Monad.State (class MonadState, get)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Effect.Aff (makeAff, nonCanceler)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import PSD3v2.Capabilities.Simulation (class SimulationM2, start, stop)
import PSD3.Data.Node (D3Link_Unswizzled, SimulationNode)
import PSD3.Internal.Attributes.Instances (Label)
import PSD3.Internal.Types (Datum_)
import PSD3v2.Simulation.Scene (SimSceneConfig)
import PSD3v2.Simulation.SceneTransition (executeSceneTransition_)
import PSD3v2.Selection.Types (SBoundOwns, SEmpty)
import Web.DOM.Element (Element)

-- | Low-level runSimulation pattern for force-directed visualizations
-- |
-- | This function implements the standard update pattern with declarative scene transitions:
-- | 1. Stop simulation
-- | 2. Compute target node positions (filter + initialize)
-- | 3. Update DOM via visualization-specific updateSimulation
-- | 4. Execute scene transitions (if configured) with enter/exit/update behaviors
-- | 5. Start simulation (immediately if no transitions, or after transition completion)
-- |
-- | Parameters:
-- | - selections: DOM group selections for nodes and links
-- | - scene: Scene configuration (filters, forces, attributes, initializers, transitions)
-- | - allNodes: Complete node dataset (NO pre-filtering!)
-- | - allLinks: Complete link dataset (NO pre-filtering!)
-- | - updateSimFn: Visualization-specific update function (e.g., Spago's updateSimulation)
-- |
-- | ## DECLARATIVE PATTERN:
-- | We pass FULL datasets + scene config to updateSimFn.
-- | The updateSimFn (via genericUpdateSimulation) handles:
-- | - Filtering nodes using scene.chooseNodes
-- | - Initializing filtered nodes using scene.nodeInitializerFunctions
-- | - Automatically filtering links to match visible nodes
-- | - Calling the SimulationM2 update() API
-- | - DOM joins (enter/update/exit pattern)
-- | - Setting tick functions
-- |
-- | ## TRANSITION HANDLING:
-- | If scene.transitionConfig is Nothing:
-- | - Instant transition (backward compatible)
-- | - Start simulation immediately after DOM update
-- |
-- | If scene.transitionConfig is Just spec:
-- | - Execute declarative scene transition with enter/exit/update behaviors
-- | - Wait for transition completion (uses Aff to make callback-based FFI awaitable)
-- | - Start simulation only after transitions complete
-- | - This prevents "nodes stuck at origin" issues during scene switches
runSimulation :: forall d attrs sel m.
  MonadAff m =>
  SimulationM2 (sel SBoundOwns Element) m =>
  { nodes :: sel SEmpty Element (SimulationNode d), links :: sel SEmpty Element (SimulationNode d) } ->
  SimSceneConfig d attrs ->
  Array (SimulationNode d) ->
  Array D3Link_Unswizzled ->
  ({ allNodes :: Array (SimulationNode d)
   , allLinks :: Array D3Link_Unswizzled
   , scene :: SimSceneConfig d attrs
   } -> m Unit) ->
  m Unit
runSimulation _selections scene allNodes allLinks updateSimFn = do
  -- STEP 1: Stop simulation before updating
  stop

  -- STEP 2: Compute target node positions for transitions
  -- Apply initializers to ALL nodes (not just filtered ones)
  -- This ensures indices match between DOM selection and targetNodes array
  -- Note: Initializers should handle filtering internally if needed (e.g., tree layout only touches tree nodes)
  let targetNodes = foldl (\nodes fn -> fn nodes) allNodes scene.nodeInitializerFunctions

  -- STEP 3: Delegate to visualization-specific update function
  -- This updates the DOM (GUP enter/exit/update), sets tick functions, and updates simulation state
  updateSimFn
    { allNodes: allNodes
    , allLinks: allLinks
    , scene: scene
    }

  -- STEP 4: Handle transitions based on scene configuration (DECLARATIVE!)
  case scene.transitionConfig of
    Nothing ->
      -- Instant transition (backward compatible)
      start

    Just spec -> do
      -- Execute declarative scene transition
      -- Use makeAff to wrap callback-based FFI into awaitable Aff action
      liftAff $ makeAff \callback -> do
        executeSceneTransition_
          spec
          "g.nodes > g"      -- Node selector (standard D3 hierarchy)
          "g.links > path"   -- Link selector (path elements for morphable links)
          targetNodes        -- Nodes with computed positions (fx/fy or x/y)
          (callback (Right unit))  -- Signal completion when transition finishes
        pure nonCanceler

      -- STEP 5: Start simulation after transition completes
      start

-- | High-level runSimulation that works with extensible state records
-- |
-- | This is the main API for applications. It handles:
-- | 1. Extracting data from state using provided accessor functions
-- | 2. Calling the low-level runSimulation pattern
-- | 3. Delegating to visualization-specific updateSimulation
-- |
-- | Uses open row types to work with any state structure that has the required fields.
-- |
-- | Parameters:
-- | - getSelections: Extract DOM selections from state
-- | - getScene: Extract scene configuration from state
-- | - getModelNodes: Extract all model nodes from state
-- | - getModelLinks: Extract all model links from state
-- | - enhanceAttributes: Modify attributes before rendering (e.g., add callbacks, tags)
-- | - updateSimFn: Visualization-specific updateSimulation function
-- |
-- | Example usage:
-- | ```purescript
-- | runSimulationFromState
-- |   (_.staging.selections)                   -- get selections
-- |   (_.scene)                                -- get scene
-- |   (fromMaybe [] <<< map _.nodes <<< _.model)  -- get nodes
-- |   (fromMaybe [] <<< map _.links <<< _.model)  -- get links
-- |   (\attrs state -> attrs { tagMap = Just state.tags })  -- enhance attrs
-- |   Graph.updateSimulation                   -- viz-specific update
-- | ```
runSimulationFromState :: forall d attrs sel m row.
  MonadAff m =>
  SimulationM2 (sel SBoundOwns Element) m =>
  MonadState { | row } m =>
  ({ | row } -> { nodes :: sel SEmpty Element (SimulationNode d), links :: sel SEmpty Element (SimulationNode d) }) ->  -- Get selections
  ({ | row } -> SimSceneConfig d attrs) ->                                           -- Get scene
  ({ | row } -> Array (SimulationNode d)) ->                                   -- Get model nodes
  ({ | row } -> Array D3Link_Unswizzled) ->                                       -- Get model links
  (attrs -> { | row } -> attrs) ->                                                -- Enhance attributes
  ({ nodes :: sel SEmpty Element (SimulationNode d), links :: sel SEmpty Element (SimulationNode d) } ->  -- UpdateSimulation function (DECLARATIVE API)
   { allNodes :: Array (SimulationNode d)                                         -- FULL dataset
   , allLinks :: Array D3Link_Unswizzled                                            -- FULL dataset
   , nodeFilter :: SimulationNode d -> Boolean                                   -- Which nodes to show
   , linkFilter :: Maybe (D3Link_Unswizzled -> Boolean)                             -- Optional visual filtering
   , nodeInitializers :: Array (Array (SimulationNode d) -> Array (SimulationNode d))  -- Tree layout, grid, etc.
   , activeForces :: Set Label
   , linksWithForce :: Datum_ -> Boolean
   } ->
   attrs ->
   m Unit) ->
  m Unit
runSimulationFromState getSelections getScene getNodes getLinks enhanceAttrs updateSimFn = do
  state <- get
  let selections = getSelections state
      scene = getScene state
      allModelNodes = getNodes state
      allModelLinks = getLinks state
      enhancedAttrs = enhanceAttrs scene.attributes state

  runSimulation selections scene allModelNodes allModelLinks
    \{ allNodes, allLinks, scene: sceneConfig } ->
      updateSimFn selections
        { allNodes: allNodes                           -- FULL dataset (declarative!)
        , allLinks: allLinks                           -- FULL dataset (declarative!)
        , nodeFilter: sceneConfig.chooseNodes          -- Node predicate
        , linkFilter: Just sceneConfig.linksShown      -- Visual link filtering (restored!)
        , nodeInitializers: sceneConfig.nodeInitializerFunctions  -- Tree layout, grid, pinning
        , activeForces: sceneConfig.activeForces
        , linksWithForce: sceneConfig.linksActive
        }
        enhancedAttrs
