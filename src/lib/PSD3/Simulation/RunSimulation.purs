module PSD3.Simulation.RunSimulation where

import Prelude

import Control.Monad.State (class MonadState, get)
import PSD3.Internal.Types (D3Selection_, Datum_)
import PSD3.Internal.Attributes.Instances (Label)
import PSD3.Simulation.Scene (SceneConfig)
import PSD3.Data.Node (D3Link_Unswizzled, D3_SimulationNode)
import PSD3.Capabilities.Simulation (class SimulationM, start)
import Data.Array (filter, foldl)
import Data.Maybe (Maybe(..))
import Data.Set (Set)

-- | Low-level runSimulation pattern for force-directed visualizations
-- |
-- | This function implements the standard update pattern:
-- | 1. Filter nodes/links based on scene predicates
-- | 2. Apply node initializers (positioning, pinning, etc.)
-- | 3. Delegate to visualization-specific updateSimulation
-- | 4. Restart the simulation
-- |
-- | Parameters:
-- | - selections: DOM group selections for nodes and links
-- | - scene: Scene configuration (filters, forces, attributes, initializers)
-- | - allNodes: Complete node dataset (before filtering)
-- | - allLinks: Complete link dataset (before filtering)
-- | - updateSimFn: Visualization-specific update function (e.g., Spago's updateSimulation)
-- |
-- | The updateSimFn should handle:
-- | - Calling the SimulationM2 update() API
-- | - DOM joins (enter/update/exit pattern)
-- | - Setting tick functions
-- | But should NOT call start() - that's handled here
runSimulation :: forall d attrs sel m.
  Monad m =>
  SimulationM sel m =>
  { nodes :: Maybe D3Selection_, links :: Maybe D3Selection_ } ->
  SceneConfig d attrs ->
  Array (D3_SimulationNode d) ->
  Array D3Link_Unswizzled ->
  ({ nodes :: Array (D3_SimulationNode d)
   , links :: Array D3Link_Unswizzled
   , scene :: SceneConfig d attrs
   } -> m Unit) ->
  m Unit
runSimulation selections scene allNodes allLinks updateSimFn = do
  -- STEP 1: Filter - Apply scene's node filter predicate
  let filteredNodes = filter scene.chooseNodes allNodes

  -- STEP 2: Initialize - Run initializers on filtered data
  -- This is where tree layouts, pinning, positioning happen
  let initializedNodes = foldl (\nodes fn -> fn nodes) filteredNodes scene.nodeInitializerFunctions

  -- STEP 3: Delegate to visualization-specific update function
  -- We pass the initialized nodes and ALL links (updateSimFn will filter links internally)
  updateSimFn
    { nodes: initializedNodes
    , links: allLinks
    , scene: scene
    }

  -- STEP 4: Restart simulation
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
  Monad m =>
  SimulationM sel m =>
  MonadState { | row } m =>
  ({ | row } -> { nodes :: Maybe D3Selection_, links :: Maybe D3Selection_ }) ->  -- Get selections
  ({ | row } -> SceneConfig d attrs) ->                                           -- Get scene
  ({ | row } -> Array (D3_SimulationNode d)) ->                                   -- Get model nodes
  ({ | row } -> Array D3Link_Unswizzled) ->                                       -- Get model links
  (attrs -> { | row } -> attrs) ->                                                -- Enhance attributes
  ({ nodes :: Maybe D3Selection_, links :: Maybe D3Selection_ } ->                -- UpdateSimulation function
   { nodes :: Array (D3_SimulationNode d)
   , links :: Array D3Link_Unswizzled
   , nodeFilter :: Maybe (D3_SimulationNode d -> Boolean)
   , linkFilter :: Maybe (D3Link_Unswizzled -> Boolean)
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
    \{ nodes: initializedNodes, links: allLinks, scene: sceneConfig } ->
      updateSimFn selections
        { nodes: initializedNodes
        , links: allLinks
        , nodeFilter: Nothing
        , linkFilter: Just sceneConfig.linksShown
        , activeForces: sceneConfig.activeForces
        , linksWithForce: sceneConfig.linksActive
        }
        enhancedAttrs
