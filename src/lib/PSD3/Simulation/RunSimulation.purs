module PSD3.Simulation.RunSimulation where

import Prelude

import PSD3.Internal.Types (D3Selection_)
import PSD3.Simulation.Scene (SceneConfig)
import PSD3.Data.Node (D3Link_Unswizzled, D3_SimulationNode)
import PSD3.Capabilities.Simulation (class SimulationM, start)
import Data.Array (filter, foldl)
import Data.Maybe (Maybe)

-- | Generic runSimulation pattern for force-directed visualizations
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

-- | Simplified version when you don't need node initializers
runSimulationSimple :: forall d attrs sel m.
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
runSimulationSimple = runSimulation
