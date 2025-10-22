-- | ForceNavigator Component - Landing Page Interactive Navigation
-- |
-- | A simplified force-directed graph for site navigation showing the library structure.
-- | Demonstrates the MiseEnScene pattern with just expand/collapse interaction.
-- |
-- | **Key Features**:
-- | - Click sections to expand/collapse their children
-- | - Smooth force-directed layout
-- | - Event listener properly separated from visualization config
module PSD3.ForceNavigator where

import Prelude

import Control.Monad.State (get)
import D3.Attributes.Sugar (onMouseEventEffectful, x)
import Debug (spy)
import D3.Data.Types (MouseEvent(..))
import D3.Simulation.Types (SimVariable(..))
import D3.Viz.ForceNavigator.Data (navigationData)
import D3.Viz.ForceNavigator.Draw as Draw
import D3.Viz.ForceNavigator.Model (NodeType(..))
import D3Tagless.Capabilities (actualizeForces, setConfigVariable, start, stop)
import D3Tagless.Instance.Simulation (evalEffectSimulation, runWithD3_Simulation)
import Data.Lens (use, (.=), (%=))
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (HalogenM)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Subscription as HS
import PSD3.ForceNavigator.Actions (Action(..), VizEvent(..))
import PSD3.ForceNavigator.Forces (forceLibrary)
import PSD3.ForceNavigator.State (State, _activeForces, _eventListener, _expandedNodes, _openSelections, _simulation, initialState, visibleLinks, visibleNodes)

component :: forall query output m. MonadAff m => H.Component query Unit output m
component = H.mkComponent
  { initialState: const $ initialState forceLibrary
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    , initialize = Just Initialize
    , finalize = Just Finalize
    }
  }

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div [] []  -- Empty render - visualization is done via D3

-- | Construct callback from event listener
simulationEvent :: HS.Listener Action -> _
simulationEvent l = onMouseEventEffectful MouseClick
  (\e d t -> liftEffect $ do
    let nodeType = Draw.datum_.nodeType d
    let nodeId = Draw.datum_.id d
    let _ = spy "🖱️ D3 Click Event" { nodeType, nodeId }
    HS.notify l (EventFromVizualization (NodeClick nodeType nodeId))
  )

-- | Start or restart the simulation with current force statuses
-- |
-- | **Pattern explanation:**
-- | - `forceStatuses` map controls which forces are enabled (On/Off)
-- | - `actualizeForces` applies the status map to the simulation
-- | - `start` begins the physics simulation (or reheats if already running)
-- |
-- | **When to call:**
-- | - After initial draw (to start the simulation)
-- | - After data updates (to reheat with new nodes/links)
-- |
-- | **To dynamically enable/disable forces:**
-- | Use `_activeForces` lens to add/remove force labels from the Set, then call runSimulation
-- | Example: `_activeForces %= Set.insert "newForce"` or `Set.delete "oldForce"`
runSimulation :: forall output m. MonadAff m => HalogenM State Action () output m Unit
runSimulation = do
  activeLabels <- use _activeForces
  evalEffectSimulation $ do
    -- Enable only the forces in the Set, disable all others
    actualizeForces activeLabels
    -- Start the simulation (or restart if already running, reheating the physics)
    start

handleAction :: forall output m.
  MonadAff m =>
  Action ->
  HalogenM State Action () output m Unit
handleAction = case _ of

  Initialize -> do
    -- 1. Set up event listener (D3 → Halogen)
    { emitter, listener } <- liftEffect $ HS.create
    void $ H.subscribe emitter
    _eventListener .= Just listener

    -- 2. Create callback from listener
    let callback = simulationEvent listener

    -- 3. Initialize D3 SVG structure with callback (only show center + sections initially)
    expanded <- use _expandedNodes
    let visibleNodesArray = visibleNodes expanded navigationData.nodes
    let visibleLinksArray = visibleLinks visibleNodesArray navigationData.links
    let initialModel = { nodes: visibleNodesArray, links: visibleLinksArray }

    selections <- evalEffectSimulation $ Draw.draw callback initialModel
    _openSelections .= Just { nodes: Just selections.nodes, links: Just selections.links }

    -- 4. Start the simulation with forces from the force library
    runSimulation

    pure unit

  Finalize -> pure unit

  EventFromVizualization (NodeClick nodeType nodeId) -> do
    let _ = spy "✅ Halogen Received Event" { nodeType, nodeId }
    case nodeType of
      Section -> do
        -- Toggle expansion for clicked section
        _expandedNodes %= \expanded ->
          if Set.member nodeId expanded
            then let _ = spy "📂 Collapsing" nodeId in Set.delete nodeId expanded
            else let _ = spy "📁 Expanding" nodeId in Set.insert nodeId expanded

        -- Get updated expansion state and listener
        expanded <- use _expandedNodes
        maybeListener <- use _eventListener

        -- Calculate new visible nodes/links
        let visibleNodesArray = visibleNodes expanded navigationData.nodes
        let visibleLinksArray = visibleLinks visibleNodesArray navigationData.links
        let updatedModel = { nodes: visibleNodesArray, links: visibleLinksArray }

        -- Update the visualization with new data
        case maybeListener of
          Just listener -> do
            let callback = simulationEvent listener
            evalEffectSimulation $ Draw.update callback updatedModel
            -- Restart simulation with new nodes/links (heats up the simulation)
            runSimulation
          Nothing -> pure unit

        pure unit
      _ -> do
        let _ = spy "ℹ️ Non-section clicked" nodeType
        pure unit  -- Only sections are expandable
