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
import D3.Attributes.Sugar (onMouseEventEffectful)
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
import PSD3.ForceNavigator.State (State, _eventListener, _expandedNodes, _forceStatuses, _openSelections, _simulation, initialState, visibleLinks, visibleNodes)

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
  (\e d t -> liftEffect $ HS.notify l (EventFromVizualization (NodeClick (Draw.datum_.nodeType d) (Draw.datum_.id d))))

handleAction :: forall output m.
  MonadAff m =>
  Action ->
  HalogenM State Action () output m Unit
handleAction = case _ of

  Initialize -> do
    -- 1. Initialize D3 SVG structure
    openSelections <- evalEffectSimulation $ Draw.draw navigationData
    _openSelections .= Just { nodes: Nothing, links: Nothing }  -- Store for potential updates

    -- 2. Set up event listener (D3 → Halogen)
    { emitter, listener } <- liftEffect $ HS.create
    void $ H.subscribe emitter
    _eventListener .= Just listener

    pure unit

  Finalize -> pure unit

  EventFromVizualization (NodeClick nodeType nodeId) -> do
    case nodeType of
      Section -> do
        -- Toggle expansion for clicked section
        _expandedNodes %= \expanded ->
          if Set.member nodeId expanded
            then Set.delete nodeId expanded
            else Set.insert nodeId expanded
        -- TODO: Update visualization with new visible nodes/links
        pure unit
      _ -> pure unit  -- Only sections are expandable
