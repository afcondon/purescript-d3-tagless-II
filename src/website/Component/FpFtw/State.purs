module PSD3.FpFtw.State where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import PSD3.Internal.Simulation.Config as F
import PSD3.Internal.Simulation.Forces (createForce, createLinkForce, initialize)
import PSD3.Internal.Simulation.Types (D3SimulationState_, Force, ForceType(..), RegularForceType(..), allNodes, initialSimulationState)

-- | Component state for FP FTW examples
type State =
  { currentExample :: String  -- Which example is currently selected
  , simulation :: D3SimulationState_  -- D3 force simulation state
  }

-- | Forces configuration for LesMis topological visualization
topologicalForceLibrary :: forall d. Map.Map String (Force d)
topologicalForceLibrary = initialize [ forces.manyBodyWeak, forces.collision, forces.links ]
  where
    forces = {
        manyBodyWeak: createForce "many body weak" (RegularForce ForceManyBody) allNodes [ F.strengthVal (-15.0) ]
      , collision:    createForce "collision"       (RegularForce ForceCollide)  allNodes [ F.radiusVal 7.0 ]
      , links:        createLinkForce Nothing [ F.distanceVal 80.0 ]
    }

-- | Initial state
initialState :: forall i. i -> State
initialState _ =
  { currentExample: "intro"
  , simulation: initialSimulationState topologicalForceLibrary
  }
