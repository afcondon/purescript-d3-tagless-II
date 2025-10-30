module PSD3.CodeAtlas.State where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import PSD3.CodeAtlas.Types (AtlasTab(..), DeclarationsData, FunctionCallsData, ModuleGraphData, SourceType)
import PSD3.Internal.Simulation.Types (D3SimulationState_, initialSimulationState)

-- | Component state
type State =
  { activeTab :: AtlasTab
  , declarationsData :: Maybe DeclarationsData
  , functionCallsData :: Maybe FunctionCallsData
  , moduleGraphData :: Maybe ModuleGraphData
  , simulation :: D3SimulationState_
  , searchQuery :: String
  , selectedKindFilter :: Maybe String  -- Filter by declaration kind
  , selectedModuleFilter :: Maybe String  -- Filter by module
  , selectedSourceFilter :: Maybe SourceType  -- Filter by project vs library code
  , loading :: Boolean
  , error :: Maybe String
  }

-- | Initial state
initialState :: forall i. i -> State
initialState _ =
  { activeTab: ExpandableBubblesTab
  , declarationsData: Nothing
  , functionCallsData: Nothing
  , moduleGraphData: Nothing
  , simulation: initialSimulationState Map.empty
  , searchQuery: ""
  , selectedKindFilter: Nothing
  , selectedModuleFilter: Nothing
  , selectedSourceFilter: Nothing
  , loading: true
  , error: Nothing
  }
