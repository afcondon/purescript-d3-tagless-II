module PSD3.CodeAtlas.State where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import PSD3.CodeAtlas.Types (AtlasTab(..), DeclarationsData, FunctionCallsData, ModuleGraphData, SourceType)
import PSD3.Internal.Simulation.Types (D3SimulationState_, initialSimulationState)

-- | Hovered module details for the details panel
type HoveredModuleInfo =
  { moduleName :: String
  , dependencies :: Array String
  , dependedOnBy :: Array String
  }

-- | Context menu for module interactions
type ContextMenuInfo =
  { moduleName :: String
  , x :: Number  -- Screen position
  , y :: Number
  }

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
  , hoveredModule :: Maybe HoveredModuleInfo  -- For the details panel
  , spotlightModeActive :: Boolean  -- Whether spotlight mode is currently active
  , currentSpotlightModule :: Maybe String  -- The module currently being spotlighted
  , contextMenu :: Maybe ContextMenuInfo  -- Context menu state
  , spotlightFunction :: Maybe (String -> Effect Unit)  -- Function to spotlight a module
  , addDepsFunction :: Maybe (String -> Effect Unit)  -- Function to add module deps to spotlight
  , makeFocusFunction :: Maybe (String -> Effect Unit)  -- Function to make module the focus
  , resetSpotlightFunction :: Maybe (Effect Unit)  -- Function to reset spotlight to overview
  , isGridLayout :: Boolean  -- Whether nodes are in grid layout (true) or force layout (false)
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
  , hoveredModule: Nothing
  , spotlightModeActive: false
  , currentSpotlightModule: Nothing
  , contextMenu: Nothing
  , spotlightFunction: Nothing
  , addDepsFunction: Nothing
  , makeFocusFunction: Nothing
  , resetSpotlightFunction: Nothing
  , isGridLayout: false
  }
