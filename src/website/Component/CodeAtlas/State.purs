module PSD3.CodeAtlas.State where

import Prelude

import Data.Maybe (Maybe(..))
import PSD3.CodeAtlas.Types (AtlasTab(..), DeclarationsData, FunctionCallsData, SourceType)

-- | Component state
type State =
  { activeTab :: AtlasTab
  , declarationsData :: Maybe DeclarationsData
  , functionCallsData :: Maybe FunctionCallsData
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
  { activeTab: DeclarationsTab
  , declarationsData: Nothing
  , functionCallsData: Nothing
  , searchQuery: ""
  , selectedKindFilter: Nothing
  , selectedModuleFilter: Nothing
  , selectedSourceFilter: Nothing
  , loading: true
  , error: Nothing
  }
