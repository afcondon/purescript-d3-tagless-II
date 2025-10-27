module PSD3.CodeAtlas.Actions where

import Prelude

import Data.Maybe (Maybe)
import PSD3.CodeAtlas.Types (AtlasTab, DeclarationsData, FunctionCallsData, ModuleGraphData, SourceType)

-- | User actions
data Action
  = Initialize
  | SetActiveTab AtlasTab
  | DataLoaded DeclarationsData FunctionCallsData ModuleGraphData
  | DataLoadFailed String
  | SetSearchQuery String
  | SetKindFilter (Maybe String)
  | SetModuleFilter (Maybe String)
  | SetSourceFilter (Maybe SourceType)
  | ClearFilters
