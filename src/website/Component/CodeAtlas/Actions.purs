module PSD3.CodeAtlas.Actions where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import PSD3.CodeAtlas.Types (AtlasTab, DeclarationsData, FunctionCallsData, ModuleGraphData, SourceType)
import PSD3.CodeAtlas.State (HoveredModuleInfo, ContextMenuInfo)

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
  | ShowModuleDetails HoveredModuleInfo
  | HideModuleDetails
  | ResetToOverview
  | EnableSpotlightMode
  | DisableSpotlightMode
  | ShowContextMenu ContextMenuInfo
  | HideContextMenu
  | SpotlightModuleFromMenu String
  | AddDepsToSpotlight String
  | MakeFocusModule String
  | SpotlightFunctionsReady
      { spotlight :: String -> Effect Unit
      , addDeps :: String -> Effect Unit
      , makeFocus :: String -> Effect Unit
      , reset :: Effect Unit
      }
  | SetCurrentSpotlightModule (Maybe String)
  | ToggleGridLayout
