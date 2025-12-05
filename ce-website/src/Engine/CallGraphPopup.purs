-- | Call Graph Popup - Display function call relationships and source code
-- |
-- | Data fetching is now done in PureScript via Loader.fetchCallGraphData.
-- | The JS FFI only handles DOM manipulation.
module Engine.CallGraphPopup
  ( showCallGraphPopup
  , showCallGraphPopupAsync
  , hideCallGraphPopup
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Data.Loader as Loader

-- =============================================================================
-- FFI Types (for passing data to JS)
-- =============================================================================

-- | Caller/callee info for rendering
type CallItemJS =
  { target :: String
  , targetModule :: String
  }

-- | Git metrics for rendering
type GitMetricsJS =
  { commitCount :: Int
  , daysSinceModified :: Int
  , authorCount :: Int
  , authors :: Array String
  }

-- | Complete popup data for rendering
type PopupDataJS =
  { moduleName :: String
  , declarationName :: String
  , callers :: Array CallItemJS
  , callees :: Array CallItemJS
  , sourceCode :: String  -- Empty string if not available
  , declarationKind :: String  -- Empty string if not available
  , hasGitMetrics :: Boolean
  , gitMetrics :: GitMetricsJS
  }

-- =============================================================================
-- FFI Declarations (DOM-only operations)
-- =============================================================================

-- | Show loading state in popup (no data fetch)
foreign import showPopupLoading_ :: String -> String -> Effect Unit

-- | Render popup with fetched data
foreign import renderPopupData_ :: PopupDataJS -> Effect Unit

-- | Show error state in popup
foreign import showPopupError_ :: String -> Effect Unit

-- | Hide the call graph popup
foreign import hideCallGraphPopup_ :: Effect Unit

-- =============================================================================
-- Public API
-- =============================================================================

-- | Show the call graph popup for a specific declaration
-- | This is the async version that fetches data in PureScript
showCallGraphPopupAsync :: String -> String -> Aff Unit
showCallGraphPopupAsync moduleName declarationName = do
  -- Show loading state immediately
  liftEffect $ showPopupLoading_ moduleName declarationName

  -- Fetch data in PureScript
  result <- Loader.fetchCallGraphData moduleName declarationName

  case result of
    Left err -> do
      log $ "[CallGraphPopup] Error fetching data: " <> err
      liftEffect $ showPopupError_ err

    Right callGraphData -> do
      log $ "[CallGraphPopup] Data loaded for " <> moduleName <> "." <> declarationName
      -- Convert to JS-friendly format and render
      let popupData = toPopupDataJS callGraphData
      liftEffect $ renderPopupData_ popupData

-- | Show the call graph popup (Effect version, launches async)
-- | This is the entry point called from Explorer
showCallGraphPopup :: String -> String -> Effect Unit
showCallGraphPopup moduleName declarationName =
  launchAff_ $ showCallGraphPopupAsync moduleName declarationName

-- | Hide the call graph popup
hideCallGraphPopup :: Effect Unit
hideCallGraphPopup = hideCallGraphPopup_

-- =============================================================================
-- Conversion Helpers
-- =============================================================================

-- | Convert CallGraphData to JS-friendly format
toPopupDataJS :: Loader.CallGraphData -> PopupDataJS
toPopupDataJS data_ =
  { moduleName: data_.moduleName
  , declarationName: data_.declarationName
  , callers: map toCallItemJS data_.callers
  , callees: map toCallItemJS data_.callees
  , sourceCode: fromMaybe "" data_.sourceCode
  , declarationKind: fromMaybe "" data_.declarationKind
  , hasGitMetrics: case data_.gitMetrics of
      Just _ -> true
      Nothing -> false
  , gitMetrics: case data_.gitMetrics of
      Just m -> m
      Nothing -> emptyGitMetrics
  }

toCallItemJS :: Loader.CallInfo -> CallItemJS
toCallItemJS call =
  { target: call.target
  , targetModule: call.targetModule
  }

emptyGitMetrics :: GitMetricsJS
emptyGitMetrics =
  { commitCount: 0
  , daysSinceModified: 0
  , authorCount: 0
  , authors: []
  }
