-- | ViewState - Four canonical views plus detail overlays
-- |
-- | Simplified architecture: four icon-selectable overview views,
-- | with Neighborhood/FunctionCalls as modal overlays.
-- | Detail views can be exited by clicking any overview icon.
module Engine.ViewState
  ( ViewState(..)
  , OverviewView(..)
  , DetailView(..)
  , NeighborhoodViewType(..)
  , viewLabel
  , viewDescription
  , neighborhoodViewLabel
  , isOverview
  , isDetail
  , toOverview
  , getBaseOverview
  , getNeighborhoodModule
  , getNeighborhoodViewType
  ) where

import Prelude

import Data.Maybe (Maybe(..))

-- =============================================================================
-- Core ADTs
-- =============================================================================

-- | The four canonical overview layouts
data OverviewView
  = TreemapView   -- All packages + all modules, clustered by package
  | TreeView      -- Used modules only, vertical tree from Main
  | ForceView     -- Used modules only, radial force layout from Main
  | TopoView      -- Packages only, topologically sorted DAG

-- | Sub-view types for neighborhood detail
data NeighborhoodViewType
  = BubblePackView  -- Default: bubble pack with function circles
  | ChordView       -- Chord diagram showing all connections
  | MatrixView      -- Adjacency matrix
  | TriptychView    -- All three views side-by-side

-- | Detail views are overlays on top of the current overview
data DetailView
  = NeighborhoodDetail String NeighborhoodViewType  -- Module name + view type
  | PackageNeighborhoodDetail String  -- Package name - shows modules in package + their deps
  | FunctionCallsDetail String -- Module name - shows call graph

-- | Combined view state
-- | Overview: one of four canonical layouts
-- | Detail: overlay showing focused information (can be dismissed to any overview)
data ViewState
  = Overview OverviewView
  | Detail DetailView

derive instance eqOverviewView :: Eq OverviewView
derive instance eqNeighborhoodViewType :: Eq NeighborhoodViewType
derive instance eqDetailView :: Eq DetailView
derive instance eqViewState :: Eq ViewState

instance showOverviewView :: Show OverviewView where
  show TreemapView = "TreemapView"
  show TreeView = "TreeView"
  show ForceView = "ForceView"
  show TopoView = "TopoView"

instance showNeighborhoodViewType :: Show NeighborhoodViewType where
  show BubblePackView = "BubblePackView"
  show ChordView = "ChordView"
  show MatrixView = "MatrixView"
  show TriptychView = "TriptychView"

-- =============================================================================
-- View Metadata
-- =============================================================================

-- | Short label for display (icon tooltip)
viewLabel :: OverviewView -> String
viewLabel TreemapView = "Grid"
viewLabel TreeView = "Tree"
viewLabel ForceView = "Radial"
viewLabel TopoView = "Packages"

-- | Short label for neighborhood sub-views
neighborhoodViewLabel :: NeighborhoodViewType -> String
neighborhoodViewLabel BubblePackView = "Bubbles"
neighborhoodViewLabel ChordView = "Chord"
neighborhoodViewLabel MatrixView = "Matrix"
neighborhoodViewLabel TriptychView = "All"

-- | Longer description for the narrative panel
viewDescription :: ViewState -> String
viewDescription (Overview TreemapView) = "All modules clustered by package"
viewDescription (Overview TreeView) = "Import hierarchy from Main"
viewDescription (Overview ForceView) = "Radial import layout"
viewDescription (Overview TopoView) = "Package dependencies"
viewDescription (Detail (NeighborhoodDetail modName viewType)) =
  "Neighborhood of " <> modName <> " (" <> neighborhoodViewLabel viewType <> ")"
viewDescription (Detail (PackageNeighborhoodDetail pkgName)) =
  "Modules in " <> pkgName
viewDescription (Detail (FunctionCallsDetail modName)) = "Function calls in " <> modName

-- =============================================================================
-- View Classification
-- =============================================================================

-- | Is this an overview (not a detail overlay)?
isOverview :: ViewState -> Boolean
isOverview (Overview _) = true
isOverview (Detail _) = false

-- | Is this a detail overlay?
isDetail :: ViewState -> Boolean
isDetail = not <<< isOverview

-- | Convert an overview enum to ViewState
toOverview :: OverviewView -> ViewState
toOverview = Overview

-- | Get the base overview for navigation purposes
-- | Detail views return a default overview (TreemapView)
getBaseOverview :: ViewState -> OverviewView
getBaseOverview (Overview ov) = ov
getBaseOverview (Detail _) = TreemapView  -- Detail views exit to Treemap by default

-- | Get the module name from a neighborhood detail view
getNeighborhoodModule :: ViewState -> Maybe String
getNeighborhoodModule (Detail (NeighborhoodDetail modName _)) = Just modName
getNeighborhoodModule _ = Nothing

-- | Get the view type from a neighborhood detail view
getNeighborhoodViewType :: ViewState -> Maybe NeighborhoodViewType
getNeighborhoodViewType (Detail (NeighborhoodDetail _ viewType)) = Just viewType
getNeighborhoodViewType _ = Nothing
