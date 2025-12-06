-- | ViewState - Four canonical views plus detail overlays
-- |
-- | Simplified architecture: four icon-selectable overview views,
-- | with Neighborhood/FunctionCalls as modal overlays.
-- | Detail views can be exited by clicking any overview icon.
module Engine.ViewState
  ( ViewState(..)
  , OverviewView(..)
  , DetailView(..)
  , viewLabel
  , viewDescription
  , isOverview
  , isDetail
  , toOverview
  , getBaseOverview
  ) where

import Prelude

-- =============================================================================
-- Core ADTs
-- =============================================================================

-- | The four canonical overview layouts
data OverviewView
  = TreemapView   -- All packages + all modules, clustered by package
  | TreeView      -- Used modules only, vertical tree from Main
  | ForceView     -- Used modules only, radial force layout from Main
  | TopoView      -- Packages only, topologically sorted DAG

-- | Detail views are overlays on top of the current overview
data DetailView
  = NeighborhoodDetail String  -- Module name - shows imports/dependents
  | FunctionCallsDetail String -- Module name - shows call graph

-- | Combined view state
-- | Overview: one of four canonical layouts
-- | Detail: overlay showing focused information (can be dismissed to any overview)
data ViewState
  = Overview OverviewView
  | Detail DetailView

derive instance eqOverviewView :: Eq OverviewView
derive instance eqDetailView :: Eq DetailView
derive instance eqViewState :: Eq ViewState

instance showOverviewView :: Show OverviewView where
  show TreemapView = "TreemapView"
  show TreeView = "TreeView"
  show ForceView = "ForceView"
  show TopoView = "TopoView"

-- =============================================================================
-- View Metadata
-- =============================================================================

-- | Short label for display (icon tooltip)
viewLabel :: OverviewView -> String
viewLabel TreemapView = "Grid"
viewLabel TreeView = "Tree"
viewLabel ForceView = "Radial"
viewLabel TopoView = "Packages"

-- | Longer description for the narrative panel
viewDescription :: ViewState -> String
viewDescription (Overview TreemapView) = "All modules clustered by package"
viewDescription (Overview TreeView) = "Import hierarchy from Main"
viewDescription (Overview ForceView) = "Radial import layout"
viewDescription (Overview TopoView) = "Package dependencies"
viewDescription (Detail (NeighborhoodDetail modName)) = "Neighborhood of " <> modName
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
