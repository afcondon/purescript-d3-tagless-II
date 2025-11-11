module D3.Viz.ForceNavigator.Draw where

import Prelude

import PSD3.Internal.Attributes.Sugar (classed, fill, fontSize, radius, remove, strokeColor, strokeWidth, text, textAnchor, transform', viewBox, x1, x2, y1, y2)
import PSD3.Internal.Types (D3Selection_, D3This_, Datum_, Element(..))
import PSD3.Internal.Selection.Types (SelectionAttribute)
import D3.Viz.ForceNavigator.Model (Category(..), NavigationRawModel, NodeType(..))
import D3.Viz.ForceNavigator.Unsafe (unboxD3SimLink, unboxD3SimNode)
import PSD3.Internal.FFI (keyIsID_, simdrag_)
import PSD3.Internal.Selection.Types (Behavior(..), DragBehavior(..))
import PSD3.Internal.Simulation.Types (Step(..))
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach, mergeSelections, on, openSelection, selectUnder, setAttributes, simpleJoin, updateJoin)
import PSD3.Shared.ZoomableViewbox (zoomableSVG)
import PSD3.Capabilities.Simulation (class SimulationM2, SimulationUpdate, addTickFunction, update)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect, liftEffect)
import Utility (getWindowWidthHeight)
import Web.Event.Internal.Types (Event)

-- | Type-safe accessors for link data
link_ = {
    source: _.source <<< unboxD3SimLink
  , target: _.target <<< unboxD3SimLink
}

-- | Type-safe accessors for node data
datum_ = {
-- Direct accessors to fields
    id: _.id <<< unboxD3SimNode
  , label: _.label <<< unboxD3SimNode
  , nodeType: _.nodeType <<< unboxD3SimNode
  , category: _.category <<< unboxD3SimNode
  , x: _.x <<< unboxD3SimNode
  , y: _.y <<< unboxD3SimNode

-- Computed accessors
  , translateNode: \d -> do
      let node = unboxD3SimNode d
      "translate(" <> show node.x <> "," <> show node.y <> ")"

  , nodeRadius: \d -> do
      let node = unboxD3SimNode d
      case node.nodeType of
        Center -> 60.0
        Section -> 50.0
        Feature -> 30.0
        Example -> 35.0

  , nodeColor: \d -> do
      let node = unboxD3SimNode d
      case node.nodeType of
        Center -> "#1e40af"  -- dark blue
        Section -> "#3b82f6" -- blue
        Feature -> "#ec4899" -- pink
        Example -> case node.category of
          Just BasicChart -> "#3b82f6"      -- blue
          Just AdvancedLayout -> "#8b5cf6" -- purple
          Just Interactive -> "#10b981"     -- green
          Just Interpreter -> "#f59e0b"     -- amber
          Just Application -> "#ef4444"     -- red
          Nothing -> "#10b981"              -- green default

  , nodeFontSize: \d -> do
      let node = unboxD3SimNode d
      case node.nodeType of
        Center -> 16.0
        Section -> 14.0
        _ -> 11.0
}

-- | Initialize the SVG structure for the navigation force layout
-- | Creates empty groups - NO DATA. Call update() immediately after to add initial data.
-- | This follows the Spago pattern: initialize creates structure, update adds/modifies data
initialize :: forall d m.
  Bind m =>
  MonadEffect m =>
  SimulationM2 D3Selection_ m =>
  m { nodes :: Maybe (D3Selection_ d), links :: Maybe (D3Selection_ d) }
initialize = do
  (Tuple w h) <- liftEffect getWindowWidthHeight
  root <- attach "div.svg-container"

  -- Use zoomableSVG helper for consistent zoom/pan behavior
  { svg, zoomGroup } <- zoomableSVG root
    { minX: -w / 2.0
    , minY: -h / 2.0
    , width: w
    , height: h
    , svgClass: "navigation"
    , innerClass: "zoom-group"
    , innerWidth: w
    , innerHeight: h
    , scaleMin: 0.5  -- 50% minimum zoom
    , scaleMax: 4.0  -- 400% maximum zoom
    }

  linksGroup <- appendTo zoomGroup Group [ classed "link", strokeColor "#999" ]
  nodesGroup <- appendTo zoomGroup Group [ classed "node" ]

  -- Return empty groups - no data yet
  pure { nodes: Just nodesGroup, links: Just linksGroup }

-- | Update the visualization with new visible nodes/links
-- | Uses General Update Pattern (enter/update/exit) following Spago pattern
-- | TODO: Update to use new SimulationM2 update API
-- | CRITICAL: Must use mergeNewDataWithSim to preserve positions and link references
{- TEMPORARILY COMMENTED OUT - needs refactoring for new API
update :: forall row m.
  Bind m =>
  MonadEffect m =>
  SimulationM2 D3Selection_ m =>
  SelectionAttribute ->
  NavigationRawModel ->
  m Unit
update clickCallback model = do
  -- Re-attach to existing groups
  (linksGroup :: D3Selection_) <- attach "g.link"
  (nodesGroup :: D3Selection_) <- attach "g.node"

  -- Open the selections for updating. Element type must match between:
  -- openSelection (Group/Line), updateJoin (Group/Line), and appendTo (Group/Line)
  node <- openSelection nodesGroup (show Group)
  link <- openSelection linksGroup (show Line)

  -- CRITICAL: Merge new data with simulation (preserves positions, validates links, swizzles)
  -- This is what makes enter/exit work correctly with force simulations
  let rawdata :: RawData _ _ _
      rawdata = { nodes: model.nodes, links: model.links }
  merged <- mergeNewDataWithSim node keyIsID_ link keyIsID_ rawdata

  -- NODES: Update using General Update Pattern
  node' <- updateJoin node Group merged.nodes keyIsID_

  -- Enter: append circles and labels to NEW node groups only
  nodeEnter <- appendTo node'.enter Group []
  _ <- appendTo nodeEnter Circle []
  _ <- appendTo nodeEnter Text []

  -- Apply attributes to ENTER circles and labels
  enterCircles <- selectUnder nodeEnter (show Circle)
  setAttributes enterCircles [
      radius datum_.nodeRadius
    , fill datum_.nodeColor
    , strokeColor "#fff"
    , strokeWidth 3.0
    , clickCallback
    ]
  enterLabels <- selectUnder nodeEnter (show Text)
  setAttributes enterLabels [
      classed "node-label"
    , fill "#fff"
    , textAnchor "middle"
    , fontSize datum_.nodeFontSize
    , text datum_.label
    ]

  -- Exit: remove old node groups
  setAttributes node'.exit [ remove ]

  -- Update: reapply attributes to EXISTING circles and labels
  updateCircles <- selectUnder node'.update (show Circle)
  setAttributes updateCircles [
      radius datum_.nodeRadius
    , fill datum_.nodeColor
    , strokeColor "#fff"
    , strokeWidth 3.0
    , clickCallback  -- Critical: reapply click handler!
    ]
  updateLabels <- selectUnder node'.update (show Text)
  setAttributes updateLabels [
      classed "node-label"
    , fill "#fff"
    , textAnchor "middle"
    , fontSize datum_.nodeFontSize
    , text datum_.label
    ]

  -- Merge enter and update selections
  mergedNodes <- mergeSelections nodeEnter node'.update

  -- Apply drag behavior to merged selection (both new and existing nodes)
  _ <- mergedNodes `on` Drag (CustomDrag "navigation" simdrag_)

  -- LINKS: Update using General Update Pattern
  link' <- updateJoin link Line merged.links keyIsID_
  -- Enter: new links
  linkEnter <- appendTo link'.enter Line []
  setAttributes linkEnter [ strokeWidth 2.0 ]
  -- Exit: remove old links
  setAttributes link'.exit [ remove ]
  -- Update: existing links (nothing special to update)
  setAttributes link'.update []
  -- Merge enter and update for tick function
  mergedLinks <- mergeSelections linkEnter link'.update

  -- Put nodes and links into the simulation (from merged selections)
  setNodesFromSelection mergedNodes
  setLinksFromSelection mergedLinks (\_ -> true)  -- All links get force

  -- Update tick functions with merged selections
  addTickFunction "nodes" $ Step mergedNodes [ transform' datum_.translateNode ]
  addTickFunction "links" $ Step mergedLinks [
      x1 (_.x <<< link_.source)
    , y1 (_.y <<< link_.source)
    , x2 (_.x <<< link_.target)
    , y2 (_.y <<< link_.target)
    ]

  pure unit
-}
