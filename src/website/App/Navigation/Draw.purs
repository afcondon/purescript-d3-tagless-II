module PSD3.Apps.Navigation.Draw where

import Prelude

import D3.Attributes.Sugar (classed, cx, cy, fill, fontSize, radius, remove, strokeColor, strokeOpacity, strokeWidth, textAnchor, transform', viewBox, x1, x2, y1, y2)
import D3.Data.Types (D3Selection_, D3This_, Datum_, Element(..))
import D3.Viz.Navigation.Model (NavigationRawModel)
import D3.Viz.Navigation.Unsafe (unboxD3SimLink, unboxD3SimNode)
import D3.FFI (keyIsID_, simdrag)
import D3.Selection (Behavior(..), DragBehavior(..))
import D3.Simulation.Types (Step(..))
import D3.Zoom (ScaleExtent(..), ZoomExtent(..))
import D3Tagless.Capabilities (class SelectionM, class SimulationM, addTickFunction, appendTo, attach, mergeNewDataWithSim, mergeSelections, on, openSelection, selectUnder, setAttributes, setLinksFromSelection, setNodesFromSelection, updateJoin)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect, liftEffect)
import PSD3.Apps.Navigation.Actions (VizEvent(..))
import Utility (getWindowWidthHeight)
import Web.Event.Internal.Types (Event)

-- | Get event from node click
getVizEventFromClick :: Event -> Datum_ -> D3This_ -> VizEvent
getVizEventFromClick e d t = NodeClick (datum_.id d)

-- | Type-safe accessors for link data
link_ = {
    source: _.source <<< unboxD3SimLink
  , target: _.target <<< unboxD3SimLink
}

-- | Type-safe accessors for node data
datum_ = {
-- Direct accessors
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
        _ -> case node.nodeType of
          _ | node.nodeType == node.nodeType -> 50.0  -- FIXME: proper pattern match
          _ -> 35.0

  , nodeColor: \d -> do
      let node = unboxD3SimNode d
      "#3b82f6"  -- Default blue for now - FIXME: implement proper color logic

  , nodeFontSize: \d -> do
      let node = unboxD3SimNode d
      14.0  -- Default size - FIXME: implement size by type
}

-- | Initialize the SVG structure and return open selections
initialize :: forall m.
  Bind m =>
  MonadEffect m =>
  SimulationM D3Selection_ m =>
  SelectionM D3Selection_ m =>
  m { nodes :: Maybe D3Selection_, links :: Maybe D3Selection_ }
initialize = do
  (Tuple w h) <- liftEffect getWindowWidthHeight
  root <- attach "div.svg-container"

  svg <- appendTo root Svg [ viewBox (-w / 2.0) (-h / 2.0) w h, classed "navigation" ]
  inner <- appendTo svg Group []
  _ <- inner `on` Drag DefaultDrag
  _ <- svg `on` Zoom {
      extent: ZoomExtent { top: 0.0, left: 0.0, bottom: h, right: w }
    , scale: ScaleExtent 0.5 4.0
    , name: "navigation"
    , target: inner
    }

  -- Create groups for links and nodes
  linksGroup <- appendTo inner Group [ classed "links" ]
  nodesGroup <- appendTo inner Group [ classed "nodes" ]

  pure { nodes: Just nodesGroup, links: Just linksGroup }

-- | Update the simulation with new data
updateSimulation :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  SimulationM D3Selection_ m =>
  { nodes :: Maybe D3Selection_, links :: Maybe D3Selection_ } ->
  NavigationRawModel ->
  (Event -> Datum_ -> D3This_ -> VizEvent) ->
  m Unit
updateSimulation { nodes: Just nodesGroup, links: Just linksGroup } model callback = do
  -- Open selections
  node <- openSelection nodesGroup (show Group)
  link <- openSelection linksGroup (show Line)

  -- Merge new data with simulation
  merged <- mergeNewDataWithSim node keyIsID_ link keyIsID_ { nodes: model.nodes, links: model.links }

  -- Update nodes
  node' <- updateJoin node Group merged.nodes keyIsID_

  -- Create new node groups
  nodeEnter <- appendTo node'.enter Group []

  -- Add circles to new nodes
  _ <- appendTo nodeEnter Circle [
      radius datum_.nodeRadius
    , fill datum_.nodeColor
    , strokeColor "#fff"
    , strokeWidth 3.0
    ]

  -- Add labels to new nodes
  _ <- appendTo nodeEnter Text [
      textAnchor "middle"
    , fill "#fff"
    , fontSize datum_.nodeFontSize
    ]

  -- Remove exiting nodes
  setAttributes node'.exit [ remove ]

  -- Update existing nodes
  updateCircles <- selectUnder node'.update (show Circle)
  setAttributes updateCircles [
      radius datum_.nodeRadius
    , fill datum_.nodeColor
    ]

  updateLabels <- selectUnder node'.update (show Text)
  setAttributes updateLabels [
      fontSize datum_.nodeFontSize
    ]

  -- Merge enter and update selections
  mergedNodeSelection <- mergeSelections nodeEnter node'.update

  -- TODO: Set text content for labels - need to figure out how to set text
  -- labelsSelection <- selectUnder mergedNodeSelection (show Text)
  -- setText labelsSelection datum_.label

  -- Add drag behavior
  void $ mergedNodeSelection `on` Drag (CustomDrag "navigation" simdrag)

  -- Update links
  link' <- updateJoin link Line merged.links keyIsID_

  -- Create new links
  linkEnter <- appendTo link'.enter Line [
      strokeColor "#999"
    , strokeOpacity 0.6
    , strokeWidth 2.0
    ]

  -- Remove exiting links
  setAttributes link'.exit [ remove ]

  -- Merge enter and update selections for links
  mergedLinksSelection <- mergeSelections linkEnter link'.update

  -- Put nodes and links into simulation
  setNodesFromSelection mergedNodeSelection
  setLinksFromSelection mergedLinksSelection (const true)

  -- Add tick functions
  addTickFunction "nodes" $
    Step mergedNodeSelection [ transform' datum_.translateNode ]

  addTickFunction "links" $
    Step mergedLinksSelection [
        x1 (_.x <<< link_.source)
      , y1 (_.y <<< link_.source)
      , x2 (_.x <<< link_.target)
      , y2 (_.y <<< link_.target)
      ]

  pure unit

-- If selections are missing, do nothing
updateSimulation _ _ _ = pure unit
