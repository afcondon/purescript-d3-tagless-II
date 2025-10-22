module D3.Viz.ForceNavigator.Draw where

import Prelude

import D3.Attributes.Sugar (classed, fill, fontSize, radius, remove, strokeColor, strokeWidth, text, textAnchor, transform', viewBox, x1, x2, y1, y2)
import D3.Data.Types (D3Selection_, D3This_, Datum_, Element(..))
import D3.Selection (SelectionAttribute)
import D3.Viz.ForceNavigator.Model (Category(..), NavigationRawModel, NodeType(..))
import D3.Viz.ForceNavigator.Unsafe (unboxD3SimLink, unboxD3SimNode)
import D3.FFI (keyIsID_, simdrag)
import D3.Selection (Behavior(..), DragBehavior(..))
import D3.Simulation.Types (Step(..))
import D3.Zoom (ScaleExtent(..), ZoomExtent(..))
import D3Tagless.Capabilities (class SelectionM, class SimulationM, addTickFunction, appendTo, attach, on, setAttributes, setLinks, setNodes, simpleJoin)
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

-- | Recipe for the navigation force layout
-- | Takes a click callback to attach to node circles
-- | Returns the node and link selections for later updates
draw :: forall row m.
  Bind m =>
  MonadEffect m =>
  SimulationM D3Selection_ m =>
  SelectionAttribute ->
  NavigationRawModel -> m { nodes :: D3Selection_, links :: D3Selection_ }
draw clickCallback model = do
  (Tuple w h) <- liftEffect getWindowWidthHeight
  (root :: D3Selection_) <- attach "div.svg-container"

  svg <- appendTo root Svg [ viewBox (-w / 2.0) (-h / 2.0) w h, classed "navigation" ]
  linksGroup <- appendTo svg Group [ classed "link", strokeColor "#999" ]
  nodesGroup <- appendTo svg Group [ classed "node" ]

  -- Load nodes and links into simulation
  nodesInSim <- setNodes model.nodes
  linksInSim <- setLinks model.links model.nodes keyIsID_

  -- Join data and set attributes
  linksSelection <- simpleJoin linksGroup Line linksInSim keyIsID_
  setAttributes linksSelection [ strokeWidth 2.0 ]

  -- Create node groups
  nodesSelection <- simpleJoin nodesGroup Group nodesInSim keyIsID_

  -- Add circles to node groups
  circlesGroup <- appendTo nodesSelection Circle []
  setAttributes circlesGroup [
      radius datum_.nodeRadius
    , fill datum_.nodeColor
    , strokeColor "#fff"
    , strokeWidth 3.0
    , clickCallback  -- Attach click event handler
    ]

  -- Add labels to node groups
  labelsGroup <- appendTo nodesSelection Text []
  setAttributes labelsGroup [
      classed "node-label"
    , fill "#fff"
    , textAnchor "middle"
    , fontSize datum_.nodeFontSize
    , text datum_.label
    ]

  -- Add tick functions
  addTickFunction "nodes" $ Step nodesSelection [ transform' datum_.translateNode ]
  addTickFunction "links" $ Step linksSelection [
      x1 (_.x <<< link_.source)
    , y1 (_.y <<< link_.source)
    , x2 (_.x <<< link_.target)
    , y2 (_.y <<< link_.target)
    ]

  -- Add drag behavior
  _ <- nodesSelection `on` Drag (CustomDrag "navigation" simdrag)

  -- Add zoom behavior
  _ <- svg `on` Zoom {
      extent: ZoomExtent { top: 0.0, left: 0.0, bottom: h, right: w }
    , scale: ScaleExtent 0.5 4.0
    , name: "navigation"
    , target: svg
    }

  pure { nodes: nodesSelection, links: linksSelection }

-- | Update the visualization with new visible nodes/links
-- | Uses General Update Pattern (enter/update/exit)
update :: forall row m.
  Bind m =>
  MonadEffect m =>
  SimulationM D3Selection_ m =>
  SelectionAttribute ->
  NavigationRawModel ->
  m Unit
update clickCallback model = do
  -- Re-attach to existing groups
  (svg :: D3Selection_) <- attach "svg.navigation"
  (linksGroup :: D3Selection_) <- attach "g.link"
  (nodesGroup :: D3Selection_) <- attach "g.node"

  -- Update simulation with new nodes/links
  nodesInSim <- setNodes model.nodes
  linksInSim <- setLinks model.links model.nodes keyIsID_

  -- Update links (enter/update/exit)
  linksSelection <- simpleJoin linksGroup Line linksInSim keyIsID_
  setAttributes linksSelection [ strokeWidth 2.0 ]

  -- Update node groups (enter/update/exit)
  nodesSelection <- simpleJoin nodesGroup Group nodesInSim keyIsID_

  -- Add circles to new node groups
  circlesGroup <- appendTo nodesSelection Circle []
  setAttributes circlesGroup [
      radius datum_.nodeRadius
    , fill datum_.nodeColor
    , strokeColor "#fff"
    , strokeWidth 3.0
    , clickCallback
    ]

  -- Add labels to new node groups
  labelsGroup <- appendTo nodesSelection Text []
  setAttributes labelsGroup [
      classed "node-label"
    , fill "#fff"
    , textAnchor "middle"
    , fontSize datum_.nodeFontSize
    , text datum_.label
    ]

  -- Update tick functions with new selections
  addTickFunction "nodes" $ Step nodesSelection [ transform' datum_.translateNode ]
  addTickFunction "links" $ Step linksSelection [
      x1 (_.x <<< link_.source)
    , y1 (_.y <<< link_.source)
    , x2 (_.x <<< link_.target)
    , y2 (_.y <<< link_.target)
    ]

  -- Add drag to new nodes
  _ <- nodesSelection `on` Drag (CustomDrag "navigation" simdrag)

  pure unit
