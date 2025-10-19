module D3.Viz.Navigation.Navigation where

import Prelude

import Control.Monad.State (class MonadState)
import D3.Attributes.Sugar (classed, cx, cy, fill, radius, remove, strokeColor, strokeOpacity, strokeWidth, transform', viewBox, x1, x2, y1, y2)
import D3.Data.Types (D3Selection_, Datum_, Element(..), Selector)
import D3.Viz.Navigation.Model (Category(..), NavigationRawModel, NodeType(..))
import D3.Viz.Navigation.Unsafe (unboxD3SimLink, unboxD3SimNode)
import D3.FFI (keyIsID_, simdrag)
import D3.Selection (Behavior(..), DragBehavior(..))
import D3.Simulation.Types (D3SimulationState_, SimVariable(..), Step(..))
import D3.Zoom (ScaleExtent(..), ZoomExtent(..))
import D3Tagless.Capabilities (class SimulationM, addTickFunction, appendTo, attach, on, setAttributes, setConfigVariable, setLinks, setNodes, simpleJoin)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect, liftEffect)
import Utility (getWindowWidthHeight)

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
  , children: _.children <<< unboxD3SimNode
  , url: _.url <<< unboxD3SimNode
  , external: _.external <<< unboxD3SimNode
  , description: _.description <<< unboxD3SimNode
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

  , fontSize: \d -> do
      let node = unboxD3SimNode d
      case node.nodeType of
        Center -> 16.0
        Section -> 14.0
        _ -> 11.0
}

-- | Recipe for the navigation force layout
draw :: forall row m.
  Bind m =>
  MonadEffect m =>
  MonadState { simulation :: D3SimulationState_ | row } m =>
  SimulationM D3Selection_ m =>
  NavigationRawModel -> Selector D3Selection_ -> m Unit
draw model selector = do
  (Tuple w h) <- liftEffect getWindowWidthHeight
  (root :: D3Selection_) <- attach selector

  svg <- appendTo root Svg [ viewBox (-w / 2.0) (-h / 2.0) w h, classed "navigation" ]
  linksGroup <- appendTo svg Group [ classed "link", strokeColor "#999", strokeOpacity 0.6 ]
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
    ]

  -- Add labels to node groups
  labelsGroup <- appendTo nodesSelection Text []
  setAttributes labelsGroup [
      classed "node-label"
    , fill "#fff"
    , strokeWidth 0.0
    ]

  -- Set label text content (this needs to be done differently - text content is not an attribute)
  -- For now we'll handle this in the tick function or initial setup

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

  setConfigVariable $ Alpha 1.0
  pure unit
