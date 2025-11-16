module D3.Viz.ForceNavigator.Draw where

import Prelude

import PSD3.Attributes (DatumFn(..))
import PSD3.Internal.Attributes.Sugar (classed, fill, fontSize, radius, remove, strokeColor, strokeWidth, text, textAnchor, transform', viewBox, x1, x2, y1, y2)
import PSD3.Internal.Types (D3Selection_, D3This_, Element(..))
import PSD3.Internal.Selection.Types (SelectionAttribute)
import D3.Viz.ForceNavigator.Model (Category(..), NavigationRawModel, NavigationSimNode, NodeType(..))
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
import Unsafe.Coerce (unsafeCoerce)

-- PHANTOM TYPE APPROACH: No more datum_/link_ accessor modules needed!
-- Use inline type annotations directly - NavigationSimNode is a plain record type (SimulationNode)

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

-- | Helper functions for node styling (using phantom types)
nodeRadius :: NavigationSimNode -> Number
nodeRadius node = case node.nodeType of
  Center -> 60.0
  Section -> 50.0
  Feature -> 30.0
  Example -> 35.0

nodeColor :: NavigationSimNode -> String
nodeColor node = case node.nodeType of
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

nodeFontSize :: NavigationSimNode -> Number
nodeFontSize node = case node.nodeType of
  Center -> 16.0
  Section -> 14.0
  _ -> 11.0

-- | Update the visualization with new visible nodes/links
-- | Uses General Update Pattern (enter/update/exit)
-- | NOTE: This uses the new SimulationM2 API with phantom types
update :: forall m.
  Bind m =>
  MonadEffect m =>
  SimulationM2 D3Selection_ m =>
  SelectionAttribute NavigationSimNode ->
  NavigationRawModel ->
  m Unit
update clickCallback model = do
  -- Re-attach to existing groups
  linksGroup <- attach "g.link"
  nodesGroup <- attach "g.node"

  -- Open the selections for updating
  node <- openSelection nodesGroup (show Group)
  link <- openSelection linksGroup (show Line)

  -- NODES: Update using General Update Pattern
  node' <- updateJoin node Group model.nodes keyIsID_

  -- Enter: append circles and labels to NEW node groups only
  nodeEnter <- appendTo node'.enter Group []
  _ <- appendTo nodeEnter Circle []
  _ <- appendTo nodeEnter Text []

  -- Apply attributes to ENTER circles and labels using phantom types
  enterCircles <- selectUnder nodeEnter (show Circle)
  setAttributes enterCircles [
      radius \(d :: NavigationSimNode) -> nodeRadius d
    , fill \(d :: NavigationSimNode) -> nodeColor d
    , strokeColor "#fff"
    , strokeWidth 3.0
    , clickCallback
    ]
  enterLabels <- selectUnder nodeEnter (show Text)
  setAttributes enterLabels [
      classed "node-label"
    , fill "#fff"
    , textAnchor "middle"
    , fontSize \(d :: NavigationSimNode) -> nodeFontSize d
    , text \(d :: NavigationSimNode) -> d.label
    ]

  -- Exit: remove old node groups
  setAttributes node'.exit [ remove ]

  -- Update: reapply attributes to EXISTING circles and labels
  updateCircles <- selectUnder node'.update (show Circle)
  setAttributes updateCircles [
      radius \(d :: NavigationSimNode) -> nodeRadius d
    , fill \(d :: NavigationSimNode) -> nodeColor d
    , strokeColor "#fff"
    , strokeWidth 3.0
    , clickCallback  -- Critical: reapply click handler!
    ]
  updateLabels <- selectUnder node'.update (show Text)
  setAttributes updateLabels [
      classed "node-label"
    , fill "#fff"
    , textAnchor "middle"
    , fontSize \(d :: NavigationSimNode) -> nodeFontSize d
    , text \(d :: NavigationSimNode) -> d.label
    ]

  -- Merge enter and update selections
  mergedNodes <- mergeSelections nodeEnter node'.update

  -- Apply drag behavior to merged selection (both new and existing nodes)
  _ <- mergedNodes `on` Drag (CustomDrag "navigation" simdrag_)

  -- LINKS: Update using General Update Pattern
  link' <- updateJoin link Line model.links keyIsID_
  -- Enter: new links
  linkEnter <- appendTo link'.enter Line []
  setAttributes linkEnter [ strokeWidth 2.0 ]
  -- Exit: remove old links
  setAttributes link'.exit [ remove ]
  -- Update: existing links (nothing special to update)
  setAttributes link'.update []
  -- Merge enter and update for tick function
  mergedLinks <- mergeSelections linkEnter link'.update

  -- Update tick functions with merged selections using phantom types
  addTickFunction "nodes" $ Step mergedNodes [
      transform' \(d :: NavigationSimNode) ->
        "translate(" <> show d.x <> "," <> show d.y <> ")"
    ]
  addTickFunction "links" $ Step mergedLinks [
      x1 (DatumFn \d -> (unsafeCoerce d).source.x :: Number)
    , y1 (DatumFn \d -> (unsafeCoerce d).source.y :: Number)
    , x2 (DatumFn \d -> (unsafeCoerce d).target.x :: Number)
    , y2 (DatumFn \d -> (unsafeCoerce d).target.y :: Number)
    ]

  pure unit
