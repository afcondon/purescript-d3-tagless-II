module D3.Viz.Spago.Draw where

import Prelude

import PSD3.Internal.Attributes.Sugar (classed, remove, strokeColor, transform', x1, x2, y1, y2)
import PSD3.Internal.Types (D3Selection_, D3This_, Datum_, Element(..))
import D3.Viz.Spago.Draw.Attributes (SpagoSceneAttributes, enterAttrs, svgAttrs, updateAttrs)
import D3.Viz.Spago.Model (datum_, link_)
import PSD3.Internal.FFI (keyIsID_, simdrag_)
import PSD3.Internal.Selection.Types (Behavior(..), DragBehavior(..))
import PSD3.Internal.Simulation.Types (Step(..))
import PSD3.Internal.Zoom (ScaleExtent(..), ZoomExtent(..))
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach, mergeSelections, on, openSelection, selectUnder, setAttributes, updateJoin)
import PSD3.Capabilities.Simulation (class SimulationM2, SimulationUpdate, addTickFunction, update)
import PSD3.Internal.Attributes.Instances (Label)
import PSD3.Data.Node (D3_SimulationNode, D3Link_Unswizzled, D3Link_Swizzled)
import Data.Array (filter)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect, liftEffect)
import PSD3.CodeExplorer.Actions (VizEvent(..))
import Utility (getWindowWidthHeight)
import Web.Event.Internal.Types (Event)

getVizEventFromClick :: Event -> Datum_ -> D3This_ -> VizEvent
getVizEventFromClick e d t = NodeClick (datum_.nodetype d) (datum_.id d)

-- | recipe for this force layout graph
initialize :: forall m.
  Bind m =>
  MonadEffect m =>
  SimulationM2 D3Selection_ m =>
  SelectionM D3Selection_ m =>
  m { nodes :: Maybe D3Selection_, links :: Maybe D3Selection_ }
initialize = do
  (Tuple w h) <- liftEffect getWindowWidthHeight
  root  <- attach "div.svg-container" -- typeclass here determined by D3Selection_ in SimulationM2

  svg   <- appendTo root Svg (svgAttrs w h) 
  inner <- appendTo svg  Group []
  _     <- inner `on` Drag DefaultDrag
  _     <- svg   `on` Zoom { extent : ZoomExtent { top: 0.0, left: 0.0 , bottom: h, right: w }
                           , scale  : ScaleExtent 0.1 4.0 -- wonder if ScaleExtent ctor could be range operator `..`
                           , name   : "spago"
                           , target : inner
                           }
  -- create the <g>'s to hold the nodes and links and pass these selection onward
  -- so that the data can be joined here each time it is changed
  -- NB links first because it looks best if links under nodes, but can be changed later with setAttributes linksGroup [ raise ] 
  linksGroup <- appendTo inner Group [ classed "links" ] 
  nodesGroup <- appendTo inner Group [ classed "nodes" ]

  pure { nodes: Just nodesGroup, links: Just linksGroup }

-- | Update simulation using the new declarative update API
-- |
-- | This replaces the old updateSimulation that manually managed data merging,
-- | link swizzling, and force engagement. Now the `update` function handles all
-- | internal complexity, we just do DOM operations and tick functions.
updateSimulation :: forall m d.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  SimulationM2 D3Selection_ m =>
  { nodes :: Maybe D3Selection_
  , links :: Maybe D3Selection_
  } ->
  { nodes :: Array (D3_SimulationNode d)
  , links :: Array D3Link_Unswizzled
  , activeForces :: Set Label
  , linksWithForce :: Datum_ -> Boolean
  } ->
  SpagoSceneAttributes ->
  m Unit
updateSimulation { nodes: Just nodesGroup, links: Just linksGroup } dataConfig attrs = do
  -- Step 1: Use the new update API to handle data merging, swizzling, and force engagement
  -- This returns enhanced data with swizzled links ready for DOM binding
  enhanced <- update
    { nodes: Just dataConfig.nodes
    , links: Just dataConfig.links
    , activeForces: Just dataConfig.activeForces
    , config: Nothing
    , keyFn: keyIsID_
    }

  -- Step 2: Open the selections for DOM operations
  node <- openSelection nodesGroup (show Group)
  link <- openSelection linksGroup (show Line)

  -- Step 3: Apply General Update Pattern to nodes
  node' <- updateJoin node Group enhanced.nodes keyIsID_

  -- Enter: create new groups with circles and text
  nodeEnter <- appendTo node'.enter Group enterAttrs
  _ <- appendTo nodeEnter Circle attrs.circles
  void $ appendTo nodeEnter Text attrs.labels

  -- Exit: remove old nodes
  setAttributes node'.exit [ remove ]

  -- Update: modify existing nodes
  setAttributes node'.update updateAttrs
  updateCirclesSelection <- selectUnder node'.update (show Circle)
  setAttributes updateCirclesSelection attrs.circles
  updateLabelsSelection <- selectUnder node'.update (show Text)
  setAttributes updateLabelsSelection attrs.labels

  -- Merge enter and update selections
  mergedNodeSelection <- mergeSelections nodeEnter node'.update

  -- Apply drag behavior
  void $ mergedNodeSelection `on` Drag (CustomDrag "spago" simdrag_)

  -- Step 4: Apply General Update Pattern to links
  -- NOTE: enhanced.links are D3LinkSwizzled, not Datum_, so we can't apply dataConfig.linksWithForce here
  -- TODO: In future, implement fine-grained link force filtering in the update API
  -- For now, all swizzled links are displayed
  link' <- updateJoin link Line enhanced.links keyIsID_

  -- Enter: create new lines
  linkEnter <- appendTo link'.enter Line [ classed link_.linkClass, strokeColor link_.color ]
  setAttributes linkEnter [ classed "enter" ]

  -- Exit: remove old links
  setAttributes link'.exit [ remove ]

  -- Update: modify existing links
  setAttributes link'.update [ classed "update" ]

  -- Merge enter and update selections
  mergedLinksShown <- mergeSelections linkEnter link'.update

  -- Step 5: Set up tick functions for animation
  -- Note: We always add tick functions with the same labels, which replaces them
  -- This is necessary because selections change when doing enter/update/exit
  addTickFunction "nodes" $
    Step mergedNodeSelection [ transform' datum_.translateNode ]
  addTickFunction "links" $
    Step mergedLinksShown [ x1 (_.x <<< link_.source), y1 (_.y <<< link_.source), x2 (_.x <<< link_.target), y2 (_.y <<< link_.target) ]

-- Fallback when selections are missing
updateSimulation _ _ _ = pure unit

