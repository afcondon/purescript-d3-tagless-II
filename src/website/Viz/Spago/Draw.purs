module D3.Viz.Spago.Draw where

import Prelude

import D3.Attributes.Sugar (classed, remove, strokeColor, transform', x1, x2, y1, y2)
import D3.Data.Types (D3Selection_, D3This_, Datum_, Element(..))
import D3.Viz.Spago.Draw.Attributes (SpagoSceneAttributes, enterAttrs, svgAttrs, updateAttrs)
import D3.Viz.Spago.Model (datum_, link_)
import D3.FFI (keyIsID_, simdrag)
import D3.Selection (Behavior(..), DragBehavior(..))
import D3.Simulation.Types (Step(..))
import D3.Zoom (ScaleExtent(..), ZoomExtent(..))
import D3Tagless.Capabilities (class SelectionM, class SimulationM, Staging, addTickFunction, appendTo, attach, mergeNewDataWithSim, mergeSelections, on, openSelection, selectUnder, setAttributes, setLinksFromSelection, setNodesFromSelection, updateJoin)
import Data.Maybe (Maybe(..))
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
  SimulationM D3Selection_ m =>
  SelectionM D3Selection_ m =>
  m { nodes :: Maybe D3Selection_, links :: Maybe D3Selection_ }
initialize = do
  (Tuple w h) <- liftEffect getWindowWidthHeight
  root  <- attach "div.svg-container" -- typeclass here determined by D3Selection_ in SimulationM

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
  
updateSimulation :: forall m d r id. 
  Eq id =>
  Bind m => 
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  SimulationM D3Selection_ m =>
  (Staging D3Selection_ d r id) ->
  SpagoSceneAttributes -> 
  m Unit
updateSimulation staging@{ selections: { nodes: Just nodesGroup, links: Just linksGroup }} attrs = do
  -- Open the selections for updating. Element type must match between:
  -- openSelection (Group/Line), updateJoin (Group/Line), and appendTo (Group/Line)
  node                  <- openSelection nodesGroup (show Group)
  link                  <- openSelection linksGroup (show Line)
  -- Merge new data with simulation (creates defensive copy for D3's join pattern)
  merged <- mergeNewDataWithSim node keyIsID_ link keyIsID_ staging.rawdata 
  -- first the nodedata
  node'                 <- updateJoin node Group merged.nodes keyIsID_
  -- put new elements (g, g.circle & g.text) into the DOM
  nodeEnter             <- appendTo node'.enter Group enterAttrs
  _ <- appendTo nodeEnter Circle attrs.circles
  void $ appendTo nodeEnter Text attrs.labels
  -- remove elements corresponding to exiting data
  setAttributes node'.exit [ remove ]
  -- change anything that needs changing on the continuing elements
  setAttributes node'.update updateAttrs 
  updateCirclesSelection <- selectUnder node'.update (show Circle)
  setAttributes updateCirclesSelection attrs.circles
  updateLabelsSelection <- selectUnder node'.update (show Text)
  setAttributes updateLabelsSelection attrs.labels
  -- now merge the update selection into the enter selection (NB other way round doesn't work)
  mergedNodeSelection   <- mergeSelections nodeEnter node'.update  -- merged enter and update becomes the `node` selection for next pass

  -- Apply drag behavior (simdrag integrates with D3 simulation for smooth physics)
  void $ mergedNodeSelection `on` Drag (CustomDrag "spago" simdrag) 
  
  -- now the linkData
  -- after merging data with existing data in sim, keyIsID_ is correct key function on links
  -- the id would be "5-8" if keyFn(link.source) == 5 && keyFn(link.target) == 8, for example
  link'                 <- updateJoin link Line merged.links keyIsID_
  -- put new element (line) into the DOM
  linkEnter             <- appendTo link'.enter Line [ classed link_.linkClass, strokeColor link_.color ]
  setAttributes linkEnter  [ classed "enter" ]
  -- remove links that are leaving
  setAttributes link'.exit    [ remove ]  
  -- update links that are staying
  setAttributes link'.update  [ classed "update" ]
  -- merge the update and enter selections for the links
  mergedlinksShown   <- mergeSelections linkEnter link'.update  -- merged enter and update becomes the `node` selection for next pass


  -- Put nodes and links into the simulation
  setNodesFromSelection mergedNodeSelection
  setLinksFromSelection mergedlinksShown staging.linksWithForce
  
  -- tick functions for each selection
  addTickFunction "nodes" $ -- NB the position of the <g> is updated, not the <circle> and <text> within it
    Step mergedNodeSelection [ transform' datum_.translateNode ]
  addTickFunction "links" $
    Step mergedlinksShown [ x1 (_.x <<< link_.source), y1 (_.y <<< link_.source), x2 (_.x <<< link_.target), y2 (_.y <<< link_.target) ]

-- alternate path, should never be used, if we can't match the selections
updateSimulation _ _ = pure unit -- something's gone badly wrong, one or both open selections (for updates) are missing

