module D3.Examples.Spago.Graph where

import Control.Monad.State (class MonadState, get)
import D3.Attributes.Sugar (AlignAspectRatio_X(..), AlignAspectRatio_Y(..), AspectRatioPreserve(..), AspectRatioSpec(..), classed, cursor, fill, height, onMouseEvent, preserveAspectRatio, radius, remove, strokeColor, text, textAnchor, transform', viewBox, width, x, x1, x2, y, y1, y2)
import D3.Data.Tree (TreeLayout(..))
import D3.Data.Types (D3Selection_, Datum_, Element(..), MouseEvent(..))
import D3.Examples.Spago.Files (NodeType(..), SpagoGraphLinkID)
import D3.Examples.Spago.Model (SpagoModel, SpagoSimNode, cancelSpotlight_, datum_, isPackage, link_, toggleSpotlight, tree_datum_)
import D3.Layouts.Hierarchical (horizontalLink', radialLink, verticalLink)
import D3.Node (D3_SimulationNode(..))
import D3.Selection (Behavior(..), ChainableS, DragBehavior(..), Join(..), node, node_)
import D3.Simulation.Types (SimulationState_(..), Step(..))
import D3.Zoom (ScaleExtent(..), ZoomExtent(..))
import D3Tagless.Capabilities (class SelectionM, class SimulationM, addSelection, addTickFunction, attach, getLinks, getNodes, getSelection, modifySelection, on, setLinks, setNodes, simulationHandle, (<+>))
import D3Tagless.Capabilities as D3
import Data.Array (filter)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Debug (trace)
import Effect.Class (class MonadEffect, liftEffect)
import Prelude (class Bind, Unit, bind, discard, negate, pure, unit, ($), (/), (<<<))
import Unsafe.Coerce (unsafeCoerce)
import Utility (getWindowWidthHeight)

-- for this (family of) visualization(s) the position updating for links and nodes is always the same
nodeTick = [ transform' datum_.translateNode ]
linkTick = [ x1 (_.x <<< link_.source)
           , y1 (_.y <<< link_.source)
           , x2 (_.x <<< link_.target)
           , y2 (_.y <<< link_.target)
           ]

-- TODO this is a problem once extracted from "script", leads to undefined in D3.js
enterLinks = [] -- [ classed link_.linkClass ] -- default invisible in CSS unless marked "visible"

enterAttrs simulation_ = 
  [ classed datum_.nodeClass, transform' datum_.translateNode
  , onMouseEvent MouseClick (\e d t -> toggleSpotlight e simulation_ d) ]
updateAttrs simulation_ = 
  [ classed datum_.nodeClass, transform' datum_.translateNode ]

-- | recipe for this force layout graph
setup :: forall m selection. 
  Bind m => 
  MonadEffect m =>
  SelectionM selection m =>
  SimulationM selection m =>
  -- SpagoModel ->
  m Unit
setup = do
  (Tuple w h) <- liftEffect getWindowWidthHeight
  simulation_ <- simulationHandle -- needed for click handler to stop / start simulation
  root        <- attach "div.svg-container"
  svg         <- root D3.+ (node Svg  [ viewBox (-w / 2.1) (-h / 2.05) w h 
                                      -- , preserveAspectRatio $ AspectRatio XMid YMid Meet 
                                      , classed "overlay"
                                      , width w, height h
                                      , cursor "grab"
                                      , onMouseEvent MouseClick (\e d t -> cancelSpotlight_ simulation_) ] )
  inner    <- svg D3.+ (node_ Group)
  _        <- inner `on` Drag DefaultDrag
  -- because the zoom event is picked up by `svg` but applied to `inner`, next line must be after previous
  _        <- svg `on` Zoom  {  extent : ZoomExtent { top: 0.0, left: 0.0 , bottom: h, right: w }
                                      , scale  : ScaleExtent 0.1 4.0 -- wonder if ScaleExtent ctor could be range operator `..`
                                      , name   : "spago"
                                      , target : inner
                                      }

  linksGroup  <- inner  D3.+ (node Group [ classed "links" ])
  nodesGroup  <- inner  D3.+ (node Group [ classed "nodes" ])
  
  addSelection "nodesGroup" nodesGroup
  addSelection "linksGroup" linksGroup
  pure unit

-- | Some examples of pre-packaged attribute sets available to the app maker
circleAttrs1 = [ radius datum_.radius, fill datum_.colorByGroup ]
circleAttrs2 = [ radius 3.0, fill datum_.colorByUsage ]
labelsAttrs1 = [ classed "label",  x 0.2, y datum_.positionLabel, textAnchor "middle", text datum_.indexAndID]
-- TODO x and y position for label would also depend on "hasChildren"
labelsAttrsH = [ classed "label",  x 4.0, y 2.0, textAnchor (tree_datum_.textAnchor Horizontal), text datum_.name]
graphAttrs = { circle: circleAttrs1, labels: labelsAttrs1 }
treeAttrs  = { circle: circleAttrs2, labels: labelsAttrsH }

updateNodes :: forall m row. 
  Bind m => 
  MonadEffect m =>
  MonadState { simulationState :: SimulationState_ | row } m =>
  SelectionM D3Selection_ m =>
  SimulationM D3Selection_ m =>
  Array SpagoSimNode ->
  { circle :: Array ChainableS, labels :: Array ChainableS } -> 
  m Unit
updateNodes nodes attrs = do
  simulation_       <- simulationHandle
  nodesInSimulation <- setNodes nodes
  maybeNodesGroup   <- getSelection "nodesGroup"

  case maybeNodesGroup of
    Nothing -> pure unit
    (Just nodesGroup) -> do
      let _ = trace { updateNodes: nodesInSimulation } \_ -> unit
      nodesSelection <- nodesGroup D3.<+> UpdateJoin Group nodesInSimulation { enter: enterAttrs simulation_
                                                                              , update: updateAttrs simulation_
                                                                              , exit: [ classed "remove" ] }
                                                                              -- , exit: [ remove ] }
      circle         <- nodesSelection D3.+ (node Circle attrs.circle)
      labels         <- nodesSelection D3.+ (node Text attrs.labels) 
      _              <- circle `on` Drag DefaultDrag

      addTickFunction "nodes" $ Step nodesSelection nodeTick
      addSelection "nodesSelection" nodesSelection

updateGraphLinks :: forall m row. 
  Bind m => 
  MonadEffect m =>
  MonadState { simulationState :: SimulationState_ | row } m =>
  SelectionM D3Selection_ m =>
  SimulationM D3Selection_ m =>
  Array SpagoGraphLinkID ->
  m Unit
updateGraphLinks links = do
  linksInSimulation <- setLinks links datum_.indexFunction -- NB this is the model-defined way of getting the index function for the NodeID -> object reference swizzling that D3 does when you set the links
  (maybeLinksGroup :: Maybe D3Selection_) <- getSelection "linksGroup"
    
  case maybeLinksGroup of
    Nothing -> pure unit
    (Just linksGroup) -> do
      -- TODO the links need valid IDs too if they are to do general update pattern, probably best to actually make them when making the model
      linksSelection <- linksGroup D3.<+> UpdateJoin Line linksInSimulation { enter: [ classed link_.linkClass, strokeColor link_.color ], update: [ classed "graphlinkSimUpdate" ], exit: [ remove ] }
      addTickFunction "links" $ Step linksSelection linkTick
      addSelection "graphlinksSelection" linksSelection

  pure unit
  
updateGraphLinks' :: forall m row. 
  Bind m => 
  MonadEffect m =>
  MonadState { simulationState :: SimulationState_ | row } m =>
  SelectionM D3Selection_ m =>
  SimulationM D3Selection_ m =>
  Array SpagoGraphLinkID ->
  m Unit
updateGraphLinks' links = do
  (maybeLinksGroup :: Maybe D3Selection_) <- getSelection "linksGroup"
    
  case maybeLinksGroup of
    Nothing -> pure unit
    (Just linksGroup) -> do
      linksSelection <- linksGroup D3.<+> UpdateJoin Line links { enter: [ classed link_.linkClass, strokeColor link_.color ], update: [ classed "graphlinkUpdate" ], exit: [ remove ] }
      addTickFunction "links" $ Step linksSelection linkTick
      addSelection "graphlinksSelection" linksSelection

  pure unit
  
updateTreeLinks :: forall m row. 
  Bind m => 
  MonadEffect m =>
  MonadState { simulationState :: SimulationState_ | row } m =>
  SelectionM D3Selection_ m =>
  SimulationM D3Selection_ m =>
  Array SpagoGraphLinkID ->
  TreeLayout -> 
  m Unit
updateTreeLinks links layout = do
  linksInSimulation <- setLinks links datum_.indexFunction

  let linkPath =
        case layout of
          Horizontal -> horizontalLink' -- the ' is because current library default horizontalLink flips x&y (tree examples written that way, should be changed)
          Radial     -> radialLink datum_.x datum_.y
          Vertical   -> verticalLink

  (maybeLinksGroup :: Maybe D3Selection_) <- getSelection "linksGroup"
  case maybeLinksGroup of
    Nothing -> pure unit
    (Just linksGroup) -> do
      linksSelection <- linksGroup D3.<+> UpdateJoin Path linksInSimulation { enter: [ classed link_.linkClass, strokeColor link_.color, linkPath ], update: [ classed "treelinkUpdate" ], exit: [ remove ] }
      addTickFunction "links" $ Step linksSelection linkTick
      addSelection "treelinksSelection" linksSelection

  pure unit
  
removeNamedSelection :: forall m row. 
  Bind m => 
  MonadEffect m =>
  MonadState { simulationState :: SimulationState_ | row } m =>
  SelectionM D3Selection_ m =>
  SimulationM D3Selection_ m =>
  String -> 
  m Unit
removeNamedSelection name = do
  (maybeSelection :: Maybe D3Selection_) <- getSelection name
  case maybeSelection of
    Nothing -> pure unit
    (Just selection) -> do
      modifySelection selection [ remove ]

  pure unit
  