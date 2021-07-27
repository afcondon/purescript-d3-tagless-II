module D3.Examples.Spago.Graph where

import Control.Monad.State (class MonadState)
import D3.Attributes.Sugar (classed, fill, height, onMouseEvent, radius, remove, strokeColor, text, transform', viewBox, width, x, x1, x2, y, y1, y2)
import D3.Data.Types (D3Selection_, Element(..), MouseEvent(..))
import D3.Examples.Spago.Files (NodeType(..), SpagoGraphLinkID)
import D3.Examples.Spago.Model (SpagoModel, SpagoSimNode, cancelSpotlight_, datum_, link_, toggleSpotlight)
import D3.Node (D3_SimulationNode(..))
import D3.Selection (Behavior(..), DragBehavior(..), Join(..), node)
import D3.Simulation.Types (SimulationState_, Step(..))
import D3.Zoom (ScaleExtent(..), ZoomExtent(..))
import D3Tagless.Capabilities (class SelectionM, class SimulationM, addSelection, addTickFunction, attach, getSelection, on, setLinks, setNodes, simulationHandle, (<+>))
import D3Tagless.Capabilities as D3
import Data.Array (filter)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect, liftEffect)
import Prelude (class Bind, Unit, bind, discard, negate, pure, unit, ($), (/), (<<<))
import Utility (getWindowWidthHeight)


nodeTick = [ transform' datum_.translateNode ]
linkTick = [ x1 (_.x <<< link_.source)
           , y1 (_.y <<< link_.source)
           , x2 (_.x <<< link_.target)
           , y2 (_.y <<< link_.target)
           ]

-- TODO this is a problem once extracted from "script", leads to undefined in D3.js
enterLinks = [] -- [ classed link_.linkClass ] -- default invisible in CSS unless marked "visible"

enterNodes simulation_ = 
  [ classed datum_.nodeClass, transform' datum_.translateNode
  , onMouseEvent MouseClick (\e d t -> toggleSpotlight e simulation_ d) ]

-- | recipe for this force layout graph
script :: forall m selection. 
  Bind m => 
  MonadEffect m =>
  SelectionM selection m =>
  SimulationM selection m =>
  SpagoModel ->
  m Unit
script model = do
  (Tuple w h) <- liftEffect getWindowWidthHeight
  simulation_ <- simulationHandle

  root        <- attach "div.svg-container"
  svg         <- root D3.+ (node Svg  [ viewBox (-w / 2.0) (-h / 2.0) w h 
                                      , classed "graph"
                                      , width w, height h
                                      , onMouseEvent MouseClick (\e d t -> cancelSpotlight_ simulation_) ] )

  nodesGroup        <- svg  D3.+ (node Group  [ classed "nodes" ])
  nodesInSimulation <- setNodes model.nodes
  nodesSelection    <- nodesGroup <+> Join Group nodesInSimulation (enterNodes simulation_)

  linksGroup        <- svg  D3.+ (node Group  enterLinks)
  linksInSimulation <- setLinks model.links.treeLinks datum_.indexFunction
  linksSelection    <- linksGroup <+> Join Line linksInSimulation enterLinks

  addTickFunction "nodes" $ Step nodesSelection nodeTick
  addTickFunction "links" $ Step linksSelection linkTick

  circle <- nodesSelection D3.+ (node Circle [ radius datum_.radius, fill datum_.colorByGroup ]) 
  labels <- nodesSelection D3.+ (node Text [ classed "label",  x 0.2, y datum_.positionLabel, text datum_.name]) 
  
  _ <- circle `on` Drag DefaultDrag
  _ <- svg    `on` Zoom  { extent : ZoomExtent { top: 0.0, left: 0.0 , bottom: h, right: w }
                         , scale  : ScaleExtent 0.2 2.0 -- wonder if ScaleExtent ctor could be range operator `..`
                         , name   : "spago"
                         }
  addSelection "linksGroup" linksGroup
  -- addSelection "linksSelection" linksSelection
  addSelection "nodesGroup" nodesGroup
  -- addSelection "nodesSelection" nodesSelection
  pure unit
  
updateNodes :: forall m row. 
  Bind m => 
  MonadEffect m =>
  MonadState { simulationState :: SimulationState_ | row } m =>
  SelectionM D3Selection_ m =>
  SimulationM D3Selection_ m =>
  Array SpagoSimNode ->
  m Unit
updateNodes nodes = do
  simulation_                             <- simulationHandle
  nodesInSimulation                       <- setNodes nodes
  (maybeNodesGroup :: Maybe D3Selection_) <- getSelection "nodesGroup"

  case maybeNodesGroup of
    Nothing -> pure unit
    (Just nodesGroup) -> do
      nodesSelection <- nodesGroup <+> UpdateJoin Group nodesInSimulation { enter: enterNodes simulation_, update: [], exit: [ remove ] } 
      circle <- nodesSelection D3.+ (node Circle [ radius datum_.radius, fill datum_.colorByGroup ]) 
      labels <- nodesSelection D3.+ (node Text [ classed "label",  x 0.2, y datum_.positionLabel, text datum_.name]) 

      addTickFunction "nodes" $ Step nodesSelection nodeTick
      addSelection "nodesSelection" nodesSelection

updateLinks :: forall m row. 
  Bind m => 
  MonadEffect m =>
  MonadState { simulationState :: SimulationState_ | row } m =>
  SelectionM D3Selection_ m =>
  SimulationM D3Selection_ m =>
  Array SpagoGraphLinkID ->
  m Unit
updateLinks links = do
  linksInSimulation <- setLinks links datum_.indexFunction

  (maybeLinksGroup :: Maybe D3Selection_) <- getSelection "linksGroup"

  case maybeLinksGroup of
    Nothing -> pure unit
    (Just linksGroup) -> do
      linksSelection <- linksGroup <+> UpdateJoin Line linksInSimulation { enter: enterLinks, update: [], exit: [ remove ] }
      addTickFunction "links" $ Step linksSelection linkTick
      addSelection "linksSelection" linksSelection
  