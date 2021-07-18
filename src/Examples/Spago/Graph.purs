module D3.Examples.Spago.Graph where

import D3.Attributes.Sugar (classed, fill, onMouseEvent, radius, strokeColor, text, transform', viewBox, x, x1, x2, y, y1, y2)
import D3.Data.Types (Datum_, Element(..), MouseEvent(..))
import D3.Examples.Spago.Model (SpagoModel, cancelSpotlight_, datum_, link_, toggleSpotlight)
import D3.Selection (Behavior(..), DragBehavior(..), Join(..), node)
import D3.Simulation.Config as F
import D3.Simulation.Forces (createForce, enableForce)
import D3.Simulation.Types (ForceType(..), Step(..))
import D3.Zoom (ScaleExtent(..), ZoomExtent(..))
import D3Tagless.Capabilities (class SelectionM, class SimulationM, addForce, addTickFunction, appendElement, attach, defaultLinkTick, on, setLinks, setNodes, simulationHandle, start, stop, (<+>))
import D3Tagless.Capabilities as D3
import D3Tagless.Instance.Simulation (exec_D3M_Simulation, run_D3M_Simulation)
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect, liftEffect)
import Prelude (class Bind, Unit, bind, discard, negate, pure, ($), (/), (<<<))
import Utility (getWindowWidthHeight)
import Web.Event.Internal.Types (Event)


-- | recipe for this force layout graph
script :: forall m selection. 
  Bind m => 
  MonadEffect m =>
  SelectionM selection m =>
  SimulationM selection m =>
  SpagoModel ->
  m selection
script model = do
  (Tuple w h) <- liftEffect getWindowWidthHeight
  simulation_ <- simulationHandle

  root        <- attach "div.svg-container"
  svg         <- root D3.+ (node Svg  [ viewBox (-w / 2.0) (-h / 2.0) w h 
                                      , classed "graph"
                                      , onMouseEvent MouseClick (\e d t -> cancelSpotlight_ simulation_) ] )

  nodesGroup        <- svg  D3.+ (node Group  [ classed "nodes" ])
  nodesInSimulation <- setNodes model.nodes
  nodesSelection    <- nodesGroup <+> Join Group nodesInSimulation [ classed datum_.nodeClass, transform' datum_.translateNode
                                                                   , onMouseEvent MouseClick (\e d t -> toggleSpotlight e simulation_ d) ]

  linksGroup        <- svg  D3.+ (node Group  [ classed "links", strokeColor "#999" ])
  linksInSimulation <- setLinks model.links datum_.indexFunction
  linksSelection    <- linksGroup <+> Join Line linksInSimulation [ classed link_.linkClass ] -- default invisible in CSS unless marked "visible"

  addTickFunction "nodes" $ Step nodesSelection  [ transform' datum_.translateNode ]
  addTickFunction "links" $ Step linksSelection [ x1 (_.x <<< link_.source)
                                                , y1 (_.y <<< link_.source)
                                                , x2 (_.x <<< link_.target)
                                                , y2 (_.y <<< link_.target)
                                                ]

  circle <- nodesSelection D3.+ (node Circle [ radius datum_.radius, fill datum_.colorByGroup ]) 
  labels <- nodesSelection D3.+ (node Text [ classed "label",  x 0.2, y datum_.positionLabel, text datum_.name]) 
  
  _ <- circle `on` Drag DefaultDrag
  _ <- svg    `on` Zoom  { extent : ZoomExtent { top: 0.0, left: 0.0 , bottom: h, right: w }
                         , scale  : ScaleExtent 0.2 2.0 -- wonder if ScaleExtent ctor could be range operator `..`
                         , name   : "spago"
                         }
  pure svg
