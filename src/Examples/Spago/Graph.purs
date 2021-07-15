module D3.Examples.Spago.Graph where

import D3.Attributes.Sugar (classed, fill, radius, strokeColor, text, transform', viewBox, x, x1, x2, y, y1, y2)
import D3.Data.Types (Element(..))
import D3.Examples.Spago.Model (SpagoModel, datum_, link_)
import D3.Selection (Behavior(..), DragBehavior(..), Join(..), node)
import D3.Simulation.Types (Step(..))
import D3.Zoom (ScaleExtent(..), ZoomExtent(..))
import D3Tagless.Capabilities (class SelectionM, class SimulationM, addTickFunction, appendElement, attach, defaultLinkTick, on, setLinks, setNodes, (<+>))
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect, liftEffect)
import Prelude (class Bind, bind, discard, negate, pure, ($), (/), (<<<))
import Utility (getWindowWidthHeight)


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
  root        <- attach "div.svg-container"
  svg         <- root `appendElement` (node Svg
                                      [ viewBox (-w / 2.0) (-h / 2.0) w h 
                                      , classed "graph"] )
  linksGroup  <- svg  `appendElement` (node Group  [ classed "links", strokeColor "#999" ])
  nodesGroup  <- svg  `appendElement` (node Group  [ classed "nodes" ])
  
  nodesInSimulation <- setNodes model.nodes
  linksInSimulation <- setLinks model.links

  linksSelection <- linksGroup <+> Join Line linksInSimulation [ classed link_.linkClass ] -- default invisible in CSS unless marked "visible"
  
  nodesSelection <- nodesGroup <+> Join Group nodesInSimulation [ classed datum_.nodeClass, transform' datum_.translateNode ]
    -- TODO -- , onMouseEvent MouseClick (\e d t -> toggleSpotlight e simulation d) ]
  

  circle  <- nodesSelection `appendElement` (node Circle [ radius datum_.radius, fill datum_.colorByGroup ]) 
  labels' <- nodesSelection `appendElement` (node Text [ classed "label",  x 0.2, y datum_.positionLabel, text datum_.name]) 
  
  addTickFunction "nodes" $ Step nodesSelection  [ transform' datum_.translateNode ]
  -- defaultLinkTick "links" linksSelection
  addTickFunction "links" $ Step linksSelection [ x1 (_.x <<< link_.source)
                                                , y1 (_.y <<< link_.source)
                                                , x2 (_.x <<< link_.target)
                                                , y2 (_.y <<< link_.target)
                                                ]
  _ <- nodesSelection `on` Drag DefaultDrag
  -- TODO this callback for the mouseclick needs access to the simulation
  -- _ <- svg `modifySelection` [ onMouseEvent MouseClick (\e d t -> cancelSpotlight_ simulation) ]
  _ <- svg `on` Zoom { extent    : ZoomExtent { top: 0.0, left: 0.0 , bottom: h, right: w }
                     , scale     : ScaleExtent 0.2 2.0 -- wonder if ScaleExtent ctor could be range operator `..`
                     , name : "spago"
                     }

  pure svg
