module D3.Examples.Spago.Graph where

import D3.Attributes.Sugar (classed, fill, onMouseEvent, radius, strokeColor, text, transform', viewBox, x, x1, x2, y, y1, y2)
import D3.Data.Types (D3Simulation_, Element(..), MouseEvent(..))
import D3.Examples.Spago.Model (SpagoModel, cancelSpotlight_, datum_, link_, toggleSpotlight)
import D3.FFI (configSimulation_, initSimulation_, setLinks_, setNodes_)
import D3.Interpreter (class SelectionM, class SimulationM, Step(..), appendElement, attach, createTickFunction, modifySelection, on, (<+>))
import D3.Selection (Behavior(..), DragBehavior(..), Join(..), Keys(..), node)
import D3.Simulation.Config (defaultConfigSimulation)
import D3.Zoom (ScaleExtent(..), ZoomExtent(..))
import Data.Tuple (Tuple(..))
import Prelude (class Bind, bind, negate, pure, unit, discard, ($), (/), (<<<))


-- | recipe for this force layout graph
script :: forall m selection. 
  Bind m => 
  SelectionM selection m =>
  SimulationM m =>
  Tuple Number Number ->
  SpagoModel ->
  m { selection :: selection, simulation :: D3Simulation_ }
script (Tuple w h) model = do
  root       <- attach "div.svg-container"
  svg        <- root `appendElement` (node Svg    [ viewBox (-w / 2.0) (-h / 2.0) w h 
                                           , classed "graph"] )
  linksGroup <- svg  `appendElement` (node Group  [ classed "links", strokeColor "#999" ])
  nodesGroup <- svg  `appendElement` (node Group  [ classed "nodes" ])

  let simulation = initSimulation_ defaultConfigSimulation
      nodes      = simulation `setNodes_` model.nodes
      -- _          = simulation `putEachForceInSimulation` initialForces
      _          = setLinks_ simulation model.links (\d i -> d.id)

  linksSelection <- linksGroup <+> Join {
      element   : Line
    , key       : UseDatumAsKey
    , "data"    : model.links
    , behaviour : [ classed link_.linkClass ] -- default invisible in CSS unless marked "visible"
  }
  nodesSelection <- nodesGroup <+> Join {
      element   : Group
    , key       : UseDatumAsKey
    , "data"    : nodes
    , behaviour : [ classed datum_.nodeClass
                  , transform' datum_.translateNode ]
                  -- , onMouseEvent MouseClick (\e d t -> toggleSpotlight e simulation d) ]
  }

  circle  <- nodesSelection `appendElement` (node Circle [ radius datum_.radius
                                                  , fill datum_.colorByGroup
                                                  ]) 
  labels' <- nodesSelection `appendElement` (node Text [ classed "label",  x 0.2, y datum_.positionLabel, text datum_.name]) 
  
  createTickFunction $ Step "nodes" nodesSelection  [ classed datum_.nodeClass, transform' datum_.translateNode ]
  createTickFunction $ Step "links" linksSelection [ x1 (_.x <<< link_.source)
                                                    , y1 (_.y <<< link_.source)
                                                    , x2 (_.x <<< link_.target)
                                                    , y2 (_.y <<< link_.target)
                                                    ]
  _ <- nodesSelection `on` Drag DefaultDrag
  _ <- svg `modifySelection` [ onMouseEvent MouseClick (\e d t -> cancelSpotlight_ simulation) ]
  _ <- svg `on` Zoom { extent    : ZoomExtent { top: 0.0, left: 0.0 , bottom: h, right: w }
                     , scale     : ScaleExtent 0.2 2.0 -- wonder if ScaleExtent ctor could be range operator `..`
                     , name : "spago"
                     }

  pure { selection: svg, simulation }
