module D3.Examples.Spago.Graph where

import D3.Attributes.Sugar (classed, fill, onMouseEvent, radius, strokeColor, text, transform', viewBox, x, x1, x2, y, y1, y2)
import D3.Data.Types (D3Simulation_, Element(..), MouseEvent(..))
import D3.Examples.Spago.Model (SpagoModel, cancelSpotlight_, datum_, link_, toggleSpotlight)
import D3.FFI (configSimulation_, initSimulation_, setLinks_, setNodes_)
import D3.Interpreter (class D3InterpreterM, append, attach, modify, on, (<+>))
import D3.Layouts.Simulation (Force(..), ForceType(..), putEachForceInSimulation)
import D3.Selection (Behavior(..), DragBehavior(..), Join(..), Keys(..), node)
import D3.Simulation.Config (defaultConfigSimulation)
import D3.Simulation.Config as F
import D3.Zoom (ScaleExtent(..), ZoomExtent(..))
import Data.Number (infinity)
import Data.Tuple (Tuple(..))
import Prelude (class Bind, bind, negate, pure, unit, (/), (<<<))


packageOnlyRadialForce :: Force
packageOnlyRadialForce = Force "packageOrbit"  ForceRadial   [ F.strength datum_.onlyPackages, F.x 0.0, F.y 0.0, F.radius 1000.0 ]

unusedModuleOnlyRadialForce :: Force
unusedModuleOnlyRadialForce = Force "unusedModuleOrbit"  ForceRadial   [ F.strength datum_.onlyUnused, F.x 0.0, F.y 0.0, F.radius 600.0 ]
      
-- | recipe for this force layout graph
script :: forall m selection. 
  Bind m => 
  D3InterpreterM selection m => 
  Tuple Number Number ->
  SpagoModel ->
  m { selection :: selection, simulation :: D3Simulation_ }
script (Tuple w h) model = do
  root       <- attach "div.svg-container"
  svg        <- root `append` (node Svg    [ viewBox (-w / 2.0) (-h / 2.0) w h 
                                           , classed "graph"] )
  linksGroup <- svg  `append` (node Group  [ classed "links", strokeColor "#999" ])
  nodesGroup <- svg  `append` (node Group  [ classed "nodes" ])

  let simulation = initSimulation_ unit
      _          = simulation `configSimulation_` defaultConfigSimulation
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
                  , transform' datum_.translateNode
                  , onMouseEvent MouseClick (\e d t -> toggleSpotlight e simulation d) ]
  }

  circle  <- nodesSelection `append` (node Circle [ radius datum_.radius
                                                  , fill datum_.colorByGroup
                                                  ]) 
  labels' <- nodesSelection `append` (node Text [ classed "label",  x 0.2, y datum_.positionLabel, text datum_.name]) 
  
  _ <- linksSelection `on` Tick { name: "links", simulation, chain: [ x1 (_.x <<< link_.source)
                                                                    , y1 (_.y <<< link_.source)
                                                                    , x2 (_.x <<< link_.target)
                                                                    , y2 (_.y <<< link_.target)
                                                                    ]}
  _ <- nodesSelection `on` Tick { name: "nodes", simulation, chain: [ classed datum_.nodeClass, transform' datum_.translateNode ]}
  _ <- nodesSelection `on` Drag DefaultDrag
  _ <- svg `modify` [ onMouseEvent MouseClick (\e d t -> cancelSpotlight_ simulation) ]
  _ <- svg `on` Zoom { extent    : ZoomExtent { top: 0.0, left: 0.0 , bottom: h, right: w }
                     , scale     : ScaleExtent 0.2 2.0 -- wonder if ScaleExtent ctor could be range operator `..`
                     , name : "spago"
                     }

  pure { selection: svg, simulation }
