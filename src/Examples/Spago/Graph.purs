module D3.Examples.Spago.Graph where

import D3.Attributes.Sugar (classed, fill, onMouseEvent, radius, strokeColor, text, transform', viewBox, x, x1, x2, y, y1, y2)
import D3.Data.Types (D3Simulation_, Element(..), MouseEvent(..))
import D3.Examples.Spago.Model (SpagoModel, cancelSpotlight_, datum, link, toggleSpotlight)
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

initialForces :: Array Force
initialForces =  
  [ Force "charge"  ForceManyBody [ F.strength (-60.0), F.theta 0.9, F.distanceMin 1.0, F.distanceMax infinity ]
  , Force "x"       ForceX        [ F.strength 0.1, F.x 0.0 ]
  , Force "y"       ForceY        [ F.strength 0.1, F.y 0.0 ]
  , Force "center"  ForceCenter   [ F.strength 0.5, F.x 0.0, F.y 0.0 ]
  , Force "collide" ForceCollide  [ F.strength 1.0, F.radius datum.collideRadius, F.iterations 1.0 ]
  ]

packageOnlyRadialForce :: Force
packageOnlyRadialForce = Force "packageOrbit"  ForceRadial   [ F.strength datum.onlyPackages, F.x 0.0, F.y 0.0, F.radius 1000.0 ]

unusedModuleOnlyRadialForce :: Force
unusedModuleOnlyRadialForce = Force "unusedModuleOrbit"  ForceRadial   [ F.strength datum.onlyUnused, F.x 0.0, F.y 0.0, F.radius 600.0 ]
      
-- | recipe for this force layout graph
script :: forall m selection. 
  Bind m => 
  D3InterpreterM selection m => 
  Tuple Number Number ->
  SpagoModel ->
  m { selection :: selection, simulation :: D3Simulation_ }
script (Tuple w h) model = do
  root       <- attach "div#spago"
  svg        <- root `append` (node Svg    [ viewBox (-w / 2.0) (-h / 2.0) w h 
                                           , classed "graph"] )
  linksGroup <- svg  `append` (node Group  [ classed "links", strokeColor "#999" ])
  nodesGroup <- svg  `append` (node Group  [ classed "nodes" ])

  let simulation = initSimulation_ unit
      _          = simulation `configSimulation_` defaultConfigSimulation
      nodes      = simulation `setNodes_` model.nodes
      _          = simulation `putEachForceInSimulation` initialForces
      _          = setLinks_ simulation model.links (\d i -> d.id)

  linksSelection <- linksGroup <+> Join {
      element   : Line
    , key       : UseDatumAsKey
    , "data"    : model.links
    , behaviour : [ classed link.linkClass ] -- default invisible in CSS unless marked "visible"
  }
  nodesSelection <- nodesGroup <+> Join {
      element   : Group
    , key       : UseDatumAsKey
    , "data"    : nodes
    , behaviour : [ classed datum.nodeClass
                  , transform' datum.translateNode
                  , onMouseEvent MouseClick (\e d t -> toggleSpotlight e simulation d) ]
  }

  circle  <- nodesSelection `append` (node Circle [ radius datum.radius
                                                  , fill datum.colorByGroup
                                                  ]) 
  labels' <- nodesSelection `append` (node Text [ classed "label",  x 0.2, y datum.positionLabel, text datum.name]) 
  
  _ <- linksSelection `on` Tick { name: "links", simulation, chain: [ x1 (_.x <<< link.source)
                                                                    , y1 (_.y <<< link.source)
                                                                    , x2 (_.x <<< link.target)
                                                                    , y2 (_.y <<< link.target)
                                                                    ]}
  _ <- nodesSelection `on` Tick { name: "nodes", simulation, chain: [ classed datum.nodeClass, transform' datum.translateNode ]}
  _ <- nodesSelection `on` Drag DefaultDrag
  _ <- svg `modify` [ onMouseEvent MouseClick (\e d t -> cancelSpotlight_ simulation) ]
  _ <- svg `on` Zoom { extent    : ZoomExtent { top: 0.0, left: 0.0 , bottom: h, right: w }
                     , scale     : ScaleExtent 0.2 2.0 -- wonder if ScaleExtent ctor could be range operator `..`
                     , name : "spago"
                     }

  pure { selection: svg, simulation }
