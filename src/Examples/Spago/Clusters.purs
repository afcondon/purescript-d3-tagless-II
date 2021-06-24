module D3.Examples.Spago.Clusters where


import D3.Examples.Spago.Model

import D3.Attributes.Sugar (classed, cx, cy, fill, lower, onMouseEvent, radius, text, viewBox, x, y)
import D3.Data.Types (D3Simulation_, Element(..), MouseEvent(..))
import D3.FFI (configSimulation_, initSimulation_, setNodes_)
import D3.Interpreter (class D3InterpreterM, append, attach, filter, modify, on, (<+>))
import D3.Layouts.Simulation (Force(..), ForceType(..), putEachForceInSimulation)
import D3.Selection (Behavior(..), DragBehavior(..), Join(..), Keys(..), node)
import D3.Simulation.Config (D3ForceHandle_, defaultConfigSimulation)
import D3.Simulation.Config as F
import D3.Zoom (ScaleExtent(..), ZoomExtent(..))
import Data.Tuple (Tuple(..))
import Prelude (class Bind, Unit, bind, negate, pure, unit, (/), (<$>))

foreign import forceClusterCollision :: Unit -> D3ForceHandle_

-- myCustomForceConfig :: Array ChainableF 
-- myCustomForceConfig = [ radius 1.0, strength 0.8, clusterPadding: 10.0 ] forceClusterCollision

initialForces :: Array Force
initialForces =  
  [ Force "x"       ForceX        [ F.strength 0.2, F.x datum_.clusterPointX ]
  , Force "y"       ForceY        [ F.strength 0.2, F.y datum_.clusterPointY ]
  , Force "collide" ForceCollide  [ F.strength 1.0, F.radius datum_.collideRadius, F.iterations 1.0 ]
  ]

forcesB :: Array Force
forcesB =  
  [ Force "x"       ForceX        [ F.strength 0.2, F.x datum_.treePointX ]
  , Force "y"       ForceY        [ F.strength 0.2, F.y datum_.treePointY ]
  , Force "collide" ForceCollide  [ F.strength 1.0, F.radius datum_.collideRadius, F.iterations 1.0 ]
  ]
      
-- | recipe for this force layout graph
script :: forall m selection. 
  Bind m => 
  D3InterpreterM selection m => 
  Tuple Number Number ->
  D3Simulation_ ->
  SpagoModel ->
  m selection
script (Tuple w h) simulation model = do
  root       <- attach "div.svg-container"
  svg        <- root `append` (node Svg    [ viewBox (-w / 2.0) (-h / 2.0) w h
                                           , classed "d3svg cluster" ] )
  nodesGroup <- svg  `append` (node Group  [ classed "nodes" ])

  let _          = simulation `configSimulation_` defaultConfigSimulation
      nodes      = simulation `setNodes_` (pinIfPackage <$> model.nodes)
      _          = simulation `putEachForceInSimulation` initialForces

  nodesSelection <- nodesGroup <+> Join { -- we're putting a group in with an eye to transitions to other layouts
      element   : Group
    , key       : UseDatumAsKey
    , "data"    : nodes
    , behaviour : [ classed datum_.nodeClass
                  , onMouseEvent MouseClick (\e d t -> toggleSpotlight e simulation d) ]
  }
  circle  <- nodesSelection `append` (node Circle [ radius datum_.radius, fill datum_.colorByGroup ]) 
  labels' <- nodesSelection `append` (node Text [ classed "label", text datum_.name ])

  packagesOnly <- filter nodesSelection "g.nodes g.package"
  _ <- packagesOnly `modify` [ lower ]
  
  _ <- circle         `on` Tick { name: "nodes",  simulation, chain: [ cx datum_.x, cy datum_.y ]}
  _ <- labels'        `on` Tick { name: "labels", simulation, chain: [ x datum_.x, y datum_.y ]}
  _ <- nodesSelection `on` Drag DefaultDrag
  _ <- svg `modify` [ onMouseEvent MouseClick (\e d t -> cancelSpotlight_ simulation) ]
  _ <- svg `on` Zoom { extent : ZoomExtent { top: 0.0, left: 0.0 , bottom: h, right: w }
                     , scale  : ScaleExtent 0.2 2.0 -- wonder if ScaleExtent ctor could be range operator `..`
                     , name   : "spago"
                     }
  pure svg
