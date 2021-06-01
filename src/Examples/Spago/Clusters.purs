module D3.Examples.Spago.Clusters where


import D3.Attributes.Sugar (classed, cx, cy, fill, lower, onMouseEvent, radius, text, viewBox, x, y)
import D3.Data.Types (D3Simulation_, Datum_, Element(..), MouseEvent(..))
import D3.Examples.Spago.Model
import D3.FFI (configSimulation_, initSimulation_, setNodes_)
import D3.Interpreter (class D3InterpreterM, append, attach, filter, modify, on, (<+>))
import D3.Layouts.Simulation (Force(..), ForceType(..), putEachForceInSimulation)
import D3.Node (NodeID, getNodeX, getNodeY)
import D3.Selection (Behavior(..), DragBehavior(..), Join(..), Keys(..), node)
import D3.Simulation.Config (D3ForceHandle_, defaultConfigSimulation)
import D3.Simulation.Config as F
import D3.Zoom (ScaleExtent(..), ZoomExtent(..))
import Data.Tuple (Tuple(..))
import Prelude (class Bind, Unit, bind, negate, pure, show, unit, ($), (/), (<$>))
import Web.Event.Internal.Types (Event)

foreign import forceClusterCollision :: Unit -> D3ForceHandle_

-- myCustomForceConfig :: Array ChainableF 
-- myCustomForceConfig = [ radius 1.0, strength 0.8, clusterPadding: 10.0 ] forceClusterCollision

spagoForcesA :: Array Force
spagoForcesA =  
  [ Force "x"       ForceX        [ F.strength 0.2, F.x datum.clusterPointX ]
  , Force "y"       ForceY        [ F.strength 0.2, F.y datum.clusterPointY ]
  , Force "collide" ForceCollide  [ F.strength 1.0, F.radius datum.collideRadius, F.iterations 1.0 ]
  ]

spagoForcesB :: Array Force
spagoForcesB =  
  [ Force "x"       ForceX        [ F.strength 0.2, F.x datum.treePointX ]
  , Force "y"       ForceY        [ F.strength 0.2, F.y datum.treePointY ]
  , Force "collide" ForceCollide  [ F.strength 1.0, F.radius datum.collideRadius, F.iterations 1.0 ]
  ]
      
-- | recipe for this force layout graph
clusterScript :: forall m selection. 
  Bind m => 
  D3InterpreterM selection m => 
  Tuple Number Number ->
  SpagoModel ->
  m { selection :: selection, simulation :: D3Simulation_ }
clusterScript (Tuple w h) model = do
  root       <- attach "div#spago"
  svg        <- root `append` (node Svg    [ viewBox (-w / 2.0) (-h / 2.0) w h
                                           , classed "cluster" ] )
  nodesGroup <- svg  `append` (node Group  [ classed "nodes" ])

  let simulation = initSimulation_ unit
      _          = simulation `configSimulation_` defaultConfigSimulation
      nodes      = simulation `setNodes_` (pinIfPackage <$> model.nodes)
      _          = simulation `putEachForceInSimulation` spagoForcesA

  nodesSelection <- nodesGroup <+> Join { -- we're putting a group in with an eye to transitions to other layouts
      element   : Group
    , key       : UseDatumAsKey
    , "data"    : nodes
    , behaviour : [ classed datum.nodeClass
                  , onMouseEvent MouseClick (\e d t -> toggleSpotlight e simulation d) ]
  }
  circle  <- nodesSelection `append` (node Circle [ radius datum.radius, fill datum.colorByGroup ]) 
  labels' <- nodesSelection `append` (node Text [ classed "label", text datum.name ])

  packagesOnly <- filter nodesSelection "g.nodes g.package"
  _ <- packagesOnly `modify` [ lower ]
  
  _ <- circle         `on` Tick { name: "nodes",  simulation, chain: [ cx getNodeX, cy getNodeY ]}
  _ <- labels'        `on` Tick { name: "labels", simulation, chain: [ x getNodeX, y getNodeY ]}
  _ <- nodesSelection `on` Drag DefaultDrag
  _ <- svg `modify` [ onMouseEvent MouseClick (\e d t -> cancelSpotlight_ simulation) ]
  _ <- svg `on` Zoom { extent : ZoomExtent { top: 0.0, left: 0.0 , bottom: h, right: w }
                     , scale  : ScaleExtent 0.2 2.0 -- wonder if ScaleExtent ctor could be range operator `..`
                     , name   : "spago"
                     }
  pure { selection: svg, simulation }

toggleSpotlight :: Event -> D3Simulation_ -> Datum_ -> Unit
toggleSpotlight event simulation d = toggleSpotlight_ event simulation nodeID nodeType
  where
    nodeID   = datum.id d
    nodeType = show $ datum.nodetype d

foreign import toggleSpotlight_ :: Event -> D3Simulation_ -> NodeID -> String -> Unit
foreign import cancelSpotlight_ :: D3Simulation_ -> Unit
