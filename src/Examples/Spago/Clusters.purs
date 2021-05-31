module D3.Examples.Spago.Clusters where

import D3.Attributes.Sugar (classed, cx, cy, fill, lower, onMouseEvent, radius, text, viewBox, x, y)
import D3.Data.Types (D3Simulation_, Datum_, Element(..), MouseEvent(..))
import D3.Examples.Spago.Attributes (colorByGroup, datumDotRadius, nodeClass)
import D3.Examples.Spago.Model (SpagoModel, chooseFocusFromSpagoSimNodeX, chooseFocusFromSpagoSimNodeY, getIdFromSpagoSimNode, getNameFromSpagoSimNode, getNodetypeFromSimNode, pinIfPackage)
import D3.FFI (configSimulation_, d3FilterSelection_, initSimulation_, setNodes_)
import D3.Interpreter (class D3InterpreterM, append, attach, filter, modify, on, (<+>))
import D3.Layouts.Simulation (Force(..), ForceType(..), putEachForceInSimulation)
import D3.Node (NodeID, getNodeX, getNodeY)
import D3.Selection (Behavior(..), DragBehavior(..), Join(..), Keys(..), node)
import D3.Simulation.Config (D3ForceHandle_, defaultConfigSimulation)
import D3.Simulation.Config as F
import D3.Zoom (ScaleExtent(..), ZoomExtent(..))
import Data.Tuple (Tuple(..))
import Prelude (class Bind, Unit, bind, negate, pure, show, unit, (/), (<$>))
import Unsafe.Coerce (unsafeCoerce)

foreign import forceClusterCollision :: Unit -> D3ForceHandle_

-- myCustomForceConfig :: Array ChainableF 
-- myCustomForceConfig = [ radius 1.0, strength 0.8, clusterPadding: 10.0 ] forceClusterCollision

fixed500 :: Datum_ -> Number
fixed500 d = 500.0

fixed200 :: Datum_ -> Number
fixed200 datum = d.nodeId
  where
    d = (unsafeCoerce datum)

fromRadius :: Datum_ -> Number
fromRadius datum = d.r
  where d = (unsafeCoerce datum)

spagoForces :: Array Force
spagoForces =  
  [ Force "x"       ForceX        [ F.strength 0.2, F.x chooseFocusFromSpagoSimNodeX ]
  , Force "y"       ForceY        [ F.strength 0.2, F.y chooseFocusFromSpagoSimNodeY ]
  , Force "collide" ForceCollide  [ F.strength 1.0, F.radius fromRadius, F.iterations 1.0 ]
  ]
      
-- | recipe for this force layout graph
clusterScript :: forall m selection. 
  Bind m => 
  D3InterpreterM selection m => 
  Tuple Number Number ->
  SpagoModel ->
  m selection
clusterScript (Tuple w h) model = do
  root       <- attach "div#spago"
  svg        <- root `append` (node Svg    [ viewBox (-w / 2.0) (-h / 2.0) w h
                                           , classed "cluster" ] )
  nodesGroup <- svg  `append` (node Group  [ classed "nodes" ])

  let simulation = initSimulation_ unit
      _          = simulation `configSimulation_` defaultConfigSimulation
      nodes      = simulation `setNodes_` (pinIfPackage <$> model.nodes)
      _          = simulation `putEachForceInSimulation` spagoForces

  nodesSelection <- nodesGroup <+> Join { -- we're putting a group in with an eye to transitions to other layouts
      element   : Group
    , key       : UseDatumAsKey
    , "data"    : nodes
    , behaviour : [ classed nodeClass
                  , onMouseEvent MouseEnter (\e d t -> spotlightNeighbours_ simulation (getIdFromSpagoSimNode d) (getNodetypeFromSimNode d)) 
                  , onMouseEvent MouseLeave (\e d t -> unSpotlightNeighbours_ simulation (getIdFromSpagoSimNode d)) ]
  }
  circle  <- nodesSelection `append` (node Circle [ radius datumDotRadius, fill colorByGroup ]) 
  labels' <- nodesSelection `append` (node Text [ classed "label", text getNameFromSpagoSimNode ])

  packagesOnly <- filter nodesSelection "g.nodes g.package"
  _ <- packagesOnly `modify` [ lower ]
  
  _ <- circle         `on` Tick { name: "nodes",  simulation, chain: [ cx getNodeX, cy getNodeY ]}
  _ <- labels'        `on` Tick { name: "labels", simulation, chain: [ x getNodeX, y getNodeY ]}
  _ <- nodesSelection `on` Drag DefaultDrag
  _ <- svg `on` Zoom { extent : ZoomExtent { top: 0.0, left: 0.0 , bottom: h, right: w }
                     , scale  : ScaleExtent 0.2 2.0 -- wonder if ScaleExtent ctor could be range operator `..`
                     , name   : "spago"
                     }
  pure svg

foreign import spotlightNeighbours_ :: D3Simulation_ -> NodeID -> String -> Unit
foreign import unSpotlightNeighbours_ :: D3Simulation_ -> NodeID -> Unit