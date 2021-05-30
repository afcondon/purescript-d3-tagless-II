module D3.Examples.Spago.Clusters where

import D3.Attributes.Sugar (classed, cx, cy, fill, radius, viewBox, x, y)
import D3.Data.Types (Element(..))
import D3.Examples.Spago.Attributes (colorByGroup, datumDotRadius, nodeClass)
import D3.Examples.Spago.Model (SpagoModel, chooseFocusFromSpagoSimNodeX, chooseFocusFromSpagoSimNodeY, getRadiusFromSpagoSimNode, gridifyByNodeID, pinIfPackage)
import D3.FFI (configSimulation_, initSimulation_, setNodes_)
import D3.Interpreter (class D3InterpreterM, append, attach, on, (<+>))
import D3.Layouts.Simulation (Force(..), ForceType(..), putEachForceInSimulation)
import D3.Node (getNodeX, getNodeY)
import D3.Selection (Behavior(..), DragBehavior(..), Join(..), Keys(..), node)
import D3.Simulation.Config (D3ForceHandle_, defaultConfigSimulation, strength)
import D3.Simulation.Config as F
import D3.Zoom (ScaleExtent(..), ZoomExtent(..))
import Data.Tuple (Tuple(..))
import Prelude (class Bind, Unit, bind, negate, pure, unit, (<$>), (/))

foreign import forceClusterCollision :: Unit -> D3ForceHandle_

-- myCustomForceConfig :: Array ChainableF 
-- myCustomForceConfig = [ radius 1.0, strength 0.8, clusterPadding: 10.0 ] forceClusterCollision
                  
spagoForces :: Array Force
spagoForces =  
  [-- Force "cluster" CustomForce  []
    Force "x"       ForceX       [ strength 0.2, F.x chooseFocusFromSpagoSimNodeX ]
  , Force "y"       ForceY       [ strength 0.2, F.y chooseFocusFromSpagoSimNodeY ]
  , Force "collide" ForceCollide  [ F.strength 1.0, F.radius getRadiusFromSpagoSimNode, F.iterations 1.0 ]
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
  svg        <- root `append` (node Svg    [ viewBox (-w / 2.0) (-h / 2.0) w h ] )
  centerDot  <- svg  `append` (node Circle [ radius 20.0, fill "red", x (w / 2.0), y h ])
  nodesGroup <- svg  `append` (node Group  [ classed "nodes" ])

  let simulation = initSimulation_ unit
      _          = simulation `configSimulation_` defaultConfigSimulation
      nodes      = simulation `setNodes_` (gridifyByNodeID <$> model.nodes)
      _          = simulation `putEachForceInSimulation` spagoForces

  nodesSelection <- nodesGroup <+> Join { -- we're putting a group in with an eye to transitions to other layouts
      element   : Group
    , key       : UseDatumAsKey
    , "data"    : nodes
    , behaviour : [ classed nodeClass ]
  }

  circle  <- nodesSelection `append` (node Circle [ radius datumDotRadius
                                                  , fill colorByGroup
                                                  ]) 
  
  _ <- circle `on` Tick { name: "nodes", simulation, chain: [ cx getNodeX, cy getNodeY ]}
  _ <- nodesSelection `on` Drag DefaultDrag
  _ <- svg `on` Zoom { extent    : ZoomExtent { top: 0.0, left: 0.0 , bottom: h, right: w }
                     , scale     : ScaleExtent 0.2 2.0 -- wonder if ScaleExtent ctor could be range operator `..`
                     , name : "spago"
                     }
  pure svg

