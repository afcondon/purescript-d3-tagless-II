module D3.Examples.Spago.Clusters where

import D3.Attributes.Sugar (classed, cx, cy, fill, radius, transform', viewBox, x, y)
import D3.Data.Types (Element(..))
import D3.Examples.Spago.Attributes (colorByGroup, datumDotRadius, nodeClass, translateNode)
import D3.Examples.Spago.Model (SpagoModel, getRadiusFromSpagoSimNode)
import D3.FFI (configSimulation_, initSimulation_, makeCustomForceConfig_, putForcesInSimulation_, setNodes_)
import D3.Interpreter (class D3InterpreterM, append, attach, on, (<+>))
import D3.Layouts.Simulation (Force(..), createForce)
import D3.Node (getNodeX, getNodeY)
import D3.Selection (Behavior(..), DragBehavior(..), Join(..), Keys(..), node)
import D3.Simulation.Config (ChainableF, CustomForceConfig_, D3ForceHandle_, defaultConfigSimulation, defaultForceXConfig, defaultForceYConfig, strength)
import D3.Zoom (ScaleExtent(..), ZoomExtent(..))
import Data.Tuple (Tuple(..))
import Prelude (class Bind, Unit, bind, negate, pure, unit, ($), (/), (<$>))

foreign import forceClusterCollision :: Unit -> D3ForceHandle_

-- myCustomForceConfig :: Array ChainableF 
-- myCustomForceConfig = [ radius 1.0, strength 0.8, clusterPadding: 10.0 ] forceClusterCollision
                  
spagoForces :: Array D3ForceHandle_
spagoForces = createForce <$> 
  [ CustomForce "cluster" [] -- TODO need to bring in the forceFunction somehow
  , ForceX      "x" [ strength 0.2 ]
  , ForceY      "y" [ strength 0.2 ]
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
      nodes      = simulation `setNodes_` model.nodes
      _          = simulation `putForcesInSimulation_` spagoForces

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

