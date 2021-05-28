module D3.Examples.Spago.Clusters where

import D3.Attributes.Sugar (classed, fill, radius, transform', viewBox, x, y)
import D3.Data.Types (Element(..))
import D3.Examples.Spago.Attributes (colorByGroup, nodeClass, translateNode)
import D3.Examples.Spago.Model (SpagoModel, getRadiusFromSpagoSimNode)
import D3.FFI (configSimulation_, initSimulation_, putForcesInSimulation_, setNodes_)
import D3.FFI.Config (CustomForceConfig(..), D3ForceHandle_, defaultConfigSimulation, defaultForceXConfig, defaultForceYConfig)
import D3.Interpreter (class D3InterpreterM, append, attach, on, (<+>))
import D3.Layouts.Simulation (Force(..), createForce)
import D3.Selection (Behavior(..), DragBehavior(..), Join(..), Keys(..), node)
import D3.Zoom (ScaleExtent(..), ZoomExtent(..))
import Data.Tuple (Tuple(..))
import Prelude (class Bind, bind, negate, pure, unit, ($), (/), (<$>))

type ClusterForceConfigOpenRow = ( radius :: Number, strength :: Number, clusterPadding :: Number )
type ClusterForceConfig = CustomForceConfig ClusterForceConfigOpenRow

defaultClusterForceConfig :: String -> CustomForceConfig ClusterForceConfigOpenRow
defaultClusterForceConfig name = 
  CustomForceConfig { 
      name
    , force         : forceClusterCollision_
    , radius        : 1.0
    , strength      : 0.8
    , clusterPadding: 10.0
  } 

foreign import forceClusterCollision_ :: ClusterForceConfig -> D3ForceHandle_

myCustomForce :: CustomForceConfig ClusterForceConfigOpenRow
myCustomForce = CustomForceConfig { 
                      name          : "cluster"
                    , force         : forceClusterCollision_
                    , radius        : 1.0
                    , strength      : 0.8
                    , clusterPadding: 10.0
                  } 
spagoForces :: Array D3ForceHandle_
spagoForces = createForce <$> 
  [ CustomForce { name: "cluster" }
  , ForceX      $ (defaultForceXConfig "x") { strength = 0.1 }
  , ForceY      $ (defaultForceYConfig "y") { strength = 0.1 }
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

  nodesSelection <- nodesGroup <+> Join {
      element   : Group
    , key       : UseDatumAsKey
    , "data"    : nodes
    , behaviour : [ classed nodeClass, transform' translateNode ]
  }

  circle  <- nodesSelection `append` (node Circle [ radius getRadiusFromSpagoSimNode
                                                  , fill colorByGroup
                                                  ]) 
  
  _ <- nodesSelection `on` Tick { name: "nodes", simulation, chain: [ classed nodeClass, transform' translateNode ]}
  _ <- nodesSelection `on` Drag DefaultDrag
  _ <- svg `on` Zoom { extent    : ZoomExtent { top: 0.0, left: 0.0 , bottom: h, right: w }
                     , scale     : ScaleExtent 0.2 2.0 -- wonder if ScaleExtent ctor could be range operator `..`
                     , name : "spago"
                     }
  pure svg

