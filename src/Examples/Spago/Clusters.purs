module D3.Examples.Spago.Clusters where


import D3.Examples.Spago.Model

import D3.Attributes.Sugar (classed, cx, cy, fill, lower, onMouseEvent, radius, text, viewBox, x, y)
import D3.Data.Types (Element(..), MouseEvent(..))
import D3.FFI (setNodes_)
import D3.Interpreter (class D3InterpreterM, append, attach, filter, modify, on, (<+>))
import D3.Layouts.Simulation (SimulationManager)
import D3.Selection (Behavior(..), DragBehavior(..), Join(..), Keys(..), node)
import D3.Simulation.Config (D3ForceHandle_)
import D3.Zoom (ScaleExtent(..), ZoomExtent(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Prelude (class Bind, Unit, bind, negate, pure, (/), (<$>))

foreign import forceClusterCollision :: Unit -> D3ForceHandle_

-- myCustomForceConfig :: Array ChainableF 
-- myCustomForceConfig = [ radius 1.0, strength 0.8, clusterPadding: 10.0 ] forceClusterCollision


-- | recipe for this force layout graph
script :: forall m selection. 
  Bind m => 
  D3InterpreterM selection m => 
  Tuple Number Number ->
  SimulationManager ->
  SpagoModel ->
  m selection
script (Tuple w h) s model = do
  let simulation = (unwrap s).simulation
  root       <- attach "div.svg-container"
  svg        <- root `append` (node Svg    [ viewBox (-w / 2.0) (-h / 2.0) w h
                                           , classed "d3svg cluster" ] )
  nodesGroup <- svg  `append` (node Group  [ classed "nodes" ])

  let nodes      = simulation `setNodes_` model.nodes

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
