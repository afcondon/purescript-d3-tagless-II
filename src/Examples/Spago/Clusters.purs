module D3.Examples.Spago.Clusters where


import D3.Examples.Spago.Model (SpagoModel, datum_)
import D3.Simulation.Types (Step(..))

import D3.Attributes.Sugar (classed, cx, cy, fill, lower, radius, text, viewBox, x, y)
import D3.Data.Types (Element(..))
import D3.FFI (D3ForceHandle_)
import D3.Selection (Behavior(..), DragBehavior(..), Join(..), Keys(..), node)
import D3.Zoom (ScaleExtent(..), ZoomExtent(..))
import D3Tagless.Capabilities (class SelectionM, class SimulationM, appendElement, attach, addTickFunction, filterSelection, modifySelection, on, setNodes, (<+>))
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect, liftEffect)
import Prelude (class Bind, Unit, bind, discard, negate, pure, ($), (/))
import Utility (getWindowWidthHeight)


-- myCustomForceConfig :: Array ChainableF 
-- myCustomForceConfig = [ radius 1.0, strength 0.8, clusterPadding: 10.0 ] forceClusterCollision
foreign import forceClusterCollision :: Unit -> D3ForceHandle_


-- | recipe for this force layout graph
script :: forall m selection. 
  Bind m =>
  MonadEffect m =>
  SelectionM selection m =>
  SimulationM m =>
  SpagoModel ->
  m selection
script model = do
  (Tuple w h) <- liftEffect getWindowWidthHeight
  root       <- attach "div.svg-container"
  svg        <- root `appendElement` (node Svg [ viewBox (-w / 2.0) (-h / 2.0) w h
                                               , classed "d3svg cluster" ] )
  nodesGroup <- svg  `appendElement` (node Group  [ classed "nodes" ])

  setNodes model.nodes

  nodesSelection <- nodesGroup <+> Join { -- we're putting a group in with an eye to transitions to other layouts
      element   : Group
    , key       : UseDatumAsKey
    , "data"    : model.nodes -- TODO this is not the in-simulation nodes, setNodes temporarily returning unit only
    , behaviour : [ classed datum_.nodeClass ]
                  --, onMouseEvent MouseClick (\e d t -> toggleSpotlight e sim.simulation d) ]
  }
  circle  <- nodesSelection `appendElement` (node Circle [ radius datum_.radius, fill datum_.colorByGroup ]) 
  labels' <- nodesSelection `appendElement` (node Text [ classed "label", text datum_.name ])

  packagesOnly <- filterSelection nodesSelection "g.nodes g.package"
  _ <- packagesOnly `modifySelection` [ lower ]
  
  addTickFunction "nodes"  $ Step  circle  [ cx datum_.x, cy datum_.y ]
  addTickFunction "labels" $ Step labels' [ x datum_.x, y datum_.y ] -- TODO is x -> x, y -> y really just a No-Op ?

  _ <- nodesSelection `on` Drag DefaultDrag
  -- _ <- svg `modifySelection` [ onMouseEvent MouseClick (\e d t -> cancelSpotlight_ sim.simulation) ]
  _ <- svg `on` Zoom { extent : ZoomExtent { top: 0.0, left: 0.0 , bottom: h, right: w }
                     , scale  : ScaleExtent 0.2 2.0 -- wonder if ScaleExtent ctor could be range operator `..`
                     , name   : "spago"
                     }
  pure svg
