module D3.Examples.LesMiserables where

import Control.Monad.State (class MonadState)
import D3.Attributes.Sugar (classed, cx, cy, fill, radius, strokeColor, strokeOpacity, strokeWidth, viewBox, x1, x2, y1, y2)
import D3.Data.Types (D3Selection_, Element(..), Selector)
import D3.Examples.LesMis.Unsafe (unboxD3SimLink, unboxD3SimNode)
import D3.Examples.LesMiserables.Model (LesMisRawModel)
import D3.FFI (keyIsID_, simdrag)
import D3.Scales (d3SchemeCategory10N_)
import D3.Selection (Behavior(..), DragBehavior(..))
import D3.Simulation.Types (D3SimulationState_, SimVariable(..), Step(..))
import D3.Zoom (ScaleExtent(..), ZoomExtent(..))
import D3Tagless.Capabilities (class SimulationM, addTickFunction, appendTo, attach, on, setAttributes, setConfigVariable, setLinks, setNodes, simpleJoin)
import Data.Int (toNumber)
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect, liftEffect)
import Prelude (class Bind, Unit, bind, discard, negate, pure, unit, ($), (/), (<<<))
import Utility (getWindowWidthHeight)

-- type-safe(ish) accessors for the data that is given to D3
-- we lose the type information in callbacks from the FFI, such as for attributes
-- but since we know what we gave we can coerce it back to the initial type.
-- Snippet_Start
-- Name: LesMisAccessors
link_ = {
    source: _.source <<< unboxD3SimLink
  , target: _.target <<< unboxD3SimLink
  , value:  _.value <<< unboxD3SimLink
  , color:  d3SchemeCategory10N_ <<< toNumber <<< _.target.group <<< unboxD3SimLink
}

datum_ = {
-- direct accessors to fields of the datum (BOILERPLATE)
    id    : _.id <<< unboxD3SimNode -- NB the id in this case is a String
  , x     : _.x <<< unboxD3SimNode
  , y     : _.y <<< unboxD3SimNode
  , group : _.group <<< unboxD3SimNode

  , colorByGroup: d3SchemeCategory10N_ <<< toNumber <<< _.group <<< unboxD3SimNode
}
-- Snippet_End

-- Snippet_Start
-- Name: LesMisScript
-- | recipe for this force layout graph
draw :: forall row m. 
  Bind m => 
  MonadEffect m =>
  MonadState { simulation :: D3SimulationState_ | row } m => 
  SimulationM D3Selection_ m =>
  LesMisRawModel -> Selector D3Selection_ -> m Unit
draw model selector = do
  (Tuple w h) <- liftEffect getWindowWidthHeight
  (root :: D3Selection_) <- attach selector
  svg        <- appendTo root Svg [ viewBox (-w / 2.0) (-h / 2.0) w h, classed "lesmis" ]
  linksGroup <- appendTo svg Group  [ classed "link", strokeColor "#999", strokeOpacity 0.6 ]
  nodesGroup <- appendTo svg Group  [ classed "node", strokeColor "#fff", strokeOpacity 1.5 ]
  
  -- in contrast to a simple SelectionM function, we have additional typeclass capabilities for simulation
  -- which we use here to introduce the nodes and links to the simulation
  nodesInSim <- setNodes model.nodes -- no staging here, we just load the nodes straight into Sim
  linksInSim <- setLinks model.links model.nodes keyIsID_

  -- joining the data from the model after it has been put into the simulation
  nodesSelection <- simpleJoin nodesGroup Circle nodesInSim keyIsID_ 
  setAttributes nodesSelection [ radius 5.0, fill datum_.colorByGroup ] 
  linksSelection <- simpleJoin linksGroup Line   linksInSim keyIsID_ 
  setAttributes linksSelection [ strokeWidth (sqrt <<< link_.value), strokeColor link_.color ]

  -- both links and nodes are updated on each step of the simulation, 
  -- in this case it's a simple translation of underlying (x,y) data for the circle centers
  -- tick functions have names, in this case "nodes" and "links"
  addTickFunction "nodes" $ Step nodesSelection [ cx datum_.x, cy datum_.y  ]
  addTickFunction "links" $ Step linksSelection [ x1 (_.x <<< link_.source)
                                                , y1 (_.y <<< link_.source)
                                                , x2 (_.x <<< link_.target)
                                                , y2 (_.y <<< link_.target)
                                                ]
  -- use default drag function (simply drags the element that's clicked on)                                              
  _ <- nodesSelection `on` Drag (CustomDrag "lesmis" simdrag)
  -- TODO create inner <g> and apply the zoom functionality to it
  _ <- svg `on`  Zoom { extent : ZoomExtent { top: 0.0, left: 0.0 , bottom: h, right: w }
                      , scale  : ScaleExtent 1.0 4.0 -- wonder if ScaleExtent ctor could be range operator `..`
                      , name   : "LesMis"
                      , target : svg
                      }
  setConfigVariable $ Alpha 1.0
  pure unit
-- Snippet_End