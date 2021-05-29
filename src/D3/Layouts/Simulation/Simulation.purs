module D3.Layouts.Simulation where

import D3.Node

import D3.Attributes.Instances (Attribute)
import D3.Data.Types (D3Simulation_)
import D3.FFI (forceCenter_, forceCollideFn_, forceCustom_, forceLink_, forceMany_, forceRadial_, forceX_, forceY_)
import D3.Simulation.Config 
import D3.Selection (DragBehavior)
import Data.Maybe (Maybe)
import Prelude (Unit)

type SimulationManager d l = (  
-- 'd' is the type of the "data" field in each node
-- 'l' is the additional row-types in the link
    label      :: String
  , simulation :: Maybe D3Simulation_
  , config     :: SimulationConfig_
  , nodes      :: Array (D3SimulationRow d)
  , idLinks    :: Array (D3_Link NodeID l)
  , objLinks   :: Array (D3_Link (D3SimulationRow d) l)
  , forces     :: Array D3ForceHandle_
  , tick       :: Unit -> Unit -- could be Effect Unit??
  , drag       :: DragBehavior -- TODO make strongly typed wrt actual Model used
)

data Force =
    ForceManyBody     ForceManyConfig_
  | ForceCenter       ForceCenterConfig_
  | ForceCollide      ForceCollideConfig_
  | ForceX            ForceXConfig_
  | ForceY            ForceYConfig_
  | ForceRadial       ForceRadialConfig_
  | ForceLink         ForceLinkConfig_
  | CustomForce       CustomForceConfig_

createForce :: Force -> D3ForceHandle_
createForce = 
  case _ of
    (ForceManyBody config) ->
      forceMany_ config 
    (ForceCenter config) ->
      forceCenter_ config
    (ForceCollide config) ->
      forceCollideFn_ config
    (ForceX config) ->
      forceX_ config
    (ForceY config) ->
      forceY_ config
    (ForceRadial config) ->
      forceRadial_ config
    (ForceLink config) ->
      forceLink_ config
       
    (CustomForce config) -> 
      forceCustom_ config
