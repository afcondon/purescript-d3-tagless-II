module D3.Layouts.Simulation where

import D3.Node

import D3.Attributes.Instances (Attribute)
import D3.Data.Types (D3Simulation_)
import D3.FFI (forceCenter_, forceCollideFn_, forceCustom_, forceLink_, forceMany_, forceRadial_, forceX_, forceY_)
import D3.Simulation.Config 
import D3.Selection (DragBehavior)
import Data.Maybe (Maybe)
import Prelude (Unit, unit)

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
    ForceManyBody     String (Array ChainableF)
  | ForceCenter       String (Array ChainableF)
  | ForceCollide      String (Array ChainableF)
  | ForceX            String (Array ChainableF)
  | ForceY            String (Array ChainableF)
  | ForceRadial       String (Array ChainableF)
  | ForceLink         String (Array ChainableF) (forall r. Array (D3_Link NodeID r))
  | CustomForce       String (Array ChainableF)

createForce :: Force -> D3ForceHandle_
createForce = 
  case _ of
    (ForceManyBody name attributes) ->
      forceMany_ unit 
    (ForceCenter name attributes) ->
      forceCenter_ unit
    (ForceCollide name attributes) ->
      forceCollideFn_ unit
    (ForceX name attributes) ->
      forceX_ unit
    (ForceY name attributes) ->
      forceY_ unit
    (ForceRadial name attributes) ->
      forceRadial_ unit
    (ForceLink name attributes links) ->
      forceLink_ unit
       
    (CustomForce name attributes) -> 
      forceCustom_ unit
