module D3.Layouts.Simulation where

import D3.FFI
import D3.Node
import D3.Simulation.Config
import Prelude

import D3.Attributes.Instances (Attribute(..), Label, unbox)
import D3.Data.Types (D3Simulation_)
import D3.Selection (ChainableS(..), DragBehavior)
import Data.Maybe (Maybe)

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

data Force = Force Label ForceType (Array ChainableF)

data ForceType = 
    ForceManyBody                                  -- strength, theta, distanceMax, distanceMin
  | ForceCenter                                    -- strength, x, y
  | ForceCollide                                   -- strength, radius, iterations
  | ForceX                                         -- strength, x
  | ForceY                                         -- strength, y
  | ForceRadial                                    -- strength, radius, x, y
  | ForceLink (forall r. Array (D3_Link NodeID r)) -- strength, id, distance, iterations, links
                                                   -- TODO need something to hold extra custom force config, perhaps?
  | CustomForce                                    -- ???

-- TODO this needs to move to the D3 interpreter, with some parallel impls for String, Meta etc
createForce :: ForceType -> D3ForceHandle_
createForce = case _ of
  ForceManyBody      -> forceMany_      unit 
  ForceCenter        -> forceCenter_    unit
  ForceCollide       -> forceCollideFn_ unit
  ForceX             -> forceX_         unit
  ForceY             -> forceY_         unit
  ForceRadial        -> forceRadial_    unit
  (ForceLink links)  -> forceLink_   links
  (CustomForce)      -> forceCustom_    unit

-- TODO at present there is no type checking on what forces have which attrs settable, see comment above
setForceAttr :: D3ForceHandle_ -> Attribute -> D3ForceHandle_
setForceAttr force_ (ToAttribute label attr) = do
  case label of
    "radius"      -> setForceRadius_      force_ (unbox attr) -- valid 
    "strength"    -> setForceStrength_    force_ (unbox attr)
    "cx"          -> setForceCx_          force_ (unbox attr)
    "cy"          -> setForceCy_          force_ (unbox attr)
    "theta"       -> setForceTheta_       force_ (unbox attr)
    "distanceMin" -> setForceDistanceMin_ force_ (unbox attr)
    "distanceMax" -> setForceDistanceMax_ force_ (unbox attr)
    "iterations"  -> setForceIterations_  force_ (unbox attr)
    "x"           -> setForceX_           force_ (unbox attr)
    "y"           -> setForceY_           force_ (unbox attr)
    "distance"    -> setForceDistance_    force_ (unbox attr)
    _ -> force_ -- no other force attributes accepted


addSingleForce :: D3Simulation_ -> Force -> D3Simulation_
addSingleForce simulation (Force label forcetype attrs) = do
  let handle_ = createForce forcetype
      _       = (\(ForceT a) -> setForceAttr handle_ a) <$> attrs
  putForceInSimulation_ simulation label handle_

putEachForceInSimulation :: D3Simulation_ -> Array Force -> D3Simulation_
putEachForceInSimulation simulation forces = do
  let _ = (addSingleForce simulation) <$> forces
  simulation
  
