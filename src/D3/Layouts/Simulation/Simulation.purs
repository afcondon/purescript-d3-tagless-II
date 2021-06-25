module D3.Layouts.Simulation where

import D3.FFI
import Prelude

import D3.Attributes.Instances (Attribute(..), Label, unbox)
import D3.Data.Types (D3Simulation_)
import D3.Node (D3_Link, NodeID)
import D3.Simulation.Config (ChainableF(..), D3ForceHandle_, SimulationConfig_, defaultConfigSimulation)
import Data.List (List)
import Data.Map (Map, empty, fromFoldable, insert, lookup, update) as M
import Data.Map.Internal (keys) as M
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

-- type SimulationManager d l = (  
-- -- 'd' is the type of the "data" field in each node
-- -- 'l' is the additional row-types in the link
--     label      :: String
--   , simulation :: Maybe D3Simulation_
--   , config     :: SimulationConfig_
--   , nodes      :: Array (D3SimulationRow d)
--   , idLinks    :: Array (D3_Link NodeID l)
--   , objLinks   :: Array (D3_Link (D3SimulationRow d) l)
--   , forces     :: Array D3ForceHandle_
--   , tick       :: Unit -> Unit -- could be Effect Unit??
--   , drag       :: DragBehavior -- TODO make strongly typed wrt actual Model used
-- )
data ForceStatus = ForceActive | ForceDisabled
derive instance eqForceStatus :: Eq ForceStatus
data SimForce = SimForce ForceStatus D3ForceHandle_

disableSimForce :: SimForce -> SimForce
disableSimForce (SimForce _ f) = SimForce ForceDisabled f

toggleSimForce :: SimForce -> SimForce
toggleSimForce (SimForce ForceActive f) = SimForce ForceDisabled f
toggleSimForce (SimForce ForceDisabled f) = SimForce ForceActive f


type SimulationManager = {
    simulation :: D3Simulation_
  , config     :: SimulationConfig_
  , running    :: Boolean
  , forces     :: M.Map Label SimForce
}

createSimulationManager :: SimulationManager 
createSimulationManager = { 
    simulation: (initSimulation_ unit) `configSimulation_` defaultConfigSimulation  
  , config: defaultConfigSimulation
  , running: true
  , forces: M.empty
}

start :: SimulationManager -> SimulationManager
start s = do
  let _ = startSimulation_  s.simulation
  s { running = true }

stop :: SimulationManager -> SimulationManager
stop s = do
  let _ = stopSimulation_  s.simulation
  s { running = false }

setForces :: Array Force -> SimulationManager -> SimulationManager
setForces forces s = addForces forces (removeAllForces s) 

addForces :: Array Force -> SimulationManager -> SimulationManager
addForces forces s = do
  let _ = (addForce s) <$> forces -- TODO make SimulationManager Semigroup and fold instead
  s

addForce :: SimulationManager -> Force -> SimulationManager
addForce s (Force label forceStatus forceType attrs) = do
  -- addForce and tag in D3 first
  let handle_ = createForce forceType
      _       = (\(ForceT a) -> setForceAttr handle_ a) <$> attrs
      _       = if forceStatus == ForceActive
                then putForceInSimulation_ s.simulation label handle_
                else s.simulation
  s { forces = M.insert label (SimForce forceStatus handle_) s.forces  }

removeAllForces :: SimulationManager -> SimulationManager
removeAllForces s = do
  let 
    _ = (setAsNullForceInSimulation_ s.simulation) <$> (M.keys s.forces)
  s { forces = (M.empty :: M.Map String SimForce) }

disableForce :: String -> SimulationManager -> SimulationManager
disableForce tag s = do
  case M.lookup tag s.forces of
    Nothing  -> s
    (Just f) -> do
      let _ = setAsNullForceInSimulation_ s.simulation tag
      s { forces = M.insert tag (disableSimForce f) s.forces }

reenableForce :: String -> SimulationManager -> SimulationManager
reenableForce tag s = do
  case M.lookup tag s.forces of
    (Just (SimForce ForceDisabled f)) -> do
      s { forces = M.insert tag (SimForce ForceActive f) s.forces }

    Nothing                           -> s -- REVIEW exception? force is not reenabled because it doesn't exist
    (Just (SimForce ForceActive _))   -> s -- REVIEW exception? force was already active

data Force = Force Label ForceStatus ForceType (Array ChainableF)

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

  (ForceLink links)  -> forceLink_      links
  (CustomForce)      -> forceCustom_    unit

-- TODO at present there is no type checking on what forces have which attrs settable, see comment above
setForceAttr :: D3ForceHandle_ -> Attribute -> D3ForceHandle_
setForceAttr force_ (ToAttribute label attr) = do
  case label of
    "radius"      -> setForceRadius_      force_ (unbox attr) -- valid 
    "strength"    -> setForceStrength_    force_ (unbox attr)
    -- "cx"          -> setForceCx_          force_ (unbox attr)
    -- "cy"          -> setForceCy_          force_ (unbox attr)
    "theta"       -> setForceTheta_       force_ (unbox attr)
    "distanceMin" -> setForceDistanceMin_ force_ (unbox attr)
    "distanceMax" -> setForceDistanceMax_ force_ (unbox attr)
    "iterations"  -> setForceIterations_  force_ (unbox attr)
    "x"           -> setForceX_           force_ (unbox attr)
    "y"           -> setForceY_           force_ (unbox attr)
    "distance"    -> setForceDistance_    force_ (unbox attr)
    _ -> force_ -- no other force attributes accepted


  
