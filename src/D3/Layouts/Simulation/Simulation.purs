module D3.Layouts.Simulation where

import D3.FFI
import Prelude

import D3.Attributes.Instances (Attribute(..), Label, unbox)
import D3.Data.Types (D3Simulation_)
import D3.Node (D3_Link, NodeID)
import D3.Simulation.Config (ChainableF(..), D3ForceHandle_, SimulationConfig_, defaultConfigSimulation)
import Data.Array (intercalate)
import Data.List (List)
import Data.Map (Map, empty, fromFoldable, insert, lookup, toUnfoldable, update) as M
import Data.Map.Internal (keys) as M
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Tuple (Tuple(..))

data ForceStatus = ForceActive | ForceDisabled
derive instance eqForceStatus :: Eq ForceStatus
instance showForceStatus :: Show ForceStatus where
  show ForceActive = "active"
  show ForceDisabled = "inactive"
data SimForce = SimForce ForceStatus D3ForceHandle_

disableSimForce :: SimForce -> SimForce
disableSimForce (SimForce _ f) = SimForce ForceDisabled f

toggleSimForce :: SimForce -> SimForce
toggleSimForce (SimForce ForceActive f) = SimForce ForceDisabled f
toggleSimForce (SimForce ForceDisabled f) = SimForce ForceActive f


newtype SimulationManager = SimulationManager {
    simulation :: D3Simulation_
  , config     :: SimulationConfig_
  , running    :: Boolean
  , forces     :: M.Map Label SimForce
}
derive instance newtypeSimulationManager :: Newtype SimulationManager _

instance showSimForce :: Show SimForce where
  show (SimForce status _) = show status

showForces :: SimulationManager -> String
showForces s = do
  let sim = unwrap s
      forces = sim.forces
  intercalate "\n" $ (\(Tuple label force) -> show label <> " " <> show force) <$> (M.toUnfoldable forces)

createSimulationManager :: SimulationManager 
createSimulationManager = wrap { 
    simulation: (initSimulation_ unit) `configSimulation_` defaultConfigSimulation  
  , config: defaultConfigSimulation
  , running: true
  , forces: M.empty
}

start :: SimulationManager -> SimulationManager
start s = do
  let sim = unwrap s
      _ = startSimulation_  sim.simulation
  wrap sim { running = true }

stop :: SimulationManager -> SimulationManager
stop s = do
  let sim = unwrap s
      _ = stopSimulation_  sim.simulation
  wrap sim { running = false }

setForces :: Array Force -> SimulationManager -> SimulationManager
setForces forces s = addForces forces (removeAllForces s) 

addForces :: Array Force -> SimulationManager -> SimulationManager
addForces forces s = do
  -- TODO make SimulationManager Semigroup and fold instead
  -- foldl (\sm f -> addForce sm f) s forces
  s

addForce :: SimulationManager -> Force -> SimulationManager
addForce s (Force label forceStatus forceType attrs) = do
  -- addForce and tag in D3 first
  let sim = unwrap s
      handle_ = createForce forceType
      _       = (\(ForceT a) -> setForceAttr handle_ a) <$> attrs
      _       = if forceStatus == ForceActive
                then putForceInSimulation_ sim.simulation label handle_
                else sim.simulation
  wrap sim { forces = M.insert label (SimForce forceStatus handle_) sim.forces  }

removeAllForces :: SimulationManager -> SimulationManager
removeAllForces s = do
  let 
    sim = unwrap s
    _ = (setAsNullForceInSimulation_ sim.simulation) <$> (M.keys sim.forces)
  wrap sim { forces = (M.empty :: M.Map String SimForce) }

disableForce :: String -> SimulationManager -> SimulationManager
disableForce tag s = do
  let sim = unwrap s
  case M.lookup tag sim.forces of
    Nothing  -> s
    (Just f) -> do
      let _ = setAsNullForceInSimulation_ sim.simulation tag
      wrap sim { forces = M.insert tag (disableSimForce f) sim.forces }

reenableForce :: String -> SimulationManager -> SimulationManager
reenableForce tag s = do
  let sim = unwrap s
  case M.lookup tag sim.forces of
    (Just (SimForce ForceDisabled f)) -> do
      wrap sim { forces = M.insert tag (SimForce ForceActive f) sim.forces }

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


  
