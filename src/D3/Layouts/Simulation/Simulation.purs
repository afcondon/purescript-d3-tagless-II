module D3.Layouts.Simulation where

import D3.FFI
import Prelude

import D3.Attributes.Instances (Attribute(..), Label, unbox)
import D3.Data.Types (D3Simulation_)
import D3.Node (D3_Link, NodeID)
import D3.Simulation.Config (ChainableF(..), D3ForceHandle_, SimulationConfig_, defaultConfigSimulation)
import Data.Array (elem, foldl, intercalate, uncons, (:))
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

toggleForceStatus :: ForceStatus -> ForceStatus
toggleForceStatus =
  case _ of
    ForceActive   -> ForceDisabled
    ForceDisabled -> ForceActive

getLabel :: Force -> Label
getLabel (Force l _ _ _ _) = l

getHandle :: Force -> D3ForceHandle_
getHandle (Force l s t cs h_) = h_


-- TODO we won't export the constructor here when we close exports
data Force = Force Label ForceStatus ForceType (Array ChainableF) D3ForceHandle_

createForce :: Label -> ForceType -> Array ChainableF -> Force
createForce l t cs = Force l ForceDisabled t cs (createForce_ t)

disableForce :: Force -> Force
disableForce (Force l _ t cs h) = Force l ForceDisabled t cs h

enableForce :: Force -> Force
enableForce (Force l _ t cs h) = Force l ForceActive t cs h
-- TODO but in fact when we toggle "Active" maybe we want to get a handle from D3 for it if it didn't have one? 

toggleForce :: Force -> Force
toggleForce (Force l s t cs h_) = Force l (toggleForceStatus s) t cs h_
-- TODO but in fact when we toggle "Active" maybe we want to get a handle from D3 for it if it didn't have one? 

newtype SimulationManager = SimulationManager {
    simulation :: D3Simulation_
  , config     :: SimulationConfig_
  , running    :: Boolean
  , forces     :: M.Map Label Force
}
derive instance Newtype SimulationManager _

instance Show Force where
  show (Force label status t cs h) = "Force: " <> label <> " " <> show status 

showForces :: SimulationManager -> String
showForces (SimulationManager sim) = do
  let forceTuples = M.toUnfoldable sim.forces
      showTuple (Tuple label force) = show label <> " " <> show force
  intercalate "\n" $ showTuple <$> forceTuples

createSimulationManager :: SimulationManager 
createSimulationManager = wrap { 
    simulation: (initSimulation_ unit) `configSimulation_` defaultConfigSimulation  
  , config: defaultConfigSimulation
  , running: true
  , forces: M.empty
}

start :: SimulationManager -> SimulationManager
start (SimulationManager sim) = do
  let _ = startSimulation_  sim.simulation
  wrap sim { running = true }

stop :: SimulationManager -> SimulationManager
stop (SimulationManager sim) = do
  let _ = stopSimulation_  sim.simulation
  wrap sim { running = false }

setForces :: Array Force -> SimulationManager -> SimulationManager
setForces forces = addForces forces <<< removeAllForces

addForces :: Array Force -> SimulationManager -> SimulationManager
addForces fs sim = 
  case uncons fs of
    Just f -> addForces f.tail (sim `addForce` f.head)
    Nothing -> sim

addForce :: SimulationManager -> Force -> SimulationManager
addForce (SimulationManager sim) force@(Force l s t attrs h_) = do
  -- addForce and label in D3 first
  let _ = (\a -> setForceAttr h_ (unwrap a)) <$> attrs
      s' = if s == ForceActive
           then putForceInSimulation_ sim.simulation l h_
           else sim.simulation
  wrap sim { forces = M.insert l force sim.forces, simulation = s'  }

enableByLabelMany :: Array Label -> SimulationManager -> SimulationManager
enableByLabelMany labels (SimulationManager sim) = do
  let updatedForces = (enableByLabels sim.simulation labels) <$> sim.forces
  wrap sim { forces = updatedForces }

disableByLabelMany :: Array Label -> SimulationManager -> SimulationManager
disableByLabelMany labels (SimulationManager sim) = do
  let updatedForces = (disableByLabels sim.simulation labels) <$> sim.forces
  wrap sim { forces = updatedForces }

removeAllForces :: SimulationManager -> SimulationManager
removeAllForces (SimulationManager sim) = do
  let _ = (setAsNullForceInSimulation_ sim.simulation) <$> (M.keys sim.forces)
  wrap sim { forces = (M.empty :: M.Map Label Force) }

disableByLabels :: D3Simulation_ -> Array Label -> Force -> Force
disableByLabels simulation labels force@(Force label _ t cs h_) =
  if label `elem` labels
  then do
    let _ = setAsNullForceInSimulation_ simulation label
    Force label ForceDisabled t cs h_
  else force

enableByLabels :: D3Simulation_ -> Array Label -> Force -> Force
enableByLabels simulation labels force@(Force label _ t cs h_) = 
  if label `elem` labels
  then do
    let _ = putForceInSimulation_ simulation label h_
    Force label ForceActive t cs h_
  else force

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
createForce_ :: ForceType -> D3ForceHandle_
createForce_ = case _ of
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


  
