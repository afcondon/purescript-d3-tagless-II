module D3.Layouts.Simulation where

import D3.FFI
import Prelude

import Control.Monad.State (class MonadState, State, get, gets, modify, modify_)
import D3.Attributes.Instances (Attribute(..), Label, unbox)
import D3.Data.Types (D3Simulation_, Datum_, PointXY)
import D3.Node (D3_Link, D3_SimulationNode, NodeID)
import D3.Simulation.Config (ChainableF(..), D3ForceHandle_, SimulationConfig_, defaultConfigSimulation)
import Data.Array (elem, foldM, foldl, intercalate, uncons, (:))
import Data.Foldable (traverse_)
import Data.List (List)
import Data.Map (Map, empty, fromFoldable, insert, lookup, toUnfoldable, update) as M
import Data.Map.Internal (keys) as M
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Tuple (Tuple(..))


newtype SimulationM a = SimulationM (State SimulationR a)

type SimulationR = {
    simulation    :: D3Simulation_
  , alpha         :: Number
  , alphaTarget   :: Number
  , alphaMin      :: Number
  , alphaDecay    :: Number
  , velocityDecay :: Number
  , running       :: Boolean
  , forces        :: M.Map Label Force
}

derive newtype instance functorSimulationM     :: Functor           SimulationM
derive newtype instance applySimulationM       :: Apply             SimulationM
derive newtype instance applicativeSimulationM :: Applicative       SimulationM
derive newtype instance bindSimulationM        :: Bind              SimulationM
derive newtype instance monadSimulationM       :: Monad             SimulationM
derive newtype instance monadStateSimulationM  :: MonadState  SimulationR  SimulationM 

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


instance Show Force where
  show (Force label status t cs h) = "Force: " <> label <> " " <> show status 

showForces :: SimulationM String
showForces = do
  sim <- get
  let forceTuples = M.toUnfoldable sim.forces
      showTuple (Tuple label force) = show label <> " " <> show force
  pure $ intercalate "\n" $ showTuple <$> forceTuples

initialSimulationState =
   {  simulation   : initSimulation_ defaultConfigSimulation  
    , alpha        : defaultConfigSimulation.alpha
    , alphaTarget  : defaultConfigSimulation.alphaTarget
    , alphaMin     : defaultConfigSimulation.alphaMin
    , alphaDecay   : defaultConfigSimulation.alphaDecay
    , velocityDecay: defaultConfigSimulation.velocityDecay
    , running      : defaultConfigSimulation.running
    , forces: M.empty
  }


setAlpha :: Number ->  SimulationM Unit
setAlpha v = do
  sim <- get
  let _ = setAlpha_ sim.simulation v
  modify_ (\s -> s { alpha = v } )

setAlphaTarget :: Number ->  SimulationM Unit
setAlphaTarget v = do
  sim <- get
  let _ = setAlphaTarget_ sim.simulation v
  modify_  (\s -> s { alphaTarget = v })

setAlphaMin :: Number ->  SimulationM Unit
setAlphaMin v = do
  sim <- get
  let _ = setAlphaMin_ sim.simulation v
  modify_  (\s -> s { alphaMin = v })

setAlphaDecay :: Number ->  SimulationM Unit
setAlphaDecay v = do
  sim <- get
  let _ = setAlphaDecay_ sim.simulation v
  modify_  (\s -> s { alphaDecay = v })

setVelocityDecay :: Number ->  SimulationM Unit
setVelocityDecay v = do
  sim <- get
  let _ = setVelocityDecay_ sim.simulation v
  modify_  (\s -> s { velocityDecay = v })

setRunning :: Boolean ->  SimulationM Unit
setRunning true = start
setRunning false = stop

-- foreign import setNodes_ :: forall d.   D3Simulation_ -> Array (D3_SimulationNode d) -> Array (D3_SimulationNode d)
setNodes :: forall d. Array (D3_SimulationNode d) -> SimulationM (Array (D3_SimulationNode d)) -- nodes come back modified
setNodes nodes = do
  sim <- get
  pure (sim.simulation `setNodes_` nodes)

start :: SimulationM Unit
start = do
  sim <- get
  let _ = startSimulation_  sim.simulation
      _ = setAlpha_ sim.simulation 1.0
  modify_ (\s -> s { running = true } )

stop :: SimulationM Unit
stop = do
  sim <- get
  let _ = stopSimulation_ sim.simulation
  modify_ (\s -> s { running = false } )

loadForces :: Array Force -> SimulationM Unit
loadForces forces = do
  removeAllForces
  traverse_ addForce forces 

addForces :: Array Force -> SimulationM Unit
addForces forces = traverse_ addForce forces

addForce :: Force -> SimulationM Unit
addForce force@(Force l status t attrs h_) = do
  -- TODO this should be a traverse_ eventually
  let _ = (\a -> setForceAttr h_ (unwrap a)) <$> attrs 
  sim <- get
  let _ = if status == ForceActive
          then putForceInSimulation force sim.simulation
          else sim.simulation
  -- if the force isn't active then we just keep it in map, with label as key
  modify_ $ (\sim -> sim { forces = M.insert l force sim.forces })

putForceInSimulation :: Force -> D3Simulation_ -> D3Simulation_
putForceInSimulation (Force l s t attrs h_) simulation_ =
  case t of
    ForceManyBody -> putForceInSimulation_ simulation_ l h_
    ForceCenter   -> putForceInSimulation_ simulation_ l h_
    ForceCollide  -> putForceInSimulation_ simulation_ l h_
    ForceX        -> putForceInSimulation_ simulation_ l h_
    ForceY        -> putForceInSimulation_ simulation_ l h_
    ForceRadial   -> putForceInSimulation_ simulation_ l h_

    (ForceLink _) -> putForceInSimulation_ simulation_ l h_

    (ForceFixPositionXY f) -> applyFixForceInSimulationXY_ simulation_ l f
    (ForceFixPositionX f)  -> applyFixForceInSimulationX_ simulation_ l f
    (ForceFixPositionY f)  -> applyFixForceInSimulationY_ simulation_ l f

    CustomForce   -> putForceInSimulation_ simulation_ l h_ -- TODO not implemented or even designed yet



enableByLabelMany :: Array Label -> SimulationM Unit
enableByLabelMany labels = do
  sim <- get
  let updatedForces = (enableByLabels sim.simulation labels) <$> sim.forces
  modify_ (\s -> s { forces = updatedForces } )

disableByLabelMany :: Array Label -> SimulationM Unit
disableByLabelMany labels = do
  sim <- get
  let updatedForces = (disableByLabels sim.simulation labels) <$> sim.forces
  modify_ (\s -> s { forces = updatedForces } )

removeAllForces :: SimulationM Unit
removeAllForces = do
  sim <- get
  let _ = (setAsNullForceInSimulation_ sim.simulation) <$> (M.keys sim.forces)
  modify_ (\s -> s { forces = (M.empty :: M.Map Label Force) } )

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
  | ForceFixPositionXY (Datum_ -> PointXY) -- function is static, provided to constructor
  | ForceFixPositionX  (Datum_ -> Number)
  | ForceFixPositionY  (Datum_ -> Number)
                                                   -- TODO need something to hold extra custom force config, perhaps?
  | CustomForce                                    -- ???

instance Show ForceType where
  show ForceManyBody           = "ForceManyBody"
  show ForceCenter             = "ForceCenter"
  show ForceCollide            = "ForceCollide"
  show ForceX                  = "ForceX"
  show ForceY                  = "ForceY"
  show ForceRadial             = "ForceRadial"
  show (ForceFixPositionXY xy) = "ForceFixPositionXY"
  show (ForceFixPositionX x)   = "ForceFixPositionX"
  show (ForceFixPositionY y)   = "ForceFixPositionY"
  show (ForceLink _)           = "ForceLink"
  show CustomForce             = "CustomForce"

showSimulationRunning :: SimulationM String
showSimulationRunning = do
  gets (\s -> if s.running
              then "Running"
              else "Paused")

forceDescription :: ForceType -> String
forceDescription = case _ of
  ForceManyBody -> 

    """The many-body (or n-body) force applies mutually amongst all nodes. It can
    be used to simulate gravity (attraction) if the strength is positive, or
    electrostatic charge (repulsion) if the strength is negative."""
      
  ForceCenter   ->
    
    """The centering force translates nodes uniformly so that the mean position
    of all nodes (the center of mass if all nodes have equal weight) is at the
    given position ⟨x,y⟩. """
  
  ForceCollide  ->

    """The collision force treats nodes as circles with a given radius, rather
    than points, and prevents nodes from overlapping."""

  ForceX        ->

    """The x-positioning force pushes nodes towards a desired position along the
    horizontal with a configurable strength."""

  ForceY        ->

    """The y-positioning force pushes nodes towards a desired position along the
    vertical with a configurable strength."""

  ForceRadial   ->

    """The radial force pushes nodes towards the closest point on a given circle."""

  (ForceFixPositionXY xy) ->

    """This \"force\" is really an over-ride for the force simulation, fixing the node at a particular point"""

  (ForceFixPositionX x) ->

    """This \"force\" is really an over-ride for the force simulation, fixing the node at a particular X dimension"""

  (ForceFixPositionY y) ->

    """This \"force\" is really an over-ride for the force simulation, fixing the node at a particular Y dimension"""

  (ForceLink _) ->

    """The link force pushes linked nodes together or apart according to the
    desired link distance. The strength of the force is proportional to the
    difference between the linked nodes’ distance and the target distance,
    similar to a spring force."""

  CustomForce   -> ""


-- TODO this needs to move to the D3 interpreter, with some parallel impls for String, Meta etc
createForce_ :: ForceType -> D3ForceHandle_
createForce_ = case _ of
  ForceManyBody             -> forceMany_      unit 
  ForceCenter               -> forceCenter_    unit
  ForceCollide              -> forceCollideFn_ unit
  ForceX                    -> forceX_         unit
  ForceY                    -> forceY_         unit
  ForceRadial               -> forceRadial_    unit

  (ForceLink links)         -> forceLink_      links
  (CustomForce)             -> forceCustom_    unit
  -- NB there is actually no "force", in D3 terms, behind the fixed "forces", hence the dummy handle that is returned
  (ForceFixPositionXY xy)   -> dummyForceHandle_ 
  (ForceFixPositionX x)     -> dummyForceHandle_
  (ForceFixPositionY y)     -> dummyForceHandle_

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


  
