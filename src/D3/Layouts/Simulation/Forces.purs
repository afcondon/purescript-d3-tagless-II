module D3.Simulation.Forces where

import Prelude

import D3.Attributes.Instances (Attribute(..), Label, unbox)
import D3.Data.Types (D3Simulation_)
import D3.FFI (D3ForceHandle_, applyFixForceInSimulationXY_, applyFixForceInSimulationX_, applyFixForceInSimulationY_, dummyForceHandle_, forceCenter_, forceCollideFn_, forceCustom_, forceLink_, forceMany_, forceRadial_, forceX_, forceY_, putForceInSimulationWithFilter_, putForceInSimulation_, removeFixForceXY_, removeFixForceX_, removeFixForceY_, setAsNullForceInSimulation_, setForceDistanceMax_, setForceDistanceMin_, setForceDistance_, setForceIterations_, setForceRadius_, setForceStrength_, setForceTheta_, setForceX_, setForceY_, setLinks_, unsetLinks_)
import D3.Simulation.Types (ChainableF, Force(..), ForceFilter(..), ForceStatus(..), ForceType(..))
import D3Tagless.Capabilities (setLinks)
import Data.Array (elem)
import Data.Maybe (Maybe(..))
import Debug (spy, trace)


toggleForceStatus :: ForceStatus -> ForceStatus
toggleForceStatus =
  case _ of
    ForceActive   -> ForceDisabled
    ForceDisabled -> ForceActive

getLabel :: Force -> Label
getLabel (Force l _ _ _ _ _) = l

getHandle :: Force -> D3ForceHandle_
getHandle (Force l s t f cs h_) = h_

createForce :: Label -> ForceType -> Maybe ForceFilter -> Array ChainableF -> Force
createForce l t f cs = Force l ForceDisabled t f cs (createForce_ t)

disableForce :: Force -> Force
disableForce (Force l _ t f cs h) = Force l ForceDisabled t f cs h

enableForce :: Force -> Force
enableForce (Force l _ t f cs h) = Force l ForceActive t f cs h
-- TODO but in fact when we toggle "Active" maybe we want to get a handle from D3 for it if it didn't have one? 

toggleForce :: Force -> Force
toggleForce (Force l s t f cs h_) = Force l (toggleForceStatus s) t f cs h_
-- TODO but in fact when we toggle "Active" maybe we want to get a handle from D3 for it if it didn't have one? 

disableByLabels :: D3Simulation_ -> Array Label -> Force -> Force
disableByLabels simulation labels force@(Force label _ t f cs h_) =
  if label `elem` labels
  then do
    let _ = -- trace { forceToRemove: label } \_ -> 
            removeForceFromSimulation force simulation
    Force label ForceDisabled t f cs h_
  else -- trace { forceKept: label } \_ -> 
       force

enableByLabels :: D3Simulation_ -> Array Label -> Force -> Force
enableByLabels simulation labels force@(Force label _ t f cs h_) = 
  if label `elem` labels
  then do
    let _ = putForceInSimulation force simulation
    Force label ForceActive t f cs h_
  else force

putForceInSimulation :: Force -> D3Simulation_ -> D3Simulation_
putForceInSimulation (Force l s t Nothing attrs h_) simulation_ =
  case t of
    ForceManyBody -> putForceInSimulation_ simulation_ l h_
    ForceCenter   -> putForceInSimulation_ simulation_ l h_
    ForceCollide  -> putForceInSimulation_ simulation_ l h_
    ForceX        -> putForceInSimulation_ simulation_ l h_
    ForceY        -> putForceInSimulation_ simulation_ l h_
    ForceRadial   -> putForceInSimulation_ simulation_ l h_

    ForceLink     -> putForceInSimulation_ simulation_ l h_ -- NB l should always be "links" effectively

    (ForceFixPositionXY fn) -> applyFixForceInSimulationXY_ simulation_ l fn (const true)
    (ForceFixPositionX fn)  -> applyFixForceInSimulationX_ simulation_ l fn (const true)
    (ForceFixPositionY fn)  -> applyFixForceInSimulationY_ simulation_ l fn (const true)

    CustomForce   -> putForceInSimulation_ simulation_ l h_ -- TODO not implemented or even designed yet
putForceInSimulation (Force l s t (Just (FilterNodes _ filter)) attrs h_) simulation_ =
  case t of
    ForceManyBody -> putForceInSimulationWithFilter_ simulation_ l filter h_
    ForceCenter   -> putForceInSimulationWithFilter_ simulation_ l filter h_
    ForceCollide  -> putForceInSimulationWithFilter_ simulation_ l filter h_
    ForceX        -> putForceInSimulationWithFilter_ simulation_ l filter h_
    ForceY        -> putForceInSimulationWithFilter_ simulation_ l filter h_
    ForceRadial   -> putForceInSimulationWithFilter_ simulation_ l filter h_

    ForceLink     -> putForceInSimulation_ simulation_ l h_ -- NB l should always be "links" effectively

    (ForceFixPositionXY fn) -> applyFixForceInSimulationXY_ simulation_ l fn filter
    (ForceFixPositionX fn)  -> applyFixForceInSimulationX_ simulation_ l fn filter
    (ForceFixPositionY fn)  -> applyFixForceInSimulationY_ simulation_ l fn filter

    CustomForce   -> putForceInSimulation_ simulation_ l h_ -- TODO not implemented or even designed yet

removeForceFromSimulation :: Force -> D3Simulation_ -> D3Simulation_
removeForceFromSimulation (Force l s t Nothing attrs h_) simulation_ =
  case t of
    ForceManyBody -> setAsNullForceInSimulation_ simulation_ l
    ForceCenter   -> setAsNullForceInSimulation_ simulation_ l
    ForceCollide  -> setAsNullForceInSimulation_ simulation_ l
    ForceX        -> setAsNullForceInSimulation_ simulation_ l
    ForceY        -> setAsNullForceInSimulation_ simulation_ l
    ForceRadial   -> setAsNullForceInSimulation_ simulation_ l

    ForceLink     -> unsetLinks_ simulation_

    (ForceFixPositionXY fn) -> removeFixForceXY_ simulation_ (const true) -- if there is no filter, must apply to all
    (ForceFixPositionX fn)  -> removeFixForceX_ simulation_ (const true) -- if there is no filter, must apply to all
    (ForceFixPositionY fn)  -> removeFixForceY_ simulation_ (const true) -- if there is no filter, must apply to all

    CustomForce   -> simulation_ -- TODO not implemented or even designed yet
removeForceFromSimulation (Force l s t (Just (FilterNodes _ filter)) attrs h_) simulation_ =
  case t of
    ForceManyBody -> setAsNullForceInSimulation_ simulation_ l
    ForceCenter   -> setAsNullForceInSimulation_ simulation_ l
    ForceCollide  -> setAsNullForceInSimulation_ simulation_ l
    ForceX        -> setAsNullForceInSimulation_ simulation_ l
    ForceY        -> setAsNullForceInSimulation_ simulation_ l
    ForceRadial   -> setAsNullForceInSimulation_ simulation_ l

    ForceLink     -> unsetLinks_ simulation_

    (ForceFixPositionXY fn) -> removeFixForceXY_ simulation_ filter
    (ForceFixPositionX fn)  -> removeFixForceX_ simulation_ filter
    (ForceFixPositionY fn)  -> removeFixForceY_ simulation_ filter

    CustomForce   -> simulation_ -- TODO not implemented or even designed yet



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

  (ForceFixPositionXY fn) ->

    """This \"force\" is really an over-ride for the force simulation, fixing the node at a particular point"""

  (ForceFixPositionX x) ->

    """This \"force\" is really an over-ride for the force simulation, fixing the node at a particular X dimension"""

  (ForceFixPositionY y) ->

    """This \"force\" is really an over-ride for the force simulation, fixing the node at a particular Y dimension"""

  ForceLink ->

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

  ForceLink                 -> forceLink_      unit -- in fact, not going to be created here, instead created on initialization of simulation (could revisit this tho)
  (CustomForce)             -> forceCustom_    unit
  -- NB there is actually no "force", in D3 terms, behind the fixed "forces", hence the dummy handle that is returned
  (ForceFixPositionXY _)  -> dummyForceHandle_ 
  (ForceFixPositionX _)   -> dummyForceHandle_
  (ForceFixPositionY _)   -> dummyForceHandle_

-- TODO at present there is no type checking on what forces have which attrs settable, see comment above
setForceAttr :: D3ForceHandle_ -> Attribute -> D3ForceHandle_
setForceAttr force_ (ToAttribute label attr) = do
  case label of
    "radius"      -> setForceRadius_      force_ (unbox attr) -- valid 
    "strength"    -> setForceStrength_    force_ (unbox attr) -- TODO add filter to this
    "theta"       -> setForceTheta_       force_ (unbox attr)
    "distanceMin" -> setForceDistanceMin_ force_ (unbox attr)
    "distanceMax" -> setForceDistanceMax_ force_ (unbox attr)
    "iterations"  -> setForceIterations_  force_ (unbox attr)
    "x"           -> setForceX_           force_ (unbox attr)
    "y"           -> setForceY_           force_ (unbox attr)
    "distance"    -> setForceDistance_    force_ (unbox attr)
    _ -> force_ -- no other force attributes accepted


  
