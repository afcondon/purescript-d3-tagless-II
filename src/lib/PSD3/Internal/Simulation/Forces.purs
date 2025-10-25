module PSD3.Internal.Simulation.Forces where

import Prelude

import PSD3.Internal.Attributes.Instances (Attr(..), AttrBuilder(..), AttributeSetter(..), Label, unboxAttr)
import PSD3.Internal.Types (D3Simulation_, Datum_)
import PSD3.Internal.FFI (D3ForceHandle_, forceCenter_, forceCollideFn_, forceLink_, forceMany_, forceRadial_, forceX_, forceY_, linksForceName, putForceInSimulation_, setAsNullForceInSimulation_, setForceDistanceMax_, setForceDistanceMin_, setForceDistance_, setForceIterations_, setForceRadius_, setForceStrength_, setForceTheta_, setForceX_, setForceY_, unsetLinks_)
import PSD3.Internal.Simulation.Types (ChainableF, Force(..), ForceFilter(..), ForceStatus(..), ForceType(..), LinkForceType(..), RegularForceType(..), _name, _status, toggleForceStatus)
import Data.Array (elem)
import Data.Foldable (class Foldable)
import Data.Lens (over, set, view)
import Data.Lens.At (at)
import Data.Map (Map, fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))


initialize   :: forall f. (Foldable f) => (Functor f) => f Force -> Map Label Force
initialize forces     = fromFoldable $ (\f -> Tuple (view _name f) f) <$> forces

putStatusMap :: Map Label ForceStatus -> Map Label Force -> Map Label Force
putStatusMap forceStatusMap forceMap = update <$> forceMap
  where
    update force =
      case (view (at (view _name force)) forceStatusMap) of -- get desired status from status map
        Nothing       -> set _status ForceDisabled force -- default is to disable
        (Just status) -> set _status status force 
    
showType :: ForceType -> String
showType = 
  case _ of
    LinkForce      -> "linkForce"
    RegularForce f -> show f

createForce :: Label -> ForceType -> Maybe ForceFilter -> Array ChainableF -> Force
createForce l t f cs = Force {
    "type": t
  , name: l
  , status: ForceDisabled
  , filter: f
  , attributes: cs
  , force_: createForce_ t
}

createLinkForce :: Maybe ForceFilter -> Array ChainableF -> Force
createLinkForce f cs = Force {
    "type": LinkForce
  , name: linksForceName
  , status: ForceDisabled
  , filter: f
  , attributes: cs
  , force_: createForce_ LinkForce
}

disableForce :: Force -> Force
disableForce = set _status ForceDisabled

enableForce :: Force -> Force
enableForce = set _status ForceActive

toggleForce :: Force -> Force
toggleForce = over _status toggleForceStatus

disableByLabels :: D3Simulation_ -> Array Label -> Force -> Force
disableByLabels simulation labels force =
  if (view _name force) `elem` labels
  then do
    let _ = removeForceFromSimulation force simulation
    disableForce force
  else force

enableByLabels :: D3Simulation_ -> Array Label -> Force -> Force
enableByLabels simulation labels force = 
  if (view _name force) `elem` labels
  then do
    let _ = putForceInSimulation force simulation
    enableForce force
  else force

enableOnlyTheseLabels :: D3Simulation_ -> Array Label -> Force -> Force
enableOnlyTheseLabels simulation labels force =
  if (view _name force) `elem` labels
  then do
    let _ = putForceInSimulation force simulation
    enableForce force
  else do
    let _ = removeForceFromSimulation force simulation
    disableForce force

updateForceInSimulation :: D3Simulation_ -> Force -> D3Simulation_
updateForceInSimulation simulation force = do
    let f = unwrap force
    let _ = (\a -> setForceAttr f.force_ f.filter (unwrap a)) <$> f.attributes -- side-effecting function that sets force's attributes
    case f.status of
      ForceActive -> putForceInSimulation force simulation
      ForceDisabled -> removeForceFromSimulation force simulation
    -- CustomForce   -> simulation_ -- REVIEW not implemented or even designed yet

putForceInSimulation :: Force -> D3Simulation_ -> D3Simulation_
putForceInSimulation (Force force) simulation_ = do
  case force.type of
    -- CustomForce   -> simulation_ -- REVIEW not implemented or even designed yet
    RegularForce _ -> putForceInSimulation_ simulation_ force.name force.force_
    -- NB putting the linkforce in the simulation doesn't put the links back in the simulation
    LinkForce      -> putForceInSimulation_ simulation_ force.name force.force_ -- FIXME need to reload the links if this is just a toggle


removeForceFromSimulation :: Force -> D3Simulation_ -> D3Simulation_
removeForceFromSimulation (Force force) simulation_ = do
  case force.type of
    -- CustomForce   -> simulation_ -- REVIEW not implemented or even designed yet
    RegularForce _ -> setAsNullForceInSimulation_ simulation_ force.name
    LinkForce      -> unsetLinks_ simulation_ -- NB we don't want to null out the links force, just remove all the links

forceDescription :: RegularForceType -> String
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

linkForceDescription :: LinkForceType -> String
linkForceDescription = case _ of
  ForceLink ->

    """The link force pushes linked nodes together or apart according to the
    desired link distance. The strength of the force is proportional to the
    difference between the linked nodes’ distance and the target distance,
    similar to a spring force."""

createForce_ :: ForceType -> D3ForceHandle_
createForce_ =
  case _ of
    RegularForce t -> createRegularForce_ t
    LinkForce      -> forceLink_ unit

-- TODO this needs to move to the D3 interpreter, with some parallel impls for String, Meta etc
createRegularForce_ :: RegularForceType -> D3ForceHandle_
createRegularForce_ = case _ of
  ForceManyBody             -> forceMany_      unit 
  ForceCenter               -> forceCenter_    unit
  ForceCollide              -> forceCollideFn_ unit
  ForceX                    -> forceX_         unit
  ForceY                    -> forceY_         unit
  ForceRadial               -> forceRadial_    unit

-- TODO at present there is no type checking on what forces have which attrs settable, see comment above
setForceAttr :: D3ForceHandle_ -> Maybe ForceFilter -> AttributeSetter -> D3ForceHandle_
setForceAttr force_ maybeFilter (AttributeSetter label attr) = do
  -- let attr' = unboxAttr attr
  case label of
    -- at present it's only strength that we can use in filters, set to 0 for excluded nodes
    "strength"    ->
      case maybeFilter of
        Nothing 
          -> setForceStrength_    force_ (unboxAttr attr)
        Just (ForceFilter _ filter)
          -> setForceStrength_    force_ (unboxAttr (attrFilter filter 0.0 attr)) 

    "radius"      -> setForceRadius_      force_ (unboxAttr attr)
    "theta"       -> setForceTheta_       force_ (unboxAttr attr)
    "distanceMin" -> setForceDistanceMin_ force_ (unboxAttr attr)
    "distanceMax" -> setForceDistanceMax_ force_ (unboxAttr attr)
    "iterations"  -> setForceIterations_  force_ (unboxAttr attr)
    "x"           -> setForceX_           force_ (unboxAttr attr)
    "y"           -> setForceY_           force_ (unboxAttr attr)
    "distance"    -> setForceDistance_    force_ (unboxAttr attr)
    _ -> force_ -- no other force attributes accepted


attrFilter :: (Datum_ -> Boolean) -> Number -> Attr -> Attr
attrFilter filter' default' = do
  let 
    addFilterToStatic :: (Datum_ -> Boolean) -> Number -> Number -> (Datum_ -> Number)
    addFilterToStatic filter value default = \d -> if filter d then value else default

    addFilterToFn :: (Datum_ -> Boolean) -> (Datum_ -> Number) -> Number -> (Datum_ -> Number)
    addFilterToFn filter fn default = \d -> if filter d then fn d else default

    -- addFilterToFnI :: (Datum_ -> Index_ -> Boolean) -> IndexedLambda Number -> Number -> IndexedLambda Number
    -- addFilterToFnI filter fni default = mkFn2 f 
    --   where
    --     f d i = if filter d i then runFn2 fni d i else default
  case _ of
    (StringAttr (Static a)) -> StringAttr (Static a)
    (StringAttr (Fn a))     -> StringAttr (Fn a)
    (StringAttr (FnI a))    -> StringAttr (FnI a)

    (NumberAttr (Static a)) -> NumberAttr (Fn (addFilterToStatic filter' a default')) -- turns static setter into dynamic because of filtering
    (NumberAttr (Fn a))     -> NumberAttr (Fn (addFilterToFn filter' a default'))
    (NumberAttr (FnI a))    -> NumberAttr (FnI a) -- NB doesn't handle filtering of indexed functions at the moment

    (ArrayAttr (Static a))  -> ArrayAttr (Static a)
    (ArrayAttr (Fn a))      -> ArrayAttr (Fn a)
    (ArrayAttr (FnI a))     -> ArrayAttr (FnI a)
