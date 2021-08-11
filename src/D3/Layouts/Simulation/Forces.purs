module D3.Simulation.Forces where

import D3.FFI
import Prelude

import D3.Attributes.Instances (Attr(..), AttrBuilder(..), AttributeSetter(..), IndexedLambda, Label, unboxAttr)
import D3.Data.Types (D3Simulation_, Datum_, Index_)
import D3.Simulation.Types (ChainableF, CustomForceType(..), FixForceType(..), Force(..), ForceLinksFilter(..), ForceNodesFilter(..), ForceStatus(..), LinkForceType(..), RegularForceType(..))
import Data.Array (elem)
import Data.Function.Uncurried (mkFn2, runFn2)
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)


toggleForceStatus :: ForceStatus -> ForceStatus
toggleForceStatus =
  case _ of
    ForceActive   -> ForceDisabled
    ForceDisabled -> ForceActive

getLinkForceHandle :: D3Simulation_ -> Maybe D3ForceHandle_
getLinkForceHandle simulation = toMaybe $ lookupForceByName_ simulation linksForceName

getLabel :: Force -> Label
getLabel (Force l _ _ _ _ _)       = l
getLabel (LinkForce l _ _ _ _)     = l
getLabel (FixForce l _ _ _ _ _)    = l

getHandle :: Force -> D3ForceHandle_
getHandle (Force l s t cs f h_)    = h_
getHandle (LinkForce l s f cs h_)  = h_
getHandle (FixForce l s t f cs h_) = h_

showType :: Force -> String
showType = 
  case _ of
    (Force _ _ t _ _ _) -> show t
    (LinkForce _ _ _ _ _) -> "linkForce"
    (FixForce _ _ t _ _ _) -> show t

isForceActive :: Force -> ForceStatus
isForceActive = 
  case _ of
    (Force _ s _ _ _ _)    -> s
    (LinkForce _ s _ _ _)  -> s
    (FixForce _ s _ _ _ _) -> s

createForce :: Label -> RegularForceType -> Maybe ForceNodesFilter -> Array ChainableF -> Force
createForce l t f cs = Force l ForceDisabled t f cs (createForce_ t)

createLinkForce :: Label -> Maybe ForceLinksFilter -> Array ChainableF -> Force
createLinkForce l f cs = LinkForce l ForceDisabled f cs (createLinkForce_ ForceLink)

createFixForce :: Label -> FixForceType -> Maybe ForceNodesFilter -> Array ChainableF -> Force
createFixForce l t f cs = FixForce l ForceDisabled t f cs (createFixForce_ t)

disableForce :: Force -> Force
disableForce (Force l _ t f cs h) = Force l ForceDisabled t f cs h
disableForce (LinkForce l _ f cs h) = LinkForce l ForceDisabled f cs h
disableForce (FixForce l _ t f cs h) = FixForce l ForceDisabled t f cs h

enableForce :: Force -> Force
enableForce (Force l _ t f cs h) = Force l ForceActive t f cs h
enableForce (LinkForce l _ f cs h) = LinkForce l ForceActive f cs h
enableForce (FixForce l _ t f cs h) = FixForce l ForceActive t f cs h
-- TODO but in fact when we toggle "Active" maybe we want to get a handle from D3 for it if it didn't have one? 

toggleForce :: Force -> Force
toggleForce (Force l s t f cs h_) = Force l (toggleForceStatus s) t f cs h_
toggleForce (LinkForce l s f cs h_) = LinkForce l (toggleForceStatus s) f cs h_
toggleForce (FixForce l s t f cs h_) = FixForce l (toggleForceStatus s) t f cs h_
-- TODO but in fact when we toggle "Active" maybe we want to get a handle from D3 for it if it didn't have one? 

disableByLabels :: D3Simulation_ -> Array Label -> Force -> Force
disableByLabels simulation labels force =
  if (getLabel force) `elem` labels
  then do
    let _ = removeForceFromSimulation force simulation
    disableForce force
  else force

enableByLabels :: D3Simulation_ -> Array Label -> Force -> Force
enableByLabels simulation labels force = 
  if (getLabel force) `elem` labels
  then do
    let _ = putForceInSimulation force simulation
    enableForce force
  else force

enableOnlyTheseLabels :: D3Simulation_ -> Array Label -> Force -> Force
enableOnlyTheseLabels simulation labels force =
  if (getLabel force) `elem` labels
  then do
    let _ = putForceInSimulation force simulation
    enableForce force
  else do
    let _ = removeForceFromSimulation force simulation
    disableForce force

putForceInSimulation :: Force -> D3Simulation_ -> D3Simulation_
putForceInSimulation (Force l s t f attrs h_) simulation_ =
  case t of
    ForceManyBody -> putForceInSimulation_ simulation_ l h_
    ForceCenter   -> putForceInSimulation_ simulation_ l h_
    ForceCollide  -> putForceInSimulation_ simulation_ l h_
    ForceX        -> putForceInSimulation_ simulation_ l h_
    ForceY        -> putForceInSimulation_ simulation_ l h_
    ForceRadial   -> putForceInSimulation_ simulation_ l h_

putForceInSimulation (LinkForce l s Nothing attrs h_) simulation_ = 
  putForceInSimulation_ simulation_ l h_
putForceInSimulation (LinkForce l s (Just (FilterLinks _ filter)) attrs h_) simulation_ = 
  putForceInSimulation_ simulation_ l h_ -- TODO actually use the filter given here

putForceInSimulation (FixForce l s t filter attrs h_) simulation_ =
  case filter of
    Nothing -> 
      case t of
        (ForceFixPositionXY fn) -> applyFixForceInSimulationXY_ simulation_ l fn (const true)
        (ForceFixPositionX fn)  -> applyFixForceInSimulationX_ simulation_ l fn (const true)
        (ForceFixPositionY fn)  -> applyFixForceInSimulationY_ simulation_ l fn (const true)
    (Just (FilterNodes _ filter)) ->
      case t of
        (ForceFixPositionXY fn) -> applyFixForceInSimulationXY_ simulation_ l fn filter
        (ForceFixPositionX fn)  -> applyFixForceInSimulationX_ simulation_ l fn filter
        (ForceFixPositionY fn)  -> applyFixForceInSimulationY_ simulation_ l fn filter

removeForceFromSimulation :: Force -> D3Simulation_ -> D3Simulation_
removeForceFromSimulation (Force l s t f attrs h_) simulation_ =
  case t of
    ForceManyBody -> setAsNullForceInSimulation_ simulation_ l
    ForceCenter   -> setAsNullForceInSimulation_ simulation_ l
    ForceCollide  -> setAsNullForceInSimulation_ simulation_ l
    ForceX        -> setAsNullForceInSimulation_ simulation_ l
    ForceY        -> setAsNullForceInSimulation_ simulation_ l
    ForceRadial   -> setAsNullForceInSimulation_ simulation_ l

removeForceFromSimulation (LinkForce l s filter attrs h_) simulation_ =
    unsetLinks_ simulation_

removeForceFromSimulation (FixForce l s t filter attrs h_) simulation_ =
  case filter of
    Nothing ->
      case t of
        (ForceFixPositionXY fn) -> removeFixForceXY_ simulation_ (const true) -- if there is no filter, must apply to all
        (ForceFixPositionX fn)  -> removeFixForceX_ simulation_ (const true) -- if there is no filter, must apply to all
        (ForceFixPositionY fn)  -> removeFixForceY_ simulation_ (const true) -- if there is no filter, must apply to all
    (Just (FilterNodes _ filter)) ->
      case t of
        (ForceFixPositionXY fn) -> removeFixForceXY_ simulation_ filter
        (ForceFixPositionX fn)  -> removeFixForceX_ simulation_ filter
        (ForceFixPositionY fn)  -> removeFixForceY_ simulation_ filter


    -- CustomForce   -> simulation_ -- TODO not implemented or even designed yet

    -- CustomForce   -> simulation_ -- TODO not implemented or even designed yet



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

fixForceDescription :: FixForceType -> String
fixForceDescription = case _ of
  (ForceFixPositionXY fn) ->

    """This \"force\" is really an over-ride for the force simulation, fixing the node at a particular point"""

  (ForceFixPositionX x) ->

    """This \"force\" is really an over-ride for the force simulation, fixing the node at a particular X dimension"""

  (ForceFixPositionY y) ->

    """This \"force\" is really an over-ride for the force simulation, fixing the node at a particular Y dimension"""

linkForceDescription :: LinkForceType -> String
linkForceDescription = case _ of
  ForceLink ->

    """The link force pushes linked nodes together or apart according to the
    desired link distance. The strength of the force is proportional to the
    difference between the linked nodes’ distance and the target distance,
    similar to a spring force."""


-- TODO this needs to move to the D3 interpreter, with some parallel impls for String, Meta etc
createForce_ :: RegularForceType -> D3ForceHandle_
createForce_ = case _ of
  ForceManyBody             -> forceMany_      unit 
  ForceCenter               -> forceCenter_    unit
  ForceCollide              -> forceCollideFn_ unit
  ForceX                    -> forceX_         unit
  ForceY                    -> forceY_         unit
  ForceRadial               -> forceRadial_    unit

createFixForce_ :: FixForceType -> D3ForceHandle_
createFixForce_ = case _ of
  -- NB there is actually no "force", in D3 terms, behind the fixed "forces", hence the dummy handle that is returned
  (ForceFixPositionXY _)  -> dummyForceHandle_ 
  (ForceFixPositionX _)   -> dummyForceHandle_
  (ForceFixPositionY _)   -> dummyForceHandle_

createLinkForce_ :: LinkForceType -> D3ForceHandle_
createLinkForce_ = case _ of
  ForceLink                 -> forceLink_      unit

-- TODO at present there is no type checking on what forces have which attrs settable, see comment above
setForceAttr :: D3ForceHandle_ -> AttributeSetter -> D3ForceHandle_
setForceAttr force_ (AttributeSetter label attr) = do
  case label of
    "radius"      -> setForceRadius_      force_ (unboxAttr attr)
    "strength"    -> setForceStrength_    force_ (unboxAttr attr)
    "theta"       -> setForceTheta_       force_ (unboxAttr attr)
    "distance"    -> setForceDistance_    force_ (unboxAttr attr)
    "distanceMin" -> setForceDistanceMin_ force_ (unboxAttr attr)
    "distanceMax" -> setForceDistanceMax_ force_ (unboxAttr attr)
    "iterations"  -> setForceIterations_  force_ (unboxAttr attr)
    "x"           -> setForceX_           force_ (unboxAttr attr)
    "y"           -> setForceY_           force_ (unboxAttr attr)
    -- NB key function attribute ONLY applies to links force and can ONLY be a (\d -> id) function
    -- this is not well captured in the type system right now 
    "keyFn"       -> setLinksKeyFunction_ force_ (unboxAttr attr) 
    -- Finally, there no other force attributes accepted
    _ -> force_ 


setForceAttrWithFilter :: D3ForceHandle_ -> (Datum_ -> Boolean) -> AttributeSetter -> D3ForceHandle_
setForceAttrWithFilter force_ filter (AttributeSetter label attr) = do
  -- let attr' = unboxAttr attr
  case label of
    "radius"      -> setForceRadius_      force_ (unboxAttr attr)
    "strength"    -> setForceStrength_    force_ (unboxAttr (attrFilter filter 0.0 attr)) 
    "theta"       -> setForceTheta_       force_ (unboxAttr attr)
    "distanceMin" -> setForceDistanceMin_ force_ (unboxAttr attr)
    "distanceMax" -> setForceDistanceMax_ force_ (unboxAttr attr)
    "iterations"  -> setForceIterations_  force_ (unboxAttr attr)
    "x"           -> setForceX_           force_ (unboxAttr attr)
    "y"           -> setForceY_           force_ (unboxAttr attr)
    "distance"    -> setForceDistance_    force_ (unboxAttr attr)
    _ -> force_ -- no other force attributes accepted


attrFilter :: (Datum_ -> Boolean) -> Number -> Attr -> Attr
attrFilter filter default = do
  let 
    addFilterToStatic :: (Datum_ -> Boolean) -> Number -> Number -> (Datum_ -> Number)
    addFilterToStatic filter value default = \d -> if filter d then value else default

    addFilterToFn :: (Datum_ -> Boolean) -> (Datum_ -> Number) -> Number -> (Datum_ -> Number)
    addFilterToFn filter fn default = \d -> if filter d then fn d else default

    addFilterToFnI :: (Datum_ -> Index_ -> Boolean) -> IndexedLambda Number -> Number -> IndexedLambda Number
    addFilterToFnI filter fni default = mkFn2 f 
      where
        f d i = if filter d i then runFn2 fni d i else default
  case _ of
    (StringAttr (Static a)) -> StringAttr (Static a)
    (StringAttr (Fn a))     -> StringAttr (Fn a)
    (StringAttr (FnI a))    -> StringAttr (FnI a)

    (NumberAttr (Static a)) -> NumberAttr (Fn (addFilterToStatic filter a default)) -- turns static setter into dynamic because of filtering
    (NumberAttr (Fn a))     -> NumberAttr (Fn (addFilterToFn filter a default))
    (NumberAttr (FnI a))    -> NumberAttr (FnI a) -- NB doesn't handle filtering of indexed functions at the moment

    (ArrayAttr (Static a))  -> ArrayAttr (Static a)
    (ArrayAttr (Fn a))      -> ArrayAttr (Fn a)
    (ArrayAttr (FnI a))     -> ArrayAttr (FnI a)

-- attrFilterI :: (Datum_ -> Index_ -> Boolean) -> Number -> Attr -> Attr
-- attrFilterI filter default = 
--   case _ of
--     (StringAttr (Static a)) -> StringAttr (Static a)
--     (StringAttr (Fn a))     -> StringAttr (Fn a)
--     (StringAttr (FnI a))    -> StringAttr (FnI a)

--     (NumberAttr (Static a)) -> NumberAttr (Fn (addFilterToStatic filter a default))
--     (NumberAttr (Fn a))     -> NumberAttr (Fn (addFilterToFn filter a default))
--     (NumberAttr (FnI a))    -> NumberAttr (FnI (addFilterToFnI filter a default))

--     (ArrayAttr (Static a))  -> ArrayAttr (Static a)
--     (ArrayAttr (Fn a))      -> ArrayAttr (Fn a)
--     (ArrayAttr (FnI a))     -> ArrayAttr (FnI a)

