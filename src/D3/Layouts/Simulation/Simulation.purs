module D3.Layouts.Simulation where

import Prelude

import D3.Selection (D3Selection_, D3Simulation_, DragBehavior)
import Unsafe.Coerce (unsafeCoerce)

-- import D3.Base.Attributes (Attr)
-- import D3.Base.Selection (Label)

-- | a record to initialize / configure simulations
type SimulationConfig_ = { 
      alpha         :: Number
    , alphaTarget   :: Number
    , alphaMin      :: Number
    , alphaDecay    :: Number
    , velocityDecay :: Number
}

defaultConfigSimulation :: SimulationConfig_
defaultConfigSimulation = { 
      alpha        : 1.0
    , alphaTarget  : 0.0
    , alphaMin     : 0.0001
    , alphaDecay   : 0.0228
    , velocityDecay: 0.4
}

-- | Force Layout core types
type D3ForceLink_ id r l = { 
    source :: D3ForceNode_ id r
  , target :: D3ForceNode_ id r
  | l
}
type D3ForceNode_ id r = { 
    id    :: id
  , index :: Number
  , x     :: Number
  , y     :: Number
  , vx    :: Number
  , vy    :: Number
  | r
}
newtype ForceName = ForceName String
data Force = Force ForceName ForceType
data ForceType =
    ForceMany
  | ForceCenter Number Number
  -- | ForceLink (Array Link) (Link -> ID)
  | ForceCollide Number
  | ForceX Number
  | ForceY Number
  | ForceRadial Number Number
  | Custom

type SimulationRecord_ id r l = { 
    label  :: String
  , config :: SimulationConfig_
  , nodes  :: Array (D3ForceNode_ id r)
  , links  :: Array (D3ForceLink_ id r l)
  , forces :: Array Force
  , tick   :: Unit -> Unit -- could be Effect Unit
  , drag   :: DragBehavior -- TODO make strongly typed wrt actual Model used
}

newtype Simulation id r l = Simulation (SimulationRecord_ id r l)

initSimulation :: forall model node link. Array Force -> model -> Array node -> Array link -> D3Simulation_
initSimulation forces model nodes links = do
  let nodes_ = unsafeCoerce nodes
      links_ = unsafeCoerce links
      simulation = initSimulation_ nodes_ defaultConfigSimulation
      _          = simulation `putForcesInSimulation` forces
      _          = simulation `setLinks_` links_
  simulation

makeCenterForce :: Number -> Number -> Force
makeCenterForce cx cy = Force (ForceName "center") $ ForceCenter cx cy

putForcesInSimulation :: D3Simulation_ -> Array Force -> D3Simulation_
putForcesInSimulation simulation forces = do
  let 
    addForce :: Force -> D3Simulation_
    addForce =
      case _ of
        (Force (ForceName label) ForceMany)                 -> forceMany_ simulation label 
        (Force (ForceName label) (ForceCenter cx cy))       -> forceCenter_ simulation label cx cy
        -- (Force (ForceLink links idFn)) -> forceLinks
        (Force (ForceName label) (ForceCollide radius_))    -> forceCollide_ simulation label radius_
        (Force (ForceName label) (ForceX x))                -> forceX_ simulation label x
        (Force (ForceName label) (ForceY y))                -> forceY_ simulation label y
        (Force (ForceName label) (ForceRadial cx cy))       -> forceRadial_ simulation label cx cy
        (Force (ForceName label) Custom)                    -> simulation -- do this later as needed
    _ = addForce <$> forces
  simulation

-- | foreign types associated with Force Layout Simulation
-- TODO structures here carried over from previous interpreter - review and refactor

foreign import initSimulation_  :: forall id r.   Array (D3ForceNode_ id r) -> SimulationConfig_ -> D3Simulation_
foreign import setNodes_        :: forall id r.   D3Simulation_ -> Array (D3ForceNode_ id r)     -> D3Simulation_
foreign import setLinks_        :: forall id r l. D3Simulation_ -> Array (D3ForceLink_ id r l)   -> D3Simulation_
foreign import startSimulation_ :: D3Simulation_ -> Unit
foreign import stopSimulation_  :: D3Simulation_ -> Unit

-- TODO this all has to change completely to work within Tagless 
-- foreign import data NativeSelection :: Type -- just temporarily defined to allow foreign functions to pass
-- foreign import addAttrFnToTick_           :: D3Selection_ -> D3Attr -> Unit
foreign import onTick_                :: D3Simulation_ -> String -> (Unit -> Unit) -> Unit
foreign import defaultSimulationDrag_ :: D3Selection_ -> D3Simulation_ -> Unit
foreign import setAlphaTarget_        :: D3Selection_ -> Number -> Unit

-- implementations / wrappers for the Force ADT
foreign import forceMany_    :: D3Simulation_ -> String                     -> D3Simulation_
foreign import forceCenter_  :: D3Simulation_ -> String -> Number -> Number -> D3Simulation_
foreign import forceRadial_  :: D3Simulation_ -> String -> Number -> Number -> D3Simulation_
foreign import forceCollide_ :: D3Simulation_ -> String -> Number           -> D3Simulation_
foreign import forceX_       :: D3Simulation_ -> String -> Number           -> D3Simulation_
foreign import forceY_       :: D3Simulation_ -> String -> Number           -> D3Simulation_
