module D3.Layouts.Simulation where

import Prelude

import D3.Selection (Chainable, D3Selection_, D3Simulation_, DragBehavior)
import Data.Map (Map)
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
type ID = Int -- TODO this needs to be polymorphic eventually, doesn't it?
type D3ForceLink_ r l = { 
    source :: D3ForceNode_ r
  , target :: D3ForceNode_ r
  | l
}
type D3ForceNode_ r = { 
    id    :: ID
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

type SimulationRecord_ r l = { 
    label  :: String
  , config :: SimulationConfig_
  , nodes  :: Array (D3ForceNode_ r)
  , links  :: Array (D3ForceLink_ r l)
  , forces :: Array Force
  , tick   :: Unit -> Unit -- could be Effect Unit
  , drag   :: DragBehavior -- TODO make strongly typed wrt actual Model used
}

newtype Simulation r l = Simulation (SimulationRecord_ r l)

initSimulation :: forall model node link. Array Force -> model -> Array node -> Array link -> D3Simulation_
initSimulation forces model nodes links = do
  let nodes_ = unsafeCoerce nodes
      links_ = unsafeCoerce links
      simulation = initSimulation_ nodes_ defaultConfigSimulation
      _          = simulation `putForcesInSimulation` forces
      _          = simulation `setLinks_` links_
  simulation

makeCenterForce :: Number -> Number -> Force
makeCenterForce width height = Force (ForceName "center") $ ForceCenter (width / 2.0) (height / 2.0)

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

foreign import initSimulation_  :: forall r.   Array (D3ForceNode_ r) -> SimulationConfig_ -> D3Simulation_
foreign import setNodes_        :: forall r.   D3Simulation_ -> Array (D3ForceNode_ r)     -> D3Simulation_
foreign import setLinks_        :: forall r l. D3Simulation_ -> Array (D3ForceLink_ r l)   -> D3Simulation_
foreign import startSimulation_ :: D3Simulation_ -> Unit
foreign import stopSimulation_  :: D3Simulation_ -> Unit

-- TODO this all has to change completely to work within Tagless 
-- foreign import data NativeSelection :: Type -- just temporarily defined to allow foreign functions to pass
-- foreign import addAttrFnToTick_           :: D3Selection_ -> D3Attr -> Unit
foreign import onTick_                    :: D3Simulation_ -> String -> (Unit -> Unit) -> Unit
foreign import defaultSimulationDrag_     :: D3Selection_ -> D3Simulation_ -> Unit
foreign import setAlphaTarget_            :: D3Selection_ -> Number -> Unit

-- implementations / wrappers for the Force ADT
foreign import forceMany_    :: D3Simulation_ -> String                     -> D3Simulation_
foreign import forceCenter_  :: D3Simulation_ -> String -> Number -> Number -> D3Simulation_
foreign import forceRadial_  :: D3Simulation_ -> String -> Number -> Number -> D3Simulation_
foreign import forceCollide_ :: D3Simulation_ -> String -> Number           -> D3Simulation_
foreign import forceX_       :: D3Simulation_ -> String -> Number           -> D3Simulation_
foreign import forceY_       :: D3Simulation_ -> String -> Number           -> D3Simulation_
