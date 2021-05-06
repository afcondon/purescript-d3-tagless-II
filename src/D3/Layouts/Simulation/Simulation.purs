module D3.Layouts.Simulation where

import D3.Data.Types
import Prelude

import D3.FFI (D3ForceLink_, D3ForceNode_, SimulationConfig_, forceCenter_, forceCollideFixed_, forceCollideFn_, forceMany_, forceRadial_, forceX_, forceY_, initSimulation_, setLinks_)
import D3.Selection (DragBehavior)
import Data.Maybe (Maybe)
import Unsafe.Coerce (unsafeCoerce)

-- import D3.Base.Attributes (Attr)
-- import D3.Base.Selection (Label)


defaultConfigSimulation :: SimulationConfig_
defaultConfigSimulation = { 
      alpha        : 1.0
    , alphaTarget  : 0.0
    , alphaMin     : 0.0001
    , alphaDecay   : 0.0228
    , velocityDecay: 0.4
}

newtype ForceName = ForceName String
data Force = Force ForceName ForceType
data ForceType =
    ForceMany -- { strength :: Maybe Number, theta :: Maybe Number, distanceMin :: Maybe Number, distanceMax :: Maybe Number } -- TODO these are all, like attributes, possibly static or functions
  | ForceCenter Number Number
  -- TODO bring back the force link below, in addition to, or replacing the insertion of the links in the simulation init
  -- | ForceLink (Array Link) (Link -> ID)
  | ForceCollideFixed Number
  | ForceCollide (Datum_ -> Number)
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
        (Force (ForceName label) ForceMany)                  -> forceMany_ simulation label 
        (Force (ForceName label) (ForceCenter cx cy))        -> forceCenter_ simulation label cx cy
        -- (Force (ForceLink links idFn))                    -> forceLinks
        (Force (ForceName label) (ForceCollideFixed radius_))-> forceCollideFixed_ simulation label radius_
        (Force (ForceName label) (ForceCollide radiusFn))    -> forceCollideFn_ simulation label radiusFn
        (Force (ForceName label) (ForceX x))                 -> forceX_ simulation label x
        (Force (ForceName label) (ForceY y))                 -> forceY_ simulation label y
        (Force (ForceName label) (ForceRadial cx cy))        -> forceRadial_ simulation label cx cy
        (Force (ForceName label) Custom)                     -> simulation -- do this later as needed
    _ = addForce <$> forces
  simulation

