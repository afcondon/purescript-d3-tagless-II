module D3.Layouts.Simulation where

import D3.FFI.Config

import D3.Data.Types (D3Simulation_)
import D3.FFI (D3ForceLink_, D3ForceNode_, forceCenter_, forceCollideFixed_, forceCollideFn_, forceMany_, forceRadialFixed_, forceRadial_, forceX_, forceY_, initSimulation_, setLinks_)
import D3.Selection (DragBehavior)
import Prelude (Unit, (<$>))
import Unsafe.Coerce (unsafeCoerce)

-- | see bottom of file for all config defaults 
newtype Simulation id r l = Simulation (SimulationRecord_ id r l)

type SimulationRecord_ id r l = { 
    label  :: String
  , config :: SimulationConfig_
  , nodes  :: Array (D3ForceNode_ id r)
  , links  :: Array (D3ForceLink_ id r l)
  , forces :: Array Force
  , tick   :: Unit -> Unit -- could be Effect Unit
  , drag   :: DragBehavior -- TODO make strongly typed wrt actual Model used
}

data Force = Force ForceType
data ForceType =
    ForceManyBody     ForceManyConfig_
  | ForceCenter       ForceCenterConfig_
  | ForceCollideFixed ForceCollideFixedConfig_
  | ForceCollide      ForceCollideConfig_
  | ForceX            ForceXConfig_
  | ForceY            ForceYConfig_
  | ForceRadialFixed  ForceRadialFixedConfig_
  | ForceRadial       ForceRadialConfig_
  | Custom
  -- TODO bring back the force link below, in addition to, or replacing the insertion of the links in the simulation init
  -- | ForceLink (Array Link) (Link -> ID)
  
initSimulation :: forall model node link. Array Force -> model -> Array node -> Array link -> D3Simulation_
initSimulation forces model nodes links = do
  let nodes_ = unsafeCoerce nodes
      links_ = unsafeCoerce links
      simulation = initSimulation_ nodes_ defaultConfigSimulation
      _          = simulation `putForcesInSimulation` forces
      _          = simulation `setLinks_` links_
  simulation

putForcesInSimulation :: D3Simulation_ -> Array Force -> D3Simulation_
putForcesInSimulation simulation forces = do
  let 
    addForce :: Force -> D3Simulation_
    addForce =
      case _ of
        (Force (ForceManyBody config)) ->
          forceMany_ simulation config 
        (Force (ForceCenter config)) ->
          forceCenter_ simulation config
        -- (Force (ForceLink links idFn)) -> forceLinks
        (Force (ForceCollideFixed config)) ->
          forceCollideFixed_ simulation config
        (Force (ForceCollide config)) ->
          forceCollideFn_ simulation config
        (Force (ForceX config)) ->
          forceX_ simulation config
        (Force (ForceY config)) ->
          forceY_ simulation config
        (Force (ForceRadialFixed config)) ->
          forceRadialFixed_ simulation config
        (Force (ForceRadial config)) ->
          forceRadial_ simulation config

        (Force Custom) -> simulation -- do this later as needed
    _ = addForce <$> forces
  simulation


