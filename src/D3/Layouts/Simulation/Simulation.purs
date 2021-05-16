module D3.Layouts.Simulation where

import D3.Data.Types (D3Simulation_)
import D3.FFI (forceCenter_, forceCollideFixed_, forceCollideFn_, forceLink_, forceMany_, forceRadialFixed_, forceRadial_, forceX_, forceY_, getNodes_, initSimulation_)
import D3.FFI.Config (ForceCenterConfig_, ForceCollideConfig_, ForceCollideFixedConfig_, ForceLinkConfig_, ForceManyConfig_, ForceRadialConfig_, ForceRadialFixedConfig_, ForceXConfig_, ForceYConfig_, SimulationConfig_)
import D3.Node 
import D3.Selection (DragBehavior)
import Prelude (Unit, (<$>))

type SimulationRecord_ d l = (  
-- 'd' is the type of the "data" field in each node
-- 'l' is the additional row-types in the link
    label     :: String
  , config    :: SimulationConfig_
  , nodes     :: Array (D3SimulationRow d)
  , idLinks   :: Array (D3_Link NodeID l)
  , objLinks  :: Array (D3_Link (D3SimulationRow d) l)
  , forces    :: Array Force
  , tick      :: Unit -> Unit -- could be Effect Unit
  , drag      :: DragBehavior -- TODO make strongly typed wrt actual Model used
)

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
  | ForceLink         ForceLinkConfig_
  | Custom
  
initSimulation :: forall nodedata noderow. -- ugly duplication, isn't there a way to express it w/o ?
  Array Force ->
  Array nodedata ->
  SimulationConfig_ -> { simulation :: D3Simulation_, nodes :: Array (D3SimulationRow noderow) }
initSimulation forces nodeData config = do
  let 
      simulation       = initSimulation_ nodeData config
      simulation'      = putForcesInSimulation simulation forces 
      initializedNodes = getNodes_ simulation'

  { simulation: simulation', nodes: initializedNodes }

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
        (Force (ForceLink config)) ->
          forceLink_ simulation config

        (Force Custom) -> simulation -- do this later as needed
    _ = addForce <$> forces
  simulation


