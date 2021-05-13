module D3.Layouts.Simulation where

import D3.FFI.Config (ForceCenterConfig_, ForceCollideConfig_, ForceCollideFixedConfig_, ForceLinkConfig_, ForceManyConfig_, ForceRadialConfig_, ForceRadialFixedConfig_, ForceXConfig_, ForceYConfig_, SimulationConfig_)

import D3.Data.Types (D3Simulation_)
import D3.FFI (forceCenter_, forceCollideFixed_, forceCollideFn_, forceLink_, forceMany_, forceRadialFixed_, forceRadial_, forceX_, forceY_, getLinks_, getNodes_, initSimulation_)
import D3.Node (D3_Link, D3_Simulation_Node) 
import D3.Selection (DragBehavior)
import Prelude (Unit, (<$>))

-- | see bottom of file for all config defaults 
newtype Simulation :: forall k. k -> Type -> Row Type -> Type
newtype Simulation id r l = Simulation (SimulationRecord_ r l)

type SimulationRecord_ d r = {  -- 'd' is the type of the "data" field in each node, 'r' is the additional row-types in the link
    label  :: String
  , config :: SimulationConfig_
  , nodes  :: Array (D3_Simulation_Node d)
  , links  :: Array (D3_Link d r)
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
  | ForceLink         ForceLinkConfig_
  | Custom
  
initSimulation :: forall nodedata linkdata. 
  Array Force ->
  Array nodedata ->
  SimulationConfig_ ->
  { simulation :: D3Simulation_, nodes :: Array (D3_Simulation_Node nodedata), links :: Array (D3_Link nodedata linkdata) }
initSimulation forces nodeData config = do
  let 
      nodes            = (\d -> { "data": d } ) <$> nodeData -- put the data into the sim node, possibly set index here??
      simulation       = initSimulation_ nodes config
      _                = simulation `putForcesInSimulation` forces -- links if any go here
      initializedNodes = getNodes_ simulation
      initializedLinks = getLinks_ simulation -- TODO must be Maybe, might not be links

  { simulation, nodes: initializedNodes, links: initializedLinks }

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


