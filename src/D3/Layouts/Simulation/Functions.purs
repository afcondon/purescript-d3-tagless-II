module D3.Simulation.Functions where

import Control.Monad.State (State, get, modify_)
import D3.FFI (onTick_, setAlphaDecay_, setAlphaMin_, setAlphaTarget_, setAlpha_, setAsNullForceInSimulation_, setLinks_, setNodes_, setVelocityDecay_, startSimulation_, stopSimulation_)
import D3.Simulation.Forces (disableByLabels, enableByLabels, putForceInSimulation, setForceAttr)
import D3.Simulation.Types (Force(..), ForceStatus(..), SimVariable(..), SimulationState_(..), Step(..))
import Data.Newtype (unwrap)
import Prelude

import D3.Attributes.Instances (Label)
import D3.Node (D3_Link, D3_SimulationNode)
import D3.Selection (applyChainableSD3)
import Data.Array (intercalate)
import Data.Array as A
import Data.Foldable (traverse_)
import Data.Map as M
import Data.Tuple (Tuple(..))
import Unsafe.Coerce (unsafeCoerce)

-- | Underlying functions which allow us to make monadic updates from OUTSIDE of a script
-- | allowing control of the simulation outside of the drawing phase which runs in D3M
simulationLoadForces :: Array Force -> State SimulationState_ Unit
simulationLoadForces forces = do
  simulationRemoveAllForces
  traverse_ simulationAddForce forces

simulationRemoveAllForces :: State SimulationState_ Unit
simulationRemoveAllForces = do
    (SS_ sim) <- get
    let _ = (setAsNullForceInSimulation_ sim.simulation) <$> (A.fromFoldable $ M.keys sim.forces)
    modify_ (\(SS_ s) -> SS_ s { forces = (M.empty :: M.Map Label Force) } )

simulationAddForce :: Force -> State SimulationState_ Unit
simulationAddForce force@(Force l status t attrs h_) = do
    -- TODO this should be a traverse_ eventually
    let _ = (\a -> setForceAttr h_ (unwrap a)) <$> attrs 
    (SS_ sim) <- get
    let _ = if status == ForceActive
            then putForceInSimulation force sim.simulation
            else sim.simulation
    -- if the force isn't active then we just keep it in map, with label as key
    modify_ $ (\s -> SS_ sim { forces = M.insert l force sim.forces })

simulationDisableForcesByLabel :: Array Label -> State SimulationState_ Unit
simulationDisableForcesByLabel labels = do
  (SS_ sim) <- get
  let updatedForces = (disableByLabels sim.simulation labels) <$> sim.forces
  modify_ (\s -> SS_ sim { forces = updatedForces } )

simulationEnableForcesByLabel :: Array Label  -> State SimulationState_ Unit
simulationEnableForcesByLabel labels  = do
  (SS_ sim) <- get
  let updatedForces = (enableByLabels sim.simulation labels) <$> sim.forces
  modify_ (\s -> SS_ sim { forces = updatedForces } )
  
simulationSetVariable :: SimVariable -> State SimulationState_ Unit
simulationSetVariable v = do
  (SS_ sim) <- get
  let sim' = case v of
            (Alpha n)         -> do
              let _ = setAlpha_ sim.simulation n
              sim { alpha = n }
            (AlphaTarget n)   -> do
              let _ = setAlphaTarget_ sim.simulation n
              sim { alphaTarget = n }
            (AlphaMin n)      -> do
              let _ = setAlphaMin_ sim.simulation n
              sim { alphaMin = n }
            (AlphaDecay n)    -> do
              let _ = setAlphaDecay_ sim.simulation n
              sim { alphaDecay = n }
            (VelocityDecay n) -> do
              let _ = setVelocityDecay_ sim.simulation n
              sim { velocityDecay = n }
  modify_ (\s -> SS_ sim' )

simulationStart :: State SimulationState_ Unit
simulationStart = do
  (SS_ sim) <- get
  let _ = startSimulation_  sim.simulation
      _ = setAlpha_ sim.simulation 1.0
  modify_ (\s -> SS_ sim { running = true, alpha = 1.0 } )

simulationStop :: State SimulationState_ Unit
simulationStop = do
  (SS_ sim) <- get
  let _ = stopSimulation_ sim.simulation
  modify_ (\s -> SS_ sim { running = false } )

simulationShowForces :: State SimulationState_ String
simulationShowForces = do
  (SS_ sim) <- get
  let forceTuples = M.toUnfoldable sim.forces
      showTuple (Tuple label force) = show label <> " " <> show force
  pure $ intercalate "\n" $ showTuple <$> forceTuples

simulationSetNodes :: forall d. Array (D3_SimulationNode d) -> State SimulationState_ (Array (D3_SimulationNode d))
simulationSetNodes nodes = do
  (SS_ sim) <- get
  let _ = sim.simulation `setNodes_` nodes
  pure nodes -- TODO return the modified nodes instead (need to explicitly model these transformations)

simulationSetLinks :: forall d r. Array (D3_Link d r) -> State SimulationState_ (Array (D3_Link d r))
simulationSetLinks links = do
  (SS_ sim) <- get
  let _ = setLinks_ sim.simulation links (\d i -> d.id)
  pure links -- TODO return the modified links, ie where indexes are replaced with object (references)

simulationCreateTickFunction :: forall selection. Label -> Step selection -> State SimulationState_ Unit
simulationCreateTickFunction label tick@(Step selection chain) = do
  (SS_ sim) <- get
  let makeTick _ = do
        -- TODO this coerce is forced upon us here due to forall selection in SimulationM
        -- going to have to parameterize simulation with selection or hide the type dep somehow
        let _ = (applyChainableSD3 (unsafeCoerce selection)) <$> chain
        unit
  modify_ $ (\s -> SS_ sim { ticks = M.insert label (unsafeCoerce tick) sim.ticks })
  pure $ onTick_ sim.simulation label makeTick
  