module D3.Simulation.Functions where

import Prelude

import Control.Monad.State (class MonadState, State, get, gets, modify_)
import D3.Attributes.Instances (Label)
import D3.FFI (onTick_, setAlphaDecay_, setAlphaMin_, setAlphaTarget_, setAlpha_, setAsNullForceInSimulation_, setLinks_, setNodes_, setVelocityDecay_, startSimulation_, stopSimulation_)
import D3.Node (D3_Link, D3_SimulationNode)
import D3.Selection (applyChainableSD3)
import D3.Simulation.Forces (disableByLabels, enableByLabels, putForceInSimulation, setForceAttr)
import D3.Simulation.Types (Force(..), ForceStatus(..), SimVariable(..), SimulationState_(..), Step(..))
import Data.Array (intercalate)
import Data.Array as A
import Data.Foldable (traverse_)
import Data.Map as M
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Debug (spy)
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

-- | Underlying functions which allow us to make monadic updates from OUTSIDE of a script
-- | allowing control of the simulation outside of the drawing phase which runs in D3M
setForcesEmpty :: SimulationState_ -> SimulationState_
setForcesEmpty (SS_ ss_) = SS_ ss_ { forces = M.empty }

setForces :: M.Map Label Force -> SimulationState_ -> SimulationState_
setForces forces (SS_ ss_) = SS_ ss_ { forces = forces }

insertForce :: Force -> SimulationState_ -> SimulationState_
insertForce force@(Force l status t attrs h_) (SS_ ss_) = SS_ ss_ { forces = M.insert l force ss_.forces }

reheatSimulation :: SimulationState_ -> SimulationState_
reheatSimulation (SS_ ss_) = SS_ ss_ { running = true, alpha = 1.0 }

stopSimulation :: SimulationState_ -> SimulationState_
stopSimulation (SS_ ss_) = SS_ ss_ { running = false }

-- type SimulationStateRow row = ( simulation :: SimulationState_ | row )

simulationLoadForces :: forall m row. 
  (MonadState { simulationState :: SimulationState_ | row } m) => 
  Array Force -> m Unit
simulationLoadForces forces = do
  simulationRemoveAllForces
  traverse_ simulationAddForce forces
  pure unit

simulationRemoveAllForces :: forall m row. 
  (MonadState { simulationState :: SimulationState_ | row } m) => 
  m Unit
simulationRemoveAllForces = do
  { simulationState: SS_ ss_} <- get
  let _ = (setAsNullForceInSimulation_ ss_.simulation_) <$> (A.fromFoldable $ M.keys ss_.forces)
      updatedSimulation = SS_ ss_ { forces = M.empty }
  modify_ (\s -> s { simulationState = updatedSimulation })

simulationAddForce :: forall m row. 
  (MonadState { simulationState :: SimulationState_ | row } m) =>
  Force -> m Unit
simulationAddForce force@(Force l status t attrs h_) = do
  let _ = (\a -> setForceAttr h_ (unwrap a)) <$> attrs 
  { simulationState: SS_ ss_} <- get
  let _ = if status == ForceActive
          then putForceInSimulation force ss_.simulation_
          else ss_.simulation_
      updatedSimulation = SS_ ss_ { forces = M.insert l force ss_.forces }
  -- if the force isn't active then we just keep it in map, with label as key
  modify_ (\s -> s { simulationState = updatedSimulation } )

simulationDisableForcesByLabel :: forall m row. 
  (MonadState { simulationState :: SimulationState_ | row } m) =>
  Array Label -> m Unit
simulationDisableForcesByLabel labels = do
  { simulationState: SS_ ss_} <- get
  let updatedForces = (disableByLabels ss_.simulation_ labels) <$> ss_.forces
      updatedSimulation = SS_ ss_ { forces = updatedForces }
  modify_ (\s -> s { simulationState = updatedSimulation })

simulationEnableForcesByLabel :: forall m row. 
  (MonadState { simulationState :: SimulationState_ | row } m) =>
  Array Label  -> m Unit
simulationEnableForcesByLabel labels  = do
  { simulationState: SS_ ss_} <- get
  let updatedForces = (enableByLabels ss_.simulation_ labels) <$> ss_.forces
      updatedSimulation = SS_ ss_ { forces = updatedForces }
  modify_ (\s -> s { simulationState = updatedSimulation })
  
simulationSetVariable :: forall m row.
  (MonadState { simulationState :: SimulationState_ | row } m) => 
  SimVariable -> m Unit
simulationSetVariable v = do
  { simulationState: SS_ ss_} <- get
  let updatedSimulation =
        case v of
          (Alpha n)         -> do
            let _ = setAlpha_ ss_.simulation_ n
            SS_ ss_ { alpha = n }
          (AlphaTarget n)   -> do
            let _ = setAlphaTarget_ ss_.simulation_ n
            SS_ ss_ { alphaTarget = n }
          (AlphaMin n)      -> do
            let _ = setAlphaMin_ ss_.simulation_ n
            SS_ ss_ { alphaMin = n }
          (AlphaDecay n)    -> do
            let _ = setAlphaDecay_ ss_.simulation_ n
            SS_ ss_ { alphaDecay = n }
          (VelocityDecay n) -> do
            let _ = setVelocityDecay_ ss_.simulation_ n
            SS_ ss_ { velocityDecay = n }
  modify_ (\s -> s { simulationState = updatedSimulation })

simulationStart :: forall m row. 
  (MonadState { simulationState :: SimulationState_ | row } m) =>
  m Unit
simulationStart = do
  { simulationState: SS_ ss_} <- get
  let _ = startSimulation_ ss_.simulation_
      _ = setAlpha_ ss_.simulation_ 1.0
      updatedSimulation = SS_ ss_ { running = true, alpha = 1.0 }
  modify_ (\state -> state { simulationState = updatedSimulation } )

simulationStop :: forall m row. 
  (MonadState { simulationState :: SimulationState_ | row } m) => 
  m Unit
simulationStop = do
  { simulationState: SS_ ss_} <- get
  let _ = stopSimulation_ ss_.simulation_
      updatedSimulation = SS_ ss_ { running = false }
  modify_ (\state -> state { simulationState = updatedSimulation } )

simulationShowForces :: forall m row. 
  (MonadState { simulationState :: SimulationState_ | row } m) =>
  m String
simulationShowForces = do
  { simulationState: SS_ ss_} <- get
  let forceTuples = M.toUnfoldable ss_.forces
      showTuple (Tuple label force) = show label <> " " <> show force
  pure $ intercalate "\n" $ showTuple <$> forceTuples

simulationSetNodes :: forall m row d. 
  (MonadState { simulationState :: SimulationState_ | row } m) => 
  Array (D3_SimulationNode d) -> m (Array (D3_SimulationNode d))
simulationSetNodes nodes = do
  { simulationState: SS_ ss_} <- get
  let _ = ss_.simulation_ `setNodes_` nodes
  pure nodes -- TODO return the modified nodes instead (need to explicitly model these transformations)

simulationSetLinks :: forall m row d r. 
  (MonadState { simulationState :: SimulationState_ | row } m) => 
  Array (D3_Link d r) -> m (Array (D3_Link d r))
simulationSetLinks links = do
  { simulationState: SS_ ss_} <- get
  let _ = setLinks_ ss_.simulation_ links (\d i -> d.id)
  pure links -- TODO return the modified links, ie where indexes are replaced with object (references)

simulationCreateTickFunction :: forall selection row m. 
  (MonadState { simulationState :: SimulationState_ | row } m) =>
  Label -> Step selection -> m Unit
simulationCreateTickFunction label tick@(Step selection chain) = do
  { simulationState: SS_ ss_} <- get
  let makeTick _ = do
        -- TODO this coerce is forced upon us here due to forall selection in SimulationM
        -- going to have to parameterize simulation with selection or hide the type dep somehow
        let _ = (applyChainableSD3 (unsafeCoerce selection)) <$> chain
        unit
      updatedTicks      = M.insert label (unsafeCoerce tick) ss_.ticks
      updatedSimulation = SS_ ss_ { ticks = updatedTicks }
      _                 = onTick_ ss_.simulation_ label makeTick
  modify_ (\s -> s { simulationState = updatedSimulation} )
  

  