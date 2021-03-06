module D3Tagless.Instance.Simulation where

import D3.Simulation.Functions
import Prelude

import Control.Monad.State (class MonadState, StateT, get, modify_, runStateT)
import D3.Data.Types (D3Selection_)
import D3.FFI (defaultLinkTick_, defaultNodeTick_, disableTick_, onTick_)
import D3.Selection (applySelectionAttributeD3)
import D3.Selection.Functions (selectionAppendElement, selectionAttach, selectionFilterSelection, selectionJoin, selectionMergeSelections, selectionModifySelection, selectionOpenSelection, selectionSelectUnder, selectionUpdateJoin)
import D3.Simulation.Types (D3SimulationState_, Step(..), _handle)
import D3Tagless.Capabilities (class SelectionM, class SimulationM, simulationHandle)
import Data.Array (partition)
import Data.Lens (use)
import Data.Map (filter, toUnfoldable)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Unsafe.Coerce (unsafeCoerce)

-- | ====================================================
-- | Simulation instance (capability) for the D3 interpreter
-- | ====================================================
newtype D3SimM :: forall k. Row Type -> k -> Type -> Type
newtype D3SimM row selection a = D3SimM (StateT { simulation :: D3SimulationState_ | row } Effect a) 

run_D3M_Simulation :: forall a row. { simulation :: D3SimulationState_ | row } -> D3SimM row D3Selection_ a -> Effect (Tuple a ({ simulation :: D3SimulationState_ | row }))
run_D3M_Simulation simulation (D3SimM state_T) = runStateT state_T simulation

exec_D3M_Simulation :: forall a row. { simulation :: D3SimulationState_ | row } -> D3SimM row D3Selection_ a -> Effect { simulation :: D3SimulationState_ | row }
exec_D3M_Simulation simulation (D3SimM state_T) = liftA1 snd $ runStateT state_T simulation

eval_D3M_Simulation :: forall a row. { simulation :: D3SimulationState_ | row } -> D3SimM row D3Selection_ a -> Effect a
eval_D3M_Simulation simulation (D3SimM state_T) = liftA1 fst $ runStateT state_T simulation

runWithD3_Simulation :: forall m a row.
  Bind m =>
  MonadState { simulation :: D3SimulationState_ | row } m =>
  MonadEffect m =>
  D3SimM row D3Selection_ a -> m Unit
runWithD3_Simulation state_T = do
    state <- get
    state' <- liftEffect $ exec_D3M_Simulation state state_T
    modify_ (\_ -> state')

evalEffectSimulation :: forall m a row.
  Bind m =>
  MonadState { simulation :: D3SimulationState_ | row } m =>
  MonadEffect m =>
  D3SimM row D3Selection_ a -> m a
evalEffectSimulation state_T = do
    state <- get
    (Tuple a state') <- liftEffect $ run_D3M_Simulation state state_T
    modify_ (\_ -> state')
    pure a

derive newtype instance functorD3SimM     :: Functor           (D3SimM row selection)
derive newtype instance applyD3SimM       :: Apply             (D3SimM row selection)
derive newtype instance applicativeD3SimM :: Applicative       (D3SimM row selection)
derive newtype instance bindD3SimM        :: Bind              (D3SimM row selection)
derive newtype instance monadD3SimM       :: Monad             (D3SimM row selection)
derive newtype instance monadEffD3SimM    :: MonadEffect       (D3SimM row selection)

derive newtype instance monadStateD3SimM  :: MonadState  { simulation :: D3SimulationState_ | row } (D3SimM row selection) 

instance showD3SimM :: Show (D3SimM row D3Selection_ a) where
  show x = "D3SimM"

instance SelectionM D3Selection_ (D3SimM row D3Selection_) where
  appendTo s_        = selectionAppendElement s_
  selectUnder s_     = selectionSelectUnder s_
  attach selector    = selectionAttach selector 
  filterSelection s_ = selectionFilterSelection s_
  mergeSelections s_ = selectionMergeSelections s_
  setAttributes s_   = selectionModifySelection s_
  on s_              = simulationOn s_ -- NB simulation "on" is handled differently from selectionOn
  openSelection s_   = selectionOpenSelection s_
  simpleJoin s_      = selectionJoin s_
  updateJoin s_      = selectionUpdateJoin s_



-- TODO should each of these facets (stop/go, forces, data, selections, tick functions)
instance SimulationM D3Selection_ (D3SimM row D3Selection_) where
-- stop and go
  start                 = simulationStart
  stop                  = simulationStop
-- management of simulation variables
  setConfigVariable v   = simulationSetVariable v
-- management of forces
  actualizeForces       = simulationUpdateForceStatuses
-- management of data 
  setNodes = simulationSetNodes
  setLinks = simulationSetLinks 
  mergeNewDataWithSim selection   = simulationMergeNewData selection
  setNodesFromSelection selection = simulationSetNodesFromSelection selection
  setLinksFromSelection selection = simulationSetLinksFromSelection selection

-- management of tick functions, what to do with the selection on each step of simulation
    -- TODO this would be the more efficient but less attractive route to defining a Tick function
  addTickFunction _ (StepTransformFFI _ _) = do
    pure unit
  addTickFunction label (Step selection chain) = do
    handle <- simulationHandle
    let makeTick _ = do
          -- TODO this coerce is forced upon us here due to forall selection in SimulationM
          let _ = chain <#> applySelectionAttributeD3 (unsafeCoerce selection)
          unit
        _ = onTick_ handle label makeTick
    pure unit

  removeTickFunction label = do
    handle <- simulationHandle
    -- TODO delete the tick function from the state
    let _ = disableTick_ handle label
    pure unit
    
-- get the underlying simulation handle out of the simulation (necessary for tick functions? )
  simulationHandle = use _handle
-- TODO is this really necessary tho? couldn't it be added to the tick function 
