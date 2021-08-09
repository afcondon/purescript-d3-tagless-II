module D3Tagless.Instance.Simulation where

import D3.Simulation.Functions
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, class Show, Unit, bind, discard, liftA1, pure, unit, ($), (<#>))

import Control.Monad.State (class MonadState, StateT, get, gets, modify_, runStateT)
import D3.Data.Types (D3Selection_)
import D3.FFI (defaultLinkTick_, defaultNodeTick_, disableTick_, onTick_)
import D3.Selection (applyChainableSD3)
import D3.Selection.Functions (selectionAppendElement, selectionAttach, selectionFilterSelection, selectionJoin, selectionModifySelection)
import D3.Simulation.Types (D3SimulationState_(..), Step(..))
import D3Tagless.Capabilities (class SelectionM, class SimulationM)
import Data.Tuple (Tuple, fst, snd)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Unsafe.Coerce (unsafeCoerce)

-- | ====================================================
-- | Simulation instance (capability) for the D3 interpreter
-- | ====================================================
newtype D3SimM :: forall k. Row Type -> k -> Type -> Type
newtype D3SimM row selection a = D3SimM (StateT { simulationState :: D3SimulationState_ | row } Effect a) 

run_D3M_Simulation :: forall a row. { simulationState :: D3SimulationState_ | row } -> D3SimM row D3Selection_ a -> Effect (Tuple a ({ simulationState :: D3SimulationState_ | row }))
run_D3M_Simulation simulation (D3SimM state_T) = runStateT state_T simulation

exec_D3M_Simulation :: forall a row. { simulationState :: D3SimulationState_ | row } -> D3SimM row D3Selection_ a -> Effect { simulationState :: D3SimulationState_ | row }
exec_D3M_Simulation simulation (D3SimM state_T) = liftA1 snd $ runStateT state_T simulation

eval_D3M_Simulation :: forall a row. { simulationState :: D3SimulationState_ | row } -> D3SimM row D3Selection_ a -> Effect a
eval_D3M_Simulation simulation (D3SimM state_T) = liftA1 fst $ runStateT state_T simulation

runEffectSimulation :: forall m a row.
  Bind m =>
  MonadState { simulationState :: D3SimulationState_ | row } m =>
  MonadEffect m =>
  D3SimM row D3Selection_ a -> m Unit
runEffectSimulation state_T = do
    state <- get
    state' <- liftEffect $ exec_D3M_Simulation state state_T
    modify_ (\_ -> state')

derive newtype instance functorD3SimM     :: Functor           (D3SimM row selection)
derive newtype instance applyD3SimM       :: Apply             (D3SimM row selection)
derive newtype instance applicativeD3SimM :: Applicative       (D3SimM row selection)
derive newtype instance bindD3SimM        :: Bind              (D3SimM row selection)
derive newtype instance monadD3SimM       :: Monad             (D3SimM row selection)
derive newtype instance monadEffD3SimM    :: MonadEffect       (D3SimM row selection)

derive newtype instance monadStateD3SimM  :: MonadState  { simulationState :: D3SimulationState_ | row } (D3SimM row selection) 

instance showD3SimM :: Show (D3SimM row D3Selection_ a) where
  show x = "D3SimM"

instance SelectionM D3Selection_ (D3SimM row D3Selection_) where
  attach selector    = selectionAttach selector 
  appendElement s_   = selectionAppendElement s_
  filterSelection s_ = selectionFilterSelection s_
  modifySelection s_ = selectionModifySelection s_
  join s_            = selectionJoin s_ -- maybe Join will also have to be different for simulation
  -- NOTE that simulation on is handled differently from selectionOn
  on s_              = simulationOn s_

-- TODO should each of these facets (stop/go, forces, data, selections, tick functions)
instance SimulationM D3Selection_ (D3SimM row D3Selection_) where
-- stop and go
  start                                = simulationStart
  stop                                 = simulationStop
-- management of simulation variables
  setConfigVariable v                  = simulationSetVariable v
-- management of forces
  addForce force                       = simulationAddForce force
  addForces forces                     = simulationAddForces forces  
  removeAllForces                      = simulationRemoveAllForces
  enableOnlyTheseForces                = simulationEnableOnlyTheseForces
  toggleForceByLabel label             = simulationToggleForce label
  setForcesByLabel { enable, disable } = 
    do
      simulationDisableForcesByLabel disable
      simulationEnableForcesByLabel  enable
-- management of data 
  prepareNodesAndLinks selection nodes links indexFn 
    = simulationPrepareNodesAndLinks selection nodes links indexFn
  getLinks                             = simulationGetLinks 
  getNodes                             = simulationGetNodes
-- uniformlyDistribute nodes         = pure $ setPositionToNaN_ nodes

-- management of selections
  addSelection label selection         = simulationAddSelection label selection
  getSelection label                   = simulationGetSelection label
-- management of tick functions, what to do with the selection on each step of simulation
    -- TODO this would be the more efficient but less attractive route to defining a Tick function
  addTickFunction label (StepTransformFFI selection function) = do
    pure unit
  addTickFunction label (Step selection chain) = do
    (SimState_ ss_) <- gets _.simulationState
    let makeTick _ = do
          -- TODO this coerce is forced upon us here due to forall selection in SimulationM
          let _ = chain <#> applyChainableSD3 (unsafeCoerce selection)
          unit
        _ = onTick_ ss_.simulation_ label makeTick
    pure unit

  removeTickFunction label = do
    (SimState_ ss_) <- gets _.simulationState
    -- TODO delete the tick function from the state
    let _ = disableTick_ ss_.simulation_ label
    pure unit

  defaultNodeTick label selection = do
    (SimState_ ss_) <- gets _.simulationState
    let _ = defaultNodeTick_ label ss_.simulation_ selection
    pure unit

  defaultLinkTick label selection = do
    (SimState_ ss_) <- gets _.simulationState
    let _ = defaultLinkTick_ label ss_.simulation_ selection
    pure unit
    
-- TODO is this really necessary tho? couldn't it be added to the tick function 
-- get the underlying simulation handle out of the simulation (necessary for tick functions? )
  simulationHandle = do
    (SimState_ { simulation_ }) <- gets _.simulationState
    pure simulation_