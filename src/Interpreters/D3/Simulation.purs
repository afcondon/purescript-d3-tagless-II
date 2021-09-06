module D3Tagless.Instance.Simulation where

import Prelude

import Control.Monad.State (class MonadState, StateT, get, modify_, runStateT)
import D3.Data.Types (D3Selection_)
import D3.FFI (defaultLinkTick_, defaultNodeTick_, disableTick_, onTick_)
import D3.Selection (applySelectionAttributeD3)
import D3.Selection.Functions (selectionAppendElement, selectionAttach, selectionFilterSelection, selectionJoin, selectionMergeSelections, selectionModifySelection, selectionOpenSelection, selectionUpdateJoin)
import D3.Simulation.Functions
import D3.Simulation.Types (D3SimulationState_, Step(..), _handle)
import D3Tagless.Capabilities (class SelectionM, class SimulationM, simulationHandle)
import Data.Lens (use)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Stories.Spago.State (_d3Simulation)
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

runEffectSimulation :: forall m a row.
  Bind m =>
  MonadState { simulation :: D3SimulationState_ | row } m =>
  MonadEffect m =>
  D3SimM row D3Selection_ a -> m Unit
runEffectSimulation state_T = do
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
  appendTo s_   = selectionAppendElement s_
  attach selector    = selectionAttach selector 
  filterSelection s_ = selectionFilterSelection s_
  mergeSelections s_ = selectionMergeSelections s_
  setAttributes s_ = selectionModifySelection s_
  on s_              = simulationOn s_ -- NB simulation "on" is handled differently from selectionOn
  openSelection s_   = selectionOpenSelection s_
  simpleJoin s_      = selectionJoin s_
  updateJoin s_      = selectionUpdateJoin s_



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
  carryOverSimStateN selection rawdata key = simulationPreservePositions selection rawdata key
  carryOverSimStateL selection rawdata key = simulationPreserveLinkReferences selection rawdata key
  setNodes           = simulationSetNodes
  setLinks           = simulationSetLinks 
  swizzleLinks       = simulationSwizzleLinks
  getNodes           = simulationGetNodes
  getLinks           = simulationGetLinks 

-- uniformlyDistribute nodes = pure $ setPositionToNaN_ nodes

-- management of selections
  -- addSelection label selection         = simulationAddSelection label selection
  -- getSelection label                   = simulationGetSelection label
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

  defaultNodeTick label selection = do
    handle <- simulationHandle
    let _ = defaultNodeTick_ label handle selection -- TODO this is hardwired to cx/cy right now
    pure unit

  defaultLinkTick label selection = do
    handle <- simulationHandle
    let _ = defaultLinkTick_ label handle selection
    pure unit
    
-- TODO is this really necessary tho? couldn't it be added to the tick function 
-- get the underlying simulation handle out of the simulation (necessary for tick functions? )
  simulationHandle = use (_d3Simulation <<< _handle)
