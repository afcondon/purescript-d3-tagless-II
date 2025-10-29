module PSD3.Interpreter.D3 where

import Prelude

import Control.Monad.State (class MonadState, StateT, get, modify_, runStateT)
import PSD3.Internal.Types (D3Selection_, D3Simulation_)
import PSD3.Internal.FFI (defaultLinkTick_, defaultNodeTick_, disableTick_, getLinksFromSimulation_, getNodes_, onTick_)
import PSD3.Internal.Selection.Types (applySelectionAttributeD3)
import PSD3.Internal.Selection.Functions (selectionAppendElement, selectionAttach, selectionFilterSelection, selectionJoin, selectionMergeSelections, selectionModifySelection, selectionNestedJoin, selectionOn, selectionOpenSelection, selectionSelectUnder, selectionUpdateJoin)
import PSD3.Internal.Simulation.Types (D3SimulationState_, Step(..), SimVariable(..), _handle, _name)
import PSD3.Internal.Simulation.Functions (simulationActualizeForces, simulationMergeNewData, simulationOn, simulationSetLinks, simulationSetLinksFromSelection, simulationSetNodes, simulationSetNodesFromSelection, simulationSetVariable, simulationStart, simulationStop)
import PSD3.Internal.Sankey.Types (SankeyLayoutState_)
import PSD3.Internal.Sankey.Functions (sankeySetData, sankeySetDataWithConfig)
import PSD3.Capabilities.Selection (class SelectionM)
import PSD3.Capabilities.Simulation (class SimulationM, class SimulationM2)
import PSD3.Capabilities.Sankey (class SankeyM)
import Data.Array as Array
import Data.Lens (use, view, (.~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map as Map
import Data.Newtype (class Newtype)
import Data.Traversable (sequence)
import Type.Proxy (Proxy(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Unsafe.Coerce (unsafeCoerce)

-- ====================================================
-- Simple D3 Monad (no state)
-- ====================================================

-- not actually using Effect in foreign fns to keep sigs simple (for now)
newtype D3M :: forall k. Type -> k -> Type -> Type
newtype D3M state selection a = D3M (StateT state Effect a)

derive newtype instance functorD3M     :: Functor           (D3M state selection)
derive newtype instance applyD3M       :: Apply             (D3M state selection)
derive newtype instance applicativeD3M :: Applicative       (D3M state selection)
derive newtype instance bindD3M        :: Bind              (D3M state selection)
derive newtype instance monadD3M       :: Monad             (D3M state selection)
derive newtype instance monadStateD3M  :: MonadState  state (D3M state selection)
derive newtype instance monadEffD3M    :: MonadEffect       (D3M state selection)

-- | Selection instance (capability) for the D3 interpreter
instance d3TaglessD3M :: SelectionM D3Selection_ (D3M state D3Selection_) where
  attach selector    = selectionAttach selector
  selectUnder s_     = selectionSelectUnder s_
  appendTo s_        = selectionAppendElement s_
  filterSelection s_ = selectionFilterSelection s_
  openSelection s_   = selectionOpenSelection s_
  mergeSelections s_ = selectionMergeSelections s_
  setAttributes s_   = selectionModifySelection s_
  simpleJoin s_      = selectionJoin s_
  nestedJoin s_      = selectionNestedJoin s_
  updateJoin s_      = selectionUpdateJoin s_
  on s_              = selectionOn s_

runD3M :: forall a. D3M Unit D3Selection_ a -> Effect (Tuple a Unit)
runD3M (D3M state_T) = runStateT state_T unit

eval_D3M :: forall a. D3M Unit D3Selection_ a -> Effect a
eval_D3M (D3M state_T) = liftA1 fst $ runStateT state_T unit

exec_D3M :: forall a. D3M Unit D3Selection_ a -> Effect Unit
exec_D3M (D3M state_T) = liftA1 snd $ runStateT state_T unit

-- ====================================================
-- Simulation Monad (with simulation state)
-- ====================================================

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
  nestedJoin s_      = selectionNestedJoin s_
  updateJoin s_      = selectionUpdateJoin s_

-- | Simplified SimulationM instance - record-based initialization
instance SimulationM D3Selection_ (D3SimM row D3Selection_) where
  init config = do
    -- 1. Initialize force library in simulation state
    let forcesMap = Map.fromFoldable $ config.forces <#> \f -> Tuple (view _name f) f
    modify_ \state -> state { simulation = state.simulation # (_Newtype <<< prop (Proxy :: Proxy "forceLibrary")) .~ forcesMap }

    -- 2. Set up nodes first (must come before forces)
    nodesInSim <- simulationSetNodes config.nodes

    -- 3. Activate specified forces (must come BEFORE setting links!)
    -- The link force needs to be active in the simulation before links are added
    simulationActualizeForces config.activeForces

    -- 4. NOW set links (returns simulation-enhanced data)
    -- This must come after activating forces because the link force needs to be
    -- registered in the D3 simulation before we can add links to it
    linksInSim <- simulationSetLinks config.links config.nodes config.keyFn

    -- 5. Set configuration variables (except Alpha - that starts the simulation!)
    -- We skip Alpha here because setting it auto-starts the simulation in D3.
    -- The caller should use start() after adding tick functions.
    simulationSetVariable $ AlphaTarget config.config.alphaTarget
    simulationSetVariable $ AlphaMin config.config.alphaMin
    simulationSetVariable $ AlphaDecay config.config.alphaDecay
    simulationSetVariable $ VelocityDecay config.config.velocityDecay

    -- 6. Add tick functions
    handle <- use _handle
    let addTick label step = case step of
          StepTransformFFI _ _ -> pure unit
          Step selection chain -> do
            let makeTick _ = do
                  let _ = chain <#> applySelectionAttributeD3 (unsafeCoerce selection)
                  unit
                _ = onTick_ handle label makeTick
            pure unit
    _ <- (sequence :: Array (D3SimM row D3Selection_ Unit) -> D3SimM row D3Selection_ (Array Unit)) $ Map.toUnfoldable config.ticks <#> \(Tuple label step) -> addTick label step

    -- 7. Return enhanced data for joining to DOM
    pure { nodes: nodesInSim, links: linksInSim }

  start = simulationStart
  stop  = simulationStop

-- | SimulationM2 instance - Declarative updates for dynamic simulations
-- |
-- | TODO: There's an intermittent bug when toggling node filters in CodeExplorer
-- | where the simulation sometimes doesn't run properly. Unable to reproduce
-- | consistently. May be related to timing of DOM updates vs simulation updates,
-- | or possibly an issue with how filters affect the data flow.
instance SimulationM2 D3Selection_ (D3SimM row D3Selection_) where
  update config = do
    -- Get current simulation state
    handle <- use _handle
    currentNodes <- pure $ getNodes_ handle

    -- Step 1: Update configuration variables if provided
    case config.config of
      Nothing -> pure unit
      Just simConfig -> do
        simulationSetVariable $ AlphaTarget simConfig.alphaTarget
        simulationSetVariable $ AlphaMin simConfig.alphaMin
        simulationSetVariable $ AlphaDecay simConfig.alphaDecay
        simulationSetVariable $ VelocityDecay simConfig.velocityDecay
        -- Note: Don't set Alpha here - caller uses start() to begin animation

    -- Step 2: Determine nodes (new or existing), applying optional filter
    nodesBeforeFilter <- case config.nodes of
      Nothing -> pure currentNodes  -- Keep existing nodes
      Just newNodes -> pure newNodes  -- Use new nodes
    let nodesFiltered = case config.nodeFilter of
          Nothing -> nodesBeforeFilter  -- No filter, use all nodes
          Just predicate -> Array.filter predicate nodesBeforeFilter  -- Apply filter
    nodesInSim <- simulationSetNodes nodesFiltered

    -- Step 3: Update active forces if provided (must come BEFORE setting links!)
    case config.activeForces of
      Nothing -> pure unit
      Just forces -> simulationActualizeForces forces

    -- Step 4: Update links if provided (must come AFTER force activation), applying optional filter
    linksInSim <- case config.links of
      Nothing -> pure $ getLinksFromSimulation_ handle  -- Keep existing swizzled links as-is
      Just newLinks -> do
        -- Apply filter to new unswizzled links if provided
        let linksFiltered = case config.linkFilter of
              Nothing -> newLinks  -- No filter, use all new links
              Just predicate -> Array.filter predicate newLinks  -- Apply filter
        simulationSetLinks linksFiltered nodesInSim config.keyFn  -- Swizzle and set

    -- Step 5: Return enhanced data for DOM binding
    pure { nodes: nodesInSim, links: linksInSim }

  -- Tick function management
  addTickFunction _ (StepTransformFFI _ _) = pure unit
  addTickFunction label (Step selection chain) = do
    handle <- use _handle
    let makeTick _ = do
          let _ = chain <#> applySelectionAttributeD3 (unsafeCoerce selection)
          unit
        _ = onTick_ handle label makeTick
    pure unit

  removeTickFunction label = do
    handle <- use _handle
    let _ = disableTick_ handle label
    pure unit

-- ====================================================
-- Sankey Monad (with sankey layout state)
-- ====================================================

newtype D3SankeyM :: forall k. Row Type -> k -> Type -> Type
newtype D3SankeyM row selection a = D3SankeyM (StateT { sankeyLayout :: SankeyLayoutState_ | row } Effect a)

run_D3M_Sankey :: forall a row. { sankeyLayout :: SankeyLayoutState_ | row } -> D3SankeyM row D3Selection_ a -> Effect (Tuple a ({ sankeyLayout :: SankeyLayoutState_ | row }))
run_D3M_Sankey state (D3SankeyM state_T) = runStateT state_T state

exec_D3M_Sankey :: forall a row. { sankeyLayout :: SankeyLayoutState_ | row } -> D3SankeyM row D3Selection_ a -> Effect { sankeyLayout :: SankeyLayoutState_ | row }
exec_D3M_Sankey state (D3SankeyM state_T) = liftA1 snd $ runStateT state_T state

eval_D3M_Sankey :: forall a row. { sankeyLayout :: SankeyLayoutState_ | row } -> D3SankeyM row D3Selection_ a -> Effect a
eval_D3M_Sankey state (D3SankeyM state_T) = liftA1 fst $ runStateT state_T state

runWithD3_Sankey :: forall m a row.
  Bind m =>
  MonadState { sankeyLayout :: SankeyLayoutState_ | row } m =>
  MonadEffect m =>
  D3SankeyM row D3Selection_ a -> m Unit
runWithD3_Sankey state_T = do
    state <- get
    state' <- liftEffect $ exec_D3M_Sankey state state_T
    modify_ (\_ -> state')

evalEffectSankey :: forall m a row.
  Bind m =>
  MonadState { sankeyLayout :: SankeyLayoutState_ | row } m =>
  MonadEffect m =>
  D3SankeyM row D3Selection_ a -> m a
evalEffectSankey state_T = do
    state <- get
    (Tuple a state') <- liftEffect $ run_D3M_Sankey state state_T
    modify_ (\_ -> state')
    pure a

derive newtype instance functorD3SankeyM     :: Functor           (D3SankeyM row selection)
derive newtype instance applyD3SankeyM       :: Apply             (D3SankeyM row selection)
derive newtype instance applicativeD3SankeyM :: Applicative       (D3SankeyM row selection)
derive newtype instance bindD3SankeyM        :: Bind              (D3SankeyM row selection)
derive newtype instance monadD3SankeyM       :: Monad             (D3SankeyM row selection)
derive newtype instance monadEffD3SankeyM    :: MonadEffect       (D3SankeyM row selection)
derive newtype instance monadStateD3SankeyM  :: MonadState { sankeyLayout :: SankeyLayoutState_ | row } (D3SankeyM row selection)

instance showD3SankeyM :: Show (D3SankeyM row D3Selection_ a) where
  show x = "D3SankeyM"

-- SelectionM instance - delegate to existing selection functions
instance SelectionM D3Selection_ (D3SankeyM row D3Selection_) where
  appendTo s_        = selectionAppendElement s_
  selectUnder s_     = selectionSelectUnder s_
  attach selector    = selectionAttach selector
  filterSelection s_ = selectionFilterSelection s_
  mergeSelections s_ = selectionMergeSelections s_
  setAttributes s_   = selectionModifySelection s_
  on s_              = selectionOn s_
  openSelection s_   = selectionOpenSelection s_
  simpleJoin s_      = selectionJoin s_
  nestedJoin s_      = selectionNestedJoin s_
  updateJoin s_      = selectionUpdateJoin s_

-- SankeyM instance - Sankey-specific operations
instance SankeyM D3Selection_ (D3SankeyM row D3Selection_) where
  setSankeyData data_ width height = sankeySetData data_ width height
  setSankeyDataWithConfig data_ width height config = sankeySetDataWithConfig data_ width height config
