module D3Tagless.Instance.Simulation where

import D3.Simulation.Functions

import Control.Monad.State (class MonadState, StateT, get, gets, runStateT)
import D3.Data.Types (D3Selection_, D3Simulation_)
import D3.FFI (d3Append_, d3AttachZoomDefaultExtent_, d3AttachZoom_, d3Data_, d3EnterAndAppend_, d3Exit_, d3FilterSelection_, d3KeyFunction_, d3SelectAllInDOM_, d3SelectionSelectAll_, defaultDrag_, defaultLinkTick_, defaultNodeTick_, disableDrag_, disableTick_, onTick_)
import D3.Selection (Behavior(..), D3_Node(..), DragBehavior(..), Join(..), Keys(..), applyChainableSD3)
import D3.Simulation.Types (SimulationState_(..), Step(..))
import D3.Zoom (ScaleExtent(..), ZoomExtent(..))
import D3Tagless.Capabilities (class SelectionM, class SimulationM, defaultNodeTick, modifySelection)
import D3Tagless.Instance.Selection (D3M(..))
import Data.Foldable (foldl)
import Data.Tuple (Tuple, fst, snd)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, class Show, bind, discard, liftA1, pure, show, unit, ($), (<$>))
import Unsafe.Coerce (unsafeCoerce)

-- | ====================================================
-- | Simulation instance (capability) for the D3 interpreter
-- | ====================================================
newtype D3SimM :: forall k. Row Type -> k -> Type -> Type
newtype D3SimM row selection a = D3SimM (StateT { simulationState :: SimulationState_ | row } Effect a) 

run_D3M_Simulation :: forall a row. { simulationState :: SimulationState_ | row } -> D3SimM row D3Selection_ a -> Effect (Tuple a ({ simulationState :: SimulationState_ | row }))
run_D3M_Simulation simulation (D3SimM state_T) = runStateT state_T simulation

exec_D3M_Simulation :: forall a row. { simulationState :: SimulationState_ | row } -> D3SimM row D3Selection_ a -> Effect { simulationState :: SimulationState_ | row }
exec_D3M_Simulation simulation (D3SimM state_T) = liftA1 snd $ runStateT state_T simulation

eval_D3M_Simulation :: forall a row. { simulationState :: SimulationState_ | row } -> D3SimM row D3Selection_ a -> Effect a
eval_D3M_Simulation simulation (D3SimM state_T) = liftA1 fst $ runStateT state_T simulation

derive newtype instance functorD3SimM     :: Functor           (D3SimM row selection)
derive newtype instance applyD3SimM       :: Apply             (D3SimM row selection)
derive newtype instance applicativeD3SimM :: Applicative       (D3SimM row selection)
derive newtype instance bindD3SimM        :: Bind              (D3SimM row selection)
derive newtype instance monadD3SimM       :: Monad             (D3SimM row selection)
derive newtype instance monadEffD3SimM    :: MonadEffect       (D3SimM row selection)

derive newtype instance monadStateD3SimM  :: MonadState  { simulationState :: SimulationState_ | row } (D3SimM row selection) 

instance showD3SimM :: Show (D3SimM row D3Selection_ a) where
  show x = "D3SimM"

instance SimulationM D3Selection_ (D3SimM row D3Selection_) where
  start                       = simulationStart
  stop                        = simulationStop

  setConfigVariable v         = simulationSetVariable v

  removeAllForces             = simulationRemoveAllForces
  loadForces forces           = simulationLoadForces forces  
  addForce force              = simulationAddForce force
  disableForcesByLabel labels = simulationDisableForcesByLabel labels
  enableForcesByLabel labels  = simulationEnableForcesByLabel labels

  setNodes nodes              = simulationSetNodes nodes
  setLinks links              = simulationSetLinks links

  addTickFunction label (Step selection chain) = do
    (SS_ ss_) <- gets _.simulationState
    let makeTick _ = do
          -- TODO this coerce is forced upon us here due to forall selection in SimulationM
          let _ = (applyChainableSD3 (unsafeCoerce selection)) <$> chain
          unit
        _ = onTick_ ss_.simulation_ label makeTick
    pure unit

  removeTickFunction label = do
    (SS_ ss_) <- gets _.simulationState
    -- TODO delete the tick function from the state
    let _ = disableTick_ ss_.simulation_ label
    pure unit

  defaultNodeTick label selection = do
    (SS_ ss_) <- gets _.simulationState
    let _ = defaultNodeTick_ label ss_.simulation_ selection
    pure unit

  defaultLinkTick label selection = do
    (SS_ ss_) <- gets _.simulationState
    let _ = defaultLinkTick_ label ss_.simulation_ selection
    pure unit


-- | ====================================================
-- | Selection instance (capability) for the D3 interpreter
-- | ====================================================
instance SelectionM D3Selection_ (D3SimM row D3Selection_) where
  attach selector = pure $ d3SelectAllInDOM_ selector 

  appendElement selection_ (D3_Node element attributes) = do
    let appended_ = d3Append_ (show element) selection_
    modifySelection appended_ attributes -- this modify is NOT stateT modify
    pure appended_

  filterSelection selection_ selector = pure $ d3FilterSelection_ selection_ selector

  modifySelection selection_ attributes = do
    let _ = foldl applyChainableSD3 selection_ attributes
    pure unit

  join selection (Join j) = do
    let 
      selectS = d3SelectionSelectAll_ (show j.element) selection
      dataS   = case j.key of
                  UseDatumAsKey    -> d3Data_        j.data    selectS 
                  (ComputeKey fn)  -> d3KeyFunction_ j.data fn selectS 
      enterS  = d3EnterAndAppend_ (show j.element) dataS
      enterS' = foldl applyChainableSD3 enterS j.behaviour
    pure enterS'

  join selection (JoinGeneral j) = do
    let
      selectS = d3SelectionSelectAll_ (show j.element) selection
      dataS  = case j.key of
                UseDatumAsKey    -> d3Data_        j.data    selectS 
                (ComputeKey fn)  -> d3KeyFunction_ j.data fn selectS 
      enterS = d3EnterAndAppend_ (show j.element) dataS
      exitS  = d3Exit_ dataS
      _      = foldl applyChainableSD3 enterS  j.behaviour.enter
      _      = foldl applyChainableSD3 exitS   j.behaviour.exit
      _      = foldl applyChainableSD3 dataS   j.behaviour.update
    pure enterS
  
  on selection (Drag drag) = do
    let _ = case drag of 
              DefaultDrag     -> defaultDrag_ selection 
              NoDrag          -> disableDrag_ selection
              (CustomDrag fn) -> defaultDrag_ selection -- TODO no custom drag implemented yet
    pure unit

  on selection (Zoom config) = do
    let 
      (ScaleExtent smallest largest) = config.scale
      target = selection
      -- TODO recover the ability to "direct" the zoom to element other than the one receiving the event
      -- ie for controllers, containers etc

    -- sticking to the rules of no ADT's on the JS side we case on the ZoomExtent here
      _ = case config.extent of
            DefaultZoomExtent -> 
              d3AttachZoomDefaultExtent_ selection {
                scaleExtent: [ smallest, largest ]
              , name  : config.name
              , target
              } 

            (ZoomExtent ze)   -> do
              d3AttachZoom_ selection { 
                extent     : [ [ ze.left, ze.top ], [ ze.right, ze.bottom ] ]
              , scaleExtent: [ smallest, largest ]
              , name  : config.name
              , target
              }
    pure unit
