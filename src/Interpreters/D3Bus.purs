module D3Tagless.D3Bus where

import Control.Monad.Reader (class MonadAsk, ReaderT, ask, asks)
import Control.Monad.State (class MonadState, StateT, get, runStateT)
import D3.Data.Types (D3Selection_)
import D3.FFI (d3Append_, d3AttachZoomDefaultExtent_, d3AttachZoom_, d3Data_, d3EnterAndAppend_, d3Exit_, d3FilterSelection_, d3KeyFunction_, d3RemoveSelection_, d3SelectAllInDOM_, d3SelectFirstInDOM_, d3SelectionIsEmpty_, d3SelectionSelectAll_, d3SelectionSelect_, defaultDrag_, disableDrag_)
import D3.Selection (Behavior(..), D3_Node(..), DragBehavior(..), Join(..), Keys(..), applyChainableSD3)
import D3.Simulation.Types (SimBusCommand(..), Step(..))
import D3.Zoom (ScaleExtent(..), ZoomExtent(..))
import D3Tagless.Capabilities (class SelectionM, class SimulationM, modifySelection)
import Data.Foldable (foldl)
import Data.Tuple (Tuple, fst, snd)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, Unit, bind, discard, liftA1, pure, show, unit, ($), (<>))
import Type.Equality (class TypeEquals, from)

-- | D3MB is a monad for running D3 scripts that use a Simulation engine over a Bus, allowing sharing of one simulation with many scripts

type SimBus selection = (Bus.BusRW (SimBusCommand selection)) 
newtype D3MB selection a = D3MB (StateT (SimBus selection) Aff a)

derive newtype instance functorD3MB     :: Functor           (D3MB selection)
derive newtype instance applyD3MB       :: Apply             (D3MB selection)
derive newtype instance applicativeD3MB :: Applicative       (D3MB selection)
derive newtype instance bindD3MB        :: Bind              (D3MB selection)
derive newtype instance monadD3MB       :: Monad             (D3MB selection)
derive newtype instance monadEffD3MB    :: MonadEffect       (D3MB selection)
derive newtype instance monadAffD3MB    :: MonadAff          (D3MB selection)
derive newtype instance monadStateD3MB  :: MonadState  (SimBus selection) (D3MB selection) 

run_D3MB_Simulation :: forall a. SimBus D3Selection_ -> D3MB D3Selection_ a -> Aff (Tuple a (SimBus D3Selection_))
run_D3MB_Simulation bus (D3MB state_T) = runStateT state_T bus

eval_D3MB_Simulation :: forall a. SimBus D3Selection_ -> D3MB D3Selection_ a -> Aff a
eval_D3MB_Simulation bus (D3MB state_T) = liftA1 fst $ runStateT state_T bus

exec_D3MB_Simulation :: forall a. SimBus D3Selection_ -> D3MB D3Selection_ a -> Aff (SimBus D3Selection_)
exec_D3MB_Simulation bus (D3MB state_T) = liftA1 snd $ runStateT state_T bus

-- | ====================================================
-- | Simulation instance (capability) for the D3 interpreter
-- | ====================================================
instance simulationCapabilityD3MB :: SimulationM (D3MB D3Selection_) where
  start = do
    simBus  <- get
    liftAff $ Bus.write Start simBus
    pure unit

  stop = do
    simBus <- get
    liftAff $ Bus.write Stop simBus
    pure unit

  removeAllForces = do
    simBus <- get
    liftAff $ Bus.write RemoveAllForces simBus
    pure unit

  loadForces forces = do
    simBus <- get
    liftAff $ Bus.write (LoadForces forces) simBus
    pure unit
  
  addForce force = do
    simBus <- get
    liftAff $ Bus.write (AddForce force) simBus
    pure unit

  disableForcesByLabel labels = do
    simBus <- get
    liftAff $ Bus.write (DisableForcesByLabel labels) simBus
    pure unit

  enableForcesByLabel labels  = do
    simBus <- get
    liftAff $ Bus.write (EnableForcesByLabel labels) simBus
    pure unit
    
  setConfigVariable c = do
    simBus <- get
    liftAff $ Bus.write (SetConfigVariable c) simBus
    pure unit

  setNodes nodes = do
    simBus <- get
    liftAff $ Bus.write (EnableForcesByLabel []) simBus
    -- liftAff $ Bus.write (SetNodes nodes) simBus
    pure unit

  setLinks links = do
    simBus <- get
    -- liftAff $ Bus.write (SetLinks links) simBus
    pure unit

  addTickFunction label (Step selection chain) = do
    simBus <- get
    pure unit

  removeTickFunction label = do
    simBus <- get
    liftAff $ Bus.write (RemoveTickFunction label) simBus
    pure unit


  -- | ====================================================
-- | Selection instance (capability) for the D3 interpreter that uses bus to talk to simulation
-- | THIS IS THE EXACT SAME CODE AS THE NON-BUS VERSION, BUT NO ORPHAN INSTANCES SO IT LIVES HERE
-- | ====================================================
instance selectionCapabilityD3MB :: SelectionM D3Selection_ (D3MB D3Selection_) where
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