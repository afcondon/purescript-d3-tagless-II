module D3Tagless.Interpreter.D3Bus where

import Control.Monad.Reader (class MonadAsk, ReaderT, ask, asks)
import D3.Data.Types (D3Selection_)
import D3.FFI (d3Append_, d3AttachZoomDefaultExtent_, d3AttachZoom_, d3Data_, d3EnterAndAppend_, d3Exit_, d3FilterSelection_, d3KeyFunction_, d3RemoveSelection_, d3SelectAllInDOM_, d3SelectFirstInDOM_, d3SelectionIsEmpty_, d3SelectionSelectAll_, d3SelectionSelect_, defaultDrag_, disableDrag_)
import D3.Selection (Behavior(..), D3_Node(..), DragBehavior(..), Join(..), Keys(..), applyChainableSD3)
import D3.Simulation.Types (SimBusCommand(..), Step(..))
import D3.Zoom (ScaleExtent(..), ZoomExtent(..))
import D3Tagless.Interpreter (class SelectionM, class SimulationM, modifySelection)
import Data.Foldable (foldl)
import Effect.Aff (Aff)
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, bind, discard, pure, show, unit, ($), (<>))
import Type.Equality (class TypeEquals, from)

newtype D3MB selection a = D3MB (ReaderT (Bus.BusRW (SimBusCommand selection)) Aff a)

derive newtype instance functorD3MB     :: Functor           (D3MB selection)
derive newtype instance applyD3MB       :: Apply             (D3MB selection)
derive newtype instance applicativeD3MB :: Applicative       (D3MB selection)
derive newtype instance bindD3MB        :: Bind              (D3MB selection)
derive newtype instance monadD3MB       :: Monad             (D3MB selection)
-- derive newtype instance monadStateD3MB  :: MonadState  (Bus.BusRW SimBusCommand) (D3MB selection) 
derive newtype instance monadEffD3MB    :: MonadEffect       (D3MB selection)
derive newtype instance monadAffD3MB    :: MonadAff          (D3MB selection)

instance monadAskD3MB :: TypeEquals e (Bus.BusRW (SimBusCommand selection)) => MonadAsk e (D3MB selection) where
  ask = D3MB $ asks from

-- | ====================================================
-- | Simulation instance (capability) for the D3 interpreter
-- | ====================================================
instance simulationD3MB :: SimulationM (D3MB D3Selection_) where
  start = do
    simBus  <- ask
    liftAff $ Bus.write Start simBus
    pure unit

  stop = do
    simBus <- ask
    liftAff $ Bus.write Stop simBus
    pure unit

  removeAllForces = do
    simBus <- ask
    liftAff $ Bus.write RemoveAllForces simBus
    pure unit

  loadForces forces = do
    simBus <- ask
    liftAff $ Bus.write (LoadForces forces) simBus
    pure unit
  
  addForce force = do
    simBus <- ask
    liftAff $ Bus.write (AddForce force) simBus
    pure unit

  disableForcesByLabel labels = do
    simBus <- ask
    liftAff $ Bus.write (DisableForcesByLabel labels) simBus
    pure unit

  enableForcesByLabel labels  = do
    simBus <- ask
    liftAff $ Bus.write (EnableForcesByLabel labels) simBus
    pure unit
    
  setConfigVariable c = do
    simBus <- ask
    liftAff $ Bus.write (SetConfigVariable c) simBus
    pure unit

  setNodes nodes = do
    simBus <- ask
    -- liftAff $ Bus.write (SetNodes nodes) simBus
    pure unit

  setLinks links = do
    simBus <- ask
    -- liftAff $ Bus.write (SetLinks links) simBus
    pure unit

  addTickFunction label (Step selection chain) = do
    simBus <- ask
    pure unit

  removeTickFunction label = do
    simBus <- ask
    liftAff $ Bus.write (RemoveTickFunction label) simBus
    pure unit


  -- | ====================================================
-- | Selection instance (capability) for the D3 interpreter
-- | THIS IS THE EXACT SAME AS THE NON-BUS VERSION, BUT NO ORPHAN INSTANCES SO IT LIVES HERE
-- | ====================================================
instance d3TaglessD3MB :: SelectionM D3Selection_ (D3MB D3Selection_) where
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