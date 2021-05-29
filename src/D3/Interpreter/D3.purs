module D3.Interpreter.D3 where

import Control.Monad.State (class MonadState, StateT, runStateT)
import D3.Attributes.Instances (Attribute(..), unbox)
import D3.Data.Types (D3Selection_)
import D3.FFI (d3AddTransition_, d3Append_, d3AttachZoomDefaultExtent_, d3AttachZoom_, d3Data_, d3EnterAndAppend_, d3Exit_, d3KeyFunction_, d3RemoveSelection_, d3SelectAllInDOM_, d3SelectionSelectAll_, d3SetAttr_, d3SetText_, defaultDrag_, defaultSimulationDrag_, disableDrag_, onTick_, selectionOn_)
import D3.Interpreter (class D3InterpreterM)
import D3.Selection (Behavior(..), ChainableS(..), D3_Node(..), DragBehavior(..), Join(..), Keys(..))
import D3.Zoom (ScaleExtent(..), ZoomExtent(..))
import Data.Foldable (foldl)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Prelude 

-- not actually using Effect in foreign fns to keep sigs simple (for now)
-- also not really making a ton of use of StateT, but it is good to have a 
-- place to stash D3's global state such as named transitions etc
newtype D3M :: forall k. k -> Type -> Type
newtype D3M selection a = D3M (StateT Unit Effect a) 
-- TODO don't really need a State instance now, could be ReaderT, however, state might make a comeback so leaving for now

derive newtype instance functorD3M     :: Functor           (D3M selection)
derive newtype instance applyD3M       :: Apply             (D3M selection)
derive newtype instance applicativeD3M :: Applicative       (D3M selection)
derive newtype instance bindD3M        :: Bind              (D3M selection)
derive newtype instance monadD3M       :: Monad             (D3M selection)
derive newtype instance monadStateD3M  :: MonadState  Unit  (D3M selection) 
derive newtype instance monadEffD3M    :: MonadEffect       (D3M selection)

runD3M :: forall a. D3M D3Selection_  a-> Effect (Tuple a Unit)
runD3M (D3M state) = runStateT state unit

instance d3TaglessD3M :: D3InterpreterM D3Selection_ (D3M D3Selection_) where
  attach selector = pure $ d3SelectAllInDOM_ selector 

  append selection_ (D3_Node element attributes) = do
    let appended_ = d3Append_ (show element) selection_
    pure $ foldl applyChainableSD3 appended_ attributes   

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

  on selection (Tick tick) = do
    let makeTick :: Array ChainableS -> D3Selection_ -> Unit -> Unit
        makeTick attributes selection_ _ = do
          let _ = (applyChainableSD3 selection_) <$> attributes
          unit
        _ = onTick_ tick.simulation tick.name (makeTick tick.chain selection)
    pure selection 
  
  on selection (Drag drag) = do
    case drag of 
      DefaultDrag     -> pure $ defaultDrag_ selection 
      NoDrag          -> pure $ disableDrag_ selection 
      (CustomDrag fn) -> pure $ defaultDrag_ selection -- TODO no custom drag implemented yet

  on selection (Zoom config) = do
    let 
      (ScaleExtent smallest largest) = config.scale
      target = selection
      -- TODO recover the ability to "direct" the zoom to element other than the one receiving the event
      -- ie for controllers, containers etc

    -- sticking to the rules of no ADT's on the JS side we case on the ZoomExtent here
    pure $ 
      case config.extent of
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


applyChainableSD3 :: D3Selection_ -> ChainableS -> D3Selection_
applyChainableSD3 selection_ (AttrT (ToAttribute label attr)) = 
  d3SetAttr_ label (unbox attr) selection_
-- NB only protection against non-text attribute for Text field is in the helper function
applyChainableSD3 selection_ (TextT (ToAttribute label attr)) = d3SetText_ (unbox attr) selection_ 
-- NB this remove call will have no effect on elements with active or pending transitions
-- and this gives rise to very counter-intuitive misbehaviour as subsequent enters clash with 
-- elements that should have been removed
applyChainableSD3 selection_ RemoveT = d3RemoveSelection_ selection_ -- "selection" here will often be a "transition"
-- for transition in D3 we must use .call(selection, transition) so that chain continues
-- in this interpreter it's enought to just return the selection instead of the transition
applyChainableSD3 selection_ (TransitionT chain transition) = do
  let tHandler = d3AddTransition_ selection_ transition
      _        = foldl applyChainableSD3 tHandler chain
  selection_ -- NB we return selection, not transition
-- for Forces in simulation which also can be static or dynamic:
-- TODO seems like this actually needs to factor out to be applyChainableSSimulation and applyChainableSSelection
-- applyChainableSD3 selection_ (ForceT (ToAttribute label attr)) = 
--   d3SetAttr_ label (unbox attr) selection_
applyChainableSD3 selection_ (OnT event listener) = selectionOn_ selection_ (show event) listener

