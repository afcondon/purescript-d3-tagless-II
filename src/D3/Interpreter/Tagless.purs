module D3.Interpreter.Tagless where

import D3.Layouts.Simulation
import Prelude

import Control.Monad.State (class MonadState, StateT, modify, modify_, put, runStateT)
import D3.Attributes.Instances (Attribute(..), unbox)
import D3.Selection (Chainable(..), D3Selection_, D3_Node(..), DragBehavior(..), Join(..), Keys(..), Selector, d3AddTransition, d3Append_, d3Data_, d3EnterAndAppend_, d3Exit_, d3KeyFunction_, d3RemoveSelection_, d3SelectAllInDOM_, d3SelectionSelectAll_, d3SetAttr_, d3SetText_)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple, fst, snd)
import Effect (Effect)
import Effect.Class (class MonadEffect)

-- not actually using Effect in foreign fns to keep sigs simple (for now)
-- also not really making a ton of use of StateT, but it is good to have a 
-- place to stash D3's global state such as named transitions etc
newtype D3M model a = D3M (StateT model Effect a) 

derive newtype instance functorD3M     :: Functor                  (D3M model)
derive newtype instance applyD3M       :: Apply                    (D3M model)
derive newtype instance applicativeD3M :: Applicative              (D3M model)
derive newtype instance bindD3M        :: Bind                     (D3M model)
derive newtype instance monadD3M       :: Monad                    (D3M model)
derive newtype instance monadStateD3M  :: MonadState model (D3M model)
derive newtype instance monadEffD3M    :: MonadEffect              (D3M model)

-- TODO see whether it can be useful to extend the interpreter here, for different visualization types
-- in particular, it could be good to have Simulation do it's join function by putting nodes / links
-- into both DOM and Simulation for example (and current implementation is gross and wrong)
class (Monad m) <= D3Tagless m where
  attach   :: Selector     -> m D3Selection_
  appendTo :: D3Selection_ -> D3_Node -> m D3Selection_
  join     :: ∀ a. D3Selection_ -> Join a -> m D3Selection_ -- but every join must be an array of something

runD3M :: ∀ a model. D3M model a -> model -> Effect (Tuple a model)
runD3M (D3M state) = runStateT state

-- d3State :: ∀ a model. model -> D3M model a -> Effect (D3State model)
-- d3State model (D3M state) = liftA1 snd $ runStateT state makeD3State'

d3Run :: ∀ a model. model -> D3M model a -> Effect a
d3Run model (D3M state) = liftA1 fst $ runStateT state model

instance d3TaglessD3M :: D3Tagless (D3M model) where
  attach selector = pure $ d3SelectAllInDOM_ selector 

  appendTo selection_ (D3_Node element attributes) = do
    let appended_ = d3Append_ (show element) selection_
    pure $ foldl applyChainable appended_ attributes    

  join selection (Join j) = do
    let 
      selectS = d3SelectionSelectAll_ (show j.element) selection
      dataS   = case j.key of
                  DatumIsUnique    -> d3Data_        j.data    selectS 
                  (ComputeKey fn)  -> d3KeyFunction_ j.data fn selectS 
      enterS  = d3EnterAndAppend_ (show j.element) dataS
      enterS' = foldl applyChainable enterS j.behaviour
    pure enterS'

  join selection (JoinSimulation j) = do
    let makeTick :: Array Chainable -> D3Selection_ -> Unit -> Unit
        makeTick attributes selection_ _ = do
          let _ = (applyChainable selection_) <$> attributes
          unit

    let 
      initialS = d3SelectionSelectAll_ (show j.element) selection
      dataS    = case j.key of
                    DatumIsUnique    -> d3Data_        j.data    initialS 
                    (ComputeKey fn)  -> d3KeyFunction_ j.data fn initialS 
      enterS   = d3EnterAndAppend_ (show j.element) dataS
      _        = foldl applyChainable enterS  j.behaviour
      _        = onTick_ j.simulation j.tickName (makeTick j.onTick enterS)
      _        = case j.onDrag of
                    DefaultDrag -> defaultSimulationDrag_ enterS j.simulation
                    _ -> unit
    pure dataS

  join selection (JoinGeneral j) = do
    let
      selectS = d3SelectionSelectAll_ (show j.element) selection
      dataS  = case j.key of
                DatumIsUnique    -> d3Data_        j.data    selectS 
                (ComputeKey fn)  -> d3KeyFunction_ j.data fn selectS 
      enterS = d3EnterAndAppend_ (show j.element) dataS
      exitS  = d3Exit_ dataS
      _        = foldl applyChainable enterS  j.behaviour.enter
      _        = foldl applyChainable exitS   j.behaviour.exit
      _        = foldl applyChainable dataS   j.behaviour.update
    pure dataS

applyChainable :: D3Selection_ -> Chainable -> D3Selection_
applyChainable selection_ (AttrT (Attribute label attr)) = -- spy "d3SetAttr" $ 
  d3SetAttr_ label (unbox attr) selection_
-- NB only protection against non-text attribute for Text field is in the helper function
applyChainable selection_ (TextT (Attribute label attr)) = d3SetText_ (unbox attr) selection_ 
-- NB this remove call will have no effect on elements with active or pending transitions
-- and this gives rise to very counter-intuitive misbehaviour as subsequent enters clash with 
-- elements that should have been removed
applyChainable selection_ RemoveT = d3RemoveSelection_ selection_ -- "selection" here will often be a "transition"
-- for transition in D3 we must use .call(selection, transition) so that chain continues
-- in this interpreter it's enought to just return the selection instead of the transition
applyChainable selection_ (TransitionT chain transition) = do
  let tHandler = d3AddTransition selection_ transition
      _        = foldl applyChainable tHandler chain
  selection_ -- NB we return selection, not transition
