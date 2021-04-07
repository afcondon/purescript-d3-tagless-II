module D3.Interpreter.Tagless where

import Control.Monad.State (class MonadState, StateT, get, modify_, runStateT)
import D3.Attributes.Instances (Attribute(..), unbox)
import D3.Selection (Chainable(..), D3Selection_, D3State(..), D3_Node(..), Join(..), Keys(..), SelectionName(..), Selector, d3AddTransition, d3Append_, d3DataKeyFn_, d3Data_, d3EnterAndAppend_, d3Exit_, d3RemoveSelection_, d3SelectAllInDOM_, d3SelectionSelectAll_, d3SetAttr_, d3SetText_, makeD3State')
import Data.Foldable (foldl)
import Data.Map (insert, lookup)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple, fst, snd)
import Debug (spy)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, bind, discard, liftA1, pure, show, ($))

-- not actually using Effect in foreign fns to keep sigs simple (for now)
newtype D3M model a = D3M (StateT (D3State model) Effect a) 

derive newtype instance functorD3M     :: Functor                    (D3M model)
derive newtype instance applyD3M       :: Apply                      (D3M model)
derive newtype instance applicativeD3M :: Applicative                (D3M model)
derive newtype instance bindD3M        :: Bind                       (D3M model)
derive newtype instance monadD3M       :: Monad                      (D3M model)
derive newtype instance monadStateD3M  :: MonadState (D3State model) (D3M model)
derive newtype instance monadEffD3M    :: MonadEffect                (D3M model)

-- TODO see whether it can be useful to extend the interpreter here, for different visualization types
-- in particular, it could be good to have Simulation do it's join function by putting nodes / links
-- into both DOM and Simulation for example (and current implementation is gross and wrong)
class (Monad m) <= D3Tagless m where
  hook     :: Selector                            -> m D3Selection_
  appendTo :: D3Selection_   -> String -> D3_Node -> m D3Selection_
  join     :: ∀ model. model -> Join model        -> m (Maybe D3Selection_)

runD3M :: ∀ a model. D3M model a -> (D3State model) -> Effect (Tuple a (D3State model))
runD3M (D3M state) = runStateT state

d3State :: ∀ a model. model -> D3M model a -> Effect (D3State model)
d3State model (D3M state) = liftA1 snd $ runStateT state (makeD3State' model)

d3Run :: ∀ a model. model -> D3M model a -> Effect a
d3Run model (D3M state) = liftA1 fst $ runStateT state (makeD3State' model)

instance d3TaglessD3M :: D3Tagless (D3M model) where
  hook selector = setSelection (SelectionName "root") $ d3SelectAllInDOM_ selector 

  appendTo selection_ name (D3_Node element attributes) = do
    let appended_ = d3Append_ (show element) selection_
    setSelection (SelectionName name) $ foldl applyChainable appended_ attributes     

  join model (Join j) = do
    (D3State state) <- get 
    case lookup j.hook state.namedSelections of
      Nothing          -> pure Nothing
      (Just hook) -> do
        let 
          selectS = d3SelectionSelectAll_ (show j.element) hook
          dataS  = case j.key of
                      DatumIsKey -> d3Data_      model j.projection selectS 
                      (KeyF fn)  -> d3DataKeyFn_ model j.projection fn selectS 
          enterS  = d3EnterAndAppend_ (show j.element) dataS
          enterS' = foldl applyChainable enterS  j.behaviour
        pure $ Just enterS'

  join model (JoinSimulation j) = do
    (D3State state) <- get 
    case lookup j.hook state.namedSelections of
      Nothing          -> pure Nothing
      (Just hook) -> do
        let 
          initialS = d3SelectionSelectAll_ (show j.element) hook
          dataS    = case j.key of
                        DatumIsKey -> d3Data_      model j.projection initialS 
                        (KeyF fn)  -> d3DataKeyFn_ model j.projection fn initialS 
          enterS   = d3EnterAndAppend_ (show j.element) dataS
          enterS'  = foldl applyChainable enterS  j.behaviour
          finalS   = foldl applyChainable enterS' j.onTick
        pure $ Just dataS

  join model (JoinGeneral j) = do
    (D3State state) <- get 
    case lookup j.hook state.namedSelections of
      Nothing          -> pure Nothing
      (Just hook) -> do
        let 
          -- (model :: D3Data_) = j.projection state.model -- TODO but in fact, it's the projection that we coerce
          selectS = d3SelectionSelectAll_ (show j.element) hook

          dataS  = case j.key of
                    DatumIsKey -> d3Data_      model j.projection selectS 
                    (KeyF fn)  -> d3DataKeyFn_ model j.projection fn selectS 

          enterS = d3EnterAndAppend_ (show j.element) dataS

          exitS  = d3Exit_ dataS

          _        = foldl applyChainable enterS  j.behaviour.enter
          _        = foldl applyChainable exitS   j.behaviour.exit
          _        = foldl applyChainable dataS j.behaviour.update

        pure $ Just dataS

setSelection :: ∀ m model. Bind m => MonadState (D3State model) m => 
  SelectionName -> D3Selection_ -> m D3Selection_
setSelection name selection_ = do
    modify_ (\(D3State d) -> D3State d { namedSelections=insert name selection_ d.namedSelections }) 
    pure selection_

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
