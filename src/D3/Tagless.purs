module D3.Interpreter.Tagless where

import D3.Selection
import Prelude

import Control.Monad.State (class MonadState, StateT, get, modify, modify_, put, runStateT)
import D3.Attributes.Instances (Attribute(..), unbox, unboxText)
import Data.Foldable (foldl)
import Data.Map (insert, lookup)
import Data.Maybe (Maybe(..))
import Data.Maybe.Last (Last(..))
import Data.Semigroup.Foldable (foldl1)
import Data.Tuple (Tuple(..), fst, snd)
import Debug (spy, trace)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Unsafe.Coerce (unsafeCoerce)

-- not actually using Effect in foreign fns to keep sigs simple (for now)
newtype D3M model a = D3M (StateT (D3State model) Effect a) 

derive newtype instance functorD3M     :: Functor                    (D3M model)
derive newtype instance applyD3M       :: Apply                      (D3M model)
derive newtype instance applicativeD3M :: Applicative                (D3M model)
derive newtype instance bindD3M        :: Bind                       (D3M model)
derive newtype instance monadD3M       :: Monad                      (D3M model)
derive newtype instance monadStateD3M  :: MonadState (D3State model) (D3M model)
derive newtype instance monadEffD3M    :: MonadEffect                (D3M model)

-- TODO
-- there's definitely some indexed monad or state machine kinda stuff here 
-- because you need to have a Selection in order to bind some data
-- and you also need to have some data before using any attributes that 
-- are (Datum -> <something>)
-- there are thus several things that are legal under the type system which
-- would lead to run time errors
class (Monad m) <= D3Tagless m where
  hook   :: Selector                          -> m D3Selection
  append :: String              -> D3_Node    -> m D3Selection
  join   :: forall model. model -> Join model -> m D3Selection

runD3M :: ∀ a model. D3M model a -> (D3State model) -> Effect (Tuple a (D3State model))
runD3M (D3M state) = runStateT state

d3State :: ∀ a model. model -> D3M model a -> Effect (D3State model)
d3State model (D3M state) = liftA1 snd $ runStateT state (makeD3State' model)

d3Run :: ∀ a model. model -> D3M model a -> Effect a
d3Run model (D3M state) = liftA1 fst $ runStateT state (makeD3State' model)

instance d3TaglessD3M :: D3Tagless (D3M model) where
  hook selector = setSelection (SelectionName "root") $ d3SelectAllInDOM_ selector 

  append name (D3_Node element attributes) = do
    (D3State state) <- get
    case state.active of
      (Last Nothing) -> pure state.active
      (Last (Just selection_)) -> do
        let appended_ = d3Append_ (show element) selection_
        setSelection (SelectionName name) $ foldl applyChainable appended_ attributes     

  join model (Join j) = do
    (D3State state) <- get 
    -- let _ = trace { state: state } \_ -> unit
    case lookup j.selection state.namedSelections of
      Nothing -> -- spy "selection not found for join: " $ 
        pure $ Last Nothing
      (Just selection) -> do
        let 
          (model :: D3Data_) = unsafeCoerce state.model -- TODO but in fact, it's the projection that we coerce
          initialS = d3SelectionSelectAll_ (show j.element) selection

          updateS  = -- spy "update: " $
                     case j.key of
                        DatumIsKey -> d3Data_      model initialS 
                        (KeyF fn)  -> d3DataKeyFn_ model fn initialS 
          enterS   = -- spy "enter: " $
                     d3EnterAndAppend_ (show j.element) updateS
          exitS    = -- spy "exit: " $ 
                     d3Exit_ updateS -- updateS.exit ??????

          _        = foldl applyChainable enterS  j.behaviour.enter
          _        = foldl applyChainable exitS   j.behaviour.exit
          _        = foldl applyChainable updateS j.behaviour.update

        pure $ Last $ Just updateS

setSelection :: forall m model. Bind m => MonadState (D3State model) m => 
  SelectionName -> D3Selection_ -> m D3Selection
setSelection name selection_ = do
    let active = Last $ Just selection_
    modify_ (\(D3State d) -> D3State d { active=active, namedSelections=insert name selection_ d.namedSelections }) 
    pure active

applyChainable :: D3Selection_ -> Chainable -> D3Selection_
applyChainable selection (AttrT (Attribute label attr)) = -- trace { applyAttr: label } \_ -> 
  d3SetAttr_ label (unbox attr) selection
-- NB only protection against non-text attribute for Text field is in the helper function
applyChainable selection (TextT (Attribute label attr)) = -- trace { applyAttr: label } \_ -> 
  d3SetText_ (unbox attr) selection 
-- for transition we must use .call(selection, transition) so that chain continues
applyChainable selection (TransitionT chain transition) = spy "start transition" do
  let tHandler = d3AddTransition selection transition
      _        = foldl applyChainable tHandler chain
  selection -- we return selection, not transition
applyChainable selection RemoveT = trace { remove: selection } \_ -> 
  d3RemoveSelection_ selection -- "selection" will often be a "transition"


