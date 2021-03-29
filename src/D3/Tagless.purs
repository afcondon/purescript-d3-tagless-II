module D3.Interpreter.Tagless where

import D3.Selection
import Prelude

import Control.Monad.State (class MonadState, StateT, get, modify, modify_, put, runStateT)
import D3.Attributes.Instances (Attribute(..), unbox)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Unsafe.Coerce (unsafeCoerce)

newtype D3M a = D3M (StateT D3State Effect a) -- not actually using Effect in foreign fns to keep sigs simple (for now)

derive newtype instance functorD3M     :: Functor           D3M
derive newtype instance applyD3M       :: Apply             D3M
derive newtype instance applicativeD3M :: Applicative       D3M
derive newtype instance bindD3M        :: Bind              D3M
derive newtype instance monadD3M       :: Monad             D3M
derive newtype instance monadStateD3M  :: MonadState D3State D3M
derive newtype instance monadEffD3M    :: MonadEffect       D3M


-- TODO
-- there's definitely some indexed monad or state machine kinda stuff here 
-- because you need to have a Selection in order to bind some data
-- and you also need to have some data before using any attributes that 
-- are (Datum -> <something>)
-- there are thus several things that are legal under the type system which
-- would lead to run time errors
class (Monad m) <= D3Tagless m where
  model  :: forall d. d -> m D3State
  hook   :: Selector -> m D3Selection
  append :: D3_Node -> m D3Selection
  join   :: Element -> EnterUpdateExit -> m D3Selection

runD3M :: ∀ a. D3M a -> D3State -> Effect (Tuple a D3State)
runD3M (D3M state) = runStateT state

d3State :: ∀ a. D3M a -> Effect D3State
d3State (D3M state) = liftA1 snd $ runStateT state emptyD3State

d3Run :: ∀ a. D3M a -> Effect a
d3Run (D3M state) = liftA1 fst $ runStateT state emptyD3State

instance d3TaglessD3M :: D3Tagless D3M where
  model         = modify <<< setData <<< unsafeCoerce
  hook selector = setSelection $ d3SelectAllInDOM_ selector 

  append node = do
    (D3State d3data selection) <- get
    setSelection $ doAppend node selection

  join element enterUpdateExit = do -- TODO add data to the join
    (D3State d3data selection) <- get
    let -- TOOD d3Data_ with data
        initialS = d3SelectionSelectAll_ (show element) selection
        -- TODO this is where it's really tricky - attribute processing with shape of data open
        updateS  = d3Data_ d3data initialS 
        _ = foldl doTransitionStep updateS enterUpdateExit.update

        enterS  = d3Append_ (show element) updateS -- TODO add Attrs for the inserted element here
        _      = foldl doTransitionStep enterS enterUpdateExit.enter

        exitS   = d3Exit_ updateS
        _       = foldl doTransitionStep exitS enterUpdateExit.exit
        _       = d3RemoveSelection_ exitS -- TODO this is actually optional but by far more common to always remove

    -- put updateS -- not clear to me what actually has to be returned from join
    pure updateS

setSelection :: forall m. Bind m => MonadState D3State m => D3Selection -> m D3Selection
setSelection newSelection = do
    modify_ (\(D3State d s) -> D3State d newSelection) 
    pure newSelection

doTransitionStep :: D3Selection -> TransitionStep -> D3Selection
doTransitionStep selection (Tuple attributes (Just transition)) = do
  let _ = (setAttributeOnSelection selection) <$> attributes
  -- returning the transition as a "selection"
  d3AddTransition selection transition
doTransitionStep selection (Tuple attributes Nothing) = do -- last stage of chain
  let _ = (setAttributeOnSelection selection) <$> attributes
  selection -- there's no next stage at end of chain, this "selection" might well be a "transition" but i don't think we care

doAppend :: D3_Node -> D3Selection -> D3Selection
doAppend (D3_Node element attributes children) selection = do
  let appended = d3Append_ (show element) selection
      _ = (setAttributeOnSelection appended) <$> attributes
      _ = (flip doAppend $ appended) <$> children
  appended

setAttributeOnSelection :: D3Selection -> Attribute -> Unit
setAttributeOnSelection selection (Attribute label attr) = d3SetAttr_ label (unbox attr) selection

appendChildToSelection :: D3Selection -> D3_Node -> D3Selection
appendChildToSelection selection (D3_Node element attributes children)  = d3Append_ (show element) selection

