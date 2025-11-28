module PSD3.Interpreter.D3 where

import Prelude

import Control.Monad.State (class MonadState, StateT, get, modify_, runStateT)
import PSD3.Internal.Types (D3Selection_)
import PSD3.Internal.Selection.Functions (selectionAppendElement, selectionAttach, selectionFilterSelection, selectionJoin, selectionMergeSelections, selectionModifySelection, selectionNestedJoin, selectionOn, selectionOpenSelection, selectionSelectUnder, selectionUpdateJoin)
import PSD3.Internal.Sankey.Types (SankeyLayoutState_)
import PSD3.Internal.Sankey.Functions (sankeySetData, sankeySetDataWithConfig)
import PSD3.Capabilities.Selection (class SelectionM)
import PSD3.Capabilities.Sankey (class SankeyM)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

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
  show _ = "D3SankeyM"

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
