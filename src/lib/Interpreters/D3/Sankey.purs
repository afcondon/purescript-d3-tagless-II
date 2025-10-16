module D3Tagless.Instance.Sankey where

import Prelude

import Control.Monad.State (class MonadState, StateT, get, modify_, runStateT)
import D3.Data.Types (D3Selection_)
import D3.Layouts.Sankey.Functions (sankeySetData, sankeySetDataWithConfig)
import D3.Layouts.Sankey.Types (SankeyLayoutState_)
import D3.Selection.Functions (selectionAppendElement, selectionAttach, selectionFilterSelection, selectionJoin, selectionMergeSelections, selectionModifySelection, selectionOn, selectionOpenSelection, selectionSelectUnder, selectionUpdateJoin)
import D3Tagless.Capabilities (class SankeyM, class SelectionM)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

-- | ====================================================
-- | Sankey instance (capability) for the D3 interpreter
-- | ====================================================
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
  updateJoin s_      = selectionUpdateJoin s_

-- SankeyM instance - Sankey-specific operations
instance SankeyM D3Selection_ (D3SankeyM row D3Selection_) where
  setSankeyData data_ width height = sankeySetData data_ width height
  setSankeyDataWithConfig data_ width height config = sankeySetDataWithConfig data_ width height config
