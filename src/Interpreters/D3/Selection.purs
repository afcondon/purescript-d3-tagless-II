module D3Tagless.Instance.Selection where


import Control.Monad.State (class MonadState, StateT, runStateT)
import D3.Data.Types (D3Selection_)
import D3.Selection.Functions (selectionAppendElement, selectionAttach, selectionFilterSelection, selectionJoin, selectionMergeSelections, selectionModifySelection, selectionOn, selectionOpenSelection, selectionUpdateJoin)
import D3Tagless.Capabilities (class SelectionM)
import Data.Tuple (Tuple, fst, snd)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, Unit, liftA1, unit, ($))

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

-- | ====================================================
-- | Selection instance (capability) for the D3 interpreter
-- | ====================================================
instance d3TaglessD3M :: SelectionM D3Selection_ (D3M state D3Selection_) where
  attach selector    = selectionAttach selector 
  appendElement s_   = selectionAppendElement s_
  filterSelection s_ = selectionFilterSelection s_
  openSelection s_   = selectionOpenSelection s_
  mergeSelections s_ = selectionMergeSelections s_
  setAttributes s_ = selectionModifySelection s_
  simpleJoin s_      = selectionJoin s_
  updateJoin s_      = selectionUpdateJoin s_
  on s_              = selectionOn s_

runD3M :: forall a. D3M Unit D3Selection_ a -> Effect (Tuple a Unit)
runD3M (D3M state_T) = runStateT state_T unit

eval_D3M :: forall a. D3M Unit D3Selection_ a -> Effect a
eval_D3M (D3M state_T) = liftA1 fst $ runStateT state_T unit

exec_D3M :: forall a. D3M Unit D3Selection_ a -> Effect Unit
exec_D3M (D3M state_T) = liftA1 snd $ runStateT state_T unit

