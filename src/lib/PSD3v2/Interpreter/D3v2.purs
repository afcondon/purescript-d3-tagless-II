module PSD3v2.Interpreter.D3v2
  ( D3v2Selection_
  , D3v2M
  , runD3v2M
  ) where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect)
import PSD3v2.Capabilities.Selection (class SelectionM)
import PSD3v2.Selection.Operations as Ops
import PSD3v2.Selection.Types (Selection)

-- | Selection type for D3v2 interpreter
-- |
-- | This is just a newtype wrapper around PSD3v2.Selection.Types.Selection
-- | to distinguish it from other interpreter's selection types.
newtype D3v2Selection_ (state :: Type) (parent :: Type) (datum :: Type)
  = D3v2Selection_ (Selection state parent datum)

-- | The D3v2 interpreter monad
-- |
-- | Wraps Effect to allow for DOM manipulation.
-- | Could be extended later to include logging, error handling, etc.
newtype D3v2M a = D3v2M (Effect a)

derive newtype instance Functor D3v2M
derive newtype instance Apply D3v2M
derive newtype instance Applicative D3v2M
derive newtype instance Bind D3v2M
derive newtype instance Monad D3v2M
derive newtype instance MonadEffect D3v2M

-- | Run the D3v2 interpreter
runD3v2M :: D3v2M ~> Effect
runD3v2M (D3v2M eff) = eff

-- | SelectionM instance for D3v2 interpreter
-- |
-- | Delegates all operations to PSD3v2.Selection.Operations,
-- | which uses the phantom types with unsafePartial for safe pattern matching.
instance SelectionM D3v2Selection_ D3v2M where

  select selector = D3v2M do
    sel <- Ops.select selector
    pure $ D3v2Selection_ sel

  selectAll selector (D3v2Selection_ sel) = D3v2M do
    result <- Ops.selectAll selector sel
    pure $ D3v2Selection_ result

  renderData elemType foldableData selector (D3v2Selection_ emptySelection) enterAttrs updateAttrs exitAttrs = D3v2M do
    result <- Ops.renderData elemType foldableData selector emptySelection enterAttrs updateAttrs exitAttrs
    pure $ D3v2Selection_ result

  joinData foldableData selector (D3v2Selection_ emptySelection) = D3v2M do
    Ops.joinData foldableData selector emptySelection

  append elemType attrs (D3v2Selection_ pendingSelection) = D3v2M do
    result <- Ops.append elemType attrs pendingSelection
    pure $ D3v2Selection_ result

  setAttrs attrs (D3v2Selection_ boundSelection) = D3v2M do
    result <- Ops.setAttrs attrs boundSelection
    pure $ D3v2Selection_ result

  remove (D3v2Selection_ exitingSelection) = D3v2M do
    Ops.remove exitingSelection

  merge (D3v2Selection_ sel1) (D3v2Selection_ sel2) = D3v2M do
    result <- Ops.merge sel1 sel2
    pure $ D3v2Selection_ result
