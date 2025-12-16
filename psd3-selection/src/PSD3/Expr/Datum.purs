-- | PSD3v3 Datum Field Access
-- |
-- | Type-safe field access using row polymorphism.
-- | Compile-time verification that fields exist on the datum type.
module PSD3.Expr.Datum
  ( class DatumExpr
  , field
  , index
  ) where

import Prim.Row as Row
import Type.Proxy (Proxy)
import Data.Symbol (class IsSymbol)

-- | Datum field access with compile-time safety
-- |
-- | The `datumRow` parameter is a Row kind (not Type), representing the
-- | fields available in the datum record. The constraint `Row.Cons sym a r datumRow`
-- | ensures:
-- | - `datumRow` contains a field named `sym`
-- | - The field has type `a`
-- | - Compiler error if field doesn't exist
-- |
-- | The interpreter works with `Record datumRow` at runtime.
class DatumExpr (repr :: Type -> Type) (datumRow :: Row Type) | repr -> datumRow where
  -- | Access a field from the datum
  -- |
  -- | ```purescript
  -- | -- With wrapper functions (recommended):
  -- | x :: forall repr. DatumExpr repr (x :: Number, y :: Number) => repr Number
  -- | x = field (Proxy :: Proxy "x")
  -- |
  -- | -- Or with visible type applications:
  -- | field @"x" :: repr Number
  -- | ```
  field :: forall a sym r
         . IsSymbol sym
        => Row.Cons sym a r datumRow
        => Proxy sym
        -> repr a

  -- | Access the element index (for indexed operations)
  index :: repr Int
