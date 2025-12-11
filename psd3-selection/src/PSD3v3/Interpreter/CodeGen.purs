-- | PSD3v3 CodeGen Interpreter
-- |
-- | Generates PureScript source code from expressions.
-- | This enables true round-tripping: visualization -> code -> visualization.
module PSD3v3.Interpreter.CodeGen
  ( CodeGen(..)
  , runCodeGen
  ) where

import Prelude hiding (add, sub, mul, div, negate, not)

import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row as Row
import Type.Proxy (Proxy)

import PSD3v3.Expr (class NumExpr, class StringExpr, class BoolExpr, class CompareExpr)
import PSD3v3.Units (class UnitExpr, class UnitArith)
import PSD3v3.Datum (class DatumExpr)

-- | CodeGen produces PureScript source code as a String
-- | The phantom type tracks what type the generated code produces
newtype CodeGen a = CodeGen String

runCodeGen :: forall a. CodeGen a -> String
runCodeGen (CodeGen s) = s

-- =============================================================================
-- NumExpr - generates numeric expressions
-- =============================================================================

instance numExprCodeGen :: NumExpr CodeGen where
  lit n = CodeGen (show n)
  add (CodeGen a) (CodeGen b) = CodeGen ("(" <> a <> " + " <> b <> ")")
  sub (CodeGen a) (CodeGen b) = CodeGen ("(" <> a <> " - " <> b <> ")")
  mul (CodeGen a) (CodeGen b) = CodeGen ("(" <> a <> " * " <> b <> ")")
  div (CodeGen a) (CodeGen b) = CodeGen ("(" <> a <> " / " <> b <> ")")
  negate (CodeGen a) = CodeGen ("(-" <> a <> ")")

-- =============================================================================
-- StringExpr - generates string expressions
-- =============================================================================

instance stringExprCodeGen :: StringExpr CodeGen where
  str s = CodeGen ("\"" <> s <> "\"")
  concat (CodeGen a) (CodeGen b) = CodeGen ("(" <> a <> " <> " <> b <> ")")

-- =============================================================================
-- BoolExpr - generates boolean expressions
-- =============================================================================

instance boolExprCodeGen :: BoolExpr CodeGen where
  bool b = CodeGen (if b then "true" else "false")
  and (CodeGen a) (CodeGen b) = CodeGen ("(" <> a <> " && " <> b <> ")")
  or (CodeGen a) (CodeGen b) = CodeGen ("(" <> a <> " || " <> b <> ")")
  not (CodeGen a) = CodeGen ("(not " <> a <> ")")
  ifThenElse (CodeGen cond) (CodeGen t) (CodeGen f) =
    CodeGen ("(if " <> cond <> " then " <> t <> " else " <> f <> ")")

-- =============================================================================
-- CompareExpr - generates comparison expressions
-- =============================================================================

instance compareExprCodeGen :: CompareExpr CodeGen where
  lt (CodeGen a) (CodeGen b) = CodeGen ("(" <> a <> " < " <> b <> ")")
  lte (CodeGen a) (CodeGen b) = CodeGen ("(" <> a <> " <= " <> b <> ")")
  gt (CodeGen a) (CodeGen b) = CodeGen ("(" <> a <> " > " <> b <> ")")
  gte (CodeGen a) (CodeGen b) = CodeGen ("(" <> a <> " >= " <> b <> ")")
  eq (CodeGen a) (CodeGen b) = CodeGen ("(" <> a <> " == " <> b <> ")")

-- =============================================================================
-- UnitExpr - generates unit constructors
-- =============================================================================

instance unitExprCodeGen :: UnitExpr CodeGen where
  px n = CodeGen ("(px " <> show n <> ")")
  em n = CodeGen ("(em " <> show n <> ")")
  rem n = CodeGen ("(rem " <> show n <> ")")
  percent n = CodeGen ("(percent " <> show n <> ")")
  vw n = CodeGen ("(vw " <> show n <> ")")
  vh n = CodeGen ("(vh " <> show n <> ")")
  unitless n = CodeGen ("(unitless " <> show n <> ")")

-- | Unit arithmetic generates function calls
instance unitArithCodeGen :: UnitArith CodeGen where
  addU (CodeGen a) (CodeGen b) = CodeGen ("(addU " <> a <> " " <> b <> ")")
  subU (CodeGen a) (CodeGen b) = CodeGen ("(subU " <> a <> " " <> b <> ")")
  scaleU (CodeGen a) n = CodeGen ("(scaleU " <> a <> " " <> show n <> ")")

-- =============================================================================
-- DatumExpr - generates field access code
-- =============================================================================

-- | For any row type, generate field access as `d.fieldName`
instance datumExprCodeGen :: DatumExpr CodeGen datumRow where
  field :: forall a sym r
         . IsSymbol sym
        => Row.Cons sym a r datumRow
        => Proxy sym
        -> CodeGen a
  field proxy = CodeGen ("d." <> reflectSymbol proxy)

  index = CodeGen "i"
