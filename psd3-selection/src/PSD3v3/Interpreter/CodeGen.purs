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

import PSD3v3.Expr (class NumExpr, class StringExpr, class BoolExpr, class CompareExpr, class StringCompareExpr)
import PSD3v3.Units (class UnitExpr, class UnitArith)
import PSD3v3.Datum (class DatumExpr)
import PSD3v3.Path (class PathExpr)

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
-- StringCompareExpr - generates string comparison expressions
-- =============================================================================

instance stringCompareExprCodeGen :: StringCompareExpr CodeGen where
  strEq (CodeGen a) (CodeGen b) = CodeGen ("(" <> a <> " == " <> b <> ")")
  strNeq (CodeGen a) (CodeGen b) = CodeGen ("(" <> a <> " /= " <> b <> ")")

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

-- =============================================================================
-- PathExpr - generates path function calls
-- =============================================================================

instance pathExprCodeGen :: PathExpr CodeGen where
  linePath (CodeGen x1) (CodeGen y1) (CodeGen x2) (CodeGen y2) =
    CodeGen ("(linePath " <> x1 <> " " <> y1 <> " " <> x2 <> " " <> y2 <> ")")

  polylinePath (CodeGen points) =
    CodeGen ("(polylinePath " <> points <> ")")

  linkHorizontal (CodeGen x1) (CodeGen y1) (CodeGen x2) (CodeGen y2) =
    CodeGen ("(linkHorizontal " <> x1 <> " " <> y1 <> " " <> x2 <> " " <> y2 <> ")")

  linkVertical (CodeGen x1) (CodeGen y1) (CodeGen x2) (CodeGen y2) =
    CodeGen ("(linkVertical " <> x1 <> " " <> y1 <> " " <> x2 <> " " <> y2 <> ")")

  linkRadial (CodeGen a1) (CodeGen r1) (CodeGen a2) (CodeGen r2) =
    CodeGen ("(linkRadial " <> a1 <> " " <> r1 <> " " <> a2 <> " " <> r2 <> ")")

  sankeyLink (CodeGen sx) (CodeGen sy0) (CodeGen sy1) (CodeGen tx) (CodeGen ty0) (CodeGen ty1) =
    CodeGen ("(sankeyLink " <> sx <> " " <> sy0 <> " " <> sy1 <> " " <> tx <> " " <> ty0 <> " " <> ty1 <> ")")

  ribbon (CodeGen sa0) (CodeGen sa1) (CodeGen ta0) (CodeGen ta1) (CodeGen ir) (CodeGen or) =
    CodeGen ("(ribbon " <> sa0 <> " " <> sa1 <> " " <> ta0 <> " " <> ta1 <> " " <> ir <> " " <> or <> ")")

  arc (CodeGen start) (CodeGen end) (CodeGen inner) (CodeGen outer) =
    CodeGen ("(arc " <> start <> " " <> end <> " " <> inner <> " " <> outer <> ")")

  closePath (CodeGen p) =
    CodeGen ("(closePath " <> p <> ")")
