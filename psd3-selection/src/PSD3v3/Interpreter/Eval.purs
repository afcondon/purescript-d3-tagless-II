-- | PSD3v3 Eval Interpreter
-- |
-- | Evaluates expressions to concrete values.
-- | This is what runs at D3 render time - datum -> value.
module PSD3v3.Interpreter.Eval
  ( Eval(..)
  , runEval
  , EvalD(..)
  , runEvalD
  ) where

import Prelude hiding (add, sub, mul, div, negate, not)
import Prelude as P

import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row as Row
import Type.Proxy (Proxy)
import Unsafe.Coerce (unsafeCoerce)

import PSD3v3.Expr (class NumExpr, class StringExpr, class BoolExpr, class CompareExpr, class StringCompareExpr)
import PSD3v3.Units (class UnitExpr, class UnitArith)
import PSD3v3.Datum (class DatumExpr)
import PSD3v3.Path (class PathExpr)
import PSD3v3.Path.Generators as Gen

-- =============================================================================
-- Simple Eval (no datum access)
-- =============================================================================

-- | Simple evaluator for expressions without datum access
newtype Eval a = Eval a

runEval :: forall a. Eval a -> a
runEval (Eval a) = a

instance numExprEval :: NumExpr Eval where
  lit n = Eval n
  add (Eval a) (Eval b) = Eval (a + b)
  sub (Eval a) (Eval b) = Eval (a - b)
  mul (Eval a) (Eval b) = Eval (a * b)
  div (Eval a) (Eval b) = Eval (a / b)
  negate (Eval a) = Eval (P.negate a)

instance stringExprEval :: StringExpr Eval where
  str s = Eval s
  concat (Eval a) (Eval b) = Eval (a <> b)

instance boolExprEval :: BoolExpr Eval where
  bool b = Eval b
  and (Eval a) (Eval b) = Eval (a && b)
  or (Eval a) (Eval b) = Eval (a || b)
  not (Eval a) = Eval (P.not a)
  ifThenElse (Eval cond) (Eval t) (Eval f) = Eval (if cond then t else f)

instance compareExprEval :: CompareExpr Eval where
  lt (Eval a) (Eval b) = Eval (a < b)
  lte (Eval a) (Eval b) = Eval (a <= b)
  gt (Eval a) (Eval b) = Eval (a > b)
  gte (Eval a) (Eval b) = Eval (a >= b)
  eq (Eval a) (Eval b) = Eval (a == b)

instance stringCompareExprEval :: StringCompareExpr Eval where
  strEq (Eval a) (Eval b) = Eval (a == b)
  strNeq (Eval a) (Eval b) = Eval (a /= b)

-- | Unit expressions evaluate to the underlying number
-- | (SVG uses unitless numbers, CSS would preserve units)
instance unitExprEval :: UnitExpr Eval where
  px n = Eval (unsafeCoerce n)
  em n = Eval (unsafeCoerce n)
  rem n = Eval (unsafeCoerce n)
  percent n = Eval (unsafeCoerce n)
  vw n = Eval (unsafeCoerce n)
  vh n = Eval (unsafeCoerce n)
  unitless n = Eval (unsafeCoerce n)

-- | Unit arithmetic - just operates on underlying numbers
instance unitArithEval :: UnitArith Eval where
  addU (Eval a) (Eval b) = Eval (unsafeCoerce (unsafeToNum a + unsafeToNum b))
  subU (Eval a) (Eval b) = Eval (unsafeCoerce (unsafeToNum a - unsafeToNum b))
  scaleU (Eval a) n = Eval (unsafeCoerce (unsafeToNum a * n))

-- Helper to extract number from phantom-typed value
unsafeToNum :: forall a. a -> Number
unsafeToNum = unsafeCoerce

-- | Path expressions - compute actual SVG path strings
instance pathExprEval :: PathExpr Eval where
  linePath (Eval x1) (Eval y1) (Eval x2) (Eval y2) =
    Eval (Gen.genLinePath x1 y1 x2 y2)
  polylinePath (Eval points) =
    Eval (Gen.genPolylinePath points)
  linkHorizontal (Eval x1) (Eval y1) (Eval x2) (Eval y2) =
    Eval (Gen.genLinkHorizontal x1 y1 x2 y2)
  linkVertical (Eval x1) (Eval y1) (Eval x2) (Eval y2) =
    Eval (Gen.genLinkVertical x1 y1 x2 y2)
  linkRadial (Eval a1) (Eval r1) (Eval a2) (Eval r2) =
    Eval (Gen.genLinkRadial a1 r1 a2 r2)
  sankeyLink (Eval sx) (Eval sy0) (Eval sy1) (Eval tx) (Eval ty0) (Eval ty1) =
    Eval (Gen.genSankeyLink sx sy0 sy1 tx ty0 ty1)
  ribbon (Eval sa0) (Eval sa1) (Eval ta0) (Eval ta1) (Eval ir) (Eval or) =
    Eval (Gen.genRibbon sa0 sa1 ta0 ta1 ir or)
  arc (Eval start) (Eval end) (Eval inner) (Eval outer) =
    Eval (Gen.genArc start end inner outer)
  closePath (Eval p) = Eval (p <> " Z")

-- =============================================================================
-- EvalD (with datum access)
-- =============================================================================

-- | Evaluator with datum access - a function from datum (and index) to value
newtype EvalD datum a = EvalD (datum -> Int -> a)

runEvalD :: forall datum a. EvalD datum a -> datum -> Int -> a
runEvalD (EvalD f) d i = f d i

instance numExprEvalD :: NumExpr (EvalD datum) where
  lit n = EvalD (\_ _ -> n)
  add (EvalD a) (EvalD b) = EvalD (\d i -> a d i + b d i)
  sub (EvalD a) (EvalD b) = EvalD (\d i -> a d i - b d i)
  mul (EvalD a) (EvalD b) = EvalD (\d i -> a d i * b d i)
  div (EvalD a) (EvalD b) = EvalD (\d i -> a d i / b d i)
  negate (EvalD a) = EvalD (\d i -> P.negate (a d i))

instance stringExprEvalD :: StringExpr (EvalD datum) where
  str s = EvalD (\_ _ -> s)
  concat (EvalD a) (EvalD b) = EvalD (\d i -> a d i <> b d i)

instance boolExprEvalD :: BoolExpr (EvalD datum) where
  bool b = EvalD (\_ _ -> b)
  and (EvalD a) (EvalD b) = EvalD (\d i -> a d i && b d i)
  or (EvalD a) (EvalD b) = EvalD (\d i -> a d i || b d i)
  not (EvalD a) = EvalD (\d i -> P.not (a d i))
  ifThenElse (EvalD cond) (EvalD t) (EvalD f) =
    EvalD (\d i -> if cond d i then t d i else f d i)

instance compareExprEvalD :: CompareExpr (EvalD datum) where
  lt (EvalD a) (EvalD b) = EvalD (\d i -> a d i < b d i)
  lte (EvalD a) (EvalD b) = EvalD (\d i -> a d i <= b d i)
  gt (EvalD a) (EvalD b) = EvalD (\d i -> a d i > b d i)
  gte (EvalD a) (EvalD b) = EvalD (\d i -> a d i >= b d i)
  eq (EvalD a) (EvalD b) = EvalD (\d i -> a d i == b d i)

instance stringCompareExprEvalD :: StringCompareExpr (EvalD datum) where
  strEq (EvalD a) (EvalD b) = EvalD (\d i -> a d i == b d i)
  strNeq (EvalD a) (EvalD b) = EvalD (\d i -> a d i /= b d i)

instance unitExprEvalD :: UnitExpr (EvalD datum) where
  px n = EvalD (\_ _ -> unsafeCoerce n)
  em n = EvalD (\_ _ -> unsafeCoerce n)
  rem n = EvalD (\_ _ -> unsafeCoerce n)
  percent n = EvalD (\_ _ -> unsafeCoerce n)
  vw n = EvalD (\_ _ -> unsafeCoerce n)
  vh n = EvalD (\_ _ -> unsafeCoerce n)
  unitless n = EvalD (\_ _ -> unsafeCoerce n)

instance unitArithEvalD :: UnitArith (EvalD datum) where
  addU (EvalD a) (EvalD b) = EvalD (\d i ->
    unsafeCoerce (unsafeToNum (a d i) + unsafeToNum (b d i)))
  subU (EvalD a) (EvalD b) = EvalD (\d i ->
    unsafeCoerce (unsafeToNum (a d i) - unsafeToNum (b d i)))
  scaleU (EvalD a) n = EvalD (\d i ->
    unsafeCoerce (unsafeToNum (a d i) * n))

instance pathExprEvalD :: PathExpr (EvalD datum) where
  linePath (EvalD x1) (EvalD y1) (EvalD x2) (EvalD y2) = EvalD (\d i ->
    Gen.genLinePath (x1 d i) (y1 d i) (x2 d i) (y2 d i))
  polylinePath (EvalD points) = EvalD (\d i ->
    Gen.genPolylinePath (points d i))
  linkHorizontal (EvalD x1) (EvalD y1) (EvalD x2) (EvalD y2) = EvalD (\d i ->
    Gen.genLinkHorizontal (x1 d i) (y1 d i) (x2 d i) (y2 d i))
  linkVertical (EvalD x1) (EvalD y1) (EvalD x2) (EvalD y2) = EvalD (\d i ->
    Gen.genLinkVertical (x1 d i) (y1 d i) (x2 d i) (y2 d i))
  linkRadial (EvalD a1) (EvalD r1) (EvalD a2) (EvalD r2) = EvalD (\d i ->
    Gen.genLinkRadial (a1 d i) (r1 d i) (a2 d i) (r2 d i))
  sankeyLink (EvalD sx) (EvalD sy0) (EvalD sy1) (EvalD tx) (EvalD ty0) (EvalD ty1) = EvalD (\d i ->
    Gen.genSankeyLink (sx d i) (sy0 d i) (sy1 d i) (tx d i) (ty0 d i) (ty1 d i))
  ribbon (EvalD sa0) (EvalD sa1) (EvalD ta0) (EvalD ta1) (EvalD ir) (EvalD or) = EvalD (\d i ->
    Gen.genRibbon (sa0 d i) (sa1 d i) (ta0 d i) (ta1 d i) (ir d i) (or d i))
  arc (EvalD start) (EvalD end) (EvalD inner) (EvalD outer) = EvalD (\d i ->
    Gen.genArc (start d i) (end d i) (inner d i) (outer d i))
  closePath (EvalD p) = EvalD (\d i -> p d i <> " Z")

-- | Datum field access - looks up field from datum at runtime
-- | The `datumRow` is a Row kind, and we work with `Record datumRow`
instance datumExprEvalD :: DatumExpr (EvalD (Record datumRow)) datumRow where
  field :: forall a sym r
         . IsSymbol sym
        => Row.Cons sym a r datumRow
        => Proxy sym
        -> EvalD (Record datumRow) a
  field proxy = EvalD (\d _ -> unsafeGetField (reflectSymbol proxy) d)

  index = EvalD (\_ i -> i)

-- | Unsafe field access (we've verified types at compile time)
foreign import unsafeGetField :: forall r a. String -> Record r -> a
