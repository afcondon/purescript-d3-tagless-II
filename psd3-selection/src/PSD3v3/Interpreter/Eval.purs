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

import PSD3v3.Expr (class NumExpr, class StringExpr, class BoolExpr, class CompareExpr)
import PSD3v3.Units (class UnitExpr, class UnitArith)
import PSD3v3.Datum (class DatumExpr)

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
