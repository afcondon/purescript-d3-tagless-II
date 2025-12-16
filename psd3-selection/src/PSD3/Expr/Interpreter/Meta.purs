-- | Meta Interpreter - Extracts source metadata from expressions
-- |
-- | This interpreter produces `AttrSource` metadata describing where
-- | attribute values come from, without evaluating them.
-- |
-- | Used alongside `EvalD` to capture both the evaluation function
-- | and source metadata for attributes.
module PSD3.Expr.Interpreter.Meta
  ( MetaD(..)
  , runMetaD
  , EvalWithMeta(..)
  , runEvalWithMeta
  , evalWithMeta
  ) where

import Prelude hiding (add, sub, mul, div, negate, not)
import Prelude as P

import Data.Number as Math
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row as Row
import Type.Proxy (Proxy(..))

import PSD3.Internal.Attribute (AttrSource(..))
import PSD3.Expr.Expr (class NumExpr, class StringExpr, class BoolExpr, class CompareExpr, class StringCompareExpr, class TrigExpr)
import PSD3.Expr.Datum (class DatumExpr)
import PSD3.Expr.Interpreter.Eval (EvalD(..), unsafeGetField)

-- | Meta interpreter that extracts source metadata
-- |
-- | Interprets expressions to produce `AttrSource` describing
-- | the structure of the expression.
newtype MetaD (datum :: Row Type) a = MetaD AttrSource

runMetaD :: forall datum a. MetaD datum a -> AttrSource
runMetaD (MetaD src) = src

-- NumExpr instance - tracks expressions
instance NumExpr (MetaD datum) where
  lit n = MetaD (StaticSource (show n))
  add (MetaD a) (MetaD b) = MetaD (combineExpr a " + " b)
  sub (MetaD a) (MetaD b) = MetaD (combineExpr a " - " b)
  mul (MetaD a) (MetaD b) = MetaD (combineExpr a " * " b)
  div (MetaD a) (MetaD b) = MetaD (combineExpr a " / " b)
  negate (MetaD a) = MetaD (prefixExpr "-" a)

-- StringExpr instance
instance StringExpr (MetaD datum) where
  str s = MetaD (StaticSource ("\"" <> s <> "\""))
  concat (MetaD a) (MetaD b) = MetaD (combineExpr a " <> " b)

-- BoolExpr instance
instance BoolExpr (MetaD datum) where
  bool b = MetaD (StaticSource (show b))
  and (MetaD a) (MetaD b) = MetaD (combineExpr a " && " b)
  or (MetaD a) (MetaD b) = MetaD (combineExpr a " || " b)
  not (MetaD a) = MetaD (prefixExpr "not " a)
  ifThenElse (MetaD c) (MetaD t) (MetaD e) = MetaD (ExprSource ("if " <> showSource c <> " then " <> showSource t <> " else " <> showSource e))

-- CompareExpr instance
instance CompareExpr (MetaD datum) where
  lt (MetaD a) (MetaD b) = MetaD (combineExpr a " < " b)
  lte (MetaD a) (MetaD b) = MetaD (combineExpr a " <= " b)
  gt (MetaD a) (MetaD b) = MetaD (combineExpr a " > " b)
  gte (MetaD a) (MetaD b) = MetaD (combineExpr a " >= " b)
  eq (MetaD a) (MetaD b) = MetaD (combineExpr a " == " b)

-- StringCompareExpr instance
instance StringCompareExpr (MetaD datum) where
  strEq (MetaD a) (MetaD b) = MetaD (combineExpr a " == " b)
  strNeq (MetaD a) (MetaD b) = MetaD (combineExpr a " /= " b)

-- TrigExpr instance
instance TrigExpr (MetaD datum) where
  sin (MetaD a) = MetaD (prefixExpr "sin " a)
  cos (MetaD a) = MetaD (prefixExpr "cos " a)
  tan (MetaD a) = MetaD (prefixExpr "tan " a)
  asin (MetaD a) = MetaD (prefixExpr "asin " a)
  acos (MetaD a) = MetaD (prefixExpr "acos " a)
  atan (MetaD a) = MetaD (prefixExpr "atan " a)
  atan2 (MetaD a) (MetaD b) = MetaD (ExprSource ("atan2 " <> showSource a <> " " <> showSource b))
  pi = MetaD (StaticSource "pi")

-- DatumExpr instance - the key one!
instance (IsSymbol sym, Row.Cons sym a r datumRow) => DatumExpr (MetaD datumRow) datumRow where
  field _ = MetaD (FieldSource (reflectSymbol (Proxy :: Proxy sym)))
  index = MetaD IndexSource

-- | Helper to combine two sources with an operator
combineExpr :: AttrSource -> String -> AttrSource -> AttrSource
combineExpr a op b = case a, b of
  -- Two simple field accesses combined = expression
  FieldSource fa, FieldSource fb -> ExprSource ("d." <> fa <> op <> "d." <> fb)
  FieldSource fa, StaticSource sb -> ExprSource ("d." <> fa <> op <> sb)
  StaticSource sa, FieldSource fb -> ExprSource (sa <> op <> "d." <> fb)
  StaticSource sa, StaticSource sb -> ExprSource (sa <> op <> sb)
  -- Anything else with expressions
  _, _ -> ExprSource (showSource a <> op <> showSource b)

-- | Helper to prefix an expression
prefixExpr :: String -> AttrSource -> AttrSource
prefixExpr prefix src = ExprSource (prefix <> showSource src)

-- | Convert AttrSource to string representation
showSource :: AttrSource -> String
showSource = case _ of
  UnknownSource -> "<unknown>"
  StaticSource s -> s
  FieldSource f -> "d." <> f
  ExprSource e -> e
  IndexSource -> "i"

-- =============================================================================
-- Combined Interpreter: EvalWithMeta
-- =============================================================================

-- | Combined interpreter that produces both evaluation function and metadata
-- |
-- | This allows extracting source metadata while also getting the evaluation function.
-- | The datum parameter is a concrete Type (like Record row), not a Row Type.
newtype EvalWithMeta datum a = EvalWithMeta
  { eval :: datum -> Int -> a
  , meta :: AttrSource
  }

runEvalWithMeta :: forall datum a. EvalWithMeta datum a -> { eval :: datum -> Int -> a, meta :: AttrSource }
runEvalWithMeta (EvalWithMeta r) = r

-- | Create EvalWithMeta from a polymorphic expression
-- |
-- | This runs both interpreters on the same expression.
-- | Note: the EvalD takes a concrete datum type, MetaD takes a Row Type
evalWithMeta :: forall datumRow a. EvalD (Record datumRow) a -> MetaD datumRow a -> EvalWithMeta (Record datumRow) a
evalWithMeta (EvalD eval) (MetaD meta) = EvalWithMeta { eval, meta }

-- NumExpr instance
instance NumExpr (EvalWithMeta datum) where
  lit n = EvalWithMeta { eval: \_ _ -> n, meta: StaticSource (show n) }
  add (EvalWithMeta a) (EvalWithMeta b) = EvalWithMeta
    { eval: \d i -> a.eval d i + b.eval d i
    , meta: combineExpr a.meta " + " b.meta
    }
  sub (EvalWithMeta a) (EvalWithMeta b) = EvalWithMeta
    { eval: \d i -> a.eval d i - b.eval d i
    , meta: combineExpr a.meta " - " b.meta
    }
  mul (EvalWithMeta a) (EvalWithMeta b) = EvalWithMeta
    { eval: \d i -> a.eval d i * b.eval d i
    , meta: combineExpr a.meta " * " b.meta
    }
  div (EvalWithMeta a) (EvalWithMeta b) = EvalWithMeta
    { eval: \d i -> a.eval d i / b.eval d i
    , meta: combineExpr a.meta " / " b.meta
    }
  negate (EvalWithMeta a) = EvalWithMeta
    { eval: \d i -> P.negate (a.eval d i)
    , meta: prefixExpr "-" a.meta
    }

-- StringExpr instance
instance StringExpr (EvalWithMeta datum) where
  str s = EvalWithMeta { eval: \_ _ -> s, meta: StaticSource ("\"" <> s <> "\"") }
  concat (EvalWithMeta a) (EvalWithMeta b) = EvalWithMeta
    { eval: \d i -> a.eval d i <> b.eval d i
    , meta: combineExpr a.meta " <> " b.meta
    }

-- BoolExpr instance
instance BoolExpr (EvalWithMeta datum) where
  bool b = EvalWithMeta { eval: \_ _ -> b, meta: StaticSource (show b) }
  and (EvalWithMeta a) (EvalWithMeta b) = EvalWithMeta
    { eval: \d i -> a.eval d i && b.eval d i
    , meta: combineExpr a.meta " && " b.meta
    }
  or (EvalWithMeta a) (EvalWithMeta b) = EvalWithMeta
    { eval: \d i -> a.eval d i || b.eval d i
    , meta: combineExpr a.meta " || " b.meta
    }
  not (EvalWithMeta a) = EvalWithMeta
    { eval: \d i -> P.not (a.eval d i)
    , meta: prefixExpr "not " a.meta
    }
  ifThenElse (EvalWithMeta c) (EvalWithMeta t) (EvalWithMeta e) = EvalWithMeta
    { eval: \d i -> if c.eval d i then t.eval d i else e.eval d i
    , meta: ExprSource ("if " <> showSource c.meta <> " then " <> showSource t.meta <> " else " <> showSource e.meta)
    }

-- TrigExpr instance
instance TrigExpr (EvalWithMeta datum) where
  sin (EvalWithMeta a) = EvalWithMeta { eval: \d i -> Math.sin (a.eval d i), meta: prefixExpr "sin " a.meta }
  cos (EvalWithMeta a) = EvalWithMeta { eval: \d i -> Math.cos (a.eval d i), meta: prefixExpr "cos " a.meta }
  tan (EvalWithMeta a) = EvalWithMeta { eval: \d i -> Math.tan (a.eval d i), meta: prefixExpr "tan " a.meta }
  asin (EvalWithMeta a) = EvalWithMeta { eval: \d i -> Math.asin (a.eval d i), meta: prefixExpr "asin " a.meta }
  acos (EvalWithMeta a) = EvalWithMeta { eval: \d i -> Math.acos (a.eval d i), meta: prefixExpr "acos " a.meta }
  atan (EvalWithMeta a) = EvalWithMeta { eval: \d i -> Math.atan (a.eval d i), meta: prefixExpr "atan " a.meta }
  atan2 (EvalWithMeta a) (EvalWithMeta b) = EvalWithMeta
    { eval: \d i -> Math.atan2 (a.eval d i) (b.eval d i)
    , meta: ExprSource ("atan2 " <> showSource a.meta <> " " <> showSource b.meta)
    }
  pi = EvalWithMeta { eval: \_ _ -> Math.pi, meta: StaticSource "pi" }

-- CompareExpr instance
instance CompareExpr (EvalWithMeta datum) where
  lt (EvalWithMeta a) (EvalWithMeta b) = EvalWithMeta { eval: \d i -> a.eval d i < b.eval d i, meta: combineExpr a.meta " < " b.meta }
  lte (EvalWithMeta a) (EvalWithMeta b) = EvalWithMeta { eval: \d i -> a.eval d i <= b.eval d i, meta: combineExpr a.meta " <= " b.meta }
  gt (EvalWithMeta a) (EvalWithMeta b) = EvalWithMeta { eval: \d i -> a.eval d i > b.eval d i, meta: combineExpr a.meta " > " b.meta }
  gte (EvalWithMeta a) (EvalWithMeta b) = EvalWithMeta { eval: \d i -> a.eval d i >= b.eval d i, meta: combineExpr a.meta " >= " b.meta }
  eq (EvalWithMeta a) (EvalWithMeta b) = EvalWithMeta { eval: \d i -> a.eval d i == b.eval d i, meta: combineExpr a.meta " == " b.meta }

-- StringCompareExpr instance
instance StringCompareExpr (EvalWithMeta datum) where
  strEq (EvalWithMeta a) (EvalWithMeta b) = EvalWithMeta { eval: \d i -> a.eval d i == b.eval d i, meta: combineExpr a.meta " == " b.meta }
  strNeq (EvalWithMeta a) (EvalWithMeta b) = EvalWithMeta { eval: \d i -> a.eval d i /= b.eval d i, meta: combineExpr a.meta " /= " b.meta }

-- DatumExpr instance - the key one!
instance (IsSymbol sym, Row.Cons sym a r datumRow) => DatumExpr (EvalWithMeta (Record datumRow)) datumRow where
  field _ = EvalWithMeta
    { eval: \d _ -> unsafeGetField (reflectSymbol (Proxy :: Proxy sym)) d
    , meta: FieldSource (reflectSymbol (Proxy :: Proxy sym))
    }
  index = EvalWithMeta
    { eval: \_ i -> i
    , meta: IndexSource
    }

