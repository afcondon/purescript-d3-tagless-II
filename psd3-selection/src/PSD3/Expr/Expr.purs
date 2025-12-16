-- | Core Expression DSL
-- |
-- | Finally tagless encoding for attribute expressions.
-- | Enables multiple interpreters: Eval (D3), CodeGen, SVG, English, etc.
module PSD3.Expr.Expr
  ( class NumExpr
  , lit
  , add
  , sub
  , mul
  , div
  , negate
  , class StringExpr
  , str
  , concat
  , class BoolExpr
  , bool
  , and
  , or
  , not
  , ifThenElse
  , class CompareExpr
  , lt
  , lte
  , gt
  , gte
  , eq
  , class StringCompareExpr
  , strEq
  , strNeq
  , class TrigExpr
  , sin
  , cos
  , tan
  , asin
  , acos
  , atan
  , atan2
  , pi
  ) where

import Prelude hiding (add, sub, mul, div, negate, not)

-- | Numeric expressions
class NumExpr repr where
  lit :: Number -> repr Number
  add :: repr Number -> repr Number -> repr Number
  sub :: repr Number -> repr Number -> repr Number
  mul :: repr Number -> repr Number -> repr Number
  div :: repr Number -> repr Number -> repr Number
  negate :: repr Number -> repr Number

-- | String expressions
class StringExpr repr where
  str :: String -> repr String
  concat :: repr String -> repr String -> repr String

-- | Boolean expressions
class BoolExpr repr where
  bool :: Boolean -> repr Boolean
  and :: repr Boolean -> repr Boolean -> repr Boolean
  or :: repr Boolean -> repr Boolean -> repr Boolean
  not :: repr Boolean -> repr Boolean
  ifThenElse :: forall a. repr Boolean -> repr a -> repr a -> repr a

-- | Comparison expressions (produce Boolean from Numbers)
class CompareExpr repr where
  lt :: repr Number -> repr Number -> repr Boolean
  lte :: repr Number -> repr Number -> repr Boolean
  gt :: repr Number -> repr Number -> repr Boolean
  gte :: repr Number -> repr Number -> repr Boolean
  eq :: repr Number -> repr Number -> repr Boolean

-- | String comparison expressions
-- | Separate from CompareExpr to keep numeric and string comparisons distinct
class StringCompareExpr repr where
  strEq :: repr String -> repr String -> repr Boolean
  strNeq :: repr String -> repr String -> repr Boolean

-- | Trigonometric expressions
-- | Essential for polar/radial visualizations (chord diagrams, radial trees, etc.)
class TrigExpr repr where
  sin :: repr Number -> repr Number
  cos :: repr Number -> repr Number
  tan :: repr Number -> repr Number
  asin :: repr Number -> repr Number
  acos :: repr Number -> repr Number
  atan :: repr Number -> repr Number
  atan2 :: repr Number -> repr Number -> repr Number  -- atan2(y, x)
  pi :: repr Number
