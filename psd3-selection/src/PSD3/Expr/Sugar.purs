-- | Syntactic Sugar
-- |
-- | Operators and helpers to make expressions more readable.
-- |
-- | Compare:
-- |   add (mul xField (lit 20.0)) (lit 200.0)
-- | vs:
-- |   xField *. 20.0 +. 200.0
module PSD3.Expr.Sugar
  ( -- Numeric operators
    (+.), addOp
  , (-.), subOp
  , (*.), mulOp
  , (/.), divOp
  , neg
    -- Numeric with literals on right
  , (+:), addLit
  , (-:), subLit
  , (*:), mulLit
  , (/:), divLit
    -- Unit operators
  , (+~), addUnitOp
  , (-~), subUnitOp
  , (*~), scaleUnitOp
    -- String operators
  , (<+>), concatOp
    -- Comparison operators (numeric)
  , (<.), ltOp
  , (<=.), lteOp
  , (>.), gtOp
  , (>=.), gteOp
  , (==.), eqOp
    -- String comparison operators
  , (===), strEqOp
  , (/==), strNeqOp
    -- Trig (re-exported from Expr)
  , module TrigExports
    -- Helpers
  , n
  , s
  ) where

import Prelude hiding (add, sub, mul, div, negate, not)

import PSD3.Expr.Expr (class NumExpr, class StringExpr, class CompareExpr, class StringCompareExpr)
import PSD3.Expr.Expr (class TrigExpr, sin, cos, tan, asin, acos, atan, atan2, pi) as TrigExports
import PSD3.Expr.Expr as E
import PSD3.Expr.Units (class UnitArith)
import PSD3.Expr.Units as U

-- =============================================================================
-- Numeric Operators (repr Number -> repr Number -> repr Number)
-- =============================================================================

infixl 6 addOp as +.
infixl 6 subOp as -.
infixl 7 mulOp as *.
infixl 7 divOp as /.

addOp :: forall repr. NumExpr repr => repr Number -> repr Number -> repr Number
addOp = E.add

subOp :: forall repr. NumExpr repr => repr Number -> repr Number -> repr Number
subOp = E.sub

mulOp :: forall repr. NumExpr repr => repr Number -> repr Number -> repr Number
mulOp = E.mul

divOp :: forall repr. NumExpr repr => repr Number -> repr Number -> repr Number
divOp = E.div

neg :: forall repr. NumExpr repr => repr Number -> repr Number
neg = E.negate

-- =============================================================================
-- Numeric with literal on right (repr Number -> Number -> repr Number)
-- Allows: xField *: 20.0  instead of  xField *. lit 20.0
-- =============================================================================

infixl 6 addLit as +:
infixl 6 subLit as -:
infixl 7 mulLit as *:
infixl 7 divLit as /:

addLit :: forall repr. NumExpr repr => repr Number -> Number -> repr Number
addLit a b = E.add a (E.lit b)

subLit :: forall repr. NumExpr repr => repr Number -> Number -> repr Number
subLit a b = E.sub a (E.lit b)

mulLit :: forall repr. NumExpr repr => repr Number -> Number -> repr Number
mulLit a b = E.mul a (E.lit b)

divLit :: forall repr. NumExpr repr => repr Number -> Number -> repr Number
divLit a b = E.div a (E.lit b)

-- =============================================================================
-- Unit Operators (repr u -> repr u -> repr u)
-- =============================================================================

infixl 6 addUnitOp as +~
infixl 6 subUnitOp as -~
infixl 7 scaleUnitOp as *~

addUnitOp :: forall repr u. UnitArith repr => repr u -> repr u -> repr u
addUnitOp = U.addU

subUnitOp :: forall repr u. UnitArith repr => repr u -> repr u -> repr u
subUnitOp = U.subU

-- | Scale a unit value: (px 10.0) *~ 2.0
scaleUnitOp :: forall repr u. UnitArith repr => repr u -> Number -> repr u
scaleUnitOp = U.scaleU

-- =============================================================================
-- String Operators
-- =============================================================================

infixr 5 concatOp as <+>

concatOp :: forall repr. StringExpr repr => repr String -> repr String -> repr String
concatOp = E.concat

-- =============================================================================
-- Comparison Operators (repr Number -> repr Number -> repr Boolean)
-- =============================================================================

infix 4 ltOp as <.
infix 4 lteOp as <=.
infix 4 gtOp as >.
infix 4 gteOp as >=.
infix 4 eqOp as ==.

ltOp :: forall repr. CompareExpr repr => repr Number -> repr Number -> repr Boolean
ltOp = E.lt

lteOp :: forall repr. CompareExpr repr => repr Number -> repr Number -> repr Boolean
lteOp = E.lte

gtOp :: forall repr. CompareExpr repr => repr Number -> repr Number -> repr Boolean
gtOp = E.gt

gteOp :: forall repr. CompareExpr repr => repr Number -> repr Number -> repr Boolean
gteOp = E.gte

eqOp :: forall repr. CompareExpr repr => repr Number -> repr Number -> repr Boolean
eqOp = E.eq

-- =============================================================================
-- String Comparison Operators (repr String -> repr String -> repr Boolean)
-- =============================================================================

infix 4 strEqOp as ===
infix 4 strNeqOp as /==

strEqOp :: forall repr. StringCompareExpr repr => repr String -> repr String -> repr Boolean
strEqOp = E.strEq

strNeqOp :: forall repr. StringCompareExpr repr => repr String -> repr String -> repr Boolean
strNeqOp = E.strNeq

-- =============================================================================
-- Short Helpers
-- =============================================================================

-- | Short alias for `lit` - creates a numeric literal
-- | Usage: n 42.0
n :: forall repr. NumExpr repr => Number -> repr Number
n = E.lit

-- | Short alias for `str` - creates a string literal
-- | Usage: s "hello"
s :: forall repr. StringExpr repr => String -> repr String
s = E.str
