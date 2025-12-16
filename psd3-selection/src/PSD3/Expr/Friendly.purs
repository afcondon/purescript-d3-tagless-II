-- | Friendly DSL for Data-Driven Attributes
-- |
-- | A more approachable API for the finally-tagless expression system.
-- | Designed for D3.js developers learning PureScript.
-- |
-- | ## Quick Start
-- |
-- | ```purescript
-- | import PSD3.Expr.Friendly
-- |
-- | -- Define field accessors for your data type
-- | _x = field @"x"
-- | _y = field @"y"
-- | _name = field @"name"
-- |
-- | -- Build expressions with readable arithmetic
-- | xPosition = _x `times` 40.0 `plus` 50.0
-- | yPosition = _y `times` 30.0
-- |
-- | -- Create attributes
-- | myAttrs =
-- |   [ computed "cx" xPosition
-- |   , computed "cy" yPosition
-- |   , staticStr "fill" "steelblue"
-- |   ]
-- | ```
-- |
-- | ## Value Constructors
-- |
-- | - `num 42.0` - numeric value
-- | - `text "hello"` - string value
-- | - `bool true` - boolean value
-- |
-- | ## Arithmetic (use with backticks for infix)
-- |
-- | - `plus`, `minus`, `times`, `dividedBy` - binary operations
-- | - `negated` - unary negation
-- |
-- | ## Attribute Constructors
-- |
-- | - `computed` - data-driven numeric/showable attribute
-- | - `computedStr` - data-driven string attribute
-- | - `static` - constant numeric/showable attribute
-- | - `staticStr` - constant string attribute
-- | - `from` - escape hatch using plain PureScript function
-- |
module PSD3.Expr.Friendly
  ( -- * Value Constructors
    num
  , text
  , bool
    -- * Arithmetic Operations (use with backticks: `plus`, `times`, etc.)
  , plus
  , minus
  , times
  , dividedBy
  , negated
    -- * Arithmetic with literals (no need to wrap right side in `num`)
  , plusN
  , minusN
  , timesN
  , dividedByN
    -- * Comparisons
  , lessThan
  , lessOrEqual
  , greaterThan
  , greaterOrEqual
  , equals
  , textEquals
  , textNotEquals
    -- * Boolean Operations
  , and_
  , or_
  , not_
  , ifThen
    -- * Trig (for radial layouts)
  , sin, cos, tan
  , asin, acos, atan, atan2
  , pi
    -- * String Operations
  , append
    -- * Field Access (re-export)
  , field
  , index
    -- * Attribute Constructors
  , computed
  , computedStr
  , computedWithIndex
  , computedStrWithIndex
  , static
  , staticStr
  , from
  , fromStr
  , fromWithIndex
  , fromStrWithIndex
    -- * Re-exports for advanced use
  , module ReExports
  ) where

import Prelude hiding (add, sub, mul, div, negate, not)

import Data.Symbol (class IsSymbol)
import Prim.Row as Row
import Type.Proxy (Proxy(..))

import PSD3.Expr.Expr (class NumExpr, class StringExpr, class BoolExpr, class CompareExpr, class StringCompareExpr, class TrigExpr)
import PSD3.Expr.Expr as E
import PSD3.Expr.Datum (class DatumExpr)
import PSD3.Expr.Datum as D
import PSD3.Expr.Interpreter.Eval (EvalD(..), runEvalD)
import PSD3.Internal.Attribute (Attribute(..), AttributeName(..), AttributeValue(..))

-- Re-exports for when users need the underlying type classes
import PSD3.Expr.Expr (class NumExpr, class StringExpr, class BoolExpr) as ReExports
import PSD3.Expr.Datum (class DatumExpr) as ReExports
import PSD3.Expr.Interpreter.Eval (EvalD) as ReExports

-- =============================================================================
-- Value Constructors
-- =============================================================================

-- | Create a numeric value
-- |
-- | ```purescript
-- | radius = num 5.0
-- | scaled = _x `times` num 20.0  -- or use `timesN` to skip the `num`
-- | ```
num :: forall repr. NumExpr repr => Number -> repr Number
num = E.lit

-- | Create a text (string) value
-- |
-- | ```purescript
-- | fill = text "steelblue"
-- | label = _name `append` text " (selected)"
-- | ```
text :: forall repr. StringExpr repr => String -> repr String
text = E.str

-- | Create a boolean value
-- |
-- | ```purescript
-- | visible = bool true
-- | ```
bool :: forall repr. BoolExpr repr => Boolean -> repr Boolean
bool = E.bool

-- =============================================================================
-- Arithmetic Operations
-- Use with backticks for readable infix: _x `plus` _y
-- =============================================================================

-- | Add two expressions
-- |
-- | ```purescript
-- | total = _x `plus` _y
-- | offset = _index `times` num 40.0 `plus` num 50.0
-- | ```
plus :: forall repr. NumExpr repr => repr Number -> repr Number -> repr Number
plus = E.add

-- | Subtract expressions
minus :: forall repr. NumExpr repr => repr Number -> repr Number -> repr Number
minus = E.sub

-- | Multiply expressions
-- |
-- | ```purescript
-- | scaled = _value `times` num 2.0
-- | ```
times :: forall repr. NumExpr repr => repr Number -> repr Number -> repr Number
times = E.mul

-- | Divide expressions
dividedBy :: forall repr. NumExpr repr => repr Number -> repr Number -> repr Number
dividedBy = E.div

-- | Negate an expression
negated :: forall repr. NumExpr repr => repr Number -> repr Number
negated = E.negate

-- =============================================================================
-- Arithmetic with literal on right (convenience - no need to wrap in `num`)
-- =============================================================================

-- | Add a literal number: `_x `plusN` 50.0`
plusN :: forall repr. NumExpr repr => repr Number -> Number -> repr Number
plusN a b = E.add a (E.lit b)

-- | Subtract a literal number
minusN :: forall repr. NumExpr repr => repr Number -> Number -> repr Number
minusN a b = E.sub a (E.lit b)

-- | Multiply by a literal number: `_index `timesN` 40.0`
timesN :: forall repr. NumExpr repr => repr Number -> Number -> repr Number
timesN a b = E.mul a (E.lit b)

-- | Divide by a literal number
dividedByN :: forall repr. NumExpr repr => repr Number -> Number -> repr Number
dividedByN a b = E.div a (E.lit b)

-- =============================================================================
-- Comparisons
-- =============================================================================

-- | Less than comparison
lessThan :: forall repr. CompareExpr repr => repr Number -> repr Number -> repr Boolean
lessThan = E.lt

-- | Less than or equal
lessOrEqual :: forall repr. CompareExpr repr => repr Number -> repr Number -> repr Boolean
lessOrEqual = E.lte

-- | Greater than comparison
greaterThan :: forall repr. CompareExpr repr => repr Number -> repr Number -> repr Boolean
greaterThan = E.gt

-- | Greater than or equal
greaterOrEqual :: forall repr. CompareExpr repr => repr Number -> repr Number -> repr Boolean
greaterOrEqual = E.gte

-- | Numeric equality
equals :: forall repr. CompareExpr repr => repr Number -> repr Number -> repr Boolean
equals = E.eq

-- | String equality
textEquals :: forall repr. StringCompareExpr repr => repr String -> repr String -> repr Boolean
textEquals = E.strEq

-- | String inequality
textNotEquals :: forall repr. StringCompareExpr repr => repr String -> repr String -> repr Boolean
textNotEquals = E.strNeq

-- =============================================================================
-- Boolean Operations
-- =============================================================================

-- | Boolean AND (use underscore to avoid keyword conflict)
and_ :: forall repr. BoolExpr repr => repr Boolean -> repr Boolean -> repr Boolean
and_ = E.and

-- | Boolean OR
or_ :: forall repr. BoolExpr repr => repr Boolean -> repr Boolean -> repr Boolean
or_ = E.or

-- | Boolean NOT
not_ :: forall repr. BoolExpr repr => repr Boolean -> repr Boolean
not_ = E.not

-- | Conditional expression
-- |
-- | ```purescript
-- | fillColor = ifThen (_value `greaterThan` num 50.0)
-- |               (text "red")
-- |               (text "blue")
-- | ```
ifThen :: forall repr a. BoolExpr repr => repr Boolean -> repr a -> repr a -> repr a
ifThen = E.ifThenElse

-- =============================================================================
-- Trigonometry (for radial/polar layouts)
-- =============================================================================

sin :: forall repr. TrigExpr repr => repr Number -> repr Number
sin = E.sin

cos :: forall repr. TrigExpr repr => repr Number -> repr Number
cos = E.cos

tan :: forall repr. TrigExpr repr => repr Number -> repr Number
tan = E.tan

asin :: forall repr. TrigExpr repr => repr Number -> repr Number
asin = E.asin

acos :: forall repr. TrigExpr repr => repr Number -> repr Number
acos = E.acos

atan :: forall repr. TrigExpr repr => repr Number -> repr Number
atan = E.atan

atan2 :: forall repr. TrigExpr repr => repr Number -> repr Number -> repr Number
atan2 = E.atan2

pi :: forall repr. TrigExpr repr => repr Number
pi = E.pi

-- =============================================================================
-- String Operations
-- =============================================================================

-- | Concatenate strings
-- |
-- | ```purescript
-- | label = _name `append` text ": " `append` _value
-- | ```
append :: forall repr. StringExpr repr => repr String -> repr String -> repr String
append = E.concat

-- =============================================================================
-- Field Access
-- =============================================================================

-- | Access a field from the datum
-- |
-- | Define field accessors for your data type:
-- |
-- | ```purescript
-- | type MyDataRow = (x :: Number, y :: Number, name :: String)
-- |
-- | _x :: forall repr. DatumExpr repr MyDataRow => repr Number
-- | _x = field @"x"
-- |
-- | _y :: forall repr. DatumExpr repr MyDataRow => repr Number
-- | _y = field @"y"
-- |
-- | _name :: forall repr. DatumExpr repr MyDataRow => repr String
-- | _name = field @"name"
-- | ```
field :: forall @sym repr datumRow a r
       . IsSymbol sym
      => Row.Cons sym a r datumRow
      => DatumExpr repr datumRow
      => repr a
field = D.field (Proxy :: Proxy sym)

-- | Access the element index (for staggered animations, positioning by index)
index :: forall repr datumRow. DatumExpr repr datumRow => repr Int
index = D.index

-- =============================================================================
-- Attribute Constructors
-- =============================================================================

-- | Create a data-driven attribute from an expression
-- |
-- | ```purescript
-- | computed "cx" (_x `timesN` 20.0 `plusN` 50.0)
-- | computed "r" (num 5.0)
-- | ```
computed :: forall datum a
          . Show a
         => String
         -> EvalD datum a
         -> Attribute datum
computed name expr = DataAttr (AttributeName name) (\d -> StringValue $ show (runEvalD expr d 0))

-- | Create a data-driven string attribute (no quotes added)
-- |
-- | ```purescript
-- | computedStr "fill" (ifThen condition (text "red") (text "blue"))
-- | computedStr "class" _category
-- | ```
computedStr :: forall datum
             . String
            -> EvalD datum String
            -> Attribute datum
computedStr name expr = DataAttr (AttributeName name) (\d -> StringValue $ runEvalD expr d 0)

-- | Create a data-driven attribute that also uses the element index
-- |
-- | ```purescript
-- | computedWithIndex "x" (index `timesN` 50.0)
-- | ```
computedWithIndex :: forall datum a
                   . Show a
                  => String
                  -> EvalD datum a
                  -> Attribute datum
computedWithIndex name expr = IndexedAttr (AttributeName name) (\d i -> StringValue $ show (runEvalD expr d i))

-- | Create a data-driven string attribute that also uses the element index
computedStrWithIndex :: forall datum
                      . String
                     -> EvalD datum String
                     -> Attribute datum
computedStrWithIndex name expr = IndexedAttr (AttributeName name) (\d i -> StringValue $ runEvalD expr d i)

-- | Create a static (constant) attribute
-- |
-- | ```purescript
-- | static "stroke-width" 2.0
-- | static "opacity" 0.8
-- | ```
static :: forall datum a. Show a => String -> a -> Attribute datum
static name value = StaticAttr (AttributeName name) (StringValue $ show value)

-- | Create a static string attribute (no quotes)
-- |
-- | ```purescript
-- | staticStr "fill" "steelblue"
-- | staticStr "stroke" "none"
-- | ```
staticStr :: forall datum. String -> String -> Attribute datum
staticStr name value = StaticAttr (AttributeName name) (StringValue value)

-- | Create an attribute from a plain PureScript function (escape hatch)
-- |
-- | Use when computations are complex or use libraries not in the DSL.
-- |
-- | ```purescript
-- | from "cx" (_.x * 20.0 + 50.0)
-- | from "r" (\d -> sqrt d.area / pi)
-- | ```
from :: forall datum a
      . Show a
     => String
     -> (datum -> a)
     -> Attribute datum
from name f = DataAttr (AttributeName name) (\d -> StringValue $ show (f d))

-- | Create a string attribute from a plain function
-- |
-- | ```purescript
-- | fromStr "class" _.category
-- | fromStr "fill" (\d -> if d.active then "green" else "gray")
-- | ```
fromStr :: forall datum
         . String
        -> (datum -> String)
        -> Attribute datum
fromStr name f = DataAttr (AttributeName name) (\d -> StringValue (f d))

-- | Create an indexed attribute from a function taking datum and index
-- |
-- | ```purescript
-- | fromWithIndex "x" (\d i -> toNumber i * 50.0)
-- | ```
fromWithIndex :: forall datum a
               . Show a
              => String
              -> (datum -> Int -> a)
              -> Attribute datum
fromWithIndex name f = IndexedAttr (AttributeName name) (\d i -> StringValue $ show (f d i))

-- | Create an indexed string attribute from a function
fromStrWithIndex :: forall datum
                  . String
                 -> (datum -> Int -> String)
                 -> Attribute datum
fromStrWithIndex name f = IndexedAttr (AttributeName name) (\d i -> StringValue (f d i))
