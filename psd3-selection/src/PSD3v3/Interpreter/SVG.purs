-- | PSD3v3 SVG Interpreter
-- |
-- | Generates SVG attribute strings from expressions.
-- | Enables server-side SVG generation - zero client JS needed!
module PSD3v3.Interpreter.SVG
  ( SVG(..)
  , runSVG
  , SVGD(..)
  , runSVGD
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
-- Simple SVG (no datum - for static visualizations)
-- =============================================================================

-- | SVG interpreter produces attribute value strings
newtype SVG a = SVG String

runSVG :: forall a. SVG a -> String
runSVG (SVG s) = s

instance numExprSVG :: NumExpr SVG where
  lit n = SVG (show n)
  add (SVG a) (SVG b) = SVG (show (unsafeParseNum a + unsafeParseNum b))
  sub (SVG a) (SVG b) = SVG (show (unsafeParseNum a - unsafeParseNum b))
  mul (SVG a) (SVG b) = SVG (show (unsafeParseNum a * unsafeParseNum b))
  div (SVG a) (SVG b) = SVG (show (unsafeParseNum a / unsafeParseNum b))
  negate (SVG a) = SVG (show (P.negate (unsafeParseNum a)))

instance stringExprSVG :: StringExpr SVG where
  str s = SVG s  -- No quotes - raw string for SVG attribute
  concat (SVG a) (SVG b) = SVG (a <> b)

instance boolExprSVG :: BoolExpr SVG where
  bool b = SVG (if b then "true" else "false")
  and (SVG a) (SVG b) = SVG (show (unsafeParseBool a && unsafeParseBool b))
  or (SVG a) (SVG b) = SVG (show (unsafeParseBool a || unsafeParseBool b))
  not (SVG a) = SVG (show (P.not (unsafeParseBool a)))
  ifThenElse (SVG cond) (SVG t) (SVG f) =
    SVG (if unsafeParseBool cond then t else f)

instance compareExprSVG :: CompareExpr SVG where
  lt (SVG a) (SVG b) = SVG (show (unsafeParseNum a < unsafeParseNum b))
  lte (SVG a) (SVG b) = SVG (show (unsafeParseNum a <= unsafeParseNum b))
  gt (SVG a) (SVG b) = SVG (show (unsafeParseNum a > unsafeParseNum b))
  gte (SVG a) (SVG b) = SVG (show (unsafeParseNum a >= unsafeParseNum b))
  eq (SVG a) (SVG b) = SVG (show (unsafeParseNum a == unsafeParseNum b))

-- | Units render with their suffix for CSS, or just number for SVG
-- | This is where the magic happens - same expression, different output format!
instance unitExprSVG :: UnitExpr SVG where
  px n = SVG (show n <> "px")   -- CSS-style with suffix
  em n = SVG (show n <> "em")
  rem n = SVG (show n <> "rem")
  percent n = SVG (show n <> "%")
  vw n = SVG (show n <> "vw")
  vh n = SVG (show n <> "vh")
  unitless n = SVG (show n)     -- No suffix for unitless

-- | Unit arithmetic for SVG - operates on strings with units
-- | Note: This is a simplified implementation; real CSS calc would be more complex
instance unitArithSVG :: UnitArith SVG where
  addU (SVG a) (SVG b) = SVG ("calc(" <> a <> " + " <> b <> ")")
  subU (SVG a) (SVG b) = SVG ("calc(" <> a <> " - " <> b <> ")")
  scaleU (SVG a) n = SVG ("calc(" <> a <> " * " <> show n <> ")")

-- =============================================================================
-- SVGD (with datum - for data-driven visualizations)
-- =============================================================================

-- | SVG with datum access - produces a function that generates SVG
newtype SVGD datum a = SVGD (datum -> Int -> String)

runSVGD :: forall datum a. SVGD datum a -> datum -> Int -> String
runSVGD (SVGD f) d i = f d i

instance numExprSVGD :: NumExpr (SVGD datum) where
  lit n = SVGD (\_ _ -> show n)
  add (SVGD a) (SVGD b) = SVGD (\d i ->
    show (unsafeParseNum (a d i) + unsafeParseNum (b d i)))
  sub (SVGD a) (SVGD b) = SVGD (\d i ->
    show (unsafeParseNum (a d i) - unsafeParseNum (b d i)))
  mul (SVGD a) (SVGD b) = SVGD (\d i ->
    show (unsafeParseNum (a d i) * unsafeParseNum (b d i)))
  div (SVGD a) (SVGD b) = SVGD (\d i ->
    show (unsafeParseNum (a d i) / unsafeParseNum (b d i)))
  negate (SVGD a) = SVGD (\d i -> show (P.negate (unsafeParseNum (a d i))))

instance stringExprSVGD :: StringExpr (SVGD datum) where
  str s = SVGD (\_ _ -> s)
  concat (SVGD a) (SVGD b) = SVGD (\d i -> a d i <> b d i)

instance boolExprSVGD :: BoolExpr (SVGD datum) where
  bool b = SVGD (\_ _ -> if b then "true" else "false")
  and (SVGD a) (SVGD b) = SVGD (\d i ->
    show (unsafeParseBool (a d i) && unsafeParseBool (b d i)))
  or (SVGD a) (SVGD b) = SVGD (\d i ->
    show (unsafeParseBool (a d i) || unsafeParseBool (b d i)))
  not (SVGD a) = SVGD (\d i -> show (P.not (unsafeParseBool (a d i))))
  ifThenElse (SVGD cond) (SVGD t) (SVGD f) = SVGD (\d i ->
    if unsafeParseBool (cond d i) then t d i else f d i)

instance compareExprSVGD :: CompareExpr (SVGD datum) where
  lt (SVGD a) (SVGD b) = SVGD (\d i ->
    show (unsafeParseNum (a d i) < unsafeParseNum (b d i)))
  lte (SVGD a) (SVGD b) = SVGD (\d i ->
    show (unsafeParseNum (a d i) <= unsafeParseNum (b d i)))
  gt (SVGD a) (SVGD b) = SVGD (\d i ->
    show (unsafeParseNum (a d i) > unsafeParseNum (b d i)))
  gte (SVGD a) (SVGD b) = SVGD (\d i ->
    show (unsafeParseNum (a d i) >= unsafeParseNum (b d i)))
  eq (SVGD a) (SVGD b) = SVGD (\d i ->
    show (unsafeParseNum (a d i) == unsafeParseNum (b d i)))

instance unitExprSVGD :: UnitExpr (SVGD datum) where
  px n = SVGD (\_ _ -> show n <> "px")
  em n = SVGD (\_ _ -> show n <> "em")
  rem n = SVGD (\_ _ -> show n <> "rem")
  percent n = SVGD (\_ _ -> show n <> "%")
  vw n = SVGD (\_ _ -> show n <> "vw")
  vh n = SVGD (\_ _ -> show n <> "vh")
  unitless n = SVGD (\_ _ -> show n)

instance unitArithSVGD :: UnitArith (SVGD datum) where
  addU (SVGD a) (SVGD b) = SVGD (\d i -> "calc(" <> a d i <> " + " <> b d i <> ")")
  subU (SVGD a) (SVGD b) = SVGD (\d i -> "calc(" <> a d i <> " - " <> b d i <> ")")
  scaleU (SVGD a) n = SVGD (\d i -> "calc(" <> a d i <> " * " <> show n <> ")")

-- | Datum field access
instance datumExprSVGD :: DatumExpr (SVGD (Record datumRow)) datumRow where
  field :: forall a sym r
         . IsSymbol sym
        => Row.Cons sym a r datumRow
        => Proxy sym
        -> SVGD (Record datumRow) a
  field proxy = SVGD (\d _ ->
    unsafeShowField (reflectSymbol proxy) d)

  index = SVGD (\_ i -> show i)

-- =============================================================================
-- FFI helpers
-- =============================================================================

foreign import unsafeParseNum :: String -> Number
foreign import unsafeParseBool :: String -> Boolean
foreign import unsafeShowField :: forall r. String -> Record r -> String
