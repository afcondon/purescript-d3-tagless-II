-- | SVG Interpreter
-- |
-- | Generates SVG attribute strings from expressions.
-- | Enables server-side SVG generation - zero client JS needed!
module PSD3.Expr.Interpreter.SVG
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

import PSD3.Expr.Expr (class NumExpr, class StringExpr, class BoolExpr, class CompareExpr, class StringCompareExpr, class TrigExpr)
import Data.Number as Math
import PSD3.Expr.Units (class UnitExpr, class UnitArith)
import PSD3.Expr.Datum (class DatumExpr)
import PSD3.Expr.Path (class PathExpr)
import PSD3.Expr.Path.Generators as Gen

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

instance stringCompareExprSVG :: StringCompareExpr SVG where
  strEq (SVG a) (SVG b) = SVG (show (a == b))
  strNeq (SVG a) (SVG b) = SVG (show (a /= b))

instance trigExprSVG :: TrigExpr SVG where
  sin (SVG a) = SVG (show (Math.sin (unsafeParseNum a)))
  cos (SVG a) = SVG (show (Math.cos (unsafeParseNum a)))
  tan (SVG a) = SVG (show (Math.tan (unsafeParseNum a)))
  asin (SVG a) = SVG (show (Math.asin (unsafeParseNum a)))
  acos (SVG a) = SVG (show (Math.acos (unsafeParseNum a)))
  atan (SVG a) = SVG (show (Math.atan (unsafeParseNum a)))
  atan2 (SVG y) (SVG x) = SVG (show (Math.atan2 (unsafeParseNum y) (unsafeParseNum x)))
  pi = SVG (show Math.pi)

-- | Path expressions for SVG - compute actual path strings
instance pathExprSVG :: PathExpr SVG where
  linePath (SVG x1) (SVG y1) (SVG x2) (SVG y2) =
    SVG (Gen.genLinePath (unsafeParseNum x1) (unsafeParseNum y1) (unsafeParseNum x2) (unsafeParseNum y2))
  polylinePath (SVG _) =
    SVG "" -- Would need to parse array from string - not practical for SVG interpreter
  linkHorizontal (SVG x1) (SVG y1) (SVG x2) (SVG y2) =
    SVG (Gen.genLinkHorizontal (unsafeParseNum x1) (unsafeParseNum y1) (unsafeParseNum x2) (unsafeParseNum y2))
  linkVertical (SVG x1) (SVG y1) (SVG x2) (SVG y2) =
    SVG (Gen.genLinkVertical (unsafeParseNum x1) (unsafeParseNum y1) (unsafeParseNum x2) (unsafeParseNum y2))
  linkRadial (SVG a1) (SVG r1) (SVG a2) (SVG r2) =
    SVG (Gen.genLinkRadial (unsafeParseNum a1) (unsafeParseNum r1) (unsafeParseNum a2) (unsafeParseNum r2))
  sankeyLink (SVG sx) (SVG sy0) (SVG sy1) (SVG tx) (SVG ty0) (SVG ty1) =
    SVG (Gen.genSankeyLink (unsafeParseNum sx) (unsafeParseNum sy0) (unsafeParseNum sy1) (unsafeParseNum tx) (unsafeParseNum ty0) (unsafeParseNum ty1))
  ribbon (SVG sa0) (SVG sa1) (SVG ta0) (SVG ta1) (SVG ir) (SVG or) =
    SVG (Gen.genRibbon (unsafeParseNum sa0) (unsafeParseNum sa1) (unsafeParseNum ta0) (unsafeParseNum ta1) (unsafeParseNum ir) (unsafeParseNum or))
  arc (SVG start) (SVG end) (SVG inner) (SVG outer) =
    SVG (Gen.genArc (unsafeParseNum start) (unsafeParseNum end) (unsafeParseNum inner) (unsafeParseNum outer))
  closePath (SVG p) = SVG (p <> " Z")

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

instance stringCompareExprSVGD :: StringCompareExpr (SVGD datum) where
  strEq (SVGD a) (SVGD b) = SVGD (\d i -> show (a d i == b d i))
  strNeq (SVGD a) (SVGD b) = SVGD (\d i -> show (a d i /= b d i))

instance trigExprSVGD :: TrigExpr (SVGD datum) where
  sin (SVGD a) = SVGD (\d i -> show (Math.sin (unsafeParseNum (a d i))))
  cos (SVGD a) = SVGD (\d i -> show (Math.cos (unsafeParseNum (a d i))))
  tan (SVGD a) = SVGD (\d i -> show (Math.tan (unsafeParseNum (a d i))))
  asin (SVGD a) = SVGD (\d i -> show (Math.asin (unsafeParseNum (a d i))))
  acos (SVGD a) = SVGD (\d i -> show (Math.acos (unsafeParseNum (a d i))))
  atan (SVGD a) = SVGD (\d i -> show (Math.atan (unsafeParseNum (a d i))))
  atan2 (SVGD y) (SVGD x) = SVGD (\d i -> show (Math.atan2 (unsafeParseNum (y d i)) (unsafeParseNum (x d i))))
  pi = SVGD (\_ _ -> show Math.pi)

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

instance pathExprSVGD :: PathExpr (SVGD datum) where
  linePath (SVGD x1) (SVGD y1) (SVGD x2) (SVGD y2) = SVGD (\d i ->
    Gen.genLinePath (unsafeParseNum (x1 d i)) (unsafeParseNum (y1 d i)) (unsafeParseNum (x2 d i)) (unsafeParseNum (y2 d i)))
  polylinePath (SVGD _) = SVGD (\_ _ -> "") -- Not practical
  linkHorizontal (SVGD x1) (SVGD y1) (SVGD x2) (SVGD y2) = SVGD (\d i ->
    Gen.genLinkHorizontal (unsafeParseNum (x1 d i)) (unsafeParseNum (y1 d i)) (unsafeParseNum (x2 d i)) (unsafeParseNum (y2 d i)))
  linkVertical (SVGD x1) (SVGD y1) (SVGD x2) (SVGD y2) = SVGD (\d i ->
    Gen.genLinkVertical (unsafeParseNum (x1 d i)) (unsafeParseNum (y1 d i)) (unsafeParseNum (x2 d i)) (unsafeParseNum (y2 d i)))
  linkRadial (SVGD a1) (SVGD r1) (SVGD a2) (SVGD r2) = SVGD (\d i ->
    Gen.genLinkRadial (unsafeParseNum (a1 d i)) (unsafeParseNum (r1 d i)) (unsafeParseNum (a2 d i)) (unsafeParseNum (r2 d i)))
  sankeyLink (SVGD sx) (SVGD sy0) (SVGD sy1) (SVGD tx) (SVGD ty0) (SVGD ty1) = SVGD (\d i ->
    Gen.genSankeyLink (unsafeParseNum (sx d i)) (unsafeParseNum (sy0 d i)) (unsafeParseNum (sy1 d i)) (unsafeParseNum (tx d i)) (unsafeParseNum (ty0 d i)) (unsafeParseNum (ty1 d i)))
  ribbon (SVGD sa0) (SVGD sa1) (SVGD ta0) (SVGD ta1) (SVGD ir) (SVGD or) = SVGD (\d i ->
    Gen.genRibbon (unsafeParseNum (sa0 d i)) (unsafeParseNum (sa1 d i)) (unsafeParseNum (ta0 d i)) (unsafeParseNum (ta1 d i)) (unsafeParseNum (ir d i)) (unsafeParseNum (or d i)))
  arc (SVGD start) (SVGD end) (SVGD inner) (SVGD outer) = SVGD (\d i ->
    Gen.genArc (unsafeParseNum (start d i)) (unsafeParseNum (end d i)) (unsafeParseNum (inner d i)) (unsafeParseNum (outer d i)))
  closePath (SVGD p) = SVGD (\d i -> p d i <> " Z")

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
