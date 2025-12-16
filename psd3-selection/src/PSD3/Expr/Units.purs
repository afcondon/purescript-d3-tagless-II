-- | Type-Safe CSS Units
-- |
-- | Phantom types for CSS units, enabling type-safe unit handling
-- | while allowing interpreters to render appropriately.
-- |
-- | Key insight: Each interpreter renders units differently:
-- | - Eval: strips units, returns raw Number (for D3 SVG which uses unitless)
-- | - CodeGen: preserves unit constructor calls `(px 10.0)`
-- | - SVG: renders with CSS suffix `"10px"` or unitless for SVG attributes
module PSD3.Expr.Units
  ( Pixels
  , Em
  , Rem
  , Percent
  , ViewportWidth
  , ViewportHeight
  , Unitless
  , class UnitExpr
  , px
  , em
  , rem
  , percent
  , vw
  , vh
  , unitless
  , class UnitArith
  , addU
  , subU
  , scaleU
  ) where

-- Phantom types for units - these track units at the type level
data Pixels
data Em
data Rem
data Percent
data ViewportWidth
data ViewportHeight
data Unitless

-- | Unit expressions - create values with specific units
-- |
-- | These constructors are interpreted differently:
-- | - Eval:    `px 10.0` → `10.0` (raw number for D3)
-- | - CodeGen: `px 10.0` → `"(px 10.0)"` (source code)
-- | - SVG:     `px 10.0` → `"10px"` or `"10"` depending on context
class UnitExpr repr where
  px :: Number -> repr Pixels
  em :: Number -> repr Em
  rem :: Number -> repr Rem
  percent :: Number -> repr Percent
  vw :: Number -> repr ViewportWidth
  vh :: Number -> repr ViewportHeight
  unitless :: Number -> repr Unitless

-- | Unit-safe arithmetic
-- |
-- | You can only add/subtract values with the same unit type.
-- | Scaling (multiply by unitless) preserves the unit.
class UnitArith repr where
  -- | Add two values with the same unit
  addU :: forall u. repr u -> repr u -> repr u

  -- | Subtract two values with the same unit
  subU :: forall u. repr u -> repr u -> repr u

  -- | Scale a unit value by a unitless factor
  scaleU :: forall u. repr u -> Number -> repr u
