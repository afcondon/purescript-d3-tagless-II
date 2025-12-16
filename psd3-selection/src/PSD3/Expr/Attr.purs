-- | PSD3v3 Attribute DSL
-- |
-- | Finally tagless encoding for SVG/HTML attributes.
-- | Each attribute function takes a repr value and produces repr Attr.
module PSD3.Expr.Attr
  ( Attr
  , class AttrExpr
  -- Position attributes
  , cx
  , cy
  , x
  , y
  , x1
  , y1
  , x2
  , y2
  , dx
  , dy
  -- Size attributes
  , r
  , rx
  , ry
  , width
  , height
  -- Style attributes
  , fill
  , stroke
  , strokeWidth
  , opacity
  , fillOpacity
  , strokeOpacity
  -- Text attributes
  , fontSize
  , fontFamily
  , textAnchor
  , dominantBaseline
  -- Transform
  , transform
  -- Path
  , d
  -- Identity
  , id_
  , class_
  ) where

import PSD3.Expr.Units (Pixels)

-- | Phantom type for attributes
data Attr

-- | Attribute expressions
-- |
-- | Note: Attributes accept `repr Pixels` for dimensional values,
-- | enabling type-safe units. Use `unitless` for pure numbers.
class AttrExpr repr where
  -- Position (typically Pixels or Unitless for SVG)
  cx :: repr Pixels -> repr Attr
  cy :: repr Pixels -> repr Attr
  x :: repr Pixels -> repr Attr
  y :: repr Pixels -> repr Attr
  x1 :: repr Pixels -> repr Attr
  y1 :: repr Pixels -> repr Attr
  x2 :: repr Pixels -> repr Attr
  y2 :: repr Pixels -> repr Attr
  dx :: repr Pixels -> repr Attr
  dy :: repr Pixels -> repr Attr

  -- Size
  r :: repr Pixels -> repr Attr
  rx :: repr Pixels -> repr Attr
  ry :: repr Pixels -> repr Attr
  width :: repr Pixels -> repr Attr
  height :: repr Pixels -> repr Attr

  -- Style (strings and numbers)
  fill :: repr String -> repr Attr
  stroke :: repr String -> repr Attr
  strokeWidth :: repr Pixels -> repr Attr
  opacity :: repr Number -> repr Attr
  fillOpacity :: repr Number -> repr Attr
  strokeOpacity :: repr Number -> repr Attr

  -- Text
  fontSize :: repr Pixels -> repr Attr
  fontFamily :: repr String -> repr Attr
  textAnchor :: repr String -> repr Attr
  dominantBaseline :: repr String -> repr Attr

  -- Transform (string for now, could be its own DSL)
  transform :: repr String -> repr Attr

  -- Path data
  d :: repr String -> repr Attr

  -- Identity
  id_ :: repr String -> repr Attr
  class_ :: repr String -> repr Attr
