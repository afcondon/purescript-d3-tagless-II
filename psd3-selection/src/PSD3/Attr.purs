-- | PSD3.Attr - Type-safe attributes for visualization elements
-- |
-- | This module provides all the attributes you need to style SVG elements.
-- | Attributes can be static values or data-driven functions.
-- |
-- | ## Usage
-- |
-- | ```purescript
-- | import PSD3.AST as A
-- | import PSD3.Attr (width, height, cx, cy, radius, fill, stroke)
-- |
-- | circle :: A.AST DataPoint
-- | circle = A.elem Circle
-- |   [ cx 100.0           -- Static value
-- |   , cy (\d -> d.y)     -- Data-driven
-- |   , radius 5.0
-- |   , fill "steelblue"
-- |   , stroke "white"
-- |   ]
-- | ```
-- |
-- | ## Attribute Types
-- |
-- | - `StaticAttr`: Same value for all elements
-- | - `DataAttr`: Value computed from datum (`datum -> value`)
-- | - `IndexedAttr`: Value computed from datum and index (`datum -> Int -> value`)
-- |
-- | ## Available Attributes
-- |
-- | **Shape/Position**: `cx`, `cy`, `x`, `y`, `x1`, `y1`, `x2`, `y2`, `width`, `height`, `radius`, `d`
-- | **Style**: `fill`, `stroke`, `strokeWidth`, `opacity`, `fillOpacity`, `strokeOpacity`
-- | **Text**: `textContent`, `fontSize`, `fontFamily`, `fontWeight`, `textAnchor`, `dominantBaseline`
-- | **Other**: `transform`, `class_`, `id_`, `viewBox`
-- |
module PSD3.Attr
  ( module Exports
  ) where

import PSD3v2.Attribute.Types
  ( Attribute(..)
  , AttributeName(..)
  , AttributeValue(..)
  , class ToAttr
  , toAttr
  , fill
  , fillOpacity
  , stroke
  , strokeWidth
  , strokeOpacity
  , strokeDasharray
  , opacity
  , cx
  , cy
  , radius
  , x
  , y
  , x1
  , y1
  , x2
  , y2
  , width
  , height
  , d
  , transform
  , class_
  , id_
  , viewBox
  , fontSize
  , fontFamily
  , fontWeight
  , textAnchor
  , dominantBaseline
  , dy
  , dx
  , textContent
  , offset
  , stopColor
  , gradientUnits
  ) as Exports
