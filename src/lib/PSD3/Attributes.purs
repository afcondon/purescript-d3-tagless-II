-- | PSD3.Attributes - All attribute functions for D3 selections
-- |
-- | This module re-exports all attribute functions for working with D3 selections.
-- | Import this to get access to all visual attributes, transformations, events, etc.
-- |
-- | ## Usage
-- |
-- | ```purescript
-- | import PSD3.Attributes
-- |
-- | myCircle = do
-- |   circle <- appendTo svg Circle
-- |     [ cx 100.0
-- |     , cy 100.0
-- |     , radius 50.0
-- |     , fill "red"
-- |     , strokeColor "black"
-- |     , strokeWidth 2.0
-- |     ]
-- |   pure circle
-- | ```
-- |
-- | ## Categories
-- |
-- | - **Position**: x, y, cx, cy, dx, dy, x1, y1, x2, y2
-- | - **Size**: width, height, radius
-- | - **Colors**: fill, fillOpacity, strokeColor, strokeWidth, strokeOpacity, backgroundColor
-- | - **Text**: text, fontSize, fontFamily, textAnchor
-- | - **SVG**: viewBox, preserveAspectRatio, d (path data)
-- | - **Styling**: classed, opacity, cursor
-- | - **Transforms**: transform, rotate, originX, originY
-- | - **Transitions**: transition, transitionWithDuration, andThen, to
-- | - **Events**: onMouseEvent, onMouseEventEffectful
-- | - **Special**: remove, autoBox
module PSD3.Attributes
  ( module Sugar
  ) where

import PSD3.Internal.Attributes.Sugar (AlignAspectRatio_X(..), AlignAspectRatio_Y(..), AspectRatioPreserve(..), AspectRatioSpec(..), andThen, assembleTransforms, autoBox, backgroundColor, classed, cursor, cx, cy, d, defaultTransition, dx, dy, fill, fillOpacity, fontFamily, fontSize, height, height100, namedTransition, onMouseEvent, onMouseEventEffectful, opacity, originX, originY, pointXY, preserveAspectRatio, radius, remove, rotate, strokeColor, strokeLineJoin, strokeOpacity, strokeWidth, strength, text, textAnchor, to, transform, transform', transition, transitionWithDuration, viewBox, width, width100, x, x1, x2, y, y1, y2) as Sugar
