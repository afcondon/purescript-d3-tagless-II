module D3.Attributes.Sugar where

import Prelude

import D3.Attributes.Instances (class ToAttr, Attribute(..), toAttr)

strokeColor :: forall a. ToAttr String a => a -> Attribute
strokeColor = Attribute "stroke" <<< toAttr

strokeOpacity :: forall a. ToAttr Number a => a -> Attribute
strokeOpacity = Attribute "stroke-opacity" <<< toAttr

strokeWidth :: forall a. ToAttr Number a => a -> Attribute
strokeWidth = Attribute "stroke-width" <<< toAttr

fill :: forall a. ToAttr String a => a -> Attribute
fill = Attribute "fill" <<< toAttr

viewBox :: Number -> Number -> Number -> Number -> Attribute
viewBox xo yo w h = Attribute "viewbox" $ toAttr [ xo, yo, w, h ]

fontFamily :: forall a. ToAttr String a => a -> Attribute
fontFamily = Attribute "font-family" <<< toAttr

textAnchor :: forall a. ToAttr String a => a -> Attribute
textAnchor = Attribute "text-anchor" <<< toAttr

radius :: forall a. ToAttr Number a => a -> Attribute
radius = Attribute "r" <<< toAttr

fontSize :: forall a. ToAttr Number a => a -> Attribute
fontSize = Attribute "font-size" <<< toAttr

width :: forall a. ToAttr Number a => a -> Attribute
width = Attribute "width" <<< toAttr

height :: forall a. ToAttr Number a => a -> Attribute
height = Attribute "height" <<< toAttr

x :: forall a. ToAttr Number a => a -> Attribute
x = Attribute "x" <<< toAttr

y :: forall a. ToAttr Number a => a -> Attribute
y = Attribute "y" <<< toAttr

r :: forall a. ToAttr Number a => a -> Attribute
r = Attribute "r" <<< toAttr

dx :: forall a. ToAttr Number a => a -> Attribute
dx = Attribute "dx" <<< toAttr

dy :: forall a. ToAttr Number a => a -> Attribute
dy = Attribute "dy" <<< toAttr

cx :: forall a. ToAttr Number a => a -> Attribute
cx = Attribute "cx" <<< toAttr

cy :: forall a. ToAttr Number a => a -> Attribute
cy = Attribute "cy" <<< toAttr

text :: forall a. ToAttr String a => a -> Attribute
text = Attribute "text" <<< toAttr

classed :: forall a. ToAttr String a => a -> Attribute
classed = Attribute "classed" <<< toAttr
