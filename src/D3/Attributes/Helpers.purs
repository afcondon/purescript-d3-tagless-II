module D3.Attributes.Sugar where

import Prelude

import D3.Attributes.Instances (class ToAttr, Attribute(..), toAttr)
import D3.Selection (Chainable(..), EasingFunction(..), Transition)
import Effect.Aff (Milliseconds(..))
import Data.Array ((:))

strokeColor :: forall a. ToAttr String a => a -> Chainable
strokeColor = AttrT <<< Attribute "stroke" <<< toAttr

strokeOpacity :: forall a. ToAttr Number a => a -> Chainable
strokeOpacity = AttrT <<< Attribute "stroke-opacity" <<< toAttr

strokeWidth :: forall a. ToAttr Number a => a -> Chainable
strokeWidth = AttrT <<< Attribute "stroke-width" <<< toAttr

fill :: forall a. ToAttr String a => a -> Chainable
fill = AttrT <<< Attribute "fill" <<< toAttr

-- TODO this definitely needs to be Number-with-unit here
viewBox :: Number -> Number -> Number -> Number -> Chainable
viewBox xo yo w h = AttrT <<< Attribute "viewbox" $ toAttr [ xo, yo, w, h ]

fontFamily :: forall a. ToAttr String a => a -> Chainable
fontFamily = AttrT <<< Attribute "font-family" <<< toAttr

textAnchor :: forall a. ToAttr String a => a -> Chainable
textAnchor = AttrT <<< Attribute "text-anchor" <<< toAttr

radius :: forall a. ToAttr Number a => a -> Chainable
radius = AttrT <<< Attribute "r" <<< toAttr

fontSize :: forall a. ToAttr Number a => a -> Chainable
fontSize = AttrT <<< Attribute "font-size" <<< toAttr

width :: forall a. ToAttr Number a => a -> Chainable
width = AttrT <<< Attribute "width" <<< toAttr

height :: forall a. ToAttr Number a => a -> Chainable
height = AttrT <<< Attribute "height" <<< toAttr

x :: forall a. ToAttr Number a => a -> Chainable
x = AttrT <<< Attribute "x" <<< toAttr

y :: forall a. ToAttr Number a => a -> Chainable
y = AttrT <<< Attribute "y" <<< toAttr

r :: forall a. ToAttr Number a => a -> Chainable
r = AttrT <<< Attribute "r" <<< toAttr

dx :: forall a. ToAttr Number a => a -> Chainable
dx = AttrT <<< Attribute "dx" <<< toAttr

dy :: forall a. ToAttr Number a => a -> Chainable
dy = AttrT <<< Attribute "dy" <<< toAttr

cx :: forall a. ToAttr Number a => a -> Chainable
cx = AttrT <<< Attribute "cx" <<< toAttr

cy :: forall a. ToAttr Number a => a -> Chainable
cy = AttrT <<< Attribute "cy" <<< toAttr

text :: forall a. ToAttr String a => a -> Chainable
text = AttrT <<< Attribute "text" <<< toAttr

classed :: forall a. ToAttr String a => a -> Chainable
classed = AttrT <<< Attribute "class" <<< toAttr


-- helpers for transitions 

defaultTransition :: Transition
defaultTransition = { name: "", delay: Milliseconds 0.0, duration: Milliseconds 0.0, easing: DefaultCubic }

-- always make this empty because the other chainable things compose at the use-point
transition :: Transition -> Chainable
transition t = TransitionT [] t

namedTransition :: String -> Chainable
namedTransition name = TransitionT [] $ defaultTransition { name = name } 

transitionWithDuration :: Milliseconds -> Chainable -- this can be mempty for monoid
transitionWithDuration duration = TransitionT [] defaultTransition { duration = duration }

with :: Chainable -> Array Chainable -> Array Chainable
with (TransitionT [] t) chain = [ TransitionT chain t ]
with (TransitionT existingChain t) newChain = [ TransitionT (existingChain <> newChain) t ]
with otherChainable chain = otherChainable:chain
