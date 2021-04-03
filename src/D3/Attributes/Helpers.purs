module D3.Attributes.Sugar where

import Prelude

import D3.Attributes.Instances (class ToAttr, Attribute(..), toAttr)
import D3.Selection (Chainable(..), EasingFunction(..), Transition)
import Data.Array ((:))
import Effect.Aff (Milliseconds(..))

strokeColor :: ∀ a. ToAttr String a => a -> Chainable
strokeColor = AttrT <<< Attribute "stroke" <<< toAttr

strokeOpacity :: ∀ a. ToAttr Number a => a -> Chainable
strokeOpacity = AttrT <<< Attribute "stroke-opacity" <<< toAttr

strokeWidth :: ∀ a. ToAttr Number a => a -> Chainable
strokeWidth = AttrT <<< Attribute "stroke-width" <<< toAttr

fill :: ∀ a. ToAttr String a => a -> Chainable
fill = AttrT <<< Attribute "fill" <<< toAttr

-- TODO this definitely needs to be Number-with-unit here
viewBox :: Number -> Number -> Number -> Number -> Chainable
viewBox xo yo w h = AttrT <<< Attribute "viewbox" $ toAttr [ xo, yo, w, h ]

fontFamily :: ∀ a. ToAttr String a => a -> Chainable
fontFamily = AttrT <<< Attribute "font-family" <<< toAttr

textAnchor :: ∀ a. ToAttr String a => a -> Chainable
textAnchor = AttrT <<< Attribute "text-anchor" <<< toAttr

radius :: ∀ a. ToAttr Number a => a -> Chainable
radius = AttrT <<< Attribute "r" <<< toAttr

fontSize :: ∀ a. ToAttr Number a => a -> Chainable
fontSize = AttrT <<< Attribute "font-size" <<< toAttr

width :: ∀ a. ToAttr Number a => a -> Chainable
width = AttrT <<< Attribute "width" <<< toAttr

height :: ∀ a. ToAttr Number a => a -> Chainable
height = AttrT <<< Attribute "height" <<< toAttr

x :: ∀ a. ToAttr Number a => a -> Chainable
x = AttrT <<< Attribute "x" <<< toAttr

y :: ∀ a. ToAttr Number a => a -> Chainable
y = AttrT <<< Attribute "y" <<< toAttr

r :: ∀ a. ToAttr Number a => a -> Chainable
r = AttrT <<< Attribute "r" <<< toAttr

dx :: ∀ a. ToAttr Number a => a -> Chainable
dx = AttrT <<< Attribute "dx" <<< toAttr

dy :: ∀ a. ToAttr Number a => a -> Chainable
dy = AttrT <<< Attribute "dy" <<< toAttr

cx :: ∀ a. ToAttr Number a => a -> Chainable
cx = AttrT <<< Attribute "cx" <<< toAttr

cy :: ∀ a. ToAttr Number a => a -> Chainable
cy = AttrT <<< Attribute "cy" <<< toAttr

text :: ∀ a. ToAttr String a => a -> Chainable
text = TextT <<< Attribute "text" <<< toAttr

classed :: ∀ a. ToAttr String a => a -> Chainable
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

remove :: Chainable
remove = RemoveT
