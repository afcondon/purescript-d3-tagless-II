module D3.Attributes.Sugar where

import D3.Attributes.Instances
import Prelude

import D3.Layouts.Hierarchical (autoBox_)
import D3.Selection (Chainable(..), EasingFunction(..), Transition)
import Data.Array (intercalate, (:))
import Debug (spy)
import Effect.Aff (Milliseconds(..))
import Unsafe.Coerce (unsafeCoerce)

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
viewBox xo yo w h = AttrT <<< Attribute "viewBox" $ toAttr vb
  where
    vb = spy "Viewbox: " $ intercalate " " $ show <$> [ xo, yo, w, h ]

autoBox :: Chainable
autoBox = AttrT <<< Attribute "viewBox" $ toAttr vb
  where
    vb = \d -> intercalate " " $ show <$> (autoBox_ d)

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

yu :: ∀ a. ToAttr NWU a => a -> Chainable
yu = AttrT <<< Attribute "y" <<< toAttr

x1 :: ∀ a. ToAttr Number a => a -> Chainable
x1 = AttrT <<< Attribute "x1" <<< toAttr

y1 :: ∀ a. ToAttr Number a => a -> Chainable
y1 = AttrT <<< Attribute "y1" <<< toAttr

x2 :: ∀ a. ToAttr Number a => a -> Chainable
x2 = AttrT <<< Attribute "x2" <<< toAttr

y2 :: ∀ a. ToAttr Number a => a -> Chainable
y2 = AttrT <<< Attribute "y2" <<< toAttr

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

cursor :: ∀ a. ToAttr String a => a -> Chainable
cursor = AttrT <<< Attribute "cursor" <<< toAttr


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

andThen :: forall a. Semigroup a => a -> a -> a
andThen = append

to :: Chainable -> Array Chainable -> Array Chainable
to (TransitionT [] t) chain = [ TransitionT chain t ]
to (TransitionT existingChain t) newChain = [ TransitionT (existingChain <> newChain) t ]
to otherChainable chain = otherChainable:chain

remove :: Chainable
remove = RemoveT


data LineJoin = Arcs | Bevel | Miter | MiterClip | Round
instance showLineJoin :: Show LineJoin where
  show Arcs      = "arcs"
  show Bevel     = "bevel"
  show Miter     = "miter"
  show MiterClip = "miter-clip"
  show Round     = "round"

strokeLineJoin :: LineJoin -> Chainable
strokeLineJoin = AttrT <<< Attribute "stroke-linejoin" <<< toAttr <<< show

-- helpers for transitions, a sequence of functions but expressed as text in the DOM
-- TODO don't export transform'
transform' :: (Datum -> String) -> Chainable
transform' = AttrT <<< Attribute "transform" <<< StringAttr <<< Fn

-- make a single (Datum -> String) function out of the array (ie sequence) of functions provided
transform :: forall a. Array (a -> String) -> Chainable
transform = transform' <<< assembleTransforms

-- we take a stack of (Datum -> String) functions and produce just one
-- we can't know here in the library code if this is safe but if the transforms themselves are written in terms of 
-- what we know the Datum will actually be (ie D3TreeNode for example) then we have some limited type checking
assembleTransforms :: ∀ a. Array (a -> String) -> (Datum -> String)
assembleTransforms fs = unsafeCoerce (\d -> intercalate " " $ flap fs d)
