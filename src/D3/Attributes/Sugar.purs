module D3.Attributes.Sugar where

import D3.Attributes.Instances (class ToAttr, Attr(..), AttrBuilder(..), Attribute(..), Listener, NWU, toAttr)
import Prelude (class Semigroup, class Show, append, bind, flap, pure, show, ($), (<$>), (<<<), (<>))
import D3.Data.Types (Datum_, EasingFunction(..), MouseEvent, Transition)
import D3.FFI (autoBox_)
import D3.Selection (Chainable(..))
import Data.Array (intercalate, (:))
import Data.Function.Uncurried (mkFn3)
import Data.Int (toNumber)
import Data.Tuple (Tuple(..))
import Debug (spy)
import Effect (Effect)
import Effect.Aff (Milliseconds(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML (window)
import Web.HTML.Window (innerHeight, innerWidth)

backgroundColor :: ∀ a. ToAttr String a => a -> Chainable
backgroundColor = AttrT <<< ToAttribute "background-color" <<< toAttr

strokeColor :: ∀ a. ToAttr String a => a -> Chainable
strokeColor = AttrT <<< ToAttribute "stroke" <<< toAttr

strokeOpacity :: ∀ a. ToAttr Number a => a -> Chainable
strokeOpacity = AttrT <<< ToAttribute "stroke-opacity" <<< toAttr

opacity :: ∀ a. ToAttr Number a => a -> Chainable
opacity = AttrT <<< ToAttribute "opacity" <<< toAttr

strokeWidth :: ∀ a. ToAttr Number a => a -> Chainable
strokeWidth = AttrT <<< ToAttribute "stroke-width" <<< toAttr

fill :: ∀ a. ToAttr String a => a -> Chainable
fill = AttrT <<< ToAttribute "fill" <<< toAttr

-- TODO this definitely needs to be Number-with-unit here
viewBox :: Number -> Number -> Number -> Number -> Chainable
viewBox xo yo w h = AttrT <<< ToAttribute "viewBox" $ toAttr vb
  where
    vb = intercalate " " $ show <$> [ xo, yo, w, h ]

autoBox :: Chainable
autoBox = AttrT <<< ToAttribute "viewBox" $ toAttr vb
  where
    vb = \d -> intercalate " " $ show <$> (autoBox_ d)

fontFamily :: ∀ a. ToAttr String a => a -> Chainable
fontFamily = AttrT <<< ToAttribute "font-family" <<< toAttr

textAnchor :: ∀ a. ToAttr String a => a -> Chainable
textAnchor = AttrT <<< ToAttribute "text-anchor" <<< toAttr

radius :: ∀ a. ToAttr Number a => a -> Chainable
radius = AttrT <<< ToAttribute "r" <<< toAttr

fontSize :: ∀ a. ToAttr Number a => a -> Chainable
fontSize = AttrT <<< ToAttribute "font-size" <<< toAttr

width :: ∀ a. ToAttr Number a => a -> Chainable
width = AttrT <<< ToAttribute "width" <<< toAttr

height :: ∀ a. ToAttr Number a => a -> Chainable
height = AttrT <<< ToAttribute "height" <<< toAttr

width100 :: Chainable
width100 = AttrT <<< ToAttribute "width" $ toAttr "100%"

height100 :: Chainable
height100 = AttrT <<< ToAttribute "height" $ toAttr "100%"

x :: ∀ a. ToAttr Number a => a -> Chainable
x = AttrT <<< ToAttribute "x" <<< toAttr

y :: ∀ a. ToAttr Number a => a -> Chainable
y = AttrT <<< ToAttribute "y" <<< toAttr

yu :: ∀ a. ToAttr NWU a => a -> Chainable
yu = AttrT <<< ToAttribute "y" <<< toAttr

x1 :: ∀ a. ToAttr Number a => a -> Chainable
x1 = AttrT <<< ToAttribute "x1" <<< toAttr

y1 :: ∀ a. ToAttr Number a => a -> Chainable
y1 = AttrT <<< ToAttribute "y1" <<< toAttr

x2 :: ∀ a. ToAttr Number a => a -> Chainable
x2 = AttrT <<< ToAttribute "x2" <<< toAttr

y2 :: ∀ a. ToAttr Number a => a -> Chainable
y2 = AttrT <<< ToAttribute "y2" <<< toAttr

dx :: ∀ a. ToAttr Number a => a -> Chainable
dx = AttrT <<< ToAttribute "dx" <<< toAttr

dy :: ∀ a. ToAttr Number a => a -> Chainable
dy = AttrT <<< ToAttribute "dy" <<< toAttr

cx :: ∀ a. ToAttr Number a => a -> Chainable
cx = AttrT <<< ToAttribute "cx" <<< toAttr

cy :: ∀ a. ToAttr Number a => a -> Chainable
cy = AttrT <<< ToAttribute "cy" <<< toAttr

text :: ∀ a. ToAttr String a => a -> Chainable
text = TextT <<< ToAttribute "text" <<< toAttr

classed :: ∀ a. ToAttr String a => a -> Chainable
classed = AttrT <<< ToAttribute "class" <<< toAttr

cursor :: ∀ a. ToAttr String a => a -> Chainable
cursor = AttrT <<< ToAttribute "cursor" <<< toAttr

onMouseEvent :: MouseEvent -> Listener -> Chainable
onMouseEvent event listener = OnT event (mkFn3 listener)

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
strokeLineJoin = AttrT <<< ToAttribute "stroke-linejoin" <<< toAttr <<< show

-- helpers for transitions, a sequence of functions but expressed as text in the DOM
-- TODO don't export transform'
transform' :: (Datum_ -> String) -> Chainable
transform' = AttrT <<< ToAttribute "transform" <<< StringAttr <<< Fn

-- make a single (Datum_ -> String) function out of the array (ie sequence) of functions provided
transform :: forall a. Array (a -> String) -> Chainable
transform = transform' <<< assembleTransforms

-- we take a stack of (Datum_ -> String) functions and produce just one
-- we can't know here in the library code if this is safe but if the transforms themselves are written in terms of 
-- what we know the Datum_ will actually be (ie D3TreeNode for example) then we have some limited type checking
assembleTransforms :: ∀ a. Array (a -> String) -> (Datum_ -> String)
assembleTransforms fs = unsafeCoerce (\d -> intercalate " " $ flap fs d)
