module D3.Attributes.Sugar where

import D3.Attributes.Instances (class ToAttr, Attr(..), AttrBuilder(..), Attribute(..), Listener, NWU, toAttr)
import D3.Data.Types (Datum_, EasingFunction(..), MouseEvent, Transition)
import D3.FFI (autoBox_)
import D3.Selection (ChainableS(..), OrderingAttribute(..))
import Data.Array (intercalate, (:))
import Data.Function.Uncurried (mkFn3)
import Data.Int (toNumber)
import Data.Tuple (Tuple(..))
import Debug (spy)
import Effect (Effect)
import Effect.Aff (Milliseconds(..))
import Prelude (class Semigroup, class Show, append, bind, flap, pure, show, ($), (<$>), (<<<), (<>))
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML (window)
import Web.HTML.Window (innerHeight, innerWidth)

backgroundColor :: ∀ a. ToAttr String a => a -> ChainableS
backgroundColor = AttrT <<< ToAttribute "background-color" <<< toAttr

strokeColor :: ∀ a. ToAttr String a => a -> ChainableS
strokeColor = AttrT <<< ToAttribute "stroke" <<< toAttr

strokeOpacity :: ∀ a. ToAttr Number a => a -> ChainableS
strokeOpacity = AttrT <<< ToAttribute "stroke-opacity" <<< toAttr

opacity :: ∀ a. ToAttr Number a => a -> ChainableS
opacity = AttrT <<< ToAttribute "opacity" <<< toAttr

strokeWidth :: ∀ a. ToAttr Number a => a -> ChainableS
strokeWidth = AttrT <<< ToAttribute "stroke-width" <<< toAttr

fill :: ∀ a. ToAttr String a => a -> ChainableS
fill = AttrT <<< ToAttribute "fill" <<< toAttr

-- TODO this definitely needs to be Number-with-unit here
viewBox :: Number -> Number -> Number -> Number -> ChainableS
viewBox xo yo w h = AttrT <<< ToAttribute "viewBox" $ toAttr vb
  where
    vb = intercalate " " $ show <$> [ xo, yo, w, h ]

autoBox :: ChainableS
autoBox = AttrT <<< ToAttribute "viewBox" $ toAttr vb
  where
    vb = \d -> intercalate " " $ show <$> (autoBox_ d)

fontFamily :: ∀ a. ToAttr String a => a -> ChainableS
fontFamily = AttrT <<< ToAttribute "font-family" <<< toAttr

textAnchor :: ∀ a. ToAttr String a => a -> ChainableS
textAnchor = AttrT <<< ToAttribute "text-anchor" <<< toAttr

radius :: ∀ a. ToAttr Number a => a -> ChainableS
radius = AttrT <<< ToAttribute "r" <<< toAttr

fontSize :: ∀ a. ToAttr Number a => a -> ChainableS
fontSize = AttrT <<< ToAttribute "font-size" <<< toAttr

width :: ∀ a. ToAttr Number a => a -> ChainableS
width = AttrT <<< ToAttribute "width" <<< toAttr

height :: ∀ a. ToAttr Number a => a -> ChainableS
height = AttrT <<< ToAttribute "height" <<< toAttr

width100 :: ChainableS
width100 = AttrT <<< ToAttribute "width" $ toAttr "100%"

height100 :: ChainableS
height100 = AttrT <<< ToAttribute "height" $ toAttr "100%"

x :: ∀ a. ToAttr Number a => a -> ChainableS
x = AttrT <<< ToAttribute "x" <<< toAttr

y :: ∀ a. ToAttr Number a => a -> ChainableS
y = AttrT <<< ToAttribute "y" <<< toAttr

yu :: ∀ a. ToAttr NWU a => a -> ChainableS
yu = AttrT <<< ToAttribute "y" <<< toAttr

x1 :: ∀ a. ToAttr Number a => a -> ChainableS
x1 = AttrT <<< ToAttribute "x1" <<< toAttr

y1 :: ∀ a. ToAttr Number a => a -> ChainableS
y1 = AttrT <<< ToAttribute "y1" <<< toAttr

x2 :: ∀ a. ToAttr Number a => a -> ChainableS
x2 = AttrT <<< ToAttribute "x2" <<< toAttr

y2 :: ∀ a. ToAttr Number a => a -> ChainableS
y2 = AttrT <<< ToAttribute "y2" <<< toAttr

dx :: ∀ a. ToAttr Number a => a -> ChainableS
dx = AttrT <<< ToAttribute "dx" <<< toAttr

dy :: ∀ a. ToAttr Number a => a -> ChainableS
dy = AttrT <<< ToAttribute "dy" <<< toAttr

cx :: ∀ a. ToAttr Number a => a -> ChainableS
cx = AttrT <<< ToAttribute "cx" <<< toAttr

cy :: ∀ a. ToAttr Number a => a -> ChainableS
cy = AttrT <<< ToAttribute "cy" <<< toAttr

text :: ∀ a. ToAttr String a => a -> ChainableS
text = TextT <<< ToAttribute "text" <<< toAttr

-- TODO classed here has destructive semantics which D3 doesn't, because in D3 you give a Boolean to indicate whether you're adding or removing the class
classed :: ∀ a. ToAttr String a => a -> ChainableS
classed = AttrT <<< ToAttribute "class" <<< toAttr

cursor :: ∀ a. ToAttr String a => a -> ChainableS
cursor = AttrT <<< ToAttribute "cursor" <<< toAttr

onMouseEvent :: MouseEvent -> Listener -> ChainableS
onMouseEvent event listener = OnT event (mkFn3 listener)

-- helpers for Forces

originX :: ∀ a. ToAttr Number a => a -> ChainableS
originX = AttrT <<< ToAttribute "originX" <<< toAttr

originY :: ∀ a. ToAttr Number a => a -> ChainableS
originY = AttrT <<< ToAttribute "originY" <<< toAttr

strength :: ∀ a. ToAttr Number a => a -> ChainableS
strength = AttrT <<< ToAttribute "strength" <<< toAttr



-- helpers for transitions 

defaultTransition :: Transition
defaultTransition = { name: "", delay: Milliseconds 0.0, duration: Milliseconds 0.0, easing: DefaultCubic }

-- always make this empty because the other chainable things compose at the use-point
transition :: Transition -> ChainableS
transition t = TransitionT [] t

namedTransition :: String -> ChainableS
namedTransition name = TransitionT [] $ defaultTransition { name = name } 

transitionWithDuration :: Milliseconds -> ChainableS -- this can be mempty for monoid
transitionWithDuration duration = TransitionT [] defaultTransition { duration = duration }

andThen :: forall a. Semigroup a => a -> a -> a
andThen = append

to :: ChainableS -> Array ChainableS -> Array ChainableS
to (TransitionT [] t) chain = [ TransitionT chain t ]
to (TransitionT existingChain t) newChain = [ TransitionT (existingChain <> newChain) t ]
to otherChainableS chain = otherChainableS:chain

remove :: ChainableS
remove = RemoveT


data LineJoin = Arcs | Bevel | Miter | MiterClip | Round
instance showLineJoin :: Show LineJoin where
  show Arcs      = "arcs"
  show Bevel     = "bevel"
  show Miter     = "miter"
  show MiterClip = "miter-clip"
  show Round     = "round"

strokeLineJoin :: LineJoin -> ChainableS
strokeLineJoin = AttrT <<< ToAttribute "stroke-linejoin" <<< toAttr <<< show

-- helpers for transitions, a sequence of functions but expressed as text in the DOM
-- TODO don't export transform'
transform' :: (Datum_ -> String) -> ChainableS
transform' = AttrT <<< ToAttribute "transform" <<< StringAttr <<< Fn

-- make a single (Datum_ -> String) function out of the array (ie sequence) of functions provided
transform :: Array (Datum_ -> String) -> ChainableS
transform = transform' <<< assembleTransforms

-- we take a stack of (Datum_ -> String) functions and produce just one
-- we can't know here in the library code if this is safe but if the transforms themselves are written in terms of 
-- what we know the Datum_ will actually be (ie D3TreeNode for example) then we have some limited type checking
assembleTransforms :: Array (Datum_ -> String) -> (Datum_ -> String)
assembleTransforms fs = unsafeCoerce (\d -> intercalate " " $ flap fs d)


-- helpers for Ordering type attributes
lower :: ChainableS
lower = OrderingT Lower
raise :: ChainableS
raise = OrderingT Raise
order :: ChainableS
order = OrderingT Order
sortSelection :: (Datum_ -> Datum_ -> Int) -> ChainableS
sortSelection compare = OrderingT (Sort compare)