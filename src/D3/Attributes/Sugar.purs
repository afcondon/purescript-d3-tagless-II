module D3.Attributes.Sugar where

import D3.Attributes.Instances (class ToAttr, Attr(..), AttrBuilder(..), AttributeSetter(..), Listener, EffectfulListener, toAttr)
import D3.Data.Types (Datum_, EasingFunction(..), MouseEvent, Transition, PointXY)
import D3.FFI (autoBox_)
import D3.Selection (SelectionAttribute(..), OrderingAttribute(..))
import Data.Array (intercalate, (:))
import Data.Function.Uncurried (mkFn3)
import Effect.Aff (Milliseconds(..))
import Effect.Uncurried (mkEffectFn3)
import Prelude (class Semigroup, class Show, append, flap, show, ($), (<$>), (<<<), (<>))
import Unsafe.Coerce (unsafeCoerce)

backgroundColor :: ∀ a. ToAttr String a => a -> SelectionAttribute
backgroundColor = AttrT <<< AttributeSetter "background-color" <<< toAttr

strokeColor :: ∀ a. ToAttr String a => a -> SelectionAttribute
strokeColor = AttrT <<< AttributeSetter "stroke" <<< toAttr

strokeOpacity :: ∀ a. ToAttr Number a => a -> SelectionAttribute
strokeOpacity = AttrT <<< AttributeSetter "stroke-opacity" <<< toAttr

opacity :: ∀ a. ToAttr Number a => a -> SelectionAttribute
opacity = AttrT <<< AttributeSetter "opacity" <<< toAttr

strokeWidth :: ∀ a. ToAttr Number a => a -> SelectionAttribute
strokeWidth = AttrT <<< AttributeSetter "stroke-width" <<< toAttr

fill :: ∀ a. ToAttr String a => a -> SelectionAttribute
fill = AttrT <<< AttributeSetter "fill" <<< toAttr

-- TODO this definitely needs to be Number-with-unit here
viewBox :: Number -> Number -> Number -> Number -> SelectionAttribute
viewBox xo yo w h = AttrT <<< AttributeSetter "viewBox" $ toAttr vb
  where
    vb = intercalate " " $ show <$> [ xo, yo, w, h ]

-- REVIEW this isn't plumbed in anywhere but it's a pattern (generating array of attrs) that could apply in multiple places potentially
-- attr for setting both x and y at the same time from a point
pointXY :: PointXY -> Array SelectionAttribute
pointXY point = 
  [ AttrT <<< AttributeSetter "x" $ toAttr point.x
  , AttrT <<< AttributeSetter "y" $ toAttr point.y ]

-- preserveAspectRatio as an attribute only applies to viewBox
preserveAspectRatio :: AspectRatioSpec -> SelectionAttribute
preserveAspectRatio = AttrT <<< AttributeSetter "preserveAspectRatio" <<< toAttr <<< show

data AlignAspectRatio_X = XMin | XMid | XMax
instance Show AlignAspectRatio_X where
  show XMin = "xMin"
  show XMid = "xMid"
  show XMax = "xMax"
data AlignAspectRatio_Y = YMin | YMid | YMax
instance Show AlignAspectRatio_Y where -- YES!!! the Y is capitalized, where the x is not!!!!
  show YMin = "YMin"
  show YMid = "YMid"
  show YMax = "YMax"
data AspectRatioPreserve = Meet | Slice | None
instance Show AspectRatioPreserve where
  show Meet  = "meet"
  show Slice = "slice"
  show None  = "none"
data AspectRatioSpec = AspectRatio AlignAspectRatio_X AlignAspectRatio_Y AspectRatioPreserve

instance Show AspectRatioSpec where
  show (AspectRatio x y None) =  "none"
  show (AspectRatio x y p)    =  show x <> show y <> " " <> show p

autoBox :: SelectionAttribute
autoBox = AttrT <<< AttributeSetter "viewBox" $ toAttr vb
  where
    vb = \d -> intercalate " " $ show <$> (autoBox_ d)

fontFamily :: ∀ a. ToAttr String a => a -> SelectionAttribute
fontFamily = AttrT <<< AttributeSetter "font-family" <<< toAttr

textAnchor :: ∀ a. ToAttr String a => a -> SelectionAttribute
textAnchor = AttrT <<< AttributeSetter "text-anchor" <<< toAttr

radius :: ∀ a. ToAttr Number a => a -> SelectionAttribute
radius = AttrT <<< AttributeSetter "r" <<< toAttr

fontSize :: ∀ a. ToAttr Number a => a -> SelectionAttribute
fontSize = AttrT <<< AttributeSetter "font-size" <<< toAttr

width :: ∀ a. ToAttr Number a => a -> SelectionAttribute
width = AttrT <<< AttributeSetter "width" <<< toAttr

height :: ∀ a. ToAttr Number a => a -> SelectionAttribute
height = AttrT <<< AttributeSetter "height" <<< toAttr

width100 :: SelectionAttribute
width100 = AttrT <<< AttributeSetter "width" $ toAttr "100%"

height100 :: SelectionAttribute
height100 = AttrT <<< AttributeSetter "height" $ toAttr "100%"

x :: ∀ a. ToAttr Number a => a -> SelectionAttribute
x = AttrT <<< AttributeSetter "x" <<< toAttr

y :: ∀ a. ToAttr Number a => a -> SelectionAttribute
y = AttrT <<< AttributeSetter "y" <<< toAttr

-- yu :: ∀ a. ToAttr NWU a => a -> SelectionAttribute
-- yu = AttrT <<< AttributeSetter "y" <<< toAttr

x1 :: ∀ a. ToAttr Number a => a -> SelectionAttribute
x1 = AttrT <<< AttributeSetter "x1" <<< toAttr

y1 :: ∀ a. ToAttr Number a => a -> SelectionAttribute
y1 = AttrT <<< AttributeSetter "y1" <<< toAttr

x2 :: ∀ a. ToAttr Number a => a -> SelectionAttribute
x2 = AttrT <<< AttributeSetter "x2" <<< toAttr

y2 :: ∀ a. ToAttr Number a => a -> SelectionAttribute
y2 = AttrT <<< AttributeSetter "y2" <<< toAttr

dx :: ∀ a. ToAttr Number a => a -> SelectionAttribute
dx = AttrT <<< AttributeSetter "dx" <<< toAttr

dy :: ∀ a. ToAttr Number a => a -> SelectionAttribute
dy = AttrT <<< AttributeSetter "dy" <<< toAttr

cx :: ∀ a. ToAttr Number a => a -> SelectionAttribute
cx = AttrT <<< AttributeSetter "cx" <<< toAttr

cy :: ∀ a. ToAttr Number a => a -> SelectionAttribute
cy = AttrT <<< AttributeSetter "cy" <<< toAttr

text :: ∀ a. ToAttr String a => a -> SelectionAttribute
text = TextT <<< AttributeSetter "text" <<< toAttr

-- TODO classed here has destructive semantics which D3 doesn't, because in D3 you give a Boolean to indicate whether you're adding or removing the class
classed :: ∀ a. ToAttr String a => a -> SelectionAttribute
classed = AttrT <<< AttributeSetter "class" <<< toAttr

cursor :: ∀ a. ToAttr String a => a -> SelectionAttribute
cursor = AttrT <<< AttributeSetter "cursor" <<< toAttr

onMouseEvent :: MouseEvent -> Listener -> SelectionAttribute
onMouseEvent event listener = OnT event (mkFn3 listener)

onMouseEventEffectful :: MouseEvent -> EffectfulListener -> SelectionAttribute
onMouseEventEffectful event listener = OnT' event (mkEffectFn3 listener)

-- helpers for Forces

originX :: ∀ a. ToAttr Number a => a -> SelectionAttribute
originX = AttrT <<< AttributeSetter "originX" <<< toAttr

originY :: ∀ a. ToAttr Number a => a -> SelectionAttribute
originY = AttrT <<< AttributeSetter "originY" <<< toAttr

strength :: ∀ a. ToAttr Number a => a -> SelectionAttribute
strength = AttrT <<< AttributeSetter "strength" <<< toAttr



-- helpers for transitions 

defaultTransition :: Transition
defaultTransition = { name: "", delay: Milliseconds 0.0, duration: Milliseconds 0.0, easing: DefaultCubic }

-- always make this empty because the other chainable things compose at the use-point
transition :: Transition -> SelectionAttribute
transition t = TransitionT [] t

namedTransition :: String -> SelectionAttribute
namedTransition name = TransitionT [] $ defaultTransition { name = name } 

transitionWithDuration :: Milliseconds -> SelectionAttribute -- this can be mempty for monoid
transitionWithDuration duration = TransitionT [] defaultTransition { duration = duration }

andThen :: forall a. Semigroup a => a -> a -> a
andThen = append

to :: SelectionAttribute -> Array SelectionAttribute -> Array SelectionAttribute
to (TransitionT [] t) chain = [ TransitionT chain t ]
to (TransitionT existingChain t) newChain = [ TransitionT (existingChain <> newChain) t ]
to otherSelectionAttribute chain = otherSelectionAttribute:chain

remove :: SelectionAttribute
remove = RemoveT


data LineJoin = Arcs | Bevel | Miter | MiterClip | Round
instance showLineJoin :: Show LineJoin where
  show Arcs      = "arcs"
  show Bevel     = "bevel"
  show Miter     = "miter"
  show MiterClip = "miter-clip"
  show Round     = "round"

strokeLineJoin :: LineJoin -> SelectionAttribute
strokeLineJoin = AttrT <<< AttributeSetter "stroke-linejoin" <<< toAttr <<< show

-- helpers for transitions, a sequence of functions but expressed as text in the DOM
-- TODO don't export transform'
transform' :: (Datum_ -> String) -> SelectionAttribute
transform' = AttrT <<< AttributeSetter "transform" <<< StringAttr <<< Fn

-- make a single (Datum_ -> String) function out of the array (ie sequence) of functions provided
transform :: Array (Datum_ -> String) -> SelectionAttribute
transform = transform' <<< assembleTransforms

-- we take a stack of (Datum_ -> String) functions and produce just one
-- we can't know here in the library code if this is safe but if the transforms themselves are written in terms of 
-- what we know the Datum_ will actually be (ie D3TreeNode for example) then we have some limited type checking
assembleTransforms :: Array (Datum_ -> String) -> (Datum_ -> String)
assembleTransforms fs = unsafeCoerce (\d -> intercalate " " $ flap fs d)


-- helpers for Ordering type attributes
lower :: SelectionAttribute
lower = OrderingT Lower
raise :: SelectionAttribute
raise = OrderingT Raise
order :: SelectionAttribute
order = OrderingT Order
sortSelection :: (Datum_ -> Datum_ -> Int) -> SelectionAttribute
sortSelection compare = OrderingT (Sort compare)