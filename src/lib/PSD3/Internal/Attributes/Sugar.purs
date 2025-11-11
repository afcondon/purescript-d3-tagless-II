module PSD3.Internal.Attributes.Sugar
  ( module PSD3.Internal.Attributes.Instances
  , module PSD3.Internal.Attributes.Sugar
  ) where

import PSD3.Internal.Attributes.Instances (class ToAttr, Attr(..), AttrBuilder(..), AttributeSetter(..), DatumFn(..), DatumFnI(..), Listener, EffectfulListener, toAttr, unwrapDatumFn)
import PSD3.Internal.Types (Datum_, EasingFunction(..), MouseEvent, Transition, PointXY)
import PSD3.Internal.FFI (autoBox_)
import PSD3.Internal.Selection.Types (SelectionAttribute(..), OrderingAttribute(..))
import Data.Array (intercalate, (:))
import Data.Function.Uncurried (mkFn3)
import Effect.Aff (Milliseconds(..))
import Effect.Uncurried (mkEffectFn3)
import Prelude (class Semigroup, class Show, append, flap, show, ($), (<$>), (<<<), (<>))
import Unsafe.Coerce (unsafeCoerce)

backgroundColor :: ∀ d a. ToAttr String a d => a -> SelectionAttribute d
backgroundColor = AttrT <<< AttributeSetter "background-color" <<< toAttr

strokeColor :: ∀ d a. ToAttr String a d => a -> SelectionAttribute d
strokeColor = AttrT <<< AttributeSetter "stroke" <<< toAttr

strokeOpacity :: ∀ d a. ToAttr Number a d => a -> SelectionAttribute d
strokeOpacity = AttrT <<< AttributeSetter "stroke-opacity" <<< toAttr

opacity :: ∀ d a. ToAttr Number a d => a -> SelectionAttribute d
opacity = AttrT <<< AttributeSetter "opacity" <<< toAttr

strokeWidth :: ∀ d a. ToAttr Number a d => a -> SelectionAttribute d
strokeWidth = AttrT <<< AttributeSetter "stroke-width" <<< toAttr

fill :: ∀ d a. ToAttr String a d => a -> SelectionAttribute d
fill = AttrT <<< AttributeSetter "fill" <<< toAttr

fillOpacity :: ∀ d a. ToAttr Number a d => a -> SelectionAttribute d
fillOpacity = AttrT <<< AttributeSetter "fill-opacity" <<< toAttr

-- TODO this definitely needs to be Number-with-unit here
viewBox :: ∀ d. Number -> Number -> Number -> Number -> SelectionAttribute d
viewBox xo yo w h = AttrT <<< AttributeSetter "viewBox" $ toAttr vb
  where
    vb = intercalate " " $ show <$> [ xo, yo, w, h ]

-- REVIEW this isn't plumbed in anywhere but it's a pattern (generating array of attrs) that could apply in multiple places potentially
-- attr for setting both x and y at the same time from a point
pointXY :: ∀ d. PointXY -> Array (SelectionAttribute d)
pointXY point =
  [ AttrT <<< AttributeSetter "x" $ toAttr point.x
  , AttrT <<< AttributeSetter "y" $ toAttr point.y ]

-- preserveAspectRatio as an attribute only applies to viewBox
-- | Aspect ratio stuff could be shared with Halogen when it is present TODO
preserveAspectRatio :: ∀ d. AspectRatioSpec -> SelectionAttribute d
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

autoBox :: ∀ d. SelectionAttribute d
autoBox = AttrT <<< AttributeSetter "viewBox" $ toAttr vb
  where
    vb :: d -> String
    vb = \d -> intercalate " " $ show <$> (autoBox_ (unsafeCoerce d))

fontFamily :: ∀ d a. ToAttr String a d => a -> SelectionAttribute d
fontFamily = AttrT <<< AttributeSetter "font-family" <<< toAttr

textAnchor :: ∀ d a. ToAttr String a d => a -> SelectionAttribute d
textAnchor = AttrT <<< AttributeSetter "text-anchor" <<< toAttr

rotate :: ∀ d a. ToAttr String a d => a -> SelectionAttribute d
rotate = AttrT <<< AttributeSetter "rotate" <<< toAttr

radius :: ∀ d a. ToAttr Number a d => a -> SelectionAttribute d
radius = AttrT <<< AttributeSetter "r" <<< toAttr

fontSize :: ∀ d a. ToAttr Number a d => a -> SelectionAttribute d
fontSize = AttrT <<< AttributeSetter "font-size" <<< toAttr

width :: ∀ d a. ToAttr Number a d => a -> SelectionAttribute d
width = AttrT <<< AttributeSetter "width" <<< toAttr

height :: ∀ d a. ToAttr Number a d => a -> SelectionAttribute d
height = AttrT <<< AttributeSetter "height" <<< toAttr

width100 :: ∀ d. SelectionAttribute d
width100 = AttrT <<< AttributeSetter "width" $ toAttr "100%"

height100 :: ∀ d. SelectionAttribute d
height100 = AttrT <<< AttributeSetter "height" $ toAttr "100%"

x :: ∀ d a. ToAttr Number a d => a -> SelectionAttribute d
x = AttrT <<< AttributeSetter "x" <<< toAttr

y :: ∀ d a. ToAttr Number a d => a -> SelectionAttribute d
y = AttrT <<< AttributeSetter "y" <<< toAttr

-- yu :: ∀ a. ToAttr NWU a => a -> SelectionAttribute
-- yu = AttrT <<< AttributeSetter "y" <<< toAttr

x1 :: ∀ d a. ToAttr Number a d => a -> SelectionAttribute d
x1 = AttrT <<< AttributeSetter "x1" <<< toAttr

y1 :: ∀ d a. ToAttr Number a d => a -> SelectionAttribute d
y1 = AttrT <<< AttributeSetter "y1" <<< toAttr

x2 :: ∀ d a. ToAttr Number a d => a -> SelectionAttribute d
x2 = AttrT <<< AttributeSetter "x2" <<< toAttr

y2 :: ∀ d a. ToAttr Number a d => a -> SelectionAttribute d
y2 = AttrT <<< AttributeSetter "y2" <<< toAttr

dx :: ∀ d a. ToAttr Number a d => a -> SelectionAttribute d
dx = AttrT <<< AttributeSetter "dx" <<< toAttr

dy :: ∀ d a. ToAttr Number a d => a -> SelectionAttribute d
dy = AttrT <<< AttributeSetter "dy" <<< toAttr

cx :: ∀ d a. ToAttr Number a d => a -> SelectionAttribute d
cx = AttrT <<< AttributeSetter "cx" <<< toAttr

cy :: ∀ d a. ToAttr Number a d => a -> SelectionAttribute d
cy = AttrT <<< AttributeSetter "cy" <<< toAttr

text :: ∀ d a. ToAttr String a d => a -> SelectionAttribute d
text = TextT <<< AttributeSetter "text" <<< toAttr

d :: ∀ d a. ToAttr String a d => a -> SelectionAttribute d
d = AttrT <<< AttributeSetter "d" <<< toAttr

-- TODO classed here has destructive semantics which D3 doesn't, because in D3 you give a Boolean to indicate whether you're adding or removing the class
classed :: ∀ d a. ToAttr String a d => a -> SelectionAttribute d
classed = AttrT <<< AttributeSetter "class" <<< toAttr

cursor :: ∀ d a. ToAttr String a d => a -> SelectionAttribute d
cursor = AttrT <<< AttributeSetter "cursor" <<< toAttr

onMouseEvent :: ∀ d. MouseEvent -> Listener -> SelectionAttribute d
onMouseEvent event listener = OnT event (mkFn3 listener)

onMouseEventEffectful :: ∀ d. MouseEvent -> EffectfulListener -> SelectionAttribute d
onMouseEventEffectful event listener = OnT' event (mkEffectFn3 listener)

-- helpers for Forces

originX :: ∀ d a. ToAttr Number a d => a -> SelectionAttribute d
originX = AttrT <<< AttributeSetter "originX" <<< toAttr

originY :: ∀ d a. ToAttr Number a d => a -> SelectionAttribute d
originY = AttrT <<< AttributeSetter "originY" <<< toAttr

strength :: ∀ d a. ToAttr Number a d => a -> SelectionAttribute d
strength = AttrT <<< AttributeSetter "strength" <<< toAttr



-- helpers for transitions 

defaultTransition :: Transition
defaultTransition = { name: "", delay: Milliseconds 0.0, duration: Milliseconds 0.0, easing: DefaultCubic }

-- always make this empty because the other chainable things compose at the use-point
transition :: ∀ d. Transition -> SelectionAttribute d
transition t = TransitionT [] t

namedTransition :: ∀ d. String -> SelectionAttribute d
namedTransition name = TransitionT [] $ defaultTransition { name = name } 

transitionWithDuration :: ∀ d. Milliseconds -> SelectionAttribute d -- this can be mempty for monoid
transitionWithDuration duration = TransitionT [] defaultTransition { duration = duration }

andThen :: forall a. Semigroup a => a -> a -> a
andThen = append

to :: ∀ d. SelectionAttribute d -> Array (SelectionAttribute d) -> Array (SelectionAttribute d)
to (TransitionT [] t) chain = [ TransitionT chain t ]
to (TransitionT existingChain t) newChain = [ TransitionT (existingChain <> newChain) t ]
to otherSelectionAttribute chain = otherSelectionAttribute:chain

remove :: ∀ d. SelectionAttribute d
remove = RemoveT


data LineJoin = Arcs | Bevel | Miter | MiterClip | Round
instance showLineJoin :: Show LineJoin where
  show Arcs      = "arcs"
  show Bevel     = "bevel"
  show Miter     = "miter"
  show MiterClip = "miter-clip"
  show Round     = "round"

strokeLineJoin :: ∀ d. LineJoin -> SelectionAttribute d
strokeLineJoin = AttrT <<< AttributeSetter "stroke-linejoin" <<< toAttr <<< show

-- helpers for transitions, a sequence of functions but expressed as text in the DOM
-- TODO don't export transform'
transform' :: ∀ d. (d -> String) -> SelectionAttribute d
transform' = AttrT <<< AttributeSetter "transform" <<< StringAttr <<< Fn

-- make a single (Datum_ -> String) function out of the array (ie sequence) of functions provided
transform :: ∀ d. Array (Datum_ -> String) -> SelectionAttribute d
transform = transform' <<< unsafeCoerce <<< assembleTransforms

-- we take a stack of (Datum_ -> String) functions and produce just one
-- we can't know here in the library code if this is safe but if the transforms themselves are written in terms of 
-- what we know the Datum_ will actually be (ie D3TreeNode for example) then we have some limited type checking
assembleTransforms :: Array (Datum_ -> String) -> (Datum_ -> String)
assembleTransforms fs = unsafeCoerce (\d -> intercalate " " $ flap fs d)


-- helpers for Ordering type attributes
lower :: ∀ d. SelectionAttribute d
lower = OrderingT Lower
raise :: ∀ d. SelectionAttribute d
raise = OrderingT Raise
order :: ∀ d. SelectionAttribute d
order = OrderingT Order
sortSelection :: ∀ d. (d -> d -> Int) -> SelectionAttribute d
sortSelection compare = OrderingT (Sort compare)