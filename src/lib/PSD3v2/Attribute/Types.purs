module PSD3v2.Attribute.Types
  ( Attribute(..)
  , AttributeName(..)
  , AttributeValue(..)
  , class ToAttr
  , toAttr
  -- Smart constructors for common attributes
  , fill
  , fillOpacity
  , stroke
  , strokeWidth
  , strokeOpacity
  , opacity
  , cx
  , cy
  , radius
  , x
  , y
  , width
  , height
  , d
  , transform
  , class_
  , id_
  , viewBox
  , fontSize
  , fontFamily
  , textAnchor
  , dy
  , dx
  , textContent
  ) where

import Prelude

-- | Type-safe attribute with datum phantom type
-- |
-- | Attributes can be:
-- | - Static: Same value for all elements
-- | - Data-driven: Value computed from datum
-- | - Indexed: Value computed from datum and index
-- |
-- | The phantom type `datum` ensures attributes are only applied
-- | to selections with matching data types.
data Attribute datum
  = StaticAttr AttributeName AttributeValue
  | DataAttr AttributeName (datum -> AttributeValue)
  | IndexedAttr AttributeName (datum -> Int -> AttributeValue)

-- We can't derive Show for function types, but we can show the structure
instance Show (Attribute datum) where
  show (StaticAttr name val) = "(StaticAttr " <> show name <> " " <> show val <> ")"
  show (DataAttr name _) = "(DataAttr " <> show name <> " <function>)"
  show (IndexedAttr name _) = "(IndexedAttr " <> show name <> " <function>)"

-- | Attribute names (SVG/HTML properties)
-- |
-- | We use a newtype to prevent typos and enable IDE autocomplete.
-- | The String inside is the actual DOM attribute name.
newtype AttributeName = AttributeName String

derive instance Eq AttributeName
derive instance Ord AttributeName
derive newtype instance Show AttributeName

-- | Attribute values
-- |
-- | We support the most common value types.
-- | The ADT ensures type safety when setting attributes.
data AttributeValue
  = StringValue String
  | NumberValue Number
  | BooleanValue Boolean

derive instance Eq AttributeValue
derive instance Ord AttributeValue

instance Show AttributeValue where
  show (StringValue s) = "StringValue " <> show s
  show (NumberValue n) = "NumberValue " <> show n
  show (BooleanValue b) = "BooleanValue " <> show b

-- ============================================================================
-- ToAttr Type Class - Enables Polymorphic Attribute Setters
-- ============================================================================

-- | Type class for converting various input types to Attribute
-- |
-- | This enables elegant polymorphic attribute setters inspired by Ian Ross' design:
-- | - `fill "red"` - static value
-- | - `fill (\d -> d.color)` - datum-driven
-- | - `fill (\d i -> if i == 0 then "red" else "blue")` - indexed
-- |
-- | The functional dependency `from -> to` allows the compiler to infer
-- | which instance to use based on the input type.
class ToAttr :: Type -> Type -> Type -> Constraint
class ToAttr to from datum | from -> to where
  toAttr :: from -> AttributeName -> Attribute datum

-- ============================================================================
-- ToAttr Instances for String
-- ============================================================================

instance ToAttr String String datum where
  toAttr value name = StaticAttr name (StringValue value)

instance ToAttr String (datum -> String) datum where
  toAttr fn name = DataAttr name (StringValue <<< fn)

instance ToAttr String (datum -> Int -> String) datum where
  toAttr fn name = IndexedAttr name (\d i -> StringValue (fn d i))

-- ============================================================================
-- ToAttr Instances for Number
-- ============================================================================

instance ToAttr Number Number datum where
  toAttr value name = StaticAttr name (NumberValue value)

instance ToAttr Number (datum -> Number) datum where
  toAttr fn name = DataAttr name (NumberValue <<< fn)

instance ToAttr Number (datum -> Int -> Number) datum where
  toAttr fn name = IndexedAttr name (\d i -> NumberValue (fn d i))

-- ============================================================================
-- ToAttr Instances for Boolean
-- ============================================================================

instance ToAttr Boolean Boolean datum where
  toAttr value name = StaticAttr name (BooleanValue value)

instance ToAttr Boolean (datum -> Boolean) datum where
  toAttr fn name = DataAttr name (BooleanValue <<< fn)

instance ToAttr Boolean (datum -> Int -> Boolean) datum where
  toAttr fn name = IndexedAttr name (\d i -> BooleanValue (fn d i))

-- ============================================================================
-- Smart Constructors - Now Polymorphic!
-- ============================================================================

-- | Fill color attribute
-- |
-- | Supports:
-- | - Static: `fill "red"`
-- | - Datum-driven: `fill (\d -> d.color)`
-- | - Indexed: `fill (\d i -> if i == 0 then "red" else "blue")`
fill :: forall datum a. ToAttr String a datum => a -> Attribute datum
fill value = toAttr value (AttributeName "fill")

-- | Fill opacity attribute (0.0 to 1.0)
-- |
-- | Controls the transparency of the fill color.
-- | 0.0 is fully transparent, 1.0 is fully opaque.
fillOpacity :: forall datum a. ToAttr Number a datum => a -> Attribute datum
fillOpacity value = toAttr value (AttributeName "fill-opacity")

-- | Stroke color attribute
stroke :: forall datum a. ToAttr String a datum => a -> Attribute datum
stroke value = toAttr value (AttributeName "stroke")

-- | Stroke width attribute
strokeWidth :: forall datum a. ToAttr Number a datum => a -> Attribute datum
strokeWidth value = toAttr value (AttributeName "stroke-width")

-- | Stroke opacity attribute
strokeOpacity :: forall datum a. ToAttr Number a datum => a -> Attribute datum
strokeOpacity value = toAttr value (AttributeName "stroke-opacity")

-- | Opacity attribute
opacity :: forall datum a. ToAttr Number a datum => a -> Attribute datum
opacity value = toAttr value (AttributeName "opacity")

-- | X center coordinate
-- |
-- | Supports:
-- | - Static: `cx 50.0`
-- | - Datum-driven: `cx (\d -> d.x)`
-- | - Indexed: `cx (\d i -> toNumber i * 100.0)`
cx :: forall datum a. ToAttr Number a datum => a -> Attribute datum
cx value = toAttr value (AttributeName "cx")

-- | Y center coordinate
cy :: forall datum a. ToAttr Number a datum => a -> Attribute datum
cy value = toAttr value (AttributeName "cy")

-- | Radius attribute
-- |
-- | Supports:
-- | - Static: `radius 10.0`
-- | - Datum-driven: `radius (\d -> d.size)`
-- | - Indexed: `radius (\d i -> toNumber i * 5.0)`
radius :: forall datum a. ToAttr Number a datum => a -> Attribute datum
radius value = toAttr value (AttributeName "r")

-- | X position
x :: forall datum a. ToAttr Number a datum => a -> Attribute datum
x value = toAttr value (AttributeName "x")

-- | Y position
y :: forall datum a. ToAttr Number a datum => a -> Attribute datum
y value = toAttr value (AttributeName "y")

-- | Width attribute
width :: forall datum a. ToAttr Number a datum => a -> Attribute datum
width value = toAttr value (AttributeName "width")

-- | Height attribute
height :: forall datum a. ToAttr Number a datum => a -> Attribute datum
height value = toAttr value (AttributeName "height")

-- | SVG path data
d :: forall datum a. ToAttr String a datum => a -> Attribute datum
d value = toAttr value (AttributeName "d")

-- | Transform attribute
transform :: forall datum a. ToAttr String a datum => a -> Attribute datum
transform value = toAttr value (AttributeName "transform")

-- | CSS class attribute
class_ :: forall datum a. ToAttr String a datum => a -> Attribute datum
class_ value = toAttr value (AttributeName "class")

-- | ID attribute
id_ :: forall datum a. ToAttr String a datum => a -> Attribute datum
id_ value = toAttr value (AttributeName "id")

-- | SVG viewBox attribute (e.g., "0 0 400 150")
viewBox :: forall datum a. ToAttr String a datum => a -> Attribute datum
viewBox value = toAttr value (AttributeName "viewBox")

-- | Font size
fontSize :: forall datum a. ToAttr Number a datum => a -> Attribute datum
fontSize value = toAttr value (AttributeName "font-size")

-- | Font family
fontFamily :: forall datum a. ToAttr String a datum => a -> Attribute datum
fontFamily value = toAttr value (AttributeName "font-family")

-- | Text anchor (start, middle, end)
textAnchor :: forall datum a. ToAttr String a datum => a -> Attribute datum
textAnchor value = toAttr value (AttributeName "text-anchor")

-- | Y offset for text
dy :: forall datum a. ToAttr Number a datum => a -> Attribute datum
dy value = toAttr value (AttributeName "dy")

-- | X offset for text
dx :: forall datum a. ToAttr Number a datum => a -> Attribute datum
dx value = toAttr value (AttributeName "dx")

-- | Text content
-- |
-- | Sets the text content of an element (typically used with Text elements).
-- | Uses the special attribute name "textContent" which will be handled
-- | by the interpreter to set element.textContent rather than an attribute.
textContent :: forall datum a. ToAttr String a datum => a -> Attribute datum
textContent value = toAttr value (AttributeName "textContent")
