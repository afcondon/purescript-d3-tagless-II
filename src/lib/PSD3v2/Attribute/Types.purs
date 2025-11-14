module PSD3v2.Attribute.Types
  ( Attribute(..)
  , AttributeName(..)
  , AttributeValue(..)
  , toAttributeName
  , class ToAttributeValue
  , toAttributeValue
  -- Smart constructors for common attributes
  , fill
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
  , text
  , fontSize
  , fontFamily
  , textAnchor
  , dy
  , dx
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Int (toNumber)
import Data.Show.Generic (genericShow)

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

toAttributeName :: String -> AttributeName
toAttributeName = AttributeName

-- | Attribute values
-- |
-- | We support the most common value types.
-- | The ADT ensures type safety when setting attributes.
data AttributeValue
  = StringValue String
  | NumberValue Number
  | BooleanValue Boolean

derive instance Eq AttributeValue
derive instance Generic AttributeValue _
instance Show AttributeValue where
  show = genericShow

-- | Convert common types to AttributeValue
class ToAttributeValue a where
  toAttributeValue :: a -> AttributeValue

instance ToAttributeValue String where
  toAttributeValue = StringValue

instance ToAttributeValue Number where
  toAttributeValue = NumberValue

instance ToAttributeValue Int where
  toAttributeValue = NumberValue <<< toNumber

instance ToAttributeValue Boolean where
  toAttributeValue = BooleanValue

-- ============================================================================
-- Smart Constructors for Common Attributes
-- ============================================================================
-- These provide a convenient, type-safe API for users.
-- Examples:
--   fill "red"                    -- Static color
--   cx (\d -> d.x)                -- Data-driven position
--   cy (\d i -> d.y + toNumber i) -- Indexed position

-- Colors and opacity
fill :: forall datum. String -> Attribute datum
fill color = StaticAttr (AttributeName "fill") (StringValue color)

stroke :: forall datum. String -> Attribute datum
stroke color = StaticAttr (AttributeName "stroke") (StringValue color)

strokeWidth :: forall datum. Number -> Attribute datum
strokeWidth w = StaticAttr (AttributeName "stroke-width") (NumberValue w)

strokeOpacity :: forall datum. Number -> Attribute datum
strokeOpacity o = StaticAttr (AttributeName "stroke-opacity") (NumberValue o)

opacity :: forall datum. Number -> Attribute datum
opacity o = StaticAttr (AttributeName "opacity") (NumberValue o)

-- Positions and dimensions
cx :: forall datum. (datum -> Number) -> Attribute datum
cx f = DataAttr (AttributeName "cx") (NumberValue <<< f)

cy :: forall datum. (datum -> Number) -> Attribute datum
cy f = DataAttr (AttributeName "cy") (NumberValue <<< f)

radius :: forall datum. Number -> Attribute datum
radius r = StaticAttr (AttributeName "r") (NumberValue r)

x :: forall datum. (datum -> Number) -> Attribute datum
x f = DataAttr (AttributeName "x") (NumberValue <<< f)

y :: forall datum. (datum -> Number) -> Attribute datum
y f = DataAttr (AttributeName "y") (NumberValue <<< f)

width :: forall datum. Number -> Attribute datum
width w = StaticAttr (AttributeName "width") (NumberValue w)

height :: forall datum. Number -> Attribute datum
height h = StaticAttr (AttributeName "height") (NumberValue h)

-- Paths and transforms
d :: forall datum. (datum -> String) -> Attribute datum
d f = DataAttr (AttributeName "d") (StringValue <<< f)

transform :: forall datum. (datum -> String) -> Attribute datum
transform f = DataAttr (AttributeName "transform") (StringValue <<< f)

-- Standard HTML attributes
class_ :: forall datum. String -> Attribute datum
class_ c = StaticAttr (AttributeName "class") (StringValue c)

id_ :: forall datum. (datum -> String) -> Attribute datum
id_ f = DataAttr (AttributeName "id") (StringValue <<< f)

-- Text attributes
text :: forall datum. (datum -> String) -> Attribute datum
text f = DataAttr (AttributeName "text") (StringValue <<< f)

fontSize :: forall datum. Number -> Attribute datum
fontSize size = StaticAttr (AttributeName "font-size") (NumberValue size)

fontFamily :: forall datum. String -> Attribute datum
fontFamily family = StaticAttr (AttributeName "font-family") (StringValue family)

textAnchor :: forall datum. String -> Attribute datum
textAnchor anchor = StaticAttr (AttributeName "text-anchor") (StringValue anchor)

dy :: forall datum. Number -> Attribute datum
dy offset = StaticAttr (AttributeName "dy") (NumberValue offset)

dx :: forall datum. Number -> Attribute datum
dx offset = StaticAttr (AttributeName "dx") (NumberValue offset)
