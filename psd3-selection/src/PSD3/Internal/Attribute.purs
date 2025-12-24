-- | Core Attribute Types
-- |
-- | Defines the fundamental types for SVG/HTML attributes in PSD3.
-- | These types are the target of the finally-tagless expression system.
-- |
-- | The `Attribute` ADT supports three patterns:
-- | - Static: Same value for all elements
-- | - Data-driven: Value computed from datum
-- | - Indexed: Value computed from datum and index
-- |
-- | ## Historical Note
-- |
-- | This module previously contained a `ToAttr` typeclass with polymorphic
-- | smart constructors (fill, cx, radius, etc.). That pattern was superseded
-- | by the finally-tagless expression system which provides multi-interpretation
-- | capabilities. See `kept-for-historical-context/ToAttr-Pattern-Documentation.md`.
module PSD3.Internal.Attribute
  ( Attribute(..)
  , AttributeName(..)
  , AttributeValue(..)
  , AttrSource(..)
  ) where

import Prelude
import Data.Functor.Contravariant (class Contravariant)

-- | Source metadata for attributes
-- |
-- | Describes where the attribute value comes from, enabling interpreters
-- | like MetaAST to show meaningful information about attribute bindings.
-- |
-- | This is automatically captured when using the DSL (field, num, etc.)
-- | but is `UnknownSource` for raw PureScript functions (escape hatches).
data AttrSource
  = UnknownSource           -- ^ Raw function, can't introspect
  | StaticSource String     -- ^ Constant value with its string representation
  | FieldSource String      -- ^ Single field access: d.fieldName
  | ExprSource String       -- ^ Computed expression: "d.x * 20 + 50"
  | IndexSource             -- ^ Uses element index
  | OpaqueSource            -- ^ Placeholder requiring metadata substitution (Emmet round-trip)

derive instance Eq AttrSource
derive instance Ord AttrSource

instance Show AttrSource where
  show UnknownSource = "UnknownSource"
  show (StaticSource s) = "(StaticSource " <> show s <> ")"
  show (FieldSource f) = "(FieldSource " <> show f <> ")"
  show (ExprSource e) = "(ExprSource " <> show e <> ")"
  show IndexSource = "IndexSource"
  show OpaqueSource = "OpaqueSource"

-- | Type-safe attribute with datum phantom type
-- |
-- | Attributes can be:
-- | - Static: Same value for all elements
-- | - Data-driven: Value computed from datum (with source metadata)
-- | - Indexed: Value computed from datum and index (with source metadata)
-- |
-- | The phantom type `datum` ensures attributes are only applied
-- | to selections with matching data types.
-- |
-- | The `AttrSource` field enables interpreters to inspect attribute origins
-- | without evaluating the functions.
data Attribute datum
  = StaticAttr AttributeName AttributeValue
  | DataAttr AttributeName AttrSource (datum -> AttributeValue)
  | IndexedAttr AttributeName AttrSource (datum -> Int -> AttributeValue)

-- We can't derive Show for function types, but we can show the structure
instance Show (Attribute datum) where
  show (StaticAttr name val) = "(StaticAttr " <> show name <> " " <> show val <> ")"
  show (DataAttr name src _) = "(DataAttr " <> show name <> " " <> show src <> " <fn>)"
  show (IndexedAttr name src _) = "(IndexedAttr " <> show name <> " " <> show src <> " <fn>)"

-- | Contravariant instance for Attribute
-- |
-- | Attributes *consume* data (they're data sinks), making them naturally contravariant.
-- | This enables attribute reuse via `cmap`:
-- |
-- | ```purescript
-- | -- Define attribute for specific type
-- | radiusAttr :: Attribute Number
-- | radiusAttr = DataAttr (AttributeName "r") NumberValue
-- |
-- | -- Adapt to work with richer type
-- | type Circle = { radius :: Number, x :: Number, y :: Number }
-- | circleRadiusAttr :: Attribute Circle
-- | circleRadiusAttr = cmap _.radius radiusAttr
-- | ```
-- |
-- | The key insight: `cmap` composes the projection function with the attribute's
-- | data accessor, allowing attributes written for simple types to work with
-- | complex types via field selection.
instance Contravariant Attribute where
  cmap _ (StaticAttr name val) = StaticAttr name val -- Static doesn't depend on datum
  cmap f (DataAttr name src g) = DataAttr name src (g <<< f) -- Compose: first project, then extract value
  cmap f (IndexedAttr name src g) = IndexedAttr name src (\b i -> g (f b) i) -- Project datum before indexing

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
