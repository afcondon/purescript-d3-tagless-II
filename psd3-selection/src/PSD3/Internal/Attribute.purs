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
  ) where

import Prelude
import Data.Functor.Contravariant (class Contravariant)

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
  cmap f (DataAttr name g) = DataAttr name (g <<< f) -- Compose: first project, then extract value
  cmap f (IndexedAttr name g) = IndexedAttr name (\b i -> g (f b) i) -- Project datum before indexing

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
