module PSD3.Internal.Selection.Types
  ( ElementType(..)
  , JoinResult(..)
  , RenderContext(..)
  , SBoundInherits
  , SBoundOwns
  , SEmpty
  , SExiting
  , SPending
  , Selection(..)
  , SelectionImpl(..)
  , elementContext
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Web.DOM.Element (Element)
import Web.DOM.Document (Document)

-- | Phantom types representing selection states
-- |
-- | These are uninhabited types used only at the type level
-- | to track what operations are legal on a selection.
data SEmpty -- Selection has parent elements but no data bound
data SBoundOwns -- Elements with data bound (owns the __data__ binding)
data SBoundInherits -- Elements with data bound (inherited from parent)
data SPending -- Data without elements (enter selection)
data SExiting -- Elements without matching data (exit selection)

-- | A type-safe D3-style selection
-- |
-- | The phantom type parameter `state` tracks what operations are legal:
-- | - `SEmpty` selections can receive data via join
-- | - `SBoundOwns` selections own their data binding, can be updated
-- | - `SBoundInherits` selections inherit parent's data, can be updated
-- | - `SPending` selections need elements appended
-- | - `SExiting` selections should be removed
-- |
-- | The `parent` type parameter tracks the parent element type.
-- | The `datum` type parameter is the data bound to each element.
-- |
-- | Examples:
-- | ```purescript
-- | svg :: Selection SEmpty Element Unit
-- | circles :: Selection SBoundOwnsOwns Element Number
-- | labels :: Selection SBoundOwnsInherits Element Number  -- inherits from circles
-- | entering :: Selection SPending SVGElement Number
-- | leaving :: Selection SExiting SVGElement Number
-- | ```
newtype Selection (state :: Type) (parent :: Type) (datum :: Type) = Selection (SelectionImpl parent datum)

-- | Functor instance for Selection
-- |
-- | Allows transforming the bound data without touching the DOM.
-- | This is pure - no effects, just data transformation.
-- |
-- | Examples:
-- | ```purescript
-- | -- Transform numeric data
-- | doubled :: Selection SBoundOwnsOwns Element Int
-- | doubled = map (_ * 2) numbers
-- |
-- | -- Extract fields from records
-- | ages :: Selection SBoundOwnsOwns Element Int
-- | ages = map _.age people
-- | ```
instance Functor (Selection state parent) where
  map f' (Selection impl) = Selection (mapSelectionImpl f' impl)
    where
    mapSelectionImpl :: forall a b. (a -> b) -> SelectionImpl parent a -> SelectionImpl parent b
    mapSelectionImpl _ (EmptySelection r) = EmptySelection r
    mapSelectionImpl f (BoundSelection r) = BoundSelection
      { elements: r.elements
      , data: map f r.data
      , indices: r.indices
      , document: r.document
      }
    mapSelectionImpl f (PendingSelection r) = PendingSelection
      { parentElements: r.parentElements
      , pendingData: map f r.pendingData
      , indices: r.indices
      , document: r.document
      }
    mapSelectionImpl f (ExitingSelection r) = ExitingSelection
      { elements: r.elements
      , data: map f r.data
      , document: r.document
      }

-- | Internal implementation of selections (not exported to users)
-- |
-- | This ADT ensures that each state has exactly the data it needs:
-- | - EmptySelection: parent elements to bind data to
-- | - BoundSelection: elements with their bound data
-- | - PendingSelection: data waiting for elements
-- | - ExitingSelection: elements to be removed
data SelectionImpl :: forall k. k -> Type -> Type
data SelectionImpl parent datum
  = EmptySelection
      { parentElements :: Array Element
      , document :: Document
      }
  | BoundSelection
      { elements :: Array Element
      , data :: Array datum
      , indices :: Maybe (Array Int) -- Nothing for regular selections, Just for update selections
      , document :: Document
      }
  | PendingSelection
      { parentElements :: Array Element
      , pendingData :: Array datum
      , indices :: Maybe (Array Int) -- Nothing for regular, Just for enter with indices
      , document :: Document
      }
  | ExitingSelection
      { elements :: Array Element
      , data :: Array datum
      , document :: Document
      }

-- | Result of a data join operation
-- |
-- | The join splits data and elements into three disjoint sets:
-- | - enter: new data that needs elements created
-- | - update: existing elements that should be updated
-- | - exit: old elements that should be removed
-- |
-- | Note the type signatures enforce correct usage:
-- | - enter is SPending (needs append)
-- | - update is SBoundOwns (owns data, can be modified)
-- | - exit is SExiting (should be removed)
-- | Polymorphic join result that works with any selection type wrapper
-- | This allows interpreters to use their own selection types (e.g., D3v2Selection_)
data JoinResult :: forall k. (Type -> Type -> k -> Type) -> Type -> k -> Type
data JoinResult sel parent datum = JoinResult
  { enter :: sel SPending parent datum
  , update :: sel SBoundOwns Element datum
  , exit :: sel SExiting Element datum
  }

-- | Output context for rendering
-- | Determines which namespace to use when creating elements
data RenderContext
  = SVGContext -- SVG namespace (for graphics)
  | HTMLContext -- HTML namespace (for DOM elements)

-- Future extensions:
-- | CanvasContext  -- Canvas 2D/WebGL rendering
-- | AudioContext   -- Web Audio API
-- | ARContext      -- Augmented reality / spatial
-- | StringContext  -- String-based interpreters (testing, pretty-printing, etc.)
--                  -- In this mode, elements become indentation groups:
--                  --   Group "chart" [...]
--                  --     Circle [cx 10, cy 20]
--                  --     Text "Hello"
--                  -- Useful for:
--                  -- - Unit testing (check structure without DOM)
--                  -- - Debugging (inspect tree as text)
--                  -- - Documentation generation
--                  -- - Alternative formats (Markdown, LaTeX, etc.)

derive instance Eq RenderContext
derive instance Ord RenderContext
derive instance Generic RenderContext _
instance Show RenderContext where
  show = genericShow

-- | Element types organized by rendering context
-- |
-- | This ADT makes the distinction between SVG and HTML elements explicit,
-- | allowing the type system to guide proper namespace handling.
data ElementType
  -- SVG elements (require SVG namespace)
  = Circle
  | Rect
  | Path
  | Line
  | Text
  | Group
  | SVG
  | Defs
  | LinearGradient
  | Stop
  | PatternFill  -- SVG pattern element for fills (used in fast/slow visual treatment)
  -- HTML elements (use default namespace)
  | Div
  | Span
  | Table
  | Tr
  | Td
  | Th
  | Tbody
  | Thead

derive instance Eq ElementType
derive instance Ord ElementType
derive instance Generic ElementType _
instance Show ElementType where
  show = genericShow

-- | Determine which rendering context an element belongs to
elementContext :: ElementType -> RenderContext
elementContext Circle = SVGContext
elementContext Rect = SVGContext
elementContext Path = SVGContext
elementContext Line = SVGContext
elementContext Text = SVGContext
elementContext Group = SVGContext
elementContext SVG = SVGContext
elementContext Defs = SVGContext
elementContext LinearGradient = SVGContext
elementContext Stop = SVGContext
elementContext PatternFill = SVGContext
elementContext Div = HTMLContext
elementContext Span = HTMLContext
elementContext Table = HTMLContext
elementContext Tr = HTMLContext
elementContext Td = HTMLContext
elementContext Th = HTMLContext
elementContext Tbody = HTMLContext
elementContext Thead = HTMLContext
