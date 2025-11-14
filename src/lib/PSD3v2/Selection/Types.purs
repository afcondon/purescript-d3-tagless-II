module PSD3v2.Selection.Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Web.DOM.Element (Element)
import Web.DOM.Document (Document)

-- | Phantom types representing selection states
-- |
-- | These are uninhabited types used only at the type level
-- | to track what operations are legal on a selection.
data SEmpty    -- Selection has parent elements but no data bound
data SBound    -- Elements with data bound (the normal working state)
data SPending  -- Data without elements (enter selection)
data SExiting  -- Elements without matching data (exit selection)

-- | A type-safe D3-style selection
-- |
-- | The phantom type parameter `state` tracks what operations are legal:
-- | - `SEmpty` selections can receive data via join
-- | - `SBound` selections can be updated with attributes
-- | - `SPending` selections need elements appended
-- | - `SExiting` selections should be removed
-- |
-- | The `parent` type parameter tracks the parent element type.
-- | The `datum` type parameter is the data bound to each element.
-- |
-- | Examples:
-- | ```purescript
-- | svg :: Selection SEmpty Element Unit
-- | circles :: Selection SBound Element Number
-- | entering :: Selection SPending SVGElement Number
-- | leaving :: Selection SExiting SVGElement Number
-- | ```
newtype Selection (state :: Type) (parent :: Type) (datum :: Type)
  = Selection (SelectionImpl parent datum)

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
      , document :: Document
      }
  | PendingSelection
      { parentElements :: Array Element
      , pendingData :: Array datum
      , document :: Document
      }
  | ExitingSelection
      { elements :: Array Element
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
-- | - update is SBound (can be modified)
-- | - exit is SExiting (should be removed)
data JoinResult parent datum = JoinResult
  { enter  :: Selection SPending parent datum
  , update :: Selection SBound Element datum
  , exit   :: Selection SExiting Element datum
  }

-- | SVG and HTML element types
-- |
-- | Used when appending new elements to specify what to create.
-- | We might expand this to support all SVG/HTML elements eventually,
-- | but starting with the most common ones for proof of concept.
data ElementType
  = Circle
  | Rect
  | Path
  | Line
  | Text
  | Group
  | SVG
  | Div
  | Span

derive instance Eq ElementType
derive instance Ord ElementType
derive instance Generic ElementType _
instance Show ElementType where
  show = genericShow
