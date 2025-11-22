module PSD3v2.Capabilities.Selection
  ( class SelectionM
  , select
  , selectAll
  , openSelection
  , renderData
  , appendData
  , joinData
  , joinDataWithKey
  , updateJoin
  , append
  , appendChild
  , appendChildInheriting
  , setAttrs
  , setAttrsExit
  , remove
  , clear
  , merge
  , on
  , renderTree
  ) where

import Prelude

import Data.Foldable (class Foldable)
import Data.Map (Map)
import Data.Maybe (Maybe)
import PSD3v2.Attribute.Types (Attribute)
import PSD3v2.Behavior.Types (Behavior)
import PSD3v2.Selection.Types (ElementType, JoinResult, SBoundOwns, SBoundInherits, SEmpty, SExiting, SPending)
import PSD3v2.VizTree.Tree (Tree)
import Web.DOM.Element (Element)

-- | Type class for selection operations
-- |
-- | This is the tagless final interface that user code programs against.
-- | Different interpreters can provide different implementations:
-- | - D3v2: Manipulates actual DOM using PSD3v2.Selection.Operations
-- | - String: Generates HTML strings
-- | - Meta: Records operations for visualization/debugging
-- | - Music: Generates audio representations
-- |
-- | The phantom type parameters (SEmpty, SBound, etc.) provide compile-time
-- | safety even though the type class itself doesn't enforce state transitions.
-- | The interpreters use these phantom types with `unsafePartial` to safely
-- | pattern match on the underlying SelectionImpl.
-- |
-- | Example user code:
-- | ```purescript
-- | drawCircles :: forall m sel. SelectionM sel m => m Unit
-- | drawCircles = do
-- |   svg <- select "svg"
-- |   circles <- renderData Circle [1, 2, 3] "circle" svg
-- |     (Just \d -> [fill "green", cx (\_ -> d * 100.0)])
-- |     Nothing
-- |     Nothing
-- |   pure unit
-- | ```
class Monad m <= SelectionM sel m | m -> sel where

  -- | Select a single element by CSS selector
  -- |
  -- | Returns an empty selection (no data bound).
  -- | The datum type is polymorphic and will be inferred from usage.
  -- | This is typically the starting point for data binding.
  select
    :: forall datum
     . String  -- CSS selector
    -> m (sel SEmpty Element datum)

  -- | Select all elements matching selector within a parent selection
  -- |
  -- | Returns an empty selection of the matched elements.
  -- | The datum type is polymorphic and will be inferred from usage.
  selectAll
    :: forall state parent parentDatum datum
     . String  -- CSS selector
    -> sel state parent parentDatum
    -> m (sel SEmpty Element datum)

  -- | Open selection (v1 compatibility API)
  -- |
  -- | Alias for selectAll with arguments flipped to match v1 convention.
  -- | In v1, openSelection took parent first, then selector.
  -- |
  -- | Example (v1 style):
  -- | ```purescript
  -- | openSel <- openSelection letterGroup "text"
  -- | ```
  openSelection
    :: forall state parent parentDatum datum
     . sel state parent parentDatum
    -> String  -- CSS selector
    -> m (sel SEmpty Element datum)

  -- | High-level data rendering (recommended for most use cases)
  -- |
  -- | Manages the entire enter-update-exit cycle automatically.
  -- | Users provide Maybe callbacks for each phase.
  -- |
  -- | This is the user-friendly API that prevents sequencing errors
  -- | by handling enter/update/exit internally.
  -- |
  -- | Example:
  -- | ```purescript
  -- | circles <- renderData Circle [1, 2, 3] "circle" svg
  -- |   (Just \d -> [fill "green", cx (\_ -> d * 100.0)])  -- Enter
  -- |   (Just \d -> [fill "orange"])                        -- Update
  -- |   Nothing                                             -- Exit (just remove)
  -- | ```
  renderData
    :: forall f parent datum
     . Foldable f
    => Ord datum
    => ElementType
    -> f datum  -- Data to bind
    -> String   -- Selector for existing elements
    -> sel SEmpty parent datum
    -> Maybe (datum -> Array (Attribute datum))  -- Enter attributes
    -> Maybe (datum -> Array (Attribute datum))  -- Update attributes
    -> Maybe (datum -> Array (Attribute datum))  -- Exit attributes (applied before removal)
    -> m (sel SBoundOwns Element datum)

  -- | Simple data append for initial renders
  -- |
  -- | A simplified variant for when you just want to create elements
  -- | without worrying about updates or exits.
  -- |
  -- | Perfect for initial renders where there are no existing elements.
  -- |
  -- | Example:
  -- | ```purescript
  -- | svg <- select "svg"
  -- | circles <- appendData Circle [1, 2, 3]
  -- |   [radius 10.0, fill "steelblue"]
  -- |   svg
  -- | ```
  appendData
    :: forall f parent parentDatum datum
     . Foldable f
    => ElementType
    -> f datum
    -> Array (Attribute datum)
    -> sel SEmpty parent parentDatum
    -> m (sel SBoundOwns Element datum)

  -- | Low-level data join for power users
  -- |
  -- | Explicitly returns enter, update, and exit selections.
  -- | Users must handle each set manually using append/setAttrs/remove.
  -- |
  -- | Example:
  -- | ```purescript
  -- | JoinResult { enter, update, exit } <- joinData [1, 2, 3] "circle" svg
  -- | enterEls <- append Circle [...] enter
  -- | updateEls <- setAttrs [...] update
  -- | remove exit
  -- | merged <- merge enterEls updateEls
  -- | ```
  joinData
    :: forall f parent parentDatum datum
     . Foldable f
    => Ord datum
    => f datum
    -> String  -- Selector for existing elements
    -> sel SEmpty parent parentDatum
    -> m (JoinResult sel parent datum)

  -- | Data join with custom key function
  -- |
  -- | Like joinData, but uses a key function to extract comparable keys
  -- | instead of requiring Ord on the data itself.
  -- |
  -- | This is essential for data types that don't have lawful Ord instances
  -- | (e.g., opaque foreign types like D3Link_Swizzled).
  -- |
  -- | Example:
  -- | ```purescript
  -- | JoinResult { enter, update, exit } <- joinDataWithKey links (\l -> l.id) "line" svg
  -- | ```
  joinDataWithKey
    :: forall f parent parentDatum datum key
     . Foldable f
    => Ord key
    => f datum
    -> (datum -> key)  -- Key extraction function
    -> String  -- Selector for existing elements
    -> sel SEmpty parent parentDatum
    -> m (JoinResult sel parent datum)

  -- | Update join (v1 compatibility API)
  -- |
  -- | This is a compatibility wrapper that provides similar semantics to v1's updateJoin.
  -- | Unlike v1 which could infer the selector from the open selection, v2 requires
  -- | passing the selector string explicitly.
  -- |
  -- | Parameters match v1 order: openSelection -> Element -> data -> keyFn -> selector
  -- | The Element parameter is kept for compatibility but not used internally.
  -- |
  -- | Example (v2 style with selector):
  -- | ```purescript
  -- | openSel <- openSelection letterGroup "text"
  -- | { enter, update, exit } <- updateJoin openSel Text letters charToKey "text"
  -- | ```
  updateJoin
    :: forall f parent parentDatum datum key elemType
     . Foldable f
    => Ord key
    => sel SEmpty parent parentDatum
    -> elemType  -- Element type (not used in v2, kept for v1 compatibility)
    -> f datum
    -> (datum -> key)  -- Key extraction function
    -> String  -- Selector string (required in v2, inferred in v1)
    -> m (JoinResult sel parent datum)

  -- | Append new elements to a pending (enter) selection
  -- |
  -- | Creates DOM elements for each datum and applies attributes.
  -- | Returns a bound selection.
  append
    :: forall parent datum
     . ElementType
    -> Array (Attribute datum)
    -> sel SPending parent datum
    -> m (sel SBoundOwns Element datum)

  -- | Set attributes on a bound selection
  -- |
  -- | Updates existing elements with new attribute values.
  -- | Returns the updated selection.
  setAttrs
    :: forall datum
     . Array (Attribute datum)
    -> sel SBoundOwns Element datum
    -> m (sel SBoundOwns Element datum)

  -- | Set attributes on an exiting selection
  -- |
  -- | Similar to setAttrs but works on selections in the exit phase.
  -- | Useful for styling elements before they animate out and are removed.
  setAttrsExit
    :: forall datum
     . Array (Attribute datum)
    -> sel SExiting Element datum
    -> m (sel SExiting Element datum)

  -- | Remove elements from an exit selection
  -- |
  -- | Removes elements from the DOM.
  remove
    :: forall datum
     . sel SExiting Element datum
    -> m Unit

  -- | Clear all children from an element
  -- |
  -- | Selects the element and removes all its children.
  -- | Useful for clearing a container before rendering new content.
  -- |
  -- | Example:
  -- | ```purescript
  -- | clear "#viz"
  -- | svg <- appendChild SVG [...] container
  -- | ```
  clear
    :: String  -- CSS selector
    -> m Unit

  -- | Merge two bound selections
  -- |
  -- | Combines selections in document order (following D3 semantics).
  -- | Useful for combining enter and update selections.
  merge
    :: forall datum
     . sel SBoundOwns Element datum
    -> sel SBoundOwns Element datum
    -> m (sel SBoundOwns Element datum)

  -- | Append a single child element to a parent selection
  -- |
  -- | Creates one new element and appends it to each parent.
  -- | Returns an empty selection of the newly created element(s).
  -- |
  -- | Example:
  -- | ```purescript
  -- | container <- select "#viz"
  -- | svg <- appendChild SVG [width 400.0, height 150.0] container
  -- | ```
  appendChild
    :: forall parent datum datumOut
     . ElementType
    -> Array (Attribute datumOut)
    -> sel SEmpty parent datum
    -> m (sel SEmpty Element datumOut)

  -- | Append a child element that inherits the parent's data
  -- |
  -- | Creates one child element for each parent, copying the parent's __data__.
  -- | Returns a selection with SBoundInherits state to document data inheritance.
  -- |
  -- | This enables nested data-bound structures like:
  -- | ```purescript
  -- | -- Create groups with data
  -- | groups <- append Group [] enterSelection  -- SBoundOwns
  -- |
  -- | -- Add circles that inherit group's data
  -- | circles <- appendChildInheriting Circle [radius 5.0] groups  -- SBoundInherits
  -- |
  -- | -- Add text that also inherits group's data
  -- | labels <- appendChildInheriting Text [textContent _.name] groups  -- SBoundInherits
  -- | ```
  appendChildInheriting
    :: forall parent datum
     . ElementType
    -> Array (Attribute datum)
    -> sel SBoundOwns parent datum
    -> m (sel SBoundInherits Element datum)

  -- | Attach a behavior (zoom, drag, etc.) to a selection
  -- |
  -- | Returns the selection unchanged to allow chaining.
  -- |
  -- | Example:
  -- | ```purescript
  -- | svg <- appendChild SVG [...] container
  -- | zoomGroup <- appendChild Group [...] svg
  -- | _ <- on (Drag defaultDrag) zoomGroup
  -- | _ <- on (Zoom $ defaultZoom (ScaleExtent 0.5 4.0) ".zoom-group") svg
  -- | ```
  on
    :: forall state elem datum
     . Behavior datum
    -> sel state elem datum
    -> m (sel state elem datum)

  -- | Declarative tree rendering (high-level API)
  -- |
  -- | Renders an entire tree structure at once, returning a map of named selections.
  -- | This is the declarative alternative to imperative appendChild chains.
  -- |
  -- | Example:
  -- | ```purescript
  -- | import PSD3v2.VizTree.Tree as T
  -- |
  -- | tree =
  -- |   T.named "svg" SVG [width 800] `T.withChildren`
  -- |     [ T.named "zoom" Group [class_ "zoom"] `T.withChild`
  -- |         T.named "nodes" Group [class_ "nodes"]
  -- |     ]
  -- |
  -- | container <- select "#viz"
  -- | selections <- renderTree container tree
  -- |
  -- | -- Access named selections
  -- | case Map.lookup "svg" selections of
  -- |   Just svg -> ...
  -- |   Nothing -> ...
  -- | ```
  renderTree
    :: forall parent parentDatum datum
     . Ord datum
    => sel SEmpty parent parentDatum
    -> Tree datum
    -> m (Map String (sel SBoundOwns Element datum))
