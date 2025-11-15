module PSD3v2.Capabilities.Selection
  ( class SelectionM
  , select
  , selectAll
  , renderData
  , joinData
  , append
  , appendChild
  , setAttrs
  , setAttrsExit
  , remove
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
import PSD3v2.Selection.Types (ElementType, JoinResult, SBound, SEmpty, SExiting, SPending)
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
    -> m (sel SBound Element datum)

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
    :: forall f parent datum
     . Foldable f
    => Ord datum
    => f datum
    -> String  -- Selector for existing elements
    -> sel SEmpty parent datum
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
    -> m (sel SBound Element datum)

  -- | Set attributes on a bound selection
  -- |
  -- | Updates existing elements with new attribute values.
  -- | Returns the updated selection.
  setAttrs
    :: forall datum
     . Array (Attribute datum)
    -> sel SBound Element datum
    -> m (sel SBound Element datum)

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

  -- | Merge two bound selections
  -- |
  -- | Combines selections in document order (following D3 semantics).
  -- | Useful for combining enter and update selections.
  merge
    :: forall datum
     . sel SBound Element datum
    -> sel SBound Element datum
    -> m (sel SBound Element datum)

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
     . Behavior
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
    :: forall parent datum
     . Ord datum
    => sel SEmpty parent datum
    -> Tree datum
    -> m (Map String (sel SBound Element datum))
