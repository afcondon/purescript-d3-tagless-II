module PSD3v2.Selection.Operations
  ( select
  , selectAll
  , append
  , setAttrs
  , remove
  , merge
  , joinData
  , renderData
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (class Foldable, traverse_)
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Partial.Unsafe (unsafePartial)
import PSD3v2.Attribute.Types (Attribute(..), AttributeName(..), AttributeValue(..))
import PSD3v2.Selection.Join as Join
import PSD3v2.Selection.Types (ElementType(..), JoinResult(..), SBound, SEmpty, SExiting, SPending, Selection(..), SelectionImpl(..))
import Web.DOM.Document (Document)
import Web.DOM.Element (Element)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)

-- | Select a single element matching the CSS selector
-- |
-- | Returns an empty selection (no data bound).
-- | This is typically the starting point for data binding.
-- |
-- | Example:
-- | ```purescript
-- | svg <- select "svg"
-- | circles <- renderData Circle [1, 2, 3] "circle" svg ...
-- | ```
select
  :: forall m
   . MonadEffect m
  => String  -- CSS selector
  -> m (Selection SEmpty Element Unit)
select selector = liftEffect do
  doc <- window >>= document <#> toDocument
  maybeElement <- querySelector_ selector doc
  case maybeElement of
    Nothing -> pure $ Selection $ EmptySelection
      { parentElements: []
      , document: doc
      }
    Just element -> pure $ Selection $ EmptySelection
      { parentElements: [element]
      , document: doc
      }

-- | Select all elements matching the CSS selector within a parent selection
-- |
-- | Returns an empty selection (no data bound yet).
-- | Use this for nested selections.
-- |
-- | Example:
-- | ```purescript
-- | svg <- select "svg"
-- | groups <- selectAll "g" svg
-- | ```
selectAll
  :: forall state parent datum m
   . MonadEffect m
  => String  -- CSS selector
  -> Selection state parent datum
  -> m (Selection SEmpty Element Unit)
selectAll selector (Selection impl) = liftEffect do
  doc <- getDocument impl
  elements <- case impl of
    EmptySelection { parentElements } ->
      querySelectorAll_ selector parentElements
    BoundSelection { elements: parentElems } ->
      querySelectorAll_ selector parentElems
    PendingSelection { parentElements } ->
      querySelectorAll_ selector parentElements
    ExitingSelection { elements: exitElems } ->
      querySelectorAll_ selector exitElems

  pure $ Selection $ EmptySelection
    { parentElements: elements
    , document: doc
    }

-- | Append new elements to a pending (enter) selection
-- |
-- | This materializes the data into DOM elements.
-- | Returns a bound selection that can be further modified.
-- |
-- | Example:
-- | ```purescript
-- | enterSelection <- append Circle
-- |   [ fill "green"
-- |   , radius 10.0
-- |   , cx (\d -> d.x)
-- |   ]
-- |   pendingSelection
-- | ```
append
  :: forall parent datum m
   . MonadEffect m
  => ElementType
  -> Array (Attribute datum)
  -> Selection SPending parent datum
  -> m (Selection SBound Element datum)
append elemType attrs (Selection impl) = liftEffect do
  let { parentElements, pendingData, document: doc } = unsafePartial case impl of
        PendingSelection r -> r
  -- Create elements for each datum
  let paired = Array.zipWith Tuple pendingData parentElements
  elements <- paired # traverseWithIndex \index (Tuple datum parent) -> do
    element <- createElement_ (elementTypeToString elemType) doc
    -- Set attributes on the new element
    applyAttributes element datum index attrs
    -- Append to parent
    appendChild_ element parent
    pure element

  pure $ Selection $ BoundSelection
    { elements
    , data: pendingData
    , document: doc
    }

-- | Set attributes on a bound selection
-- |
-- | Updates existing elements with new attribute values.
-- | This is used for the "update" part of enter-update-exit.
-- |
-- | Example:
-- | ```purescript
-- | updated <- setAttrs
-- |   [ fill "orange"
-- |   , cx (\d -> d.x)
-- |   ]
-- |   boundSelection
-- | ```
setAttrs
  :: forall datum m
   . MonadEffect m
  => Array (Attribute datum)
  -> Selection SBound Element datum
  -> m (Selection SBound Element datum)
setAttrs attrs (Selection impl) = liftEffect do
  let { elements, data: datumArray, document: doc } = unsafePartial case impl of
        BoundSelection r -> r
  -- Apply attributes to each element
  let paired = Array.zipWith Tuple datumArray elements
  paired # traverseWithIndex_ \index (Tuple datum element) ->
    applyAttributes element datum index attrs

  pure $ Selection $ BoundSelection
    { elements
    , data: datumArray
    , document: doc
    }

-- | Remove elements from an exit selection
-- |
-- | Removes the elements from the DOM.
-- | This is the final step for exiting data.
-- |
-- | Example:
-- | ```purescript
-- | remove exitSelection
-- | ```
remove
  :: forall datum m
   . MonadEffect m
  => Selection SExiting Element datum
  -> m Unit
remove (Selection impl) = liftEffect do
  let { elements } = unsafePartial case impl of
        ExitingSelection r -> r
  elements # traverse_ \element ->
    removeElement_ element

-- | Merge two bound selections
-- |
-- | Follows D3 semantics: concatenates in document order.
-- | Useful for combining enter and update selections.
-- |
-- | Example:
-- | ```purescript
-- | allCircles <- merge enterSelection updateSelection
-- | ```
merge
  :: forall datum m
   . MonadEffect m
  => Selection SBound Element datum
  -> Selection SBound Element datum
  -> m (Selection SBound Element datum)
merge (Selection impl1) (Selection impl2) = do
  let { elements: els1, data: data1, document: doc } = unsafePartial case impl1 of
        BoundSelection r -> r
  let { elements: els2, data: data2 } = unsafePartial case impl2 of
        BoundSelection r -> r
  pure $ Selection $ BoundSelection
    { elements: els1 <> els2
    , data: data1 <> data2
    , document: doc
    }

-- | Low-level data join for power users
-- |
-- | Explicitly returns enter, update, and exit selections.
-- | Users must handle each set manually.
-- |
-- | Example:
-- | ```purescript
-- | JoinResult { enter, update, exit } <- joinData [1, 2, 3] "circle" svg
-- | enterEls <- append Circle [...] enter
-- | updateEls <- setAttrs [...] update
-- | remove exit
-- | ```
joinData
  :: forall f parent datum m
   . MonadEffect m
  => Foldable f
  => Ord datum
  => f datum
  -> String  -- Element selector for existing elements
  -> Selection SEmpty parent datum
  -> m (JoinResult parent datum)
joinData foldableData selector (Selection impl) = liftEffect do
  let { parentElements, document: doc } = unsafePartial case impl of
        EmptySelection r -> r
  -- Query for existing elements within parents
  existingElements <- querySelectorAll_ selector parentElements

  -- Get old bindings (elements with their bound data)
  oldBindings <- existingElements # traverse \element -> do
    maybeDatum <- getElementData_ element
    pure $ { element, datum: maybeDatum }

  -- Filter to only elements that have data bound
  let validOldBindings = oldBindings # Array.mapMaybe \{ element, datum } ->
        datum <#> \d -> { element, datum: d }

  -- Convert foldable to array
  let newDataArray = Array.fromFoldable foldableData

  -- Run pure join algorithm
  let joinSets = Join.computeJoin newDataArray validOldBindings

  -- Build typed selections for each set
  let enterSelection = Selection $ PendingSelection
        { parentElements
        , pendingData: joinSets.enter
        , document: doc
        }

  let updateSelection = Selection $ BoundSelection
        { elements: joinSets.update <#> _.element
        , data: joinSets.update <#> _.newDatum
        , document: doc
        }

  let exitSelection = Selection $ ExitingSelection
        { elements: joinSets.exit <#> _.element
        , document: doc
        }

  pure $ JoinResult
    { enter: enterSelection
    , update: updateSelection
    , exit: exitSelection
    }

-- | High-level data rendering for most users
-- |
-- | Manages the entire enter-update-exit cycle automatically.
-- | Users provide Maybe callbacks for each phase.
-- |
-- | This is the recommended API for 90% of use cases.
-- |
-- | Example:
-- | ```purescript
-- | circles <- renderData Circle [1, 2, 3] "circle" svg
-- |   (Just \d -> [fill "green", cx (\_ -> d * 100.0)])  -- Enter
-- |   (Just \d -> [fill "orange"])                        -- Update
-- |   Nothing                                             -- Exit (just remove)
-- | ```
renderData
  :: forall f parent datum m
   . MonadEffect m
  => Foldable f
  => Ord datum
  => ElementType
  -> f datum
  -> String  -- Element selector
  -> Selection SEmpty parent datum
  -> Maybe (datum -> Array (Attribute datum))  -- Enter attributes
  -> Maybe (datum -> Array (Attribute datum))  -- Update attributes
  -> Maybe (datum -> Array (Attribute datum))  -- Exit attributes (applied before removal)
  -> m (Selection SBound Element datum)
renderData elemType foldableData selector emptySelection enterAttrs updateAttrs exitAttrs = do
  -- Perform the join
  JoinResult { enter, update, exit } <- joinData foldableData selector emptySelection

  -- Handle enter
  enterBound <- case enterAttrs of
    Nothing -> append elemType [] enter
    Just _mkAttrs -> do
      -- TODO: Need to apply per-datum attributes
      -- For now, we'll use empty attributes
      append elemType [] enter

  -- Handle update
  updateBound <- case updateAttrs of
    Nothing -> pure update
    Just _mkAttrs -> do
      -- TODO: Apply update attributes per-datum
      pure update

  -- Handle exit
  case exitAttrs of
    Nothing -> remove exit
    Just _mkAttrs -> do
      -- TODO: Apply exit attributes before removing
      remove exit

  -- Merge enter and update
  merge enterBound updateBound

-- ============================================================================
-- Helper Functions
-- ============================================================================

getDocument :: forall parent datum. SelectionImpl parent datum -> Effect Document
getDocument (EmptySelection { document: doc }) = pure doc
getDocument (BoundSelection { document: doc }) = pure doc
getDocument (PendingSelection { document: doc }) = pure doc
getDocument (ExitingSelection { document: doc }) = pure doc

-- | Apply attributes to an element
applyAttributes :: forall datum. Element -> datum -> Int -> Array (Attribute datum) -> Effect Unit
applyAttributes element datum index attrs =
  attrs # traverse_ \attr -> case attr of
    StaticAttr (AttributeName name) value ->
      setAttribute_ name (attributeValueToString value) element

    DataAttr (AttributeName name) f ->
      setAttribute_ name (attributeValueToString (f datum)) element

    IndexedAttr (AttributeName name) f ->
      setAttribute_ name (attributeValueToString (f datum index)) element

attributeValueToString :: AttributeValue -> String
attributeValueToString (StringValue s) = s
attributeValueToString (NumberValue n) = show n
attributeValueToString (BooleanValue b) = show b

elementTypeToString :: ElementType -> String
elementTypeToString Circle = "circle"
elementTypeToString Rect = "rect"
elementTypeToString Path = "path"
elementTypeToString Line = "line"
elementTypeToString Text = "text"
elementTypeToString Group = "g"
elementTypeToString SVG = "svg"
elementTypeToString Div = "div"
elementTypeToString Span = "span"

-- ============================================================================
-- FFI Declarations
-- ============================================================================

foreign import querySelector_ :: String -> Document -> Effect (Maybe Element)
foreign import querySelectorAll_ :: String -> Array Element -> Effect (Array Element)
foreign import createElement_ :: String -> Document -> Effect Element
foreign import setAttribute_ :: String -> String -> Element -> Effect Unit
foreign import appendChild_ :: Element -> Element -> Effect Unit
foreign import removeElement_ :: Element -> Effect Unit
foreign import getElementData_ :: forall datum. Element -> Effect (Maybe datum)
foreign import setElementData_ :: forall datum. datum -> Element -> Effect Unit
