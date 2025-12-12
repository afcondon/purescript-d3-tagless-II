module PSD3v2.Selection.Operations
  ( select
  , selectAll
  , selectAllWithData
  , append
  , appendChild
  , appendChildInheriting
  , setAttrs
  , setAttrsExit
  , remove
  , clear
  , syncDOMToData
  , merge
  , joinData
  , joinDataWithKey
  , renderData
  , appendData
  , on
  , renderTree
  , reselect
  , elementTypeToString
  ) where

import Prelude hiding (append)

import Data.Array as Array
import Data.Foldable (class Foldable, traverse_)
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Effect.Uncurried (mkEffectFn2)
import PSD3v2.Attribute.Types (Attribute(..), AttributeName(..), AttributeValue(..))
import PSD3v2.Behavior.FFI as BehaviorFFI
import PSD3v2.Behavior.Types (Behavior(..), DragConfig(..), ZoomConfig(..), ScaleExtent(..))
import PSD3v2.Selection.Join as Join
import PSD3v2.Selection.Types (ElementType(..), JoinResult(..), RenderContext(..), SBoundInherits, SBoundOwns, SEmpty, SExiting, SPending, Selection(..), SelectionImpl(..), elementContext)
import PSD3v2.Transition.FFI as TransitionFFI
import PSD3v2.Transition.Types (TransitionConfig)
import PSD3v2.VizTree.Tree (Tree(..))
import Partial.Unsafe (unsafePartial, unsafeCrashWith)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Document (Document)
import Web.DOM.Document as Document
import Web.DOM.Element (Element, fromNode, toNode, toParentNode)
import Web.DOM.Element as Element
import Web.DOM.Node as Node
import Web.DOM.NodeList as NodeList
import Web.DOM.ParentNode (QuerySelector(..), querySelector, querySelectorAll)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document)
import Web.UIEvent.MouseEvent (MouseEvent, clientX, clientY, pageX, pageY)

-- FFI for offsetX/offsetY (not yet in purescript-web-uievents - PR candidate)
foreign import offsetX :: MouseEvent -> Number
foreign import offsetY :: MouseEvent -> Number

-- | Select a single element matching the CSS selector
-- |
-- | Returns an empty selection (no data bound).
-- | The datum type is polymorphic and will be inferred from usage.
-- | This is typically the starting point for data binding.
-- |
-- | Example:
-- | ```purescript
-- | svg <- select "svg"
-- | circles <- renderData Circle [1, 2, 3] "circle" svg ...
-- | ```
select
  :: forall m datum
   . MonadEffect m
  => String -- CSS selector
  -> m (Selection SEmpty Element datum)
select selector = liftEffect do
  htmlDoc <- window >>= document
  let doc = toDocument htmlDoc
  let parentNode = HTMLDocument.toParentNode htmlDoc
  maybeElement <- querySelector (QuerySelector selector) parentNode
  case maybeElement of
    Nothing -> pure $ Selection $ EmptySelection
      { parentElements: []
      , document: doc
      }
    Just element -> pure $ Selection $ EmptySelection
      { parentElements: [ element ]
      , document: doc
      }

-- | Select all elements matching the CSS selector within a parent selection
-- |
-- | Returns an empty selection (no data bound yet).
-- | The datum type is polymorphic and will be inferred from usage.
-- | Use this for nested selections.
-- |
-- | Example:
-- | ```purescript
-- | svg <- select "svg"
-- | groups <- selectAll "g" svg
-- | ```
selectAll
  :: forall state parent parentDatum datum m
   . MonadEffect m
  => String -- CSS selector
  -> Selection state parent parentDatum
  -> m (Selection SEmpty Element datum)
selectAll selector (Selection impl) = liftEffect do
  doc <- getDocument impl
  -- Get the parent elements (where new children will be appended)
  -- NOT the query results - those are fetched by joinData when needed
  let
    parentElems = case impl of
      EmptySelection { parentElements } -> parentElements
      BoundSelection { elements } -> elements
      PendingSelection { parentElements } -> parentElements
      ExitingSelection { elements } -> elements

  pure $ Selection $ EmptySelection
    { parentElements: parentElems -- Preserve parents for appending
    , document: doc
    }

-- | Select all elements matching selector and extract their bound data
-- |
-- | Use this when selecting child elements that have inherited data from their parent.
-- | This is necessary when you want to use the selection with transitions that need
-- | access to the bound data (like withTransitionStaggered).
-- |
-- | Example:
-- | ```purescript
-- | -- After creating nodes with appendChildInheriting
-- | groups <- selectSimulationGroups
-- | circles <- selectAllWithData "circle" groups.nodes
-- | withTransitionStaggered config delayFn circles [fill colorByDepth]
-- | ```
selectAllWithData
  :: forall state parent parentDatum datum m
   . MonadEffect m
  => String -- CSS selector
  -> Selection state parent parentDatum
  -> m (Selection SBoundOwns Element datum)
selectAllWithData selector (Selection impl) = liftEffect do
  doc <- getDocument impl
  elements <- case impl of
    EmptySelection { parentElements } ->
      querySelectorAllElements selector parentElements
    BoundSelection { elements: parentElems } ->
      querySelectorAllElements selector parentElems
    PendingSelection { parentElements } ->
      querySelectorAllElements selector parentElements
    ExitingSelection { elements: exitElems } ->
      querySelectorAllElements selector exitElems

  -- Extract __data__ from each element
  dataArray <- traverse
    ( \el -> do
        nullableDatum <- getElementData_ el
        pure $ case toMaybe nullableDatum of
          Just d -> d
          Nothing -> unsafeCoerce unit -- Fallback if no data (shouldn't happen with appendChildInheriting)
    )
    elements

  pure $ Selection $ BoundSelection
    { elements: elements
    , data: dataArray
    , indices: Nothing
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
  -> m (Selection SBoundOwns Element datum)
append elemType attrs (Selection impl) = liftEffect do
  let
    { parentElements, pendingData, indices, document: doc } = unsafePartial case impl of
      PendingSelection r -> r
  -- Create elements for each datum
  -- In D3, if there's one parent, all elements go to that parent
  -- If there are multiple parents, they're distributed (but that's rare)
  let
    parent = case Array.head parentElements of
      Just p -> p
      Nothing -> unsafePartial $ Array.unsafeIndex parentElements 0 -- Should never happen

  elements <- pendingData # traverseWithIndex \arrayIndex datum -> do
    -- Use logical index from indices array if present (for enter selections from joins)
    let
      logicalIndex = case indices of
        Just indexArray -> unsafePartial $ Array.unsafeIndex indexArray arrayIndex
        Nothing -> arrayIndex

    element <- createElementWithNS elemType doc
    -- Set attributes on the new element using logical index
    applyAttributes element datum logicalIndex attrs
    -- Bind data to element (CRITICAL for data joins!)
    setElementData_ datum element
    -- Append to parent
    let elementNode = toNode element
    let parentNode = toNode parent
    Node.appendChild elementNode parentNode
    pure element

  pure $ Selection $ BoundSelection
    { elements
    , data: pendingData
    , indices -- Preserve indices from pending selection (for enter selections from joins)
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
  -> Selection SBoundOwns Element datum
  -> m (Selection SBoundOwns Element datum)
setAttrs attrs (Selection impl) = liftEffect do
  let
    { elements, data: datumArray, indices, document: doc } = unsafePartial case impl of
      BoundSelection r -> r
  -- Apply attributes to each element, using logical indices if present
  let paired = Array.zipWith Tuple datumArray elements
  paired # traverseWithIndex_ \arrayIndex (Tuple datum element) -> do
    let
      logicalIndex = case indices of
        Just indexArray -> unsafePartial $ Array.unsafeIndex indexArray arrayIndex
        Nothing -> arrayIndex
    applyAttributes element datum logicalIndex attrs

  pure $ Selection $ BoundSelection
    { elements
    , data: datumArray
    , indices -- Preserve indices from input selection
    , document: doc
    }

-- | Set attributes on an exiting selection
-- |
-- | Similar to setAttrs but for selections in the exit phase.
-- | Useful for styling elements before they are removed.
-- |
-- | Example:
-- | ```purescript
-- | setAttrsExit [fill "brown", class_ "exiting"] exitSelection
-- | ```
setAttrsExit
  :: forall datum m
   . MonadEffect m
  => Array (Attribute datum)
  -> Selection SExiting Element datum
  -> m (Selection SExiting Element datum)
setAttrsExit attrs (Selection impl) = liftEffect do
  let
    { elements, data: datumArray, document: doc } = unsafePartial case impl of
      ExitingSelection r -> r
  -- Apply attributes to each element
  let paired = Array.zipWith Tuple datumArray elements
  paired # traverseWithIndex_ \index (Tuple datum element) ->
    applyAttributes element datum index attrs

  pure $ Selection $ ExitingSelection
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
  let
    { elements } = unsafePartial case impl of
      ExitingSelection r -> r
  elements # traverse_ \element -> do
    let node = toNode element
    maybeParent <- Node.parentNode node
    case maybeParent of
      Just parent -> Node.removeChild node parent
      Nothing -> pure unit -- Element not in DOM, nothing to remove

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
  :: forall m
   . MonadEffect m
  => String -- CSS selector
  -> m Unit
clear selector = liftEffect $ clearElement_ selector

-- FFI for clearing an element's children
foreign import clearElement_ :: String -> Effect Unit

-- | Sync DOM transform positions back to __data__.x and __data__.y
-- |
-- | Reads the current transform attribute from each element matching the selector
-- | and updates the bound data's x/y coordinates. Essential for transitioning from
-- | CSS animations to force simulation - ensures simulation sees current positions.
-- |
-- | Example:
-- | ```purescript
-- | -- After tree reveal animation completes:
-- | syncDOMToData "g.nodes > g"  -- Sync group positions to node data
-- | start  -- Simulation continues from current positions
-- | ```
syncDOMToData
  :: forall m
   . MonadEffect m
  => String -- CSS selector for elements to sync
  -> m Unit
syncDOMToData selector = liftEffect $ syncDOMToData_ selector

-- FFI for syncing DOM transforms to __data__
foreign import syncDOMToData_ :: String -> Effect Unit

-- | Append a single child element to a parent selection
-- |
-- | Creates one new element and appends it to each parent in the selection.
-- | Returns an empty selection of the newly created element(s).
-- |
-- | This is different from `append` which creates elements for each datum
-- | in a pending (enter) selection. `appendChild` is for structural elements
-- | like creating an SVG container.
-- |
-- | Example:
-- | ```purescript
-- | container <- select "#viz"
-- | svg <- appendChild SVG [width 400.0, height 150.0] container
-- | circles <- renderData Circle [1, 2, 3] "circle" svg ...
-- | ```
appendChild
  :: forall parent datum datumOut m
   . MonadEffect m
  => ElementType
  -> Array (Attribute datumOut)
  -> Selection SEmpty parent datum
  -> m (Selection SEmpty Element datumOut)
appendChild elemType attrs (Selection impl) = liftEffect do
  let
    { parentElements, document: doc } = unsafePartial case impl of
      EmptySelection r -> r

  -- Create one element for each parent
  elements <- parentElements # traverse \parent -> do
    element <- createElementWithNS elemType doc
    -- Apply attributes with a dummy datum (attributes should be static for appendChild)
    -- We use unit as the datum since structural elements typically don't need data
    let dummyDatum = unsafeCoerce unit :: datumOut
    applyAttributes element dummyDatum 0 attrs
    -- Append to parent
    let elementNode = toNode element
    let parentNode = toNode parent
    Node.appendChild elementNode parentNode
    pure element

  pure $ Selection $ EmptySelection
    { parentElements: elements
    , document: doc
    }

-- | Append child elements with an optional explicit datum for attribute functions
-- | When datumOpt is Nothing, uses a dummy datum (like appendChild)
-- | When datumOpt is Just d, uses d for all datum-dependent attributes
appendChildWithDatum
  :: forall parent datum datumOut m
   . MonadEffect m
  => ElementType
  -> Array (Attribute datumOut)
  -> Maybe datumOut -- Optional datum for attribute functions
  -> Selection SEmpty parent datum
  -> m (Selection SEmpty Element datumOut)
appendChildWithDatum elemType attrs datumOpt (Selection impl) = liftEffect do
  let
    { parentElements, document: doc } = unsafePartial case impl of
      EmptySelection r -> r

  -- Create one element for each parent
  elements <- parentElements # traverse \parent -> do
    element <- createElementWithNS elemType doc
    -- Apply attributes with the provided datum or a dummy if none
    let datum = case datumOpt of
          Just d -> d
          Nothing -> unsafeCoerce unit :: datumOut
    applyAttributes element datum 0 attrs
    -- Append to parent
    let elementNode = toNode element
    let parentNode = toNode parent
    Node.appendChild elementNode parentNode
    pure element

  pure $ Selection $ EmptySelection
    { parentElements: elements
    , document: doc
    }

-- | Append child elements to a data-bound selection, inheriting parent's data
-- |
-- | This is the key function for creating nested SVG structures where children
-- | need access to their parent's bound data. The children don't own the data
-- | binding - they inherit it from their parent.
-- |
-- | Semantics: Each parent element gets one child. The child inherits the parent's
-- | data by copying the __data__ reference (a performance optimization vs. traversing
-- | the DOM tree on every attribute access).
-- |
-- | Type signature documents the data flow:
-- | - Parent must be SBoundOwns (owns the data to inherit from)
-- | - Child is SBoundInherits (borrows parent's data)
-- | - Both parent and child have same datum type
-- |
-- | Example:
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
  :: forall parent datum m
   . MonadEffect m
  => ElementType
  -> Array (Attribute datum)
  -> Selection SBoundOwns parent datum
  -> m (Selection SBoundInherits Element datum)
appendChildInheriting elemType attrs (Selection impl) = liftEffect do
  let
    { elements: parentElements, data: dataArray, document: doc } = unsafePartial case impl of
      BoundSelection r -> r

  -- Create one child for each parent, inheriting the parent's data
  childElements <- Array.zipWith Tuple parentElements dataArray # traverseWithIndex \idx (Tuple parent datum) -> do
    -- Create child element
    child <- createElementWithNS elemType doc

    -- Copy parent's __data__ to child (the "caching" optimization)
    -- This allows child to access data without DOM traversal
    setElementData_ datum child

    -- Apply attributes using the inherited data
    applyAttributes child datum idx attrs

    -- Append child to parent
    let childNode = toNode child
    let parentNode = toNode parent
    Node.appendChild childNode parentNode

    pure child

  -- Return as SBoundInherits selection
  pure $ Selection $ BoundSelection
    { elements: childElements
    , data: dataArray -- Same data as parent
    , indices: Nothing
    , document: doc
    }

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
  => Selection SBoundOwns Element datum
  -> Selection SBoundOwns Element datum
  -> m (Selection SBoundOwns Element datum)
merge (Selection impl1) (Selection impl2) = do
  let
    { elements: els1, data: data1, document: doc } = unsafePartial case impl1 of
      BoundSelection r -> r
  let
    { elements: els2, data: data2 } = unsafePartial case impl2 of
      BoundSelection r -> r
  pure $ Selection $ BoundSelection
    { elements: els1 <> els2
    , data: data1 <> data2
    , indices: Nothing -- Merged selections lose index information
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
  :: forall f parent parentDatum datum m
   . MonadEffect m
  => Foldable f
  => Ord datum
  => f datum
  -> String -- Element selector for existing elements
  -> Selection SEmpty parent parentDatum
  -> m (JoinResult Selection parent datum)
joinData foldableData selector (Selection impl) = liftEffect do
  let
    { parentElements, document: doc } = unsafePartial case impl of
      EmptySelection r -> r
  -- Query for existing elements within parents
  existingElements <- querySelectorAllElements selector parentElements

  -- Get old bindings (elements with their bound data)
  oldBindings <- existingElements # traverse \element -> do
    nullableDatum <- getElementData_ element
    let maybeDatum = toMaybe nullableDatum
    pure $ { element, datum: maybeDatum }

  -- Log what data we found on existing elements
  liftEffect $ log $ "joinData: found " <> show (Array.length existingElements) <> " existing elements with selector '" <> selector <> "'"
  liftEffect $ log $ "joinData: " <> show (Array.length (Array.mapMaybe _.datum oldBindings)) <> " of those have data bound"

  -- Filter to only elements that have data bound
  let
    validOldBindings = oldBindings # Array.mapMaybe \{ element, datum } ->
      datum <#> \d -> { element, datum: d }

  -- Convert foldable to array
  let newDataArray = Array.fromFoldable foldableData

  -- Run pure join algorithm
  let joinSets = Join.computeJoin newDataArray validOldBindings

  -- Build typed selections for each set
  -- Sort enter bindings by newIndex to match the order in the new data
  let sortedEnter = Array.sortBy (\a b -> compare a.newIndex b.newIndex) joinSets.enter

  let
    enterSelection = Selection $ PendingSelection
      { parentElements
      , pendingData: sortedEnter <#> _.datum
      , indices: Just (sortedEnter <#> _.newIndex) -- Preserve logical positions for element creation
      , document: doc
      }

  -- Sort update bindings by newIndex to match the order in the new data
  let sortedUpdate = Array.sortBy (\a b -> compare a.newIndex b.newIndex) joinSets.update

  let
    updateSelection = Selection $ BoundSelection
      { elements: sortedUpdate <#> _.element
      , data: sortedUpdate <#> _.newDatum
      , indices: Just (sortedUpdate <#> _.newIndex) -- Preserve logical positions for transitions
      , document: doc
      }

  let
    exitSelection = Selection $ ExitingSelection
      { elements: joinSets.exit <#> _.element
      , data: joinSets.exit <#> _.datum
      , document: doc
      }

  pure $ JoinResult
    { enter: enterSelection
    , update: updateSelection
    , exit: exitSelection
    }

-- | Low-level data join with custom key function
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
-- | enterEls <- append Line [...] enter
-- | updateEls <- setAttrs [...] update
-- | remove exit
-- | ```
joinDataWithKey
  :: forall f parent parentDatum datum key m
   . MonadEffect m
  => Foldable f
  => Eq key
  => f datum
  -> (datum -> key) -- Key extraction function
  -> String -- Element selector for existing elements
  -> Selection SEmpty parent parentDatum
  -> m (JoinResult Selection parent datum)
joinDataWithKey foldableData keyFn selector (Selection impl) = liftEffect do
  let
    { parentElements, document: doc } = unsafePartial case impl of
      EmptySelection r -> r
  -- Query for existing elements within parents
  existingElements <- querySelectorAllElements selector parentElements

  -- Get old bindings (elements with their bound data)
  oldBindings <- existingElements # traverse \element -> do
    nullableDatum <- getElementData_ element
    let maybeDatum = toMaybe nullableDatum
    pure $ { element, datum: maybeDatum }

  -- Log what data we found on existing elements
  liftEffect $ log $ "joinDataWithKey: found " <> show (Array.length existingElements) <> " existing elements with selector '" <> selector <> "'"
  liftEffect $ log $ "joinDataWithKey: " <> show (Array.length (Array.mapMaybe _.datum oldBindings)) <> " of those have data bound"

  -- Filter to only elements that have data bound
  let
    validOldBindings = oldBindings # Array.mapMaybe \{ element, datum } ->
      datum <#> \d -> { element, datum: d }

  -- Convert foldable to array
  let newDataArray = Array.fromFoldable foldableData

  -- Run pure join algorithm with key function
  let joinSets = Join.computeJoinWithKey newDataArray validOldBindings keyFn

  -- Build typed selections for each set
  -- Sort enter bindings by newIndex to match the order in the new data
  let sortedEnter = Array.sortBy (\a b -> compare a.newIndex b.newIndex) joinSets.enter

  let
    enterSelection = Selection $ PendingSelection
      { parentElements
      , pendingData: sortedEnter <#> _.datum
      , indices: Just (sortedEnter <#> _.newIndex) -- Preserve logical positions for element creation
      , document: doc
      }

  -- Sort update bindings by newIndex to match the order in the new data
  let sortedUpdate = Array.sortBy (\a b -> compare a.newIndex b.newIndex) joinSets.update

  let
    updateSelection = Selection $ BoundSelection
      { elements: sortedUpdate <#> _.element
      , data: sortedUpdate <#> _.newDatum
      , indices: Just (sortedUpdate <#> _.newIndex) -- Preserve logical positions for transitions
      , document: doc
      }

  let
    exitSelection = Selection $ ExitingSelection
      { elements: joinSets.exit <#> _.element
      , data: joinSets.exit <#> _.datum
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
  -> String -- Element selector
  -> Selection SEmpty parent datum
  -> Maybe (datum -> Array (Attribute datum)) -- Enter attributes
  -> Maybe (datum -> Array (Attribute datum)) -- Update attributes
  -> Maybe (datum -> Array (Attribute datum)) -- Exit attributes (applied before removal)
  -> m (Selection SBoundOwns Element datum)
renderData elemType foldableData selector emptySelection enterAttrs updateAttrs exitAttrs = do
  -- Perform the join
  JoinResult { enter, update, exit } <- joinData foldableData selector emptySelection

  -- Handle enter: append elements then apply per-datum attributes if provided
  enterBound <- do
    -- First append all elements with no attributes
    bound <- append elemType [] enter
    -- Then apply per-datum attributes if provided
    case enterAttrs of
      Nothing -> pure bound
      Just mkAttrs -> applyPerDatumAttrs mkAttrs bound

  -- Handle update: apply per-datum attributes if provided
  updateBound <- case updateAttrs of
    Nothing -> pure update
    Just mkAttrs -> applyPerDatumAttrs mkAttrs update

  -- Handle exit: apply attributes then remove
  case exitAttrs of
    Nothing -> remove exit
    Just mkAttrs -> do
      _ <- applyPerDatumAttrs mkAttrs exit
      remove exit

  -- Merge enter and update
  merge enterBound updateBound

-- | Simple data append for initial renders
-- |
-- | A simplified variant of renderData for when you just want to create
-- | elements from data without worrying about enter/update/exit cycles.
-- |
-- | This is perfect for initial renders where you know there are no
-- | existing elements to update or remove.
-- |
-- | Example:
-- | ```purescript
-- | svg <- select "svg"
-- | circles <- appendData Circle [1, 2, 3]
-- |   [radius 10.0, fill "steelblue", cx (\d _ -> d * 100.0)]
-- |   svg
-- | ```
appendData
  :: forall f parent parentDatum datum m
   . MonadEffect m
  => Foldable f
  => ElementType
  -> f datum
  -> Array (Attribute datum)
  -> Selection SEmpty parent parentDatum
  -> m (Selection SBoundOwns Element datum)
appendData elemType foldableData attrs emptySelection = liftEffect do
  -- Extract parent elements and document from the empty selection
  let Selection impl = emptySelection
  let
    { parentElements, document: doc } = unsafePartial case impl of
      EmptySelection r -> r

  -- Convert data to array
  let dataArray = Array.fromFoldable foldableData

  -- Create a pending selection with the data
  let
    pendingSelection = Selection $ PendingSelection
      { parentElements: parentElements
      , pendingData: dataArray
      , indices: Just (Array.range 0 (Array.length dataArray - 1))
      , document: doc
      }

  -- Append elements with attributes
  append elemType attrs pendingSelection

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- | Apply per-datum attributes to a bound selection
-- |
-- | Takes a function that generates attributes for each datum,
-- | and applies those attributes to the corresponding elements.
applyPerDatumAttrs
  :: forall datum m state
   . MonadEffect m
  => (datum -> Array (Attribute datum))
  -> Selection state Element datum
  -> m (Selection state Element datum)
applyPerDatumAttrs mkAttrs (Selection impl) = liftEffect do
  case impl of
    BoundSelection { elements, data: datumArray } -> do
      -- Apply attributes to each (element, datum) pair
      let paired = Array.zipWith Tuple datumArray elements
      paired # traverseWithIndex_ \index (Tuple datum element) -> do
        let attrs = mkAttrs datum
        applyAttributes element datum index attrs
      -- Return the selection unchanged
      pure $ Selection impl
    _ -> pure $ Selection impl -- Non-bound selections: no-op

getDocument :: forall parent datum. SelectionImpl parent datum -> Effect Document
getDocument (EmptySelection { document: doc }) = pure doc
getDocument (BoundSelection { document: doc }) = pure doc
getDocument (PendingSelection { document: doc }) = pure doc
getDocument (ExitingSelection { document: doc }) = pure doc

-- | Create an element with the appropriate namespace
-- | Uses the element's rendering context to determine namespace
createElementWithNS :: ElementType -> Document -> Effect Element
createElementWithNS elemType doc =
  case elementContext elemType of
    SVGContext ->
      -- SVG elements need the SVG namespace
      Document.createElementNS (Just "http://www.w3.org/2000/svg") (elementTypeToString elemType) doc
    HTMLContext ->
      -- HTML elements use default namespace
      Document.createElement (elementTypeToString elemType) doc

-- | Query all matching elements within parent elements
-- | Uses web-dom library functions instead of custom FFI
querySelectorAllElements :: String -> Array Element -> Effect (Array Element)
querySelectorAllElements selector parents = do
  -- Convert each parent to ParentNode and query
  nodeArrays <- parents # traverse \parent -> do
    let parentNode = toParentNode parent
    nodeList <- querySelectorAll (QuerySelector selector) parentNode
    nodes <- NodeList.toArray nodeList
    -- Filter and convert Nodes to Elements
    pure $ Array.mapMaybe fromNode nodes
  -- Flatten the array of arrays
  pure $ Array.concat nodeArrays

-- | Apply a behavior to a single DOM element
-- |
-- | This is the core function that attaches D3 behaviors to elements.
-- | Used by both `on` (for Selection-based API) and `renderTree` (for TreeAPI).
applyBehaviorToElement :: forall datum. Behavior datum -> Element -> Effect Unit
applyBehaviorToElement (Zoom (ZoomConfig { scaleExtent: ScaleExtent scaleMin scaleMax, targetSelector })) element =
  void $ BehaviorFFI.attachZoom_ element scaleMin scaleMax targetSelector
applyBehaviorToElement (Drag SimpleDrag) element =
  void $ BehaviorFFI.attachSimpleDrag_ element unit
applyBehaviorToElement (Drag (SimulationDrag simId)) element =
  -- Look up simulation by ID in the global registry
  void $ BehaviorFFI.attachSimulationDragById_ element simId
applyBehaviorToElement (Drag (SimulationDragNested simId)) element =
  -- For nested datum structure (datum.node is the simulation node)
  void $ BehaviorFFI.attachSimulationDragNestedById_ element simId
applyBehaviorToElement (Click handler) element =
  void $ BehaviorFFI.attachClick_ element handler
applyBehaviorToElement (ClickWithDatum handler) element =
  void $ BehaviorFFI.attachClickWithDatum_ element handler
applyBehaviorToElement (MouseEnter handler) element =
  void $ BehaviorFFI.attachMouseEnter_ element handler
applyBehaviorToElement (MouseLeave handler) element =
  void $ BehaviorFFI.attachMouseLeave_ element handler
applyBehaviorToElement (Highlight { enter, leave }) element =
  void $ BehaviorFFI.attachHighlight_ element enter leave
applyBehaviorToElement (MouseMoveWithInfo handler) element =
  void $ BehaviorFFI.attachMouseMoveWithEvent_ element $ mkEffectFn2 \d evt ->
    handler
      { datum: d
      , clientX: toNumber $ clientX evt
      , clientY: toNumber $ clientY evt
      , pageX: toNumber $ pageX evt
      , pageY: toNumber $ pageY evt
      , offsetX: offsetX evt
      , offsetY: offsetY evt
      }
applyBehaviorToElement (MouseEnterWithInfo handler) element =
  void $ BehaviorFFI.attachMouseEnterWithEvent_ element $ mkEffectFn2 \d evt ->
    handler
      { datum: d
      , clientX: toNumber $ clientX evt
      , clientY: toNumber $ clientY evt
      , pageX: toNumber $ pageX evt
      , pageY: toNumber $ pageY evt
      , offsetX: offsetX evt
      , offsetY: offsetY evt
      }
applyBehaviorToElement (MouseLeaveWithInfo handler) element =
  void $ BehaviorFFI.attachMouseLeaveWithEvent_ element $ mkEffectFn2 \d evt ->
    handler
      { datum: d
      , clientX: toNumber $ clientX evt
      , clientY: toNumber $ clientY evt
      , pageX: toNumber $ pageX evt
      , pageY: toNumber $ pageY evt
      , offsetX: offsetX evt
      , offsetY: offsetY evt
      }

-- | Attach a behavior (zoom, drag, etc.) to a selection
-- |
-- | Works with any selection type - extracts elements and applies D3 behavior.
-- | Returns the selection unchanged to allow chaining.
-- |
-- | Example:
-- | ```purescript
-- | svg <- appendChild SVG [...] container
-- | zoomGroup <- appendChild Group [...] svg
-- | _ <- on (Drag defaultDrag) zoomGroup
-- | _ <- on (Zoom $ defaultZoom (ScaleExtent 0.5 4.0) ".zoom-group") svg
-- | ```
on :: forall state elem datum. Behavior datum -> Selection state elem datum -> Effect (Selection state elem datum)
on behavior selection@(Selection impl) = do
  -- Extract elements from the selection
  let elements = getElements impl

  -- Apply the behavior to each element using the top-level helper
  traverse_ (applyBehaviorToElement behavior) elements

  -- Return selection unchanged
  pure selection
  where
  -- Extract elements from any selection type
  getElements :: SelectionImpl elem datum -> Array Element
  getElements (EmptySelection { parentElements }) = parentElements
  getElements (BoundSelection { elements: els }) = els
  getElements (PendingSelection { parentElements }) = parentElements
  getElements (ExitingSelection { elements: els }) = els

-- | Attach a behavior with simulation access (for SimulationDrag)
-- |
-- | Apply attributes to an element
applyAttributes :: forall datum. Element -> datum -> Int -> Array (Attribute datum) -> Effect Unit
applyAttributes element datum index attrs =
  attrs # traverse_ \attr -> case attr of
    StaticAttr (AttributeName name) value ->
      if name == "textContent" then setTextContent_ (attributeValueToString value) element
      else Element.setAttribute name (attributeValueToString value) element

    DataAttr (AttributeName name) f ->
      let
        val = attributeValueToString (f datum)
      in
        if name == "textContent" then setTextContent_ val element
        else Element.setAttribute name val element

    IndexedAttr (AttributeName name) f ->
      let
        val = attributeValueToString (f datum index)
      in
        if name == "textContent" then setTextContent_ val element
        else Element.setAttribute name val element

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
elementTypeToString Defs = "defs"
elementTypeToString LinearGradient = "linearGradient"
elementTypeToString Stop = "stop"
elementTypeToString Div = "div"
elementTypeToString Span = "span"
elementTypeToString Table = "table"
elementTypeToString Tr = "tr"
elementTypeToString Td = "td"
elementTypeToString Th = "th"
elementTypeToString Tbody = "tbody"
elementTypeToString Thead = "thead"

-- | Convert string to ElementType (inverse of elementTypeToString)
stringToElementType :: String -> ElementType
stringToElementType "circle" = Circle
stringToElementType "rect" = Rect
stringToElementType "path" = Path
stringToElementType "line" = Line
stringToElementType "text" = Text
stringToElementType "g" = Group
stringToElementType "svg" = SVG
stringToElementType "defs" = Defs
stringToElementType "linearGradient" = LinearGradient
stringToElementType "stop" = Stop
stringToElementType "div" = Div
stringToElementType "span" = Span
stringToElementType "table" = Table
stringToElementType "tr" = Tr
stringToElementType "td" = Td
stringToElementType "th" = Th
stringToElementType "tbody" = Tbody
stringToElementType "thead" = Thead
stringToElementType _ = Group -- Default to Group for unknown types

-- | FFI function to set textContent property
foreign import setTextContent_ :: String -> Element -> Effect Unit

-- ============================================================================
-- Transition Helpers for Tree API
-- ============================================================================

-- | Apply a transition with attributes to an array of elements
-- | Used for enter/update phases - elements are animated but not removed
-- |
-- | Supports staggered delays: if config.staggerDelay is set, each element's
-- | delay is: baseDelay + (index * staggerDelay)
applyTransitionToElements
  :: forall datum
   . TransitionConfig
  -> Array (Tuple Element datum) -- Elements with their data
  -> Array (Attribute datum) -- Attributes to transition to
  -> Effect Unit
applyTransitionToElements config elementDatumPairs attrs = do
  let Milliseconds duration = config.duration
  let baseDelay = maybe 0.0 unwrap config.delay
  let stagger = fromMaybe 0.0 config.staggerDelay
  let easingNullable = TransitionFFI.maybeEasingToNullable config.easing

  elementDatumPairs # traverseWithIndex_ \index (Tuple element datum) -> do
    -- Calculate effective delay: baseDelay + (index * staggerDelay)
    let effectiveDelay = baseDelay + (toNumber index * stagger)
    let delayNullable = toNullable (Just effectiveDelay)

    -- Create transition for this element
    transition <- TransitionFFI.createTransition_ duration delayNullable easingNullable element

    -- Apply each attribute to the transition (animates to target value)
    attrs # traverse_ \attr -> case attr of
      StaticAttr (AttributeName name) value ->
        TransitionFFI.transitionSetAttribute_ name (attributeValueToString value) transition

      DataAttr (AttributeName name) f ->
        TransitionFFI.transitionSetAttribute_ name (attributeValueToString (f datum)) transition

      IndexedAttr (AttributeName name) f ->
        TransitionFFI.transitionSetAttribute_ name (attributeValueToString (f datum index)) transition

-- | Apply a transition to a single element with an explicit index
-- | Used when elements need per-element attrs but shared stagger timing
applyTransitionToSingleElement
  :: forall datum
   . TransitionConfig
  -> Int                    -- Explicit index for stagger calculation
  -> Element
  -> datum
  -> Array (Attribute datum)
  -> Effect Unit
applyTransitionToSingleElement config index element datum attrs = do
  let Milliseconds duration = config.duration
  let baseDelay = maybe 0.0 unwrap config.delay
  let stagger = fromMaybe 0.0 config.staggerDelay
  let effectiveDelay = baseDelay + (toNumber index * stagger)
  let delayNullable = toNullable (Just effectiveDelay)
  let easingNullable = TransitionFFI.maybeEasingToNullable config.easing

  -- Create transition for this element
  transition <- TransitionFFI.createTransition_ duration delayNullable easingNullable element

  -- Apply each attribute to the transition
  attrs # traverse_ \attr -> case attr of
    StaticAttr (AttributeName name) value ->
      TransitionFFI.transitionSetAttribute_ name (attributeValueToString value) transition

    DataAttr (AttributeName name) f ->
      TransitionFFI.transitionSetAttribute_ name (attributeValueToString (f datum)) transition

    IndexedAttr (AttributeName name) f ->
      TransitionFFI.transitionSetAttribute_ name (attributeValueToString (f datum index)) transition

-- | Apply a transition with removal to an array of elements
-- | Used for exit phase - elements animate out then are removed from DOM
-- |
-- | Supports staggered delays: if config.staggerDelay is set, each element's
-- | delay is: baseDelay + (index * staggerDelay)
applyExitTransitionToElements
  :: forall datum
   . TransitionConfig
  -> Array (Tuple Element datum) -- Elements with their data
  -> Array (Attribute datum) -- Attributes to transition to before removal
  -> Effect Unit
applyExitTransitionToElements config elementDatumPairs attrs = do
  let Milliseconds duration = config.duration
  let baseDelay = maybe 0.0 unwrap config.delay
  let stagger = fromMaybe 0.0 config.staggerDelay
  let easingNullable = TransitionFFI.maybeEasingToNullable config.easing

  elementDatumPairs # traverseWithIndex_ \index (Tuple element datum) -> do
    -- Calculate effective delay: baseDelay + (index * staggerDelay)
    let effectiveDelay = baseDelay + (toNumber index * stagger)
    let delayNullable = toNullable (Just effectiveDelay)

    -- Create transition for this element
    transition <- TransitionFFI.createTransition_ duration delayNullable easingNullable element

    -- Apply each attribute to the transition
    attrs # traverse_ \attr -> case attr of
      StaticAttr (AttributeName name) value ->
        TransitionFFI.transitionSetAttribute_ name (attributeValueToString value) transition

      DataAttr (AttributeName name) f ->
        TransitionFFI.transitionSetAttribute_ name (attributeValueToString (f datum)) transition

      IndexedAttr (AttributeName name) f ->
        TransitionFFI.transitionSetAttribute_ name (attributeValueToString (f datum index)) transition

    -- Schedule removal after transition completes
    TransitionFFI.transitionRemove_ transition

-- | Extract element-datum pairs from an exiting selection
getExitingElementDatumPairs
  :: forall datum
   . Selection SExiting Element datum
  -> Array (Tuple Element datum)
getExitingElementDatumPairs (Selection impl) =
  let
    { elements, data: datumArray } = unsafePartial case impl of
      ExitingSelection r -> r
  in
    Array.zipWith Tuple elements datumArray

-- | Extract element-datum pairs from a bound selection
getBoundElementDatumPairs
  :: forall datum
   . Selection SBoundOwns Element datum
  -> Array (Tuple Element datum)
getBoundElementDatumPairs (Selection impl) =
  let
    { elements, data: datumArray } = unsafePartial case impl of
      BoundSelection r -> r
  in
    Array.zipWith Tuple elements datumArray

-- | Extract just the elements from a bound selection
getElementsFromBoundSelection
  :: forall datum
   . Selection SBoundOwns Element datum
  -> Array Element
getElementsFromBoundSelection (Selection impl) =
  unsafePartial case impl of
    BoundSelection r -> r.elements

-- ============================================================================
-- Declarative Tree Rendering
-- ============================================================================

-- | Helper: Render a single node with an explicit datum for attribute application
-- | When datumOpt is Just, uses that datum for attribute functions instead of a dummy
renderNodeHelperWithDatum
  :: forall p pd d
   . Ord d -- Needed for joinData
  => Selection SEmpty p pd
  -> Tree d
  -> Maybe d -- Optional datum to use for attribute functions
  -> Effect (Tuple Element (Map String (Selection SBoundOwns Element d)))
renderNodeHelperWithDatum parentSel (Node node) datumOpt = do
  -- Create this element with proper datum for attributes
  childSel <- appendChildWithDatum node.elemType node.attrs datumOpt parentSel

  -- Get the first element from the created selection
  let Selection impl = childSel
  let
    element = case impl of
      EmptySelection rec -> case Array.head rec.parentElements of
        Just el -> el
        Nothing -> unsafePartial $ unsafeCrashWith "renderTree: appendChild returned empty selection"
      _ -> unsafePartial $ unsafeCrashWith "renderTree: appendChild should return EmptySelection"

  -- Attach behaviors to this element
  traverse_ (\behavior -> applyBehaviorToElement behavior element) node.behaviors

  -- Recursively render children, passing datum through
  childMaps <- traverse (\child -> renderNodeHelperWithDatum childSel child datumOpt) node.children
  let combinedChildMap = Array.foldl Map.union Map.empty (map snd childMaps)

  -- Add this node to the map if it has a name
  let
    selectionsMap = case node.name of
      Just name -> Map.insert name (unsafeCoerce childSel :: Selection SBoundOwns Element d) combinedChildMap
      Nothing -> combinedChildMap

  pure $ Tuple element selectionsMap

-- For non-Node cases called via renderNodeHelperWithDatum, delegate to renderNodeHelper
-- (Join cases manage their own data through the join logic, no datum passing needed)
renderNodeHelperWithDatum parentSel tree@(Join _) _ = renderNodeHelper parentSel tree
renderNodeHelperWithDatum parentSel tree@(NestedJoin _) _ = renderNodeHelper parentSel tree
renderNodeHelperWithDatum parentSel tree@(SceneJoin _) _ = renderNodeHelper parentSel tree
renderNodeHelperWithDatum parentSel tree@(SceneNestedJoin _) _ = renderNodeHelper parentSel tree

-- | Helper: Render a single node and its children (no explicit datum)
-- | Returns the created element and accumulated selections map
renderNodeHelper
  :: forall p pd d
   . Ord d -- Needed for joinData
  => Selection SEmpty p pd
  -> Tree d
  -> Effect (Tuple Element (Map String (Selection SBoundOwns Element d)))
-- For Node, delegate to renderNodeHelperWithDatum with Nothing (no datum passed)
renderNodeHelper parentSel (Node node) = renderNodeHelperWithDatum parentSel (Node node) Nothing

-- Render a data join
-- This is where the magic happens: we create N copies of the template,
-- one for each datum, using the actual joinData operation
renderNodeHelper parentSel (Join joinSpec) = do
  -- 1. Perform data join (enter/update/exit)
  JoinResult { enter: enterSel, update: updateSel, exit: exitSel } <- joinData joinSpec.joinData joinSpec.key parentSel

  -- Log join results
  let Selection enterImpl = enterSel
  let Selection updateImpl = updateSel
  let Selection exitImpl = exitSel
  liftEffect $ do
    let
      enterCount = case enterImpl of
        PendingSelection rec -> Array.length rec.pendingData
        _ -> 0
    let
      updateCount = case updateImpl of
        BoundSelection rec -> Array.length rec.data
        _ -> 0
    let
      exitCount = case exitImpl of
        BoundSelection rec -> Array.length rec.data
        ExitingSelection rec -> Array.length rec.data
        _ -> 0
    log $ "Tree API Join '" <> joinSpec.name <> "': enter=" <> show enterCount <> ", update=" <> show updateCount <> ", exit=" <> show exitCount

  -- 2. Handle EXIT: remove exiting elements
  _ <- remove exitSel

  -- 3. Handle ENTER: create new elements from template
  enterElementsAndMaps <- renderTemplatesForPendingSelection joinSpec.template enterSel

  let enterElements = map fst enterElementsAndMaps
  let enterChildMaps = map snd enterElementsAndMaps

  -- 4. Handle UPDATE: update existing element attributes (children not tracked)
  updateElements <- renderTemplatesForBoundSelection joinSpec.template updateSel

  -- 5. Combine enter and update elements
  -- Note: Only ENTER elements contribute child selections (UPDATE only updates root attrs)
  let allElements = enterElements <> updateElements
  let combinedChildMap = Array.foldl Map.union Map.empty enterChildMaps

  -- 6. Get data array and document (from enter or update)
  let Selection enterImpl = enterSel
  let Selection updateImpl = updateSel

  -- Extract document from whichever selection is non-empty
  let
    doc = unsafePartial case enterImpl of
      PendingSelection rec -> rec.document
      _ -> case updateImpl of
        BoundSelection rec -> rec.document

  -- Extract data arrays
  let
    allData = unsafePartial case enterImpl of
      PendingSelection rec -> rec.pendingData
      _ -> []
  let
    updateData = unsafePartial case updateImpl of
      BoundSelection rec -> rec.data
      _ -> []

  -- 7. Create a bound selection from all elements (enter + update)
  let
    boundSel = Selection $ BoundSelection
      { elements: allElements
      , data: allData <> updateData
      , indices: Just (Array.range 0 (Array.length allElements - 1))
      , document: doc
      }

  -- 8. Add the join's collection to the selections map
  let selectionsMap = Map.insert joinSpec.name (unsafeCoerce boundSel) combinedChildMap

  -- 9. Get first element for return value (or dummy if DOM was removed during navigation)
  firstElement <- case Array.head allElements of
    Just el -> pure el
    Nothing -> createElementWithNS Group doc  -- Dummy element, not attached to DOM

  pure $ Tuple firstElement selectionsMap

-- Render a nested data join (with type decomposition)
-- This handles cases like Array (Array a) → rows → cells
renderNodeHelper parentSel (NestedJoin joinSpec) = do
  -- 1. Perform data join on outer data
  JoinResult { enter: enterSel } <- joinData joinSpec.joinData joinSpec.key parentSel

  -- 2. For each outer datum, create the wrapper element and render inner items
  elementsAndMaps <- renderNestedTemplatesForPendingSelection
    joinSpec.decompose
    joinSpec.template
    (stringToElementType joinSpec.key)
    enterSel

  let elements = map fst elementsAndMaps
  let childMaps = map snd elementsAndMaps
  let combinedChildMap = Array.foldl Map.union Map.empty childMaps

  -- 3. Get the data array and document from enter selection
  let Selection enterImpl = enterSel
  let
    { pendingData, document: doc } = unsafePartial case enterImpl of
      PendingSelection rec -> rec

  -- 4. Create a bound selection from the rendered elements
  let
    boundSel = Selection $ BoundSelection
      { elements
      , data: pendingData
      , indices: Just (Array.range 0 (Array.length elements - 1))
      , document: doc
      }

  -- 5. Add the join's collection to the selections map
  let selectionsMap = Map.insert joinSpec.name (unsafeCoerce boundSel) combinedChildMap

  -- 6. Get first element for return value (or dummy if DOM was removed during navigation)
  firstElement <- case Array.head elements of
    Just el -> pure el
    Nothing -> createElementWithNS Group doc  -- Dummy element, not attached to DOM

  pure $ Tuple firstElement selectionsMap

-- Render a scene join with General Update Pattern behavior
-- This implements enter/update/exit with behavior specs
renderNodeHelper parentSel (SceneJoin joinSpec) = do
  -- 1. Perform data join (enter/update/exit)
  -- Use key function if provided, otherwise fall back to Eq-based join
  JoinResult { enter: enterSel, update: updateSel, exit: exitSel } <-
    case joinSpec.keyFn of
      Just keyFn -> joinDataWithKey joinSpec.joinData keyFn joinSpec.key parentSel
      Nothing -> joinData joinSpec.joinData joinSpec.key parentSel

  -- Log join results
  let Selection enterImpl = enterSel
  let Selection updateImpl = updateSel
  let Selection exitImpl = exitSel
  liftEffect $ do
    let
      enterCount = case enterImpl of
        PendingSelection rec -> Array.length rec.pendingData
        _ -> 0
    let
      updateCount = case updateImpl of
        BoundSelection rec -> Array.length rec.data
        _ -> 0
    let
      exitCount = case exitImpl of
        BoundSelection rec -> Array.length rec.data
        ExitingSelection rec -> Array.length rec.data
        _ -> 0
    log $ "Tree API SceneJoin '" <> joinSpec.name <> "': enter=" <> show enterCount <> ", update=" <> show updateCount <> ", exit=" <> show exitCount

  -- 2. Handle EXIT with behavior
  --
  -- EXIT TRANSITION DESIGN:
  -- When a transition is specified, we must NOT apply exit attrs immediately.
  -- D3 transitions work by animating FROM current values TO target values.
  -- If we set attrs before creating the transition, there's nothing to animate.
  -- Instead, we create the transition and let it animate to the exit attrs,
  -- then D3's transition.remove() handles cleanup after animation completes.
  case joinSpec.exitBehavior of
    Just exitBehavior -> do
      case exitBehavior.transition of
        Just transConfig -> do
          let pairs = getExitingElementDatumPairs exitSel
          liftEffect $ applyExitTransitionToElements transConfig pairs exitBehavior.attrs
        Nothing -> do
          -- No transition - apply attrs immediately, then remove
          _ <- setAttrsExit exitBehavior.attrs exitSel
          _ <- remove exitSel
          pure unit
    Nothing -> do
      -- No exit behavior specified, just remove
      _ <- remove exitSel
      pure unit

  -- 3. Handle ENTER with behavior
  --
  -- ENTER TRANSITION DESIGN:
  -- For enter transitions, elements need to start at initialAttrs and animate
  -- to their final template attrs. We achieve this by:
  -- 1. Modifying the template to include initialAttrs at the END of the attrs array
  --    (Array append means last value wins when same attribute appears twice)
  -- 2. Rendering elements with this modified template (they start at initialAttrs)
  -- 3. Creating a transition that animates to the original template attrs
  enterElementsAndMaps <- case joinSpec.enterBehavior of
    Just enterBehavior -> do
      -- Modify template: append initialAttrs so they override template attrs
      -- (last occurrence of an attribute wins when applied to DOM)
      let
        modifiedTemplate datum = case joinSpec.template datum of
          Node nodeSpec -> Node nodeSpec { attrs = nodeSpec.attrs <> enterBehavior.initialAttrs }
          other -> other -- For non-Node trees, can't modify attrs easily

      -- Render with modified template (elements start with initialAttrs)
      rendered <- renderTemplatesForPendingSelection modifiedTemplate enterSel

      -- Apply enter transition to animate from initialAttrs to final template attrs
      case enterBehavior.transition of
        Just transConfig -> do
          let enterElements = map fst rendered
          let Selection pendImpl = enterSel
          let
            pendingData = unsafePartial case pendImpl of
              PendingSelection rec -> rec.pendingData
          let pairs = Array.zipWith Tuple enterElements pendingData
          -- For each element, extract final attrs from original template and animate to them
          -- Use traverseWithIndex_ to get proper stagger index across all elements
          liftEffect $ pairs # traverseWithIndex_ \index (Tuple element datum) -> do
            let
              finalAttrs = case joinSpec.template datum of
                Node nodeSpec -> nodeSpec.attrs
                _ -> [] -- Non-Node templates don't have attrs
            applyTransitionToSingleElement transConfig index element datum finalAttrs
        Nothing -> pure unit

      pure rendered
    Nothing ->
      -- No enter behavior, use base template
      renderTemplatesForPendingSelection joinSpec.template enterSel

  let enterElements = map fst enterElementsAndMaps
  let enterChildMaps = map snd enterElementsAndMaps

  -- 4. Handle UPDATE with behavior (children not tracked, only root attrs updated)
  updateElements <- case joinSpec.updateBehavior of
    Just updateBehavior -> do
      -- Check for transition
      case updateBehavior.transition of
        Just transConfig -> do
          -- Apply attributes via transition (animated)
          -- IMPORTANT: When transitioning, we must NOT call renderTemplatesForBoundSelection
          -- because that would immediately set attrs, overwriting the transition's start values.
          -- Instead, we only animate to the updateBehavior.attrs, which should be the target.
          let pairs = getBoundElementDatumPairs updateSel
          -- Use traverseWithIndex_ to get proper stagger index
          liftEffect $ pairs # traverseWithIndex_ \index (Tuple element datum) ->
            applyTransitionToSingleElement transConfig index element datum updateBehavior.attrs
          -- Just return the existing elements - don't re-render and overwrite
          pure $ getElementsFromBoundSelection updateSel
        Nothing -> do
          -- No transition, apply attributes immediately
          _ <- setAttrs updateBehavior.attrs updateSel
          -- Then render with template to update structure
          renderTemplatesForBoundSelection joinSpec.template updateSel
    Nothing ->
      -- No update behavior, just re-render with template
      renderTemplatesForBoundSelection joinSpec.template updateSel

  -- 5. Combine enter and update elements (same as Join)
  -- Note: Only ENTER elements contribute child selections (UPDATE only updates root attrs)
  let allElements = enterElements <> updateElements
  let combinedChildMap = Array.foldl Map.union Map.empty enterChildMaps

  -- 6. Get data array and document
  let Selection enterImpl = enterSel
  let Selection updateImpl = updateSel

  let
    doc = unsafePartial case enterImpl of
      PendingSelection rec -> rec.document
      _ -> case updateImpl of
        BoundSelection rec -> rec.document

  let
    allData = unsafePartial case enterImpl of
      PendingSelection rec -> rec.pendingData
      _ -> []
  let
    updateData = unsafePartial case updateImpl of
      BoundSelection rec -> rec.data
      _ -> []

  -- 7. Create a bound selection from all elements (enter + update)
  let
    boundSel = Selection $ BoundSelection
      { elements: allElements
      , data: allData <> updateData
      , indices: Just (Array.range 0 (Array.length allElements - 1))
      , document: doc
      }

  -- 8. Add the join's collection to the selections map
  let selectionsMap = Map.insert joinSpec.name (unsafeCoerce boundSel) combinedChildMap

  -- 9. Get first element for return value (or dummy if DOM was removed during navigation)
  firstElement <- case Array.head allElements of
    Just el -> pure el
    Nothing -> createElementWithNS Group doc  -- Dummy element, not attached to DOM

  pure $ Tuple firstElement selectionsMap

-- SceneNestedJoin: Combines NestedJoin's type decomposition with SceneJoin's GUP behaviors
renderNodeHelper parentSel (SceneNestedJoin joinSpec) = do
  -- 1. Decompose outer data into inner data
  --    joinSpec.joinData :: Array outerDatum
  --    joinSpec.decompose :: outerDatum -> Array innerDatum
  --    We need to flatten: Array outerDatum -> Array (Array innerDatum) -> Array innerDatum
  let
    innerData :: Array _ -- Type is erased, but logically Array innerDatum
    innerData = join $ map (unsafeCoerce joinSpec.decompose) joinSpec.joinData

  -- 2. Perform data join on the INNER data (enter/update/exit)
  --    IMPORTANT: We use joinDataWithKey with JSON stringify because types are erased.
  --    The compiler thinks datum is the outer type, but at runtime it's the decomposed inner type.
  --    Using the Ord instance would compare with wrong field names and crash.
  JoinResult { enter: enterSel, update: updateSel, exit: exitSel } <-
    joinDataWithKey innerData jsonStringify_ joinSpec.key parentSel

  -- Log join results (extract counts from selection implementations)
  liftEffect $ do
    let Selection enterImpl = unsafeCoerce enterSel
    let
      enterCount = case enterImpl of
        PendingSelection rec -> Array.length rec.pendingData
        _ -> 0
    let Selection updateImpl = unsafeCoerce updateSel
    let
      updateCount = case updateImpl of
        BoundSelection rec -> Array.length rec.data
        _ -> 0
    let Selection exitImpl = unsafeCoerce exitSel
    let
      exitCount = case exitImpl of
        BoundSelection rec -> Array.length rec.data
        ExitingSelection rec -> Array.length rec.data
        _ -> 0
    log $ "Tree API SceneNestedJoin '" <> joinSpec.name <> "': enter=" <> show enterCount <> ", update=" <> show updateCount <> ", exit=" <> show exitCount

  -- 3. Handle EXIT with behavior
  --
  -- EXIT TRANSITION DESIGN:
  -- When a transition is specified, we must NOT apply exit attrs immediately.
  -- D3 transitions work by animating FROM current values TO target values.
  -- If we set attrs before creating the transition, there's nothing to animate.
  -- Instead, we create the transition and let it animate to the exit attrs,
  -- then D3's transition.remove() handles cleanup after animation completes.
  case joinSpec.exitBehavior of
    Just exitBehavior -> do
      case (unsafeCoerce exitBehavior).transition of
        Just transConfig -> do
          let pairs = getExitingElementDatumPairs exitSel
          liftEffect $ applyExitTransitionToElements transConfig pairs (unsafeCoerce exitBehavior.attrs)
        Nothing -> do
          -- No transition - apply attrs immediately, then remove
          _ <- setAttrsExit (unsafeCoerce exitBehavior.attrs) exitSel
          _ <- remove exitSel
          pure unit
    Nothing -> do
      _ <- remove exitSel
      pure unit

  -- 4. Handle ENTER with behavior
  --
  -- ENTER TRANSITION DESIGN:
  -- For enter transitions, elements need to start at initialAttrs and animate
  -- to their final template attrs. We achieve this by:
  -- 1. Modifying the template to include initialAttrs at the END of the attrs array
  --    (Array append means last value wins when same attribute appears twice)
  -- 2. Rendering elements with this modified template (they start at initialAttrs)
  -- 3. Creating a transition that animates to the original template attrs
  --
  -- TYPE ERASURE NOTE:
  -- SceneNestedJoin uses unsafeCoerce because the outer datum type differs from
  -- the inner (decomposed) datum type. The template and behaviors are typed for
  -- inner datum but we're in a context typed for outer datum. At runtime, the
  -- data IS the correct inner type due to decomposition, so the coercion is safe.
  enterElementsAndMaps <- case joinSpec.enterBehavior of
    Just enterBehavior -> do
      -- Modify template: append initialAttrs so they override template attrs
      -- (last occurrence of an attribute wins when applied to DOM)
      let
        modifiedTemplate datum = case (unsafeCoerce joinSpec.template) datum of
          Node nodeSpec -> Node nodeSpec { attrs = nodeSpec.attrs <> (unsafeCoerce enterBehavior.initialAttrs) }
          other -> other
      rendered <- renderTemplatesForPendingSelection modifiedTemplate enterSel

      -- Apply enter transition to animate from initialAttrs to final template attrs
      case (unsafeCoerce enterBehavior).transition of
        Just transConfig -> do
          let enterElements = map fst rendered
          let Selection pendImpl = enterSel
          let
            pendingData = unsafePartial case pendImpl of
              PendingSelection rec -> rec.pendingData
          let pairs = Array.zipWith Tuple enterElements pendingData
          -- For each element, extract final attrs from original template and animate to them
          -- Use traverseWithIndex_ to get proper stagger index across all elements
          liftEffect $ pairs # traverseWithIndex_ \index (Tuple element datum) -> do
            let
              finalAttrs = case (unsafeCoerce joinSpec.template) datum of
                Node nodeSpec -> nodeSpec.attrs
                _ -> []
            applyTransitionToSingleElement transConfig index element datum finalAttrs
        Nothing -> pure unit

      pure rendered
    Nothing -> renderTemplatesForPendingSelection (unsafeCoerce joinSpec.template) enterSel

  -- 5. Handle UPDATE with behavior (children not tracked, only root attrs updated)
  updateElements <- case joinSpec.updateBehavior of
    Just updateBehavior -> do
      -- Check for transition
      case (unsafeCoerce updateBehavior).transition of
        Just transConfig -> do
          -- Apply attributes via transition (animated)
          -- IMPORTANT: When transitioning, we must NOT call renderTemplatesForBoundSelection
          -- because that would immediately set attrs, overwriting the transition's start values.
          let pairs = getBoundElementDatumPairs updateSel
          liftEffect $ pairs # traverseWithIndex_ \index (Tuple element datum) ->
            applyTransitionToSingleElement transConfig index element datum (unsafeCoerce updateBehavior.attrs)
          -- Just return the existing elements - don't re-render and overwrite
          pure $ getElementsFromBoundSelection updateSel
        Nothing -> do
          -- No transition, apply attributes immediately
          _ <- setAttrs (unsafeCoerce updateBehavior.attrs) updateSel
          renderTemplatesForBoundSelection (unsafeCoerce joinSpec.template) updateSel
    Nothing -> renderTemplatesForBoundSelection (unsafeCoerce joinSpec.template) updateSel

  -- 6-9. Combine results (same as SceneJoin)
  -- Note: Only ENTER elements contribute child selections (UPDATE only updates root attrs)
  let
    enterElements = map fst enterElementsAndMaps
    allElements = enterElements <> updateElements

  let
    enterChildMaps = map snd enterElementsAndMaps
    combinedChildMap = Array.foldl Map.union Map.empty enterChildMaps

  -- Get document from selections
  let Selection enterImpl = unsafeCoerce enterSel
  let Selection updateImpl = unsafeCoerce updateSel

  let
    doc = unsafePartial case enterImpl of
      PendingSelection rec -> rec.document
      _ -> case updateImpl of
        BoundSelection rec -> rec.document

  -- Create bound selection from all elements
  let
    boundSel = Selection $ BoundSelection
      { elements: allElements
      , data: innerData
      , indices: Just (Array.range 0 (Array.length allElements - 1))
      , document: doc
      }

  let selectionsMap = Map.insert joinSpec.name (unsafeCoerce boundSel) combinedChildMap

  -- Get first element for return value (or dummy if DOM was removed during navigation)
  firstElement <- case Array.head allElements of
    Just el -> pure el
    Nothing -> createElementWithNS Group doc  -- Dummy element, not attached to DOM

  pure $ Tuple firstElement selectionsMap

-- | Helper: Render nested templates for pending selection
-- |
-- | For each outer datum:
-- | 1. Create the wrapper element (e.g., TR)
-- | 2. Decompose to get inner data
-- | 3. Render template for each inner datum
renderNestedTemplatesForPendingSelection
  :: forall outerDatum innerDatum parent
   . Ord innerDatum
  => (outerDatum -> Array outerDatum) -- Decomposer (type-erased to outerDatum)
  -> (outerDatum -> Tree outerDatum) -- Template (type-erased to outerDatum)
  -> ElementType -- Wrapper element type
  -> Selection SPending parent outerDatum -- Pending selection (pendingData has type Array outerDatum)
  -> Effect (Array (Tuple Element (Map String (Selection SBoundOwns Element innerDatum))))
renderNestedTemplatesForPendingSelection decomposer templateFn wrapperType pendingSel = do
  let Selection impl = pendingSel
  let
    { parentElements, pendingData, document: doc } = unsafePartial case impl of
      PendingSelection rec -> rec

  traverseWithIndex
    ( \idx outerDatum -> do
        let parentIdx = idx `mod` Array.length parentElements
        case Array.index parentElements parentIdx of
          Nothing -> unsafeCrashWith "renderNestedTemplatesForPendingSelection: no parent elements"
          Just parent -> do
            -- Create the wrapper element (e.g., <tr>)
            wrapperElement <- createElementWithNS wrapperType doc
            let wrapperNode = toNode wrapperElement
            let parentNode = toNode parent
            Node.appendChild wrapperNode parentNode

            -- Decompose to get inner data
            let innerDataArray = decomposer outerDatum

            -- Render template for each inner datum
            -- Type erasure: innerDataArray has Array outerDatum, but contains actual inner data at runtime
            innerMaps <- traverse
              ( \innerDatumErased -> do
                  let innerDatum = unsafeCoerce innerDatumErased :: innerDatum
                  let tree = unsafeCoerce templateFn innerDatumErased :: Tree innerDatum

                  let
                    singleParentSel = Selection $ EmptySelection
                      { parentElements: [ wrapperElement ]
                      , document: doc
                      }

                  -- Pass the inner datum for attribute functions
                  Tuple _ childSelections <- renderNodeHelperWithDatum singleParentSel tree (Just innerDatum)
                  pure childSelections
              )
              innerDataArray

            let combinedInnerMap = Array.foldl Map.union Map.empty innerMaps
            pure $ Tuple wrapperElement combinedInnerMap
    )
    pendingData

-- | Helper: Render templates for each datum in a pending selection
-- |
-- | For each datum in the pending selection:
-- | 1. Call template function to build the tree
-- | 2. Render that tree to create the element
-- | 3. Return the element and its child selections
renderTemplatesForPendingSelection
  :: forall datum parent
   . Ord datum -- Needed for recursive renderNodeHelper calls
  => (datum -> Tree datum) -- Template function
  -> Selection SPending parent datum -- Pending selection from join (pendingData has type Array datum)
  -> Effect (Array (Tuple Element (Map String (Selection SBoundOwns Element datum))))
renderTemplatesForPendingSelection templateFn pendingSel = do
  -- Extract pending data from the selection
  let Selection impl = pendingSel
  let
    { parentElements, pendingData, document: doc } = unsafePartial case impl of
      PendingSelection rec -> rec

  -- Guard: if no parent elements (e.g., DOM removed during navigation), return empty
  -- This prevents crashes when simulations try to render after their container is gone
  if Array.null parentElements then pure []
  else
    -- For each (parent, datum) pair, render the template
    traverseWithIndex
      ( \idx datum -> do
          -- Get the parent element for this datum
          -- Distribute data across parents cyclically if there are more data than parents
          let parentIdx = idx `mod` Array.length parentElements
          case Array.index parentElements parentIdx of
            Nothing -> unsafeCrashWith "renderTemplatesForPendingSelection: no parent elements"
            Just parent -> do
              -- Build the template tree for this datum
              let tree = templateFn datum

              -- Create an empty selection for this single parent
              let
                singleParentSel = Selection $ EmptySelection
                  { parentElements: [ parent ]
                  , document: doc
                  }

              -- Render the tree with the datum for attribute functions
              Tuple element childSelections <- renderNodeHelperWithDatum singleParentSel tree (Just datum)

              -- CRITICAL: Bind the datum to the root element created by the template
              -- This allows future joins to match this element with updated data
              setElementData_ (unsafeCoerce datum) element

              pure $ Tuple element childSelections
      )
      pendingData

-- | Helper: Update existing elements from a template function for each datum in a bound selection
-- |
-- | For UPDATE phase of data join:
-- | 1. Extract elements and data from bound selection
-- | 2. For each element, apply template to update root element attributes
-- | 3. Return the elements (children are NOT updated - use merge for combined selections)
-- |
-- | Note: This only updates root element attributes. Child elements within the template
-- | are not recursively updated. For working with all elements after a data join,
-- | use `merge enterSelection updateSelection` to get a combined selection.
renderTemplatesForBoundSelection
  :: forall datum parent
   . Ord datum
  => (datum -> Tree datum) -- Template function
  -> Selection SBoundOwns parent datum -- Bound selection from join (update set)
  -> Effect (Array Element)
renderTemplatesForBoundSelection templateFn boundSel = do
  -- Extract elements and data from the bound selection
  let Selection impl = boundSel
  let
    { elements, data: dataArray, document: doc } = unsafePartial case impl of
      BoundSelection rec -> rec

  -- For each (element, datum) pair, update the element based on template
  traverseWithIndex
    ( \idx datum -> do
        case Array.index elements idx of
          Nothing -> unsafeCrashWith "renderTemplatesForBoundSelection: index out of bounds"
          Just element -> do
            -- Build the template tree for this datum
            let tree = templateFn datum

            -- Update the element's attributes based on the template
            _ <- updateElementFromTree element datum idx tree doc

            pure element
    )
    dataArray

-- | Helper: Update a single element's attributes from a tree template
updateElementFromTree :: forall datum. Element -> datum -> Int -> Tree datum -> Document -> Effect Unit
updateElementFromTree element datum index tree doc = do
  -- CRITICAL: Bind the datum to the element so future joins can match it
  setElementData_ (unsafeCoerce datum) element

  case tree of
    Node nodeSpec -> do
      -- Apply each attribute from the template to the element
      applyAttributes element datum index nodeSpec.attrs
    _ -> pure unit -- For joins/groups, we don't update attributes directly

-- | Helper: Render children from a template function for each datum in a bound selection
-- |
-- | For each element in the bound selection:
-- | 1. Extract the datum bound to that element
-- | 2. Call template function to build child trees
-- | 3. Render those trees as children of the element
-- | 4. Collect all named selections from all children
appendChildrenFromTemplate
  :: forall datum
   . Ord datum -- Needed for recursive renderNodeHelper calls
  => (datum -> Tree datum) -- Template function
  -> Selection SBoundOwns Element datum -- Parent selection with bound data
  -> Effect (Map String (Selection SBoundOwns Element datum))
appendChildrenFromTemplate templateFn boundSel = do
  -- Extract elements and data from the bound selection
  let Selection impl = boundSel
  let
    { elements, data: dataArray, document: doc } = unsafePartial case impl of
      BoundSelection rec -> rec

  -- For each (element, datum) pair, render children
  childMaps <- traverseWithIndex
    ( \idx datum -> do
        -- Get the element for this datum
        case Array.index elements idx of
          Nothing -> pure Map.empty
          Just element -> do
            -- Build the template tree for this datum
            let childTree = templateFn datum

            -- Create an empty selection for this single element
            let
              singleParentSel = Selection $ EmptySelection
                { parentElements: [ element ]
                , document: doc
                }

            -- Render the child tree with datum for attribute functions
            Tuple _ childSelections <- renderNodeHelperWithDatum singleParentSel childTree (Just datum)
            pure childSelections
    )
    dataArray

  -- Combine all the child selection maps
  pure $ Array.foldl Map.union Map.empty childMaps

-- | Helper: Render children from a nested template with decomposition
-- |
-- | For each element in the bound selection:
-- | 1. Extract the outer datum bound to that element
-- | 2. Apply decomposer to get inner data array
-- | 3. For each inner datum, call template to build child tree
-- | 4. Render those trees as children of the element
-- | 5. Collect all named selections from all children
appendChildrenFromNestedTemplate
  :: forall outerDatum innerDatum
   . Ord innerDatum -- Needed for recursive renderNodeHelper calls
  => (outerDatum -> Array innerDatum) -- Decomposer function
  -> (innerDatum -> Tree innerDatum) -- Template function
  -> Selection SBoundOwns Element outerDatum -- Parent selection with outer data
  -> Effect (Map String (Selection SBoundOwns Element innerDatum))
appendChildrenFromNestedTemplate decomposer templateFn boundSel = do
  -- Extract elements and data from the bound selection
  let Selection impl = boundSel
  let
    { elements, data: outerDataArray, document: doc } = unsafePartial case impl of
      BoundSelection rec -> rec

  -- For each (element, outerDatum) pair, decompose and render inner children
  childMaps <- traverseWithIndex
    ( \idx outerDatum -> do
        -- Get the element for this outer datum
        case Array.index elements idx of
          Nothing -> pure Map.empty
          Just element -> do
            -- Decompose outer datum to get inner data
            let innerDataArray = decomposer outerDatum

            -- For each inner datum, render the template
            innerMaps <- traverse
              ( \innerDatum -> do
                  -- Build the template tree for this inner datum
                  let childTree = templateFn innerDatum

                  -- Create an empty selection for this single element
                  let
                    singleParentSel = Selection $ EmptySelection
                      { parentElements: [ element ]
                      , document: doc
                      }

                  -- Render the child tree with datum for attribute functions
                  Tuple _ childSelections <- renderNodeHelperWithDatum singleParentSel childTree (Just innerDatum)
                  pure childSelections
              )
              innerDataArray

            -- Combine selections from all inner items
            pure $ Array.foldl Map.union Map.empty innerMaps
    )
    outerDataArray

  -- Combine all the child selection maps
  pure $ Array.foldl Map.union Map.empty childMaps

-- | Render a declarative tree structure
-- |
-- | Walks the tree, creates DOM elements, and returns a map of named selections.
-- | This is the core implementation of the declarative API.
renderTree
  :: forall parent parentDatum datum
   . Ord datum -- Needed for data joins
  => Selection SEmpty parent parentDatum
  -> Tree datum
  -> Effect (Map String (Selection SBoundOwns Element datum))
renderTree parent tree = do
  -- Use a State-like pattern to accumulate named selections
  -- Returns (element created, map of named selections in subtree)
  Tuple _ selectionsMap <- renderNodeHelper parent tree
  pure selectionsMap

-- | Extract a named selection from a renderTree result and convert to SEmpty
-- |
-- | This is useful for the two-tree pattern where you need to render different
-- | datum types in sequence:
-- |
-- | ```purescript
-- | axesSelections <- renderTree container axesTree
-- | chartGroup <- reselect "chartGroup" axesSelections
-- | barsSelections <- renderTree chartGroup barsTree
-- | ```
-- |
-- | If the named selection is not found, returns an empty selection.
reselect
  :: forall datum datumOut
   . String -- Name of the selection to extract
  -> Map String (Selection SBoundOwns Element datum)
  -> Effect (Selection SEmpty Element datumOut)
reselect name selectionsMap = do
  doc <- window >>= document <#> toDocument
  case Map.lookup name selectionsMap of
    Just (Selection impl) -> do
      -- Extract the elements from the selection
      -- Note: selections in the map might be EmptySelection (from appendChild)
      -- or BoundSelection (from data joins), so we handle both
      let
        elements = unsafePartial case impl of
          BoundSelection r -> r.elements
          EmptySelection r -> r.parentElements
      -- Return as empty selection (parent for next render)
      pure $ Selection $ EmptySelection
        { parentElements: elements
        , document: doc
        }
    Nothing -> do
      -- Return empty selection with no parents
      pure $ Selection $ EmptySelection
        { parentElements: []
        , document: doc
        }

-- ============================================================================
-- FFI Declarations (D3-specific data binding)
-- ============================================================================

-- | Get data bound to an element (D3-style __data__ property)
-- | Returns Nullable which we convert to Maybe using Data.Nullable.toMaybe
foreign import getElementData_ :: forall datum. Element -> Effect (Nullable datum)

-- | Set data on an element (D3-style __data__ property)
foreign import setElementData_ :: forall datum. datum -> Element -> Effect Unit

-- | JSON stringify for use as key function in joins
-- | This allows comparing objects by their JSON representation
-- | Used when types are erased and we can't rely on Eq instances
foreign import jsonStringify_ :: forall a. a -> String
