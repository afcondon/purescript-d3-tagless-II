module PSD3v2.Selection.Operations
  ( select
  , selectAll
  , append
  , appendChild
  , setAttrs
  , setAttrsExit
  , remove
  , merge
  , joinData
  , renderData
  , on
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (class Foldable, traverse_)
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)
import PSD3v2.Attribute.Types (Attribute(..), AttributeName(..), AttributeValue(..))
import PSD3v2.Behavior.Types (Behavior(..), DragConfig(..), ZoomConfig(..), ScaleExtent(..))
import PSD3v2.Behavior.FFI as BehaviorFFI
import PSD3v2.Selection.Join as Join
import PSD3v2.Selection.Types (ElementType(..), JoinResult(..), SBound, SEmpty, SExiting, SPending, Selection(..), SelectionImpl(..))
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
  => String  -- CSS selector
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
      { parentElements: [element]
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
  => String  -- CSS selector
  -> Selection state parent parentDatum
  -> m (Selection SEmpty Element datum)
selectAll selector (Selection impl) = liftEffect do
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
  let { parentElements, pendingData, indices, document: doc } = unsafePartial case impl of
        PendingSelection r -> r
  -- Create elements for each datum
  -- In D3, if there's one parent, all elements go to that parent
  -- If there are multiple parents, they're distributed (but that's rare)
  let parent = case Array.head parentElements of
        Just p -> p
        Nothing -> unsafePartial $ Array.unsafeIndex parentElements 0  -- Should never happen

  elements <- pendingData # traverseWithIndex \arrayIndex datum -> do
    -- Use logical index from indices array if present (for enter selections from joins)
    let logicalIndex = case indices of
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
    , indices  -- Preserve indices from pending selection (for enter selections from joins)
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
  let { elements, data: datumArray, indices, document: doc } = unsafePartial case impl of
        BoundSelection r -> r
  -- Apply attributes to each element, using logical indices if present
  let paired = Array.zipWith Tuple datumArray elements
  paired # traverseWithIndex_ \arrayIndex (Tuple datum element) -> do
    let logicalIndex = case indices of
          Just indexArray -> unsafePartial $ Array.unsafeIndex indexArray arrayIndex
          Nothing -> arrayIndex
    applyAttributes element datum logicalIndex attrs

  pure $ Selection $ BoundSelection
    { elements
    , data: datumArray
    , indices  -- Preserve indices from input selection
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
  let { elements, data: datumArray, document: doc } = unsafePartial case impl of
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
  let { elements } = unsafePartial case impl of
        ExitingSelection r -> r
  elements # traverse_ \element -> do
    let node = toNode element
    maybeParent <- Node.parentNode node
    case maybeParent of
      Just parent -> Node.removeChild node parent
      Nothing -> pure unit  -- Element not in DOM, nothing to remove

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
  let { parentElements, document: doc } = unsafePartial case impl of
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
    , indices: Nothing  -- Merged selections lose index information
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
  -> m (JoinResult Selection parent datum)
joinData foldableData selector (Selection impl) = liftEffect do
  let { parentElements, document: doc } = unsafePartial case impl of
        EmptySelection r -> r
  -- Query for existing elements within parents
  existingElements <- querySelectorAllElements selector parentElements

  -- Get old bindings (elements with their bound data)
  oldBindings <- existingElements # traverse \element -> do
    nullableDatum <- getElementData_ element
    let maybeDatum = toMaybe nullableDatum
    pure $ { element, datum: maybeDatum }

  -- Filter to only elements that have data bound
  let validOldBindings = oldBindings # Array.mapMaybe \{ element, datum } ->
        datum <#> \d -> { element, datum: d }

  -- Convert foldable to array
  let newDataArray = Array.fromFoldable foldableData

  -- Run pure join algorithm
  let joinSets = Join.computeJoin newDataArray validOldBindings

  -- Build typed selections for each set
  -- Sort enter bindings by newIndex to match the order in the new data
  let sortedEnter = Array.sortBy (\a b -> compare a.newIndex b.newIndex) joinSets.enter

  let enterSelection = Selection $ PendingSelection
        { parentElements
        , pendingData: sortedEnter <#> _.datum
        , indices: Just (sortedEnter <#> _.newIndex)  -- Preserve logical positions for element creation
        , document: doc
        }

  -- Sort update bindings by newIndex to match the order in the new data
  let sortedUpdate = Array.sortBy (\a b -> compare a.newIndex b.newIndex) joinSets.update

  let updateSelection = Selection $ BoundSelection
        { elements: sortedUpdate <#> _.element
        , data: sortedUpdate <#> _.newDatum
        , indices: Just (sortedUpdate <#> _.newIndex)  -- Preserve logical positions for transitions
        , document: doc
        }

  let exitSelection = Selection $ ExitingSelection
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
  -> String  -- Element selector
  -> Selection SEmpty parent datum
  -> Maybe (datum -> Array (Attribute datum))  -- Enter attributes
  -> Maybe (datum -> Array (Attribute datum))  -- Update attributes
  -> Maybe (datum -> Array (Attribute datum))  -- Exit attributes (applied before removal)
  -> m (Selection SBound Element datum)
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
    _ -> pure $ Selection impl  -- Non-bound selections: no-op

getDocument :: forall parent datum. SelectionImpl parent datum -> Effect Document
getDocument (EmptySelection { document: doc }) = pure doc
getDocument (BoundSelection { document: doc }) = pure doc
getDocument (PendingSelection { document: doc }) = pure doc
getDocument (ExitingSelection { document: doc }) = pure doc

-- | Create an element with the appropriate namespace
-- | SVG elements must be created in the SVG namespace to preserve case-sensitive attributes
createElementWithNS :: ElementType -> Document -> Effect Element
createElementWithNS elemType doc =
  case elemType of
    -- SVG elements need the SVG namespace
    Circle -> Document.createElementNS (Just "http://www.w3.org/2000/svg") "circle" doc
    Rect -> Document.createElementNS (Just "http://www.w3.org/2000/svg") "rect" doc
    Path -> Document.createElementNS (Just "http://www.w3.org/2000/svg") "path" doc
    Line -> Document.createElementNS (Just "http://www.w3.org/2000/svg") "line" doc
    Text -> Document.createElementNS (Just "http://www.w3.org/2000/svg") "text" doc
    Group -> Document.createElementNS (Just "http://www.w3.org/2000/svg") "g" doc
    SVG -> Document.createElementNS (Just "http://www.w3.org/2000/svg") "svg" doc
    -- HTML elements use default namespace
    Div -> Document.createElement "div" doc
    Span -> Document.createElement "span" doc

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
on :: forall state elem datum. Behavior -> Selection state elem datum -> Effect (Selection state elem datum)
on behavior selection@(Selection impl) = do
  -- Extract elements from the selection
  let elements = getElements impl

  -- Apply the behavior to each element
  traverse_ (applyBehavior behavior) elements

  -- Return selection unchanged
  pure selection
  where
    -- Extract elements from any selection type
    getElements :: SelectionImpl elem datum -> Array Element
    getElements (EmptySelection { parentElements }) = parentElements
    getElements (BoundSelection { elements: els }) = els
    getElements (PendingSelection { parentElements }) = parentElements
    getElements (ExitingSelection { elements: els }) = els

    -- Apply behavior to a single element
    applyBehavior :: Behavior -> Element -> Effect Unit
    applyBehavior (Zoom (ZoomConfig { scaleExtent: ScaleExtent scaleMin scaleMax, targetSelector })) element =
      void $ BehaviorFFI.attachZoom_ element scaleMin scaleMax targetSelector
    applyBehavior (Drag (DragConfig _)) element =
      void $ BehaviorFFI.attachDrag_ element unit

-- | Apply attributes to an element
applyAttributes :: forall datum. Element -> datum -> Int -> Array (Attribute datum) -> Effect Unit
applyAttributes element datum index attrs =
  attrs # traverse_ \attr -> case attr of
    StaticAttr (AttributeName name) value ->
      if name == "textContent"
        then setTextContent_ (attributeValueToString value) element
        else Element.setAttribute name (attributeValueToString value) element

    DataAttr (AttributeName name) f ->
      let val = attributeValueToString (f datum)
      in if name == "textContent"
           then setTextContent_ val element
           else Element.setAttribute name val element

    IndexedAttr (AttributeName name) f ->
      let val = attributeValueToString (f datum index)
      in if name == "textContent"
           then setTextContent_ val element
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
elementTypeToString Div = "div"
elementTypeToString Span = "span"

-- | FFI function to set textContent property
foreign import setTextContent_ :: String -> Element -> Effect Unit

-- ============================================================================
-- FFI Declarations (D3-specific data binding)
-- ============================================================================

-- | Get data bound to an element (D3-style __data__ property)
-- | Returns Nullable which we convert to Maybe using Data.Nullable.toMaybe
-- | This is the only custom FFI we need - everything else uses web-dom library
foreign import getElementData_ :: forall datum. Element -> Effect (Nullable datum)

-- | Set data on an element (D3-style __data__ property)
foreign import setElementData_ :: forall datum. datum -> Element -> Effect Unit
