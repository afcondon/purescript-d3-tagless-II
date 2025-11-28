-- | EXPERIMENTAL: Selection Query Language
-- |
-- | **Status: Experimental - API may change**
-- |
-- | Provides CSS-style querying across named selections for dynamic selection manipulation.
-- | This module is orthogonal to the core selection API and can be used selectively.
-- |
-- | ## Known Issues and Limitations
-- |
-- | ### 1. D3v2Selection_ Unwrapping
-- |
-- | When using with the D3v2 interpreter, you **cannot** simply `unsafeCoerce` from
-- | `Map String (D3v2Selection_ ...)` to `Map String (Selection ...)`.
-- | The newtype constructor must be properly unwrapped:
-- |
-- | ```purescript
-- | -- ❌ WRONG: unsafeCoerce doesn't properly unwrap newtype constructors
-- | let unwrapped = unsafeCoerce selections
-- | groupCircles <- queryAll ".group-1" unwrapped  -- Returns empty!
-- |
-- | -- ✅ RIGHT: Pattern match to unwrap each selection
-- | let unwrapped = map (\(D3v2Selection_ sel) -> sel) selections
-- | groupCircles <- queryAll ".group-1" unwrapped  -- Works correctly
-- |
-- | -- ✅ BEST: Use the wrapper function
-- | groupCircles <- queryAllD3v2 ".group-1" selections  -- Handles unwrapping
-- | ```
-- |
-- | See `queryAllD3v2` in `PSD3v2.Interpreter.D3v2` for the correct wrapper.
-- |
-- | ### 2. Phantom Type Transitions
-- |
-- | Query functions return `SEmpty` selections (no bound data), but you may know
-- | that elements have data from prior joins. Currently requires `unsafeCoerce`
-- | to transition from `SEmpty` to `SBoundOwns` for attribute operations.
-- |
-- | ### 3. Controlled Coercion
-- |
-- | The `datumOut` type parameter is polymorphic, allowing the caller to specify
-- | the expected datum type. This is safe only if you know what data is bound
-- | to the queried elements. Type safety is enforced by later operations.
-- |
-- | ## Alternative Approaches
-- |
-- | For simpler use cases, consider:
-- | - **Direct DOM API**: Use `document.querySelectorAll` from JavaScript/FFI
-- | - **CSS selector passthrough**: Store selectors as strings, apply later in
-- |   a visualization builder/live-editor context
-- | - **Document-level queries**: Simpler API that searches the whole document
-- |   instead of within named selections
-- |
-- | ## Intended Use Cases
-- |
-- | - Interactive visualization builders with live-editing
-- | - Prototyping with CSS selectors before principled implementation
-- | - Dynamic selection and modification based on data properties
-- | - Testing and debugging selections in development
-- | - Meta-tree editing with selector annotations and lambda attributes
-- |
-- | ## Example Usage
-- |
-- | ```purescript
-- | -- Query within a named selection
-- | activeCircles <- queryIn "nodesGroup" "circle.active" selections
-- |
-- | -- Query across all selections (with D3v2 interpreter)
-- | allCircles <- queryAllD3v2 "circle" selections
-- |
-- | -- Filter by data predicate (requires bound selection)
-- | largeNodes <- filterByData (_.value > 100) nodeCircles
-- |
-- | -- Traverse DOM structure
-- | parents <- parentElements nodeCircles
-- | allDescendants <- descendants "circle" containerGroup
-- |
-- | -- Combine selections
-- | allShapes <- union [circles, rects, paths]
-- | ```
module PSD3v2.Selection.Query
  ( -- * Core Queries
    queryIn
  , queryAll
  , queryFirst
  , queryInBound

  -- * CSS-based Filtering
  , filterByClass
  , filterByAttribute
  , hasClass
  , hasAttribute

  -- * Data-based Filtering
  , filterByData
  , findByData

  -- * DOM Traversal
  , parentElements
  , children
  , descendants
  , siblings
  , ancestors

  -- * Combinators
  , union
  , intersect
  , difference

  -- * Utilities
  , isEmpty
  , size
  , first
  , last
  , nth
  , toArray
  ) where

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import PSD3v2.Selection.Types (SBoundOwns, SEmpty, Selection(..), SelectionImpl(..))
import Partial.Unsafe (unsafePartial)
import Unsafe.Reference (unsafeRefEq)
import Web.DOM.Document (Document)
import Web.DOM.DOMTokenList as DOMTokenList
import Web.DOM.Element (Element, classList, getAttribute, toParentNode, fromNode)
import Web.DOM.Element as Element
import Web.DOM.Node (Node, childNodes, parentNode)
import Web.DOM.NodeList as NodeList
import Web.DOM.ParentNode (QuerySelector(..), querySelector, querySelectorAll)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Extract elements from any selection type
getElements :: forall state elem datum. Selection state elem datum -> Array Element
getElements (Selection impl) = unsafePartial case impl of
  EmptySelection r -> r.parentElements
  BoundSelection r -> r.elements
  PendingSelection r -> r.parentElements
  ExitingSelection r -> r.elements

-- | Extract document from any selection type
getDocument :: forall state elem datum. Selection state elem datum -> Effect Document
getDocument (Selection impl) = case impl of
  EmptySelection r -> pure r.document
  BoundSelection r -> pure r.document
  PendingSelection r -> pure r.document
  ExitingSelection r -> pure r.document

-- | Remove duplicate elements using reference equality
-- | (Elements don't have Ord or Eq instances, so we use unsafeRefEq)
nubElements :: Array Element -> Array Element
nubElements arr = go [] arr
  where
  go acc remaining =
    case Array.uncons remaining of
      Nothing -> acc
      Just { head: x, tail: xs } ->
        let alreadySeen = Array.any (\elem -> unsafeRefEq elem x) acc
        in if alreadySeen then go acc xs
           else go (Array.snoc acc x) xs

-- | Create an empty selection from elements
emptySelectionFromElements :: forall datum. Array Element -> Effect (Selection SEmpty Element datum)
emptySelectionFromElements elements = do
  doc <- window >>= document <#> toDocument
  pure $ Selection $ EmptySelection
    { parentElements: elements
    , document: doc
    }

-- | Query all matching descendants within a set of parent elements
querySelectorAllElements :: String -> Array Element -> Effect (Array Element)
querySelectorAllElements selector parents = do
  nodeArrays <- parents # traverse \parent -> do
    let parentNode = toParentNode parent
    nodeList <- querySelectorAll (QuerySelector selector) parentNode
    nodes <- NodeList.toArray nodeList
    pure $ Array.mapMaybe fromNode nodes
  pure $ Array.concat nodeArrays

-- | Query first matching descendant within a set of parent elements
querySelectorFirstElement :: String -> Array Element -> Effect (Maybe Element)
querySelectorFirstElement selector parents = do
  -- Try each parent in order until we find a match
  let tryParent parent = do
        let parentNode = toParentNode parent
        querySelector (QuerySelector selector) parentNode
  results <- traverse tryParent parents
  pure $ Array.head $ Array.mapMaybe identity results

--------------------------------------------------------------------------------
-- Core Queries
--------------------------------------------------------------------------------

-- | Query within a specific named selection
-- |
-- | Similar to D3's `d3.select("#foo").selectAll("circle.active")`
-- | Returns all matching descendants within the named selection.
-- |
-- | Example:
-- | ```purescript
-- | activeCircles <- queryIn "nodesGroup" "circle.active" selections
-- | ```
queryIn
  :: forall datum datumOut
   . String  -- ^ Name of selection in map
  -> String  -- ^ CSS selector
  -> Map String (Selection SBoundOwns Element datum)
  -> Effect (Selection SEmpty Element datumOut)
queryIn name selector selectionsMap = do
  case Map.lookup name selectionsMap of
    Nothing -> emptySelectionFromElements []
    Just selection -> do
      let elements = getElements selection
      matches <- querySelectorAllElements selector elements
      emptySelectionFromElements matches

-- | Query across all selections in the map
-- |
-- | Finds all elements matching selector in any named selection.
-- | Results are in document order with duplicates removed.
-- |
-- | Example:
-- | ```purescript
-- | allCircles <- queryAll "circle" selections
-- | allActiveElements <- queryAll ".active" selections
-- | ```
queryAll
  :: forall datum datumOut
   . String  -- ^ CSS selector
  -> Map String (Selection SBoundOwns Element datum)
  -> Effect (Selection SEmpty Element datumOut)
queryAll selector selectionsMap = do
  let allSelections = Array.fromFoldable $ Map.values selectionsMap
  let allElements = Array.concat $ map getElements allSelections
  matches <- querySelectorAllElements selector allElements
  -- Remove duplicates using reference equality
  let uniqueMatches = nubElements matches
  emptySelectionFromElements uniqueMatches

-- | Query for first matching element within a named selection
-- |
-- | Like `queryIn` but returns only the first match.
-- |
-- | Example:
-- | ```purescript
-- | firstActive <- queryFirst "nodesGroup" "circle.active" selections
-- | ```
queryFirst
  :: forall datum datumOut
   . String  -- ^ Name of selection in map
  -> String  -- ^ CSS selector
  -> Map String (Selection SBoundOwns Element datum)
  -> Effect (Selection SEmpty Element datumOut)
queryFirst name selector selectionsMap = do
  case Map.lookup name selectionsMap of
    Nothing -> emptySelectionFromElements []
    Just selection -> do
      let elements = getElements selection
      maybeMatch <- querySelectorFirstElement selector elements
      emptySelectionFromElements $ fromMaybe [] (maybeMatch <#> Array.singleton)

-- | Query within a bound selection directly
-- |
-- | Like `selectAll` but with clearer naming for query contexts.
-- |
-- | Example:
-- | ```purescript
-- | circles <- queryInBound "circle" nodeGroup
-- | ```
queryInBound
  :: forall datum datumOut
   . String  -- ^ CSS selector
  -> Selection SBoundOwns Element datum
  -> Effect (Selection SEmpty Element datumOut)
queryInBound selector selection = do
  let elements = getElements selection
  matches <- querySelectorAllElements selector elements
  emptySelectionFromElements matches

--------------------------------------------------------------------------------
-- CSS-based Filtering
--------------------------------------------------------------------------------

-- | Filter selection by CSS class
-- |
-- | Returns only elements that have the specified class.
-- |
-- | Example:
-- | ```purescript
-- | activeNodes <- filterByClass "active" allNodes
-- | ```
filterByClass
  :: forall datumOut
   . String  -- ^ Class name to filter by
  -> Selection SEmpty Element datumOut
  -> Effect (Selection SEmpty Element datumOut)
filterByClass className selection = do
  let elements = getElements selection
  filtered <- elements # Array.filterA \element -> do
    classes <- classList element
    DOMTokenList.contains classes className
  emptySelectionFromElements filtered

-- | Filter selection by attribute value
-- |
-- | Returns only elements where the attribute matches the value.
-- |
-- | Example:
-- | ```purescript
-- | visibleNodes <- filterByAttribute "data-visible" "true" allNodes
-- | ```
filterByAttribute
  :: forall datumOut
   . String  -- ^ Attribute name
  -> String  -- ^ Attribute value to match
  -> Selection SEmpty Element datumOut
  -> Effect (Selection SEmpty Element datumOut)
filterByAttribute attrName attrValue selection = do
  let elements = getElements selection
  filtered <- elements # Array.filterA \element -> do
    maybeValue <- getAttribute attrName element
    pure $ maybeValue == Just attrValue
  emptySelectionFromElements filtered

-- | Check if selection has elements with the given class
-- |
-- | Example:
-- | ```purescript
-- | hasActive <- hasClass "active" nodes
-- | ```
hasClass
  :: forall datum
   . String  -- ^ Class name
  -> Selection SEmpty Element datum
  -> Effect Boolean
hasClass className selection = do
  let elements = getElements selection
  results <- elements # traverse \element -> do
    classes <- classList element
    DOMTokenList.contains classes className
  pure $ Array.any identity results

-- | Check if selection has elements with the given attribute
-- |
-- | Example:
-- | ```purescript
-- | hasDataId <- hasAttribute "data-id" nodes
-- | ```
hasAttribute
  :: forall datum
   . String  -- ^ Attribute name
  -> Selection SEmpty Element datum
  -> Effect Boolean
hasAttribute attrName selection = do
  let elements = getElements selection
  results <- elements # traverse \element ->
    getAttribute attrName element <#> isJust
  pure $ Array.any identity results

--------------------------------------------------------------------------------
-- Data-based Filtering
--------------------------------------------------------------------------------

-- | Filter a bound selection by data predicate
-- |
-- | Returns only elements where the bound data matches the predicate.
-- | This requires a bound selection (SBoundOwns) since we need access to data.
-- |
-- | Example:
-- | ```purescript
-- | largeNodes <- filterByData (_.value > 100) nodeCircles
-- | activeNodes <- filterByData (_.active) nodeCircles
-- | ```
filterByData
  :: forall datum
   . (datum -> Boolean)  -- ^ Predicate on bound data
  -> Selection SBoundOwns Element datum
  -> Effect (Selection SBoundOwns Element datum)
filterByData predicate (Selection impl) = do
  let { elements, data: boundData, indices, document: doc } = unsafePartial case impl of
        BoundSelection r -> r

  -- Filter elements and data together
  let filtered = Array.zipWith Tuple elements boundData
        # Array.filter (\(Tuple _ datum) -> predicate datum)

  let filteredElements = map (\(Tuple elem _) -> elem) filtered
  let filteredData = map (\(Tuple _ datum) -> datum) filtered

  pure $ Selection $ BoundSelection
    { elements: filteredElements
    , data: filteredData
    , indices: indices
    , document: doc
    }

-- | Find first element where data matches predicate
-- |
-- | Like `filterByData` but returns only the first match.
-- |
-- | Example:
-- | ```purescript
-- | selectedNode <- findByData (_.id == nodeId) nodeCircles
-- | ```
findByData
  :: forall datum
   . (datum -> Boolean)
  -> Selection SBoundOwns Element datum
  -> Effect (Selection SBoundOwns Element datum)
findByData predicate (Selection impl) = do
  let { elements, data: boundData, indices, document: doc } = unsafePartial case impl of
        BoundSelection r -> r

  let zipped = Array.zipWith Tuple elements boundData
  let maybeMatch = Array.find (\(Tuple _ datum) -> predicate datum) zipped

  case maybeMatch of
    Nothing -> pure $ Selection $ BoundSelection
      { elements: []
      , data: []
      , indices: indices
      , document: doc
      }
    Just (Tuple elem datum) -> pure $ Selection $ BoundSelection
      { elements: [elem]
      , data: [datum]
      , indices: indices
      , document: doc
      }

--------------------------------------------------------------------------------
-- DOM Traversal
--------------------------------------------------------------------------------

-- | Get parent elements of the selection
-- |
-- | Returns the immediate parent of each element in the selection.
-- | Duplicates are removed.
-- |
-- | Example:
-- | ```purescript
-- | groups <- parentElements circles
-- | ```
parentElements
  :: forall datumOut
   . Selection SEmpty Element datumOut
  -> Effect (Selection SEmpty Element datumOut)
parentElements selection = do
  let elements = getElements selection
  parents <- elements # traverse (Element.toNode >>> parentNode)
  let uniqueParents = nubElements $ Array.mapMaybe (fromNode <=< identity) parents
  emptySelectionFromElements uniqueParents

-- | Get direct children of the selection
-- |
-- | Returns all element children of each element in the selection.
-- |
-- | Example:
-- | ```purescript
-- | childCircles <- children nodeGroups
-- | ```
children
  :: forall datumOut
   . Selection SEmpty Element datumOut
  -> Effect (Selection SEmpty Element datumOut)
children selection = do
  let elements = getElements selection
  childArrays <- elements # traverse \element -> do
    nodeList <- Element.toNode element # childNodes
    nodes <- NodeList.toArray nodeList
    pure $ Array.mapMaybe fromNode nodes
  emptySelectionFromElements $ Array.concat childArrays

-- | Get all descendants matching selector
-- |
-- | Like `queryInBound` but works with empty selections too.
-- |
-- | Example:
-- | ```purescript
-- | allCircles <- descendants "circle" container
-- | ```
descendants
  :: forall datum datumOut
   . String  -- ^ CSS selector
  -> Selection SEmpty Element datum
  -> Effect (Selection SEmpty Element datumOut)
descendants selector selection = do
  let elements = getElements selection
  matches <- querySelectorAllElements selector elements
  emptySelectionFromElements matches

-- | Get sibling elements
-- |
-- | Returns all sibling elements (excluding the elements themselves).
-- |
-- | Example:
-- | ```purescript
-- | otherNodes <- siblings selectedNode
-- | ```
siblings
  :: forall datumOut
   . Selection SEmpty Element datumOut
  -> Effect (Selection SEmpty Element datumOut)
siblings selection = do
  let elements = getElements selection
  -- Helper to check if an element is in the original selection
  let isOriginal elem = Array.any (\orig -> unsafeRefEq elem orig) elements

  siblingArrays <- elements # traverse \element -> do
    maybeParent <- Element.toNode element # parentNode
    case maybeParent of
      Nothing -> pure []
      Just parent -> do
        nodeList <- childNodes parent
        nodes <- NodeList.toArray nodeList
        pure $ Array.mapMaybe fromNode nodes

  -- Remove duplicates and exclude original elements
  let allSiblings = Array.concat siblingArrays
  let uniqueSiblings = nubElements allSiblings
  let filteredSiblings = Array.filter (not <<< isOriginal) uniqueSiblings

  emptySelectionFromElements filteredSiblings

-- | Get all ancestors of the selection
-- |
-- | Returns all ancestor elements up to the document root.
-- | Useful for finding containing groups or SVG elements.
-- |
-- | Example:
-- | ```purescript
-- | containers <- ancestors circle
-- | ```
ancestors
  :: forall datumOut
   . Selection SEmpty Element datumOut
  -> Effect (Selection SEmpty Element datumOut)
ancestors selection = do
  let elements = getElements selection

  -- For each element, walk up the tree collecting ancestors
  ancestorArrays <- elements # traverse \element -> do
    let walkUp :: Element -> Effect (Array Element)
        walkUp elem = do
          maybeParent <- Element.toNode elem # parentNode
          case maybeParent >>= fromNode of
            Nothing -> pure []
            Just parent -> do
              rest <- walkUp parent
              pure $ Array.cons parent rest
    walkUp element

  emptySelectionFromElements $ nubElements $ Array.concat ancestorArrays

--------------------------------------------------------------------------------
-- Combinators
--------------------------------------------------------------------------------

-- | Union (combine) multiple selections
-- |
-- | Merges selections in document order with duplicates removed.
-- |
-- | Example:
-- | ```purescript
-- | allShapes <- union [circles, rects, paths]
-- | ```
union
  :: forall datumOut
   . Array (Selection SEmpty Element datumOut)
  -> Effect (Selection SEmpty Element datumOut)
union selections = do
  let allElements = Array.concat $ map getElements selections
  let uniqueElements = nubElements allElements
  emptySelectionFromElements uniqueElements

-- | Intersection of two selections
-- |
-- | Returns only elements present in both selections.
-- |
-- | Example:
-- | ```purescript
-- | activeVisible <- intersect activeNodes visibleNodes
-- | ```
intersect
  :: forall datumOut
   . Selection SEmpty Element datumOut
  -> Selection SEmpty Element datumOut
  -> Effect (Selection SEmpty Element datumOut)
intersect sel1 sel2 = do
  let elements1 = getElements sel1
  let elements2 = getElements sel2
  -- Use reference equality to check membership
  let isInElements2 elem1 = Array.any (\elem2 -> unsafeRefEq elem1 elem2) elements2
  let intersection = Array.filter isInElements2 elements1
  emptySelectionFromElements intersection

-- | Difference between two selections
-- |
-- | Returns elements in first selection that are not in second.
-- |
-- | Example:
-- | ```purescript
-- | inactiveNodes <- difference allNodes activeNodes
-- | ```
difference
  :: forall datumOut
   . Selection SEmpty Element datumOut
  -> Selection SEmpty Element datumOut
  -> Effect (Selection SEmpty Element datumOut)
difference sel1 sel2 = do
  let elements1 = getElements sel1
  let elements2 = getElements sel2
  -- Use reference equality to check membership
  let isInElements2 elem1 = Array.any (\elem2 -> unsafeRefEq elem1 elem2) elements2
  let diff = Array.filter (not <<< isInElements2) elements1
  emptySelectionFromElements diff

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Check if selection is empty (has no elements)
-- |
-- | Example:
-- | ```purescript
-- | when (isEmpty selection) $ log "No elements found"
-- | ```
isEmpty :: forall state elem datum. Selection state elem datum -> Boolean
isEmpty selection = Array.null $ getElements selection

-- | Get number of elements in selection
-- |
-- | Example:
-- | ```purescript
-- | count <- size circles
-- | log $ "Found " <> show count <> " circles"
-- | ```
size :: forall state elem datum. Selection state elem datum -> Int
size selection = Array.length $ getElements selection

-- | Get first element from selection
-- |
-- | Example:
-- | ```purescript
-- | firstNode <- first allNodes
-- | ```
first
  :: forall datumOut
   . Selection SEmpty Element datumOut
  -> Effect (Selection SEmpty Element datumOut)
first selection = do
  let elements = getElements selection
  emptySelectionFromElements $ fromMaybe [] (Array.head elements <#> Array.singleton)

-- | Get last element from selection
-- |
-- | Example:
-- | ```purescript
-- | lastNode <- last allNodes
-- | ```
last
  :: forall datumOut
   . Selection SEmpty Element datumOut
  -> Effect (Selection SEmpty Element datumOut)
last selection = do
  let elements = getElements selection
  emptySelectionFromElements $ fromMaybe [] (Array.last elements <#> Array.singleton)

-- | Get nth element from selection (0-indexed)
-- |
-- | Example:
-- | ```purescript
-- | thirdNode <- nth 2 allNodes
-- | ```
nth
  :: forall datumOut
   . Int  -- ^ Index (0-based)
  -> Selection SEmpty Element datumOut
  -> Effect (Selection SEmpty Element datumOut)
nth index selection = do
  let elements = getElements selection
  emptySelectionFromElements $ fromMaybe [] (Array.index elements index <#> Array.singleton)

-- | Convert selection to array of elements
-- |
-- | Useful for manual iteration or analysis.
-- |
-- | Example:
-- | ```purescript
-- | elements <- toArray circles
-- | traverse_ processElement elements
-- | ```
toArray :: forall state elem datum. Selection state elem datum -> Array Element
toArray = getElements
