module PSD3v2.Selection.Indexed
  ( IxSelectionM(..)
  , runIxSelectionM
  , select
  , selectAll
  , append
  , setAttrs
  , remove
  , merge
  , joinData
  ) where

import Prelude

import Control.Applicative.Indexed (class IxApplicative)
import Control.Apply.Indexed (class IxApply)
import Control.Bind.Indexed (class IxBind)
import Control.Monad.Indexed (class IxMonad)
import Data.Array as Array
import Data.Functor.Indexed (class IxFunctor)
import Data.Foldable (class Foldable, traverse_)
import Partial.Unsafe (unsafePartial)
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import PSD3v2.Attribute.Types (Attribute(..), AttributeName(..), AttributeValue(..))
import PSD3v2.Selection.Join as Join
import PSD3v2.Selection.Types (ElementType(..), JoinResult(..), SBound, SEmpty, SExiting, SPending, Selection(..), SelectionImpl(..))
import Web.DOM.Document (Document)
import Web.DOM.Element (Element)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)

-- | Indexed monad for type-safe selection operations
-- |
-- | The type parameters track state transitions:
-- | - `i`: Input state (e.g., SPending)
-- | - `o`: Output state (e.g., SBound)
-- | - `a`: Result type
-- |
-- | **What indexed monads give us:**
-- | - Compile-time verification of operation sequencing
-- | - Can't append to a selection that's already bound
-- | - Can't remove a selection that's pending
-- | - State transitions are explicit in types
-- |
-- | **What they DON'T give us:**
-- | - Still need `unsafePartial` for pattern matching (phantom types are erased at runtime)
-- | - The indexed monad wrapper doesn't change this - we're still pattern matching on `SelectionImpl`
-- |
-- | Example:
-- | ```purescript
-- | circles :: IxSelectionM SEmpty SBound (Selection SBound Element Number)
-- | circles =
-- |   select "svg"
-- |   :>>= \svg -> joinData [1, 2, 3] "circle" svg
-- |   :>>= \{ enter, update, exit } -> append Circle [...] enter
-- | ```
-- |
-- | The compiler will reject:
-- | ```purescript
-- | badExample =
-- |   select "svg"
-- |   :>>= \svg -> append Circle [] svg  -- ERROR: Can't append to SEmpty!
-- | ```
newtype IxSelectionM :: forall k1 k2. k1 -> k2 -> Type -> Type
newtype IxSelectionM i o a = IxSelectionM (Effect a)

-- | Extract the Effect from an indexed selection operation
runIxSelectionM :: forall i o a. IxSelectionM i o a -> Effect a
runIxSelectionM (IxSelectionM eff) = eff

-- | IxFunctor instance
instance ixFunctorIxSelectionM :: IxFunctor IxSelectionM where
  imap f (IxSelectionM ma) = IxSelectionM (f <$> ma)

-- | IxApply instance
instance ixApplyIxSelectionM :: IxApply IxSelectionM where
  iapply (IxSelectionM mf) (IxSelectionM ma) = IxSelectionM (mf <*> ma)

-- | IxApplicative instance
instance ixApplicativeIxSelectionM :: IxApplicative IxSelectionM where
  ipure a = IxSelectionM (pure a)

-- | IxBind instance allows composition with :>>=
instance ixBindIxSelectionM :: IxBind IxSelectionM where
  ibind (IxSelectionM ma) f = IxSelectionM do
    a <- ma
    runIxSelectionM (f a)

-- | IxMonad instance (automatically derived from IxApplicative and IxBind)
instance ixMonadIxSelectionM :: IxMonad IxSelectionM

-- ============================================================================
-- Type-Safe Operations Using Indexed Monad
-- ============================================================================

-- | Select a single element (returns SEmpty selection)
select
  :: String  -- CSS selector
  -> IxSelectionM SEmpty SEmpty (Selection SEmpty Element Unit)
select selector = IxSelectionM do
  doc <- window >>= document <#> toDocument
  maybeElement <- querySelector_ selector doc
  pure case maybeElement of
    Nothing -> Selection $ EmptySelection
      { parentElements: []
      , document: doc
      }
    Just element -> Selection $ EmptySelection
      { parentElements: [element]
      , document: doc
      }

-- | Select all elements within a parent
selectAll
  :: forall parent datum
   . String
  -> Selection SEmpty parent datum
  -> IxSelectionM SEmpty SEmpty (Selection SEmpty Element Unit)
selectAll selector (Selection impl) = IxSelectionM do
  let { parentElements, document: doc } = unsafePartial case impl of
        EmptySelection r -> r
  elements <- querySelectorAll_ selector parentElements
  pure $ Selection $ EmptySelection
    { parentElements: elements
    , document: doc
    }

-- | Append elements to a pending (enter) selection
-- |
-- | State transition: SPending → SBound
-- | Now the compiler can verify the input is SPending!
append
  :: forall parent datum
   . ElementType
  -> Array (Attribute datum)
  -> Selection SPending parent datum
  -> IxSelectionM SPending SBound (Selection SBound Element datum)
append elemType attrs (Selection impl) = IxSelectionM do
  let { parentElements, pendingData, indices, document: doc } = unsafePartial case impl of
        PendingSelection r -> r
  -- Create elements for each datum
  let paired = Array.zipWith Tuple pendingData parentElements
  elements <- paired # traverseWithIndex \arrayIndex (Tuple datum parent) -> do
    -- Use logical index from indices array if present (for enter selections from joins)
    let logicalIndex = case indices of
          Just indexArray -> unsafePartial $ Array.unsafeIndex indexArray arrayIndex
          Nothing -> arrayIndex

    element <- createElement_ (elementTypeToString elemType) doc
    -- Set attributes on the new element using logical index
    applyAttributes element datum logicalIndex attrs
    -- Bind data to element
    setElementData_ datum element
    -- Append to parent
    appendChild_ element parent
    pure element

  pure $ Selection $ BoundSelection
    { elements
    , data: pendingData
    , indices  -- Preserve indices from pending selection (for enter selections from joins)
    , document: doc
    }

-- | Set attributes on a bound selection
-- |
-- | State transition: SBound → SBound
setAttrs
  :: forall datum
   . Array (Attribute datum)
  -> Selection SBound Element datum
  -> IxSelectionM SBound SBound (Selection SBound Element datum)
setAttrs attrs (Selection impl) = IxSelectionM do
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

-- | Remove elements from an exit selection
-- |
-- | State transition: SExiting → SExiting (or could be SExiting → SEmpty)
remove
  :: forall datum
   . Selection SExiting Element datum
  -> IxSelectionM SExiting SExiting Unit
remove (Selection impl) = IxSelectionM do
  let { elements } = unsafePartial case impl of
        ExitingSelection r -> r
  elements # traverse_ \element ->
    removeElement_ element

-- | Merge two bound selections
-- |
-- | State transition: SBound → SBound
merge
  :: forall datum
   . Selection SBound Element datum
  -> Selection SBound Element datum
  -> IxSelectionM SBound SBound (Selection SBound Element datum)
merge (Selection impl1) (Selection impl2) = IxSelectionM do
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

-- | Low-level data join
-- |
-- | State transition: SEmpty → JoinResult
-- | The JoinResult contains selections in different states (SPending, SBound, SExiting)
joinData
  :: forall f parent datum
   . Foldable f
  => Ord datum
  => f datum
  -> String  -- Element selector for existing elements
  -> Selection SEmpty parent datum
  -> IxSelectionM SEmpty SEmpty (JoinResult Selection parent datum)
joinData foldableData selector (Selection impl) = IxSelectionM do
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

-- ============================================================================
-- Helper Functions
-- ============================================================================

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
