module PSD3v2.Interpreter.D3v2
  ( D3v2Selection_
  , D3v2M
  , runD3v2M
  , reselectD3v2
  , queryAllD3v2
  ) where

import Prelude

import Data.Array as Array
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Partial.Unsafe (unsafePartial)
import PSD3v2.Attribute.Types (Attribute(..), AttributeName(..), AttributeValue(..))
import PSD3v2.Capabilities.Selection (class SelectionM)
import PSD3v2.Capabilities.Transition (class TransitionM)
import PSD3v2.Selection.Operations as Ops
import PSD3v2.Selection.Query as Query
import PSD3v2.Selection.Types (Selection(..), SelectionImpl(..), SBoundOwns, SEmpty, JoinResult(..))
import PSD3v2.Transition.FFI as TransitionFFI
import Web.DOM.Element (Element)

-- | Selection type for D3v2 interpreter
-- |
-- | This is just a newtype wrapper around PSD3v2.Selection.Types.Selection
-- | to distinguish it from other interpreter's selection types.
newtype D3v2Selection_ (state :: Type) (parent :: Type) (datum :: Type)
  = D3v2Selection_ (Selection state parent datum)

-- | The D3v2 interpreter monad (without simulation state)
-- |
-- | Wraps Effect to allow for DOM manipulation.
newtype D3v2M a = D3v2M (Effect a)

derive newtype instance Functor D3v2M
derive newtype instance Apply D3v2M
derive newtype instance Applicative D3v2M
derive newtype instance Bind D3v2M
derive newtype instance Monad D3v2M
derive newtype instance MonadEffect D3v2M

-- | Run the D3v2 interpreter
runD3v2M :: D3v2M ~> Effect
runD3v2M (D3v2M eff) = eff

-- | SelectionM instance for D3v2 interpreter
-- |
-- | Delegates all operations to PSD3v2.Selection.Operations,
-- | which uses the phantom types with unsafePartial for safe pattern matching.
instance SelectionM D3v2Selection_ D3v2M where

  select selector = D3v2M do
    sel <- Ops.select selector
    pure $ D3v2Selection_ sel

  selectAll selector (D3v2Selection_ sel) = D3v2M do
    result <- Ops.selectAll selector sel
    pure $ D3v2Selection_ result

  openSelection (D3v2Selection_ sel) selector = D3v2M do
    result <- Ops.selectAll selector sel
    pure $ D3v2Selection_ result

  selectAllWithData selector (D3v2Selection_ sel) = D3v2M do
    result <- Ops.selectAllWithData selector sel
    pure $ D3v2Selection_ result

  renderData elemType foldableData selector (D3v2Selection_ emptySelection) enterAttrs updateAttrs exitAttrs = D3v2M do
    result <- Ops.renderData elemType foldableData selector emptySelection enterAttrs updateAttrs exitAttrs
    pure $ D3v2Selection_ result

  appendData elemType foldableData attrs (D3v2Selection_ emptySelection) = D3v2M do
    result <- Ops.appendData elemType foldableData attrs emptySelection
    pure $ D3v2Selection_ result

  joinData foldableData selector (D3v2Selection_ emptySelection) = D3v2M do
    JoinResult { enter, update, exit } <- Ops.joinData foldableData selector emptySelection
    pure $ JoinResult
      { enter: D3v2Selection_ enter
      , update: D3v2Selection_ update
      , exit: D3v2Selection_ exit
      }

  joinDataWithKey foldableData keyFn selector (D3v2Selection_ emptySelection) = D3v2M do
    JoinResult { enter, update, exit } <- Ops.joinDataWithKey foldableData keyFn selector emptySelection
    pure $ JoinResult
      { enter: D3v2Selection_ enter
      , update: D3v2Selection_ update
      , exit: D3v2Selection_ exit
      }

  updateJoin (D3v2Selection_ emptySelection) _elemType foldableData keyFn selector = D3v2M do
    JoinResult { enter, update, exit } <- Ops.joinDataWithKey foldableData keyFn selector emptySelection
    pure $ JoinResult
      { enter: D3v2Selection_ enter
      , update: D3v2Selection_ update
      , exit: D3v2Selection_ exit
      }

  append elemType attrs (D3v2Selection_ pendingSelection) = D3v2M do
    result <- Ops.append elemType attrs pendingSelection
    pure $ D3v2Selection_ result

  setAttrs attrs (D3v2Selection_ boundSelection) = D3v2M do
    result <- Ops.setAttrs attrs boundSelection
    pure $ D3v2Selection_ result

  setAttrsExit attrs (D3v2Selection_ exitingSelection) = D3v2M do
    result <- Ops.setAttrsExit attrs exitingSelection
    pure $ D3v2Selection_ result

  remove (D3v2Selection_ exitingSelection) = D3v2M do
    Ops.remove exitingSelection

  clear selector = D3v2M do
    Ops.clear selector

  merge (D3v2Selection_ sel1) (D3v2Selection_ sel2) = D3v2M do
    result <- Ops.merge sel1 sel2
    pure $ D3v2Selection_ result

  appendChild elemType attrs (D3v2Selection_ emptySelection) = D3v2M do
    result <- Ops.appendChild elemType attrs emptySelection
    pure $ D3v2Selection_ result

  appendChildInheriting elemType attrs (D3v2Selection_ boundSelection) = D3v2M do
    result <- Ops.appendChildInheriting elemType attrs boundSelection
    pure $ D3v2Selection_ result

  on behavior (D3v2Selection_ selection) = D3v2M do
    result <- Ops.on behavior selection
    pure $ D3v2Selection_ result

  renderTree (D3v2Selection_ parent) tree = D3v2M do
    selectionsMap <- Ops.renderTree parent tree
    pure $ map D3v2Selection_ selectionsMap

-- | TransitionM instance for D3v2 interpreter
-- |
-- | Implements animated transitions using D3's transition engine.
-- | Applies transitions to each element in the bound selection.
instance TransitionM D3v2Selection_ D3v2M where

  withTransition config (D3v2Selection_ selection) attrs = D3v2M do
    -- Extract elements, data, and indices from the bound selection
    let { elements, data: datumArray, indices } = unsafePartial case selection of
          Selection (BoundSelection r) -> r

    -- Get transition configuration
    let Milliseconds duration = config.duration

    -- Apply transition to each element with its corresponding datum and index
    let paired = Array.zipWith (\d e -> {datum: d, element: e}) datumArray elements
    paired # traverseWithIndex_ \arrayIndex { datum, element } -> do
      -- Use logical index from indices array if present, otherwise use array index
      let logicalIndex = case indices of
            Just indexArray -> unsafePartial $ Array.unsafeIndex indexArray arrayIndex
            Nothing -> arrayIndex

      -- Create a D3 transition for this element
      transition <- TransitionFFI.createTransition_
        duration
        (TransitionFFI.maybeMillisecondsToNullable config.delay)
        (TransitionFFI.maybeEasingToNullable config.easing)
        element

      -- Apply each attribute to the transition
      attrs # traverse_ \attr -> case attr of
        StaticAttr (AttributeName name) value ->
          TransitionFFI.transitionSetAttribute_ name (attributeValueToString value) transition

        DataAttr (AttributeName name) f ->
          TransitionFFI.transitionSetAttribute_ name (attributeValueToString (f datum)) transition

        IndexedAttr (AttributeName name) f ->
          TransitionFFI.transitionSetAttribute_ name (attributeValueToString (f datum logicalIndex)) transition

  withTransitionExit config (D3v2Selection_ selection) attrs = D3v2M do
    -- Extract elements and data from the exiting selection
    let { elements, data: datumArray } = unsafePartial case selection of
          Selection (ExitingSelection r) -> r

    -- Get transition configuration
    let Milliseconds duration = config.duration

    -- Apply transition to each element with its corresponding datum and index
    let paired = Array.zipWith (\d e -> {datum: d, element: e}) datumArray elements
    paired # traverseWithIndex_ \index { datum, element } -> do
      -- Create a D3 transition for this element
      transition <- TransitionFFI.createTransition_
        duration
        (TransitionFFI.maybeMillisecondsToNullable config.delay)
        (TransitionFFI.maybeEasingToNullable config.easing)
        element

      -- Apply each attribute to the transition
      attrs # traverse_ \attr -> case attr of
        StaticAttr (AttributeName name) value ->
          TransitionFFI.transitionSetAttribute_ name (attributeValueToString value) transition

        DataAttr (AttributeName name) f ->
          TransitionFFI.transitionSetAttribute_ name (attributeValueToString (f datum)) transition

        IndexedAttr (AttributeName name) f ->
          TransitionFFI.transitionSetAttribute_ name (attributeValueToString (f datum index)) transition

      -- Remove element after transition completes (D3 pattern: transition.remove())
      TransitionFFI.transitionRemove_ transition

  withTransitionStaggered config delayFn (D3v2Selection_ selection) attrs = D3v2M do
    -- Extract elements, data, and indices from the bound selection
    let { elements, data: datumArray, indices } = unsafePartial case selection of
          Selection (BoundSelection r) -> r

    -- Get transition configuration
    let Milliseconds duration = config.duration

    -- Apply transition to each element with its corresponding datum and index
    let paired = Array.zipWith (\d e -> {datum: d, element: e}) datumArray elements
    paired # traverseWithIndex_ \arrayIndex { datum, element } -> do
      -- Use logical index from indices array if present, otherwise use array index
      let logicalIndex = case indices of
            Just indexArray -> unsafePartial $ Array.unsafeIndex indexArray arrayIndex
            Nothing -> arrayIndex

      -- Compute the delay for this element using the delay function
      let Milliseconds elementDelay = delayFn datum logicalIndex

      -- Create a D3 transition for this element with computed delay
      transition <- TransitionFFI.createTransition_
        duration
        (toNullable (Just elementDelay))
        (TransitionFFI.maybeEasingToNullable config.easing)
        element

      -- Apply each attribute to the transition
      attrs # traverse_ \attr -> case attr of
        StaticAttr (AttributeName name) value ->
          TransitionFFI.transitionSetAttribute_ name (attributeValueToString value) transition

        DataAttr (AttributeName name) f ->
          TransitionFFI.transitionSetAttribute_ name (attributeValueToString (f datum)) transition

        IndexedAttr (AttributeName name) f ->
          TransitionFFI.transitionSetAttribute_ name (attributeValueToString (f datum logicalIndex)) transition

  withTransitionExitStaggered config delayFn (D3v2Selection_ selection) attrs = D3v2M do
    -- Extract elements and data from the exiting selection
    let { elements, data: datumArray } = unsafePartial case selection of
          Selection (ExitingSelection r) -> r

    -- Get transition configuration
    let Milliseconds duration = config.duration

    -- Apply transition to each element with its corresponding datum and index
    let paired = Array.zipWith (\d e -> {datum: d, element: e}) datumArray elements
    paired # traverseWithIndex_ \index { datum, element } -> do
      -- Compute the delay for this element using the delay function
      let Milliseconds elementDelay = delayFn datum index

      -- Create a D3 transition for this element with computed delay
      transition <- TransitionFFI.createTransition_
        duration
        (toNullable (Just elementDelay))
        (TransitionFFI.maybeEasingToNullable config.easing)
        element

      -- Apply each attribute to the transition
      attrs # traverse_ \attr -> case attr of
        StaticAttr (AttributeName name) value ->
          TransitionFFI.transitionSetAttribute_ name (attributeValueToString value) transition

        DataAttr (AttributeName name) f ->
          TransitionFFI.transitionSetAttribute_ name (attributeValueToString (f datum)) transition

        IndexedAttr (AttributeName name) f ->
          TransitionFFI.transitionSetAttribute_ name (attributeValueToString (f datum index)) transition

      -- Remove element after transition completes (D3 pattern: transition.remove())
      TransitionFFI.transitionRemove_ transition

-- Helper function to convert AttributeValue to String
attributeValueToString :: AttributeValue -> String
attributeValueToString (StringValue s) = s
attributeValueToString (NumberValue n) = show n
attributeValueToString (BooleanValue b) = show b

-- | Helper function for reselecting from D3v2 renderTree results
-- |
-- | This wraps the `reselect` function from Operations to work with D3v2Selection_ newtype.
reselectD3v2
  :: forall datum datumOut
   . String
  -> Map.Map String (D3v2Selection_ SBoundOwns Element datum)
  -> Effect (D3v2Selection_ SEmpty Element datumOut)
reselectD3v2 name selectionsMap = do
  -- Unwrap D3v2Selection_ to Selection
  let unwrappedMap = map (\(D3v2Selection_ sel) -> sel) selectionsMap
  -- Call reselect
  result <- Ops.reselect name unwrappedMap
  -- Wrap result back in D3v2Selection_
  pure $ D3v2Selection_ result

-- | Query using CSS selector across all named selections
-- |
-- | Like `queryAll` but works with D3v2Selection_ wrapper.
-- | Properly unwraps the newtype before querying.
queryAllD3v2
  :: forall datum datumOut
   . String  -- ^ CSS selector
  -> Map.Map String (D3v2Selection_ SBoundOwns Element datum)
  -> Effect (D3v2Selection_ SEmpty Element datumOut)
queryAllD3v2 selector selectionsMap = do
  -- Unwrap D3v2Selection_ to Selection - CRITICAL!
  let unwrappedMap = map (\(D3v2Selection_ sel) -> sel) selectionsMap
  -- Call queryAll from Query module
  result <- Query.queryAll selector unwrappedMap
  -- Wrap result back in D3v2Selection_
  pure $ D3v2Selection_ result
