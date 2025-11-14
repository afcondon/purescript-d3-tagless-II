module PSD3v2.Interpreter.D3v2
  ( D3v2Selection_
  , D3v2M
  , D3v2SimM
  , runD3v2M
  , runD3v2SimM
  , execD3v2SimM
  , evalD3v2SimM
  ) where

import Prelude

import Control.Monad.State (class MonadState, StateT, runStateT, evalStateT, execStateT)
import Data.Array as Array
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse_)
import Data.Tuple (Tuple, snd, fst)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Partial.Unsafe (unsafePartial)
import PSD3.Internal.Simulation.Types (D3SimulationState_)
import PSD3v2.Attribute.Types (Attribute(..), AttributeName(..), AttributeValue(..))
import PSD3v2.Capabilities.Selection (class SelectionM)
import PSD3v2.Capabilities.Transition (class TransitionM)
import PSD3v2.Selection.Operations as Ops
import PSD3v2.Selection.Types (Selection(..), SelectionImpl(..), SBound, JoinResult(..))
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

-- | The D3v2 simulation monad (with simulation state)
-- |
-- | Like D3v2M but adds StateT for managing simulation state.
-- | Required for force-directed graphs.
newtype D3v2SimM row d a = D3v2SimM (StateT { simulation :: D3SimulationState_ d | row } Effect a)

derive newtype instance Functor (D3v2SimM row d)
derive newtype instance Apply (D3v2SimM row d)
derive newtype instance Applicative (D3v2SimM row d)
derive newtype instance Bind (D3v2SimM row d)
derive newtype instance Monad (D3v2SimM row d)
derive newtype instance MonadEffect (D3v2SimM row d)
derive newtype instance MonadState { simulation :: D3SimulationState_ d | row } (D3v2SimM row d)

-- | Run the D3v2 simulation interpreter and return result + final state
runD3v2SimM :: forall a d row. { simulation :: D3SimulationState_ d | row } -> D3v2SimM row d a -> Effect (Tuple a { simulation :: D3SimulationState_ d | row })
runD3v2SimM state (D3v2SimM st) = runStateT st state

-- | Run the D3v2 simulation interpreter and return only the final state
execD3v2SimM :: forall a d row. { simulation :: D3SimulationState_ d | row } -> D3v2SimM row d a -> Effect { simulation :: D3SimulationState_ d | row }
execD3v2SimM state (D3v2SimM st) = map snd $ runStateT st state

-- | Run the D3v2 simulation interpreter and return only the result
evalD3v2SimM :: forall a d row. { simulation :: D3SimulationState_ d | row } -> D3v2SimM row d a -> Effect a
evalD3v2SimM state (D3v2SimM st) = map fst $ runStateT st state

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

  renderData elemType foldableData selector (D3v2Selection_ emptySelection) enterAttrs updateAttrs exitAttrs = D3v2M do
    result <- Ops.renderData elemType foldableData selector emptySelection enterAttrs updateAttrs exitAttrs
    pure $ D3v2Selection_ result

  joinData foldableData selector (D3v2Selection_ emptySelection) = D3v2M do
    JoinResult { enter, update, exit } <- Ops.joinData foldableData selector emptySelection
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

  merge (D3v2Selection_ sel1) (D3v2Selection_ sel2) = D3v2M do
    result <- Ops.merge sel1 sel2
    pure $ D3v2Selection_ result

  appendChild elemType attrs (D3v2Selection_ emptySelection) = D3v2M do
    result <- Ops.appendChild elemType attrs emptySelection
    pure $ D3v2Selection_ result

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

-- Helper function to convert AttributeValue to String
attributeValueToString :: AttributeValue -> String
attributeValueToString (StringValue s) = s
attributeValueToString (NumberValue n) = show n
attributeValueToString (BooleanValue b) = show b

-- =============================================================================
-- D3v2SimM Instances (SelectionM + TransitionM + SimulationM2)
-- =============================================================================

-- | SelectionM instance for D3v2SimM
-- |
-- | Same as D3v2M but lifted through StateT
instance SelectionM D3v2Selection_ (D3v2SimM row d) where
  select selector = liftEffect $ do
    sel <- Ops.select selector
    pure $ D3v2Selection_ sel

  appendChild elemType attrs (D3v2Selection_ parent) = liftEffect $ do
    child <- Ops.appendChild elemType attrs parent
    pure $ D3v2Selection_ child

  joinData foldable tag (D3v2Selection_ parent) = liftEffect $ do
    JoinResult result <- Ops.joinData foldable tag parent
    pure $ JoinResult
      { enter: D3v2Selection_ result.enter
      , update: D3v2Selection_ result.update
      , exit: D3v2Selection_ result.exit
      }

  append elemType attrs (D3v2Selection_ pending) = liftEffect $ do
    bound <- Ops.append elemType attrs pending
    pure $ D3v2Selection_ bound

  setAttrs attrs (D3v2Selection_ bound) = liftEffect $ do
    bound' <- Ops.setAttrs attrs bound
    pure $ D3v2Selection_ bound'

  setAttrsExit attrs (D3v2Selection_ exiting) = liftEffect $ do
    exiting' <- Ops.setAttrsExit attrs exiting
    pure $ D3v2Selection_ exiting'

  remove (D3v2Selection_ exiting) = liftEffect $
    Ops.remove exiting

  selectAll tag (D3v2Selection_ parent) = liftEffect $ do
    selection <- Ops.selectAll tag parent
    pure $ D3v2Selection_ selection

  renderData elemType foldable tag (D3v2Selection_ parent) enterFn updateFn exitFn = liftEffect $ do
    bound <- Ops.renderData elemType foldable tag parent enterFn updateFn exitFn
    pure $ D3v2Selection_ bound

  merge (D3v2Selection_ sel1) (D3v2Selection_ sel2) = liftEffect $ do
    merged <- Ops.merge sel1 sel2
    pure $ D3v2Selection_ merged

-- | TransitionM instance for D3v2SimM
-- |
-- | Same as D3v2M but lifted through StateT
instance TransitionM D3v2Selection_ (D3v2SimM row d) where
  withTransition transConfig (D3v2Selection_ selection) attrs = liftEffect $
    case selection of
      Selection (BoundSelection { elements, data: datumArray, indices }) -> do
        let Milliseconds duration = transConfig.duration
        let paired = Array.zipWith (\element datum -> { element, datum }) elements datumArray

        paired # traverseWithIndex_ \arrayIndex { datum, element } -> do
          let logicalIndex = case indices of
                Just indexArray -> unsafePartial $ Array.unsafeIndex indexArray arrayIndex
                Nothing -> arrayIndex

          transition <- TransitionFFI.createTransition_
            duration
            (TransitionFFI.maybeMillisecondsToNullable transConfig.delay)
            (TransitionFFI.maybeEasingToNullable transConfig.easing)
            element

          attrs # traverse_ \attr -> case attr of
            StaticAttr (AttributeName name) value ->
              TransitionFFI.transitionSetAttribute_ name (attributeValueToString value) transition

            DataAttr (AttributeName name) fn ->
              TransitionFFI.transitionSetAttribute_ name (attributeValueToString $ fn datum) transition

            IndexedAttr (AttributeName name) fn ->
              TransitionFFI.transitionSetAttribute_ name (attributeValueToString $ fn datum logicalIndex) transition

      _ -> pure unit

  withTransitionExit transConfig (D3v2Selection_ selection) attrs = liftEffect $
    case selection of
      Selection (ExitingSelection { elements }) -> do
        let Milliseconds duration = transConfig.duration

        elements # traverse_ \element -> do
          transition <- TransitionFFI.createTransition_
            duration
            (TransitionFFI.maybeMillisecondsToNullable transConfig.delay)
            (TransitionFFI.maybeEasingToNullable transConfig.easing)
            element

          attrs # traverse_ \attr -> case attr of
            StaticAttr (AttributeName name) value ->
              TransitionFFI.transitionSetAttribute_ name (attributeValueToString value) transition

            _ -> pure unit  -- Exit selections don't have data, so only static attrs work

          TransitionFFI.transitionRemove_ transition

      _ -> pure unit
