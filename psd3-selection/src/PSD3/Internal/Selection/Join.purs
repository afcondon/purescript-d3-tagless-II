module PSD3.Internal.Selection.Join
  ( computeJoin
  , computeJoinWithKey
  , ElementBinding(..)
  , EnterBinding(..)
  , UpdateBinding(..)
  , JoinSets(..)
  ) where

import Prelude

import Data.Array as Array
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Maybe (Maybe(..), fromMaybe)
import Partial.Unsafe (unsafePartial)
import Web.DOM.Element (Element)

-- | Represents an element with its bound datum
type ElementBinding datum =
  { element :: Element
  , datum :: datum
  }

-- | Represents a datum entering with its index in the new data array
type EnterBinding datum =
  { datum :: datum
  , newIndex :: Int  -- Index in the new data array (for positioning)
  }

-- | Represents an element being updated from old to new datum
type UpdateBinding datum =
  { element :: Element
  , oldDatum :: datum
  , newDatum :: datum
  , newIndex :: Int  -- Index in the new data array (for positioning)
  }

-- | The three disjoint sets resulting from a join
type JoinSets datum =
  { enter :: Array (EnterBinding datum)
  , update :: Array (UpdateBinding datum)
  , exit :: Array (ElementBinding datum)
  }

-- | Pure join algorithm matching D3 semantics with support for duplicate datums
-- |
-- | This implements the data join pattern:
-- | 1. New data not in old elements -> enter
-- | 2. Data matching existing elements -> update
-- | 3. Old elements not in new data -> exit
-- |
-- | Handles duplicates correctly by matching in order. For example,
-- | if old data is "HELLO" and new data is "WORLD", the two 'L's in "HELLO"
-- | will match correctly with elements in order.
-- |
-- | Users control identity semantics via their Eq instance:
-- |
-- | ```purescript
-- | instance Eq Node where
-- |   eq (Node a) (Node b) = a.id == b.id  -- Identity by ID
-- | ```
-- |
-- | Time complexity: O(n * m) worst case, where n = new data length, m = old elements length
-- | Space complexity: O(m) for tracking remaining elements
-- |
-- | Properties:
-- | - Disjoint: No element appears in multiple sets
-- | - Complete: All new data appears in enter ∪ update
-- | - Complete: All old elements appear in update ∪ exit
-- | - Order: enter and update preserve input order
-- | - Duplicates: Handles duplicate datums by matching in order
computeJoin
  :: forall datum
   . Eq datum
  => Array datum                           -- New data to bind
  -> Array (ElementBinding datum)          -- Currently bound elements
  -> JoinSets datum
computeJoin newData oldBindings =
  let
    -- Process each new datum with its index, tracking which old elements we've used
    matchResult = foldlWithIndex processNewDatum
      { remainingOld: oldBindings
      , enter: []
      , update: []
      , matchedIndices: []
      }
      newData

    -- Exit: old elements that weren't matched (not in matchedIndices)
    exitBindings = matchResult.remainingOld

  in
    { enter: Array.reverse matchResult.enter   -- Reverse to restore input order
    , update: Array.reverse matchResult.update -- Reverse to restore input order
    , exit: exitBindings
    }
  where
    processNewDatum newIndex state datum =
      -- Find first remaining old element with matching datum
      case Array.findIndex (\{ datum: d } -> d == datum) state.remainingOld of
        -- No match found -> enter (track newIndex for positioning)
        Nothing ->
          let enterBinding = { datum, newIndex }
          in state { enter = Array.cons enterBinding state.enter }

        -- Match found -> update, and remove from remaining
        Just oldIndex ->
          let
            matched = unsafePartial $ Array.unsafeIndex state.remainingOld oldIndex
            newRemaining = fromMaybe state.remainingOld $ Array.deleteAt oldIndex state.remainingOld
            updateBinding =
              { element: matched.element
              , oldDatum: datum
              , newDatum: datum
              , newIndex: newIndex  -- Track the index in the new data array
              }
          in
            state
              { update = Array.cons updateBinding state.update
              , remainingOld = newRemaining
              , matchedIndices = Array.cons oldIndex state.matchedIndices
              }

-- | Pure join algorithm with custom key function
-- |
-- | Like computeJoin, but uses a key function to extract comparable keys
-- | instead of requiring Eq on the data itself.
-- |
-- | This is essential for data types that don't have lawful Eq instances
-- | (e.g., opaque foreign types like D3Link_Swizzled).
-- |
-- | Example:
-- | ```purescript
-- | computeJoinWithKey newLinks oldLinks (\l -> unsafeCoerce l # _.id)
-- | ```
computeJoinWithKey
  :: forall datum key
   . Eq key
  => Array datum                           -- New data to bind
  -> Array (ElementBinding datum)          -- Currently bound elements
  -> (datum -> key)                        -- Key extraction function
  -> JoinSets datum
computeJoinWithKey newData oldBindings keyFn =
  let
    -- Process each new datum with its index, tracking which old elements we've used
    matchResult = foldlWithIndex processNewDatum
      { remainingOld: oldBindings
      , enter: []
      , update: []
      , matchedIndices: []
      }
      newData

    -- Exit: old elements that weren't matched
    exitBindings = matchResult.remainingOld

  in
    { enter: Array.reverse matchResult.enter   -- Reverse to restore input order
    , update: Array.reverse matchResult.update -- Reverse to restore input order
    , exit: exitBindings
    }
  where
    processNewDatum newIndex state datum =
      let newKey = keyFn datum
      in
        -- Find first remaining old element with matching key
        case Array.findIndex (\{ datum: d } -> keyFn d == newKey) state.remainingOld of
          -- No match found -> enter (track newIndex for positioning)
          Nothing ->
            let enterBinding = { datum, newIndex }
            in state { enter = Array.cons enterBinding state.enter }

          -- Match found -> update, and remove from remaining
          Just oldIndex ->
            let
              matched = unsafePartial $ Array.unsafeIndex state.remainingOld oldIndex
              newRemaining = fromMaybe state.remainingOld $ Array.deleteAt oldIndex state.remainingOld
              updateBinding =
                { element: matched.element
                , oldDatum: datum
                , newDatum: datum
                , newIndex: newIndex  -- Track the index in the new data array
                }
            in
              state
                { update = Array.cons updateBinding state.update
                , remainingOld = newRemaining
                , matchedIndices = Array.cons oldIndex state.matchedIndices
                }
