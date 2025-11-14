module PSD3v2.Selection.Join
  ( computeJoin
  , ElementBinding(..)
  , UpdateBinding(..)
  , JoinSets(..)
  ) where

import Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Web.DOM.Element (Element)

-- | Represents an element with its bound datum
type ElementBinding datum =
  { element :: Element
  , datum :: datum
  }

-- | Represents an element being updated from old to new datum
type UpdateBinding datum =
  { element :: Element
  , oldDatum :: datum
  , newDatum :: datum
  }

-- | The three disjoint sets resulting from a join
type JoinSets datum =
  { enter :: Array datum
  , update :: Array (UpdateBinding datum)
  , exit :: Array (ElementBinding datum)
  }

-- | Pure join algorithm matching D3 semantics
-- |
-- | This implements the data join pattern:
-- | 1. New data not in old elements -> enter
-- | 2. Data matching existing elements -> update
-- | 3. Old elements not in new data -> exit
-- |
-- | The Ord constraint is used for efficient matching via Map.
-- | Users control identity semantics via their Eq instance:
-- |
-- | ```purescript
-- | instance Eq Node where
-- |   eq (Node a) (Node b) = a.id == b.id  -- Identity by ID
-- | ```
-- |
-- | Time complexity: O(n + m) where n = new data length, m = old elements length
-- | Space complexity: O(m) for the Map
-- |
-- | Properties (see test suite):
-- | - Disjoint: No element appears in multiple sets
-- | - Complete: All new data appears in enter ∪ update
-- | - Complete: All old elements appear in update ∪ exit
-- | - Order: enter and update preserve input order
computeJoin
  :: forall datum
   . Ord datum
  => Array datum                           -- New data to bind
  -> Array (ElementBinding datum)          -- Currently bound elements
  -> JoinSets datum
computeJoin newData oldBindings =
  let
    -- Track which old elements we matched (for exit calculation)
    -- We use a mutable approach via foldl to build matched set
    matchResult = Array.foldl processNewDatum { matched: Map.empty, enter: [], update: [] } newData

    -- Compute exit: old elements not in matched set
    exitBindings = Array.filter (\{ datum } -> not $ Map.member datum matchResult.matched) oldBindings

  in
    { enter: Array.reverse matchResult.enter   -- Reverse to restore input order
    , update: Array.reverse matchResult.update -- Reverse to restore input order
    , exit: exitBindings
    }
  where
    -- Build a map from datum to element for O(1) lookup
    oldMap :: Map.Map datum Element
    oldMap = Map.fromFoldable $ oldBindings <#> \{ datum, element } ->
      Tuple datum element

    processNewDatum state datum =
      case Map.lookup datum oldMap of
        -- Datum not in old elements -> enter
        Nothing ->
          state { enter = Array.cons datum state.enter }

        -- Datum matches old element -> update
        Just element ->
          let
            updateBinding = { element, oldDatum: datum, newDatum: datum }
          in
            state
              { update = Array.cons updateBinding state.update
              , matched = Map.insert datum element state.matched
              }
