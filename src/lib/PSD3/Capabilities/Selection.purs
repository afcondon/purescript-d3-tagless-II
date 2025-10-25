module PSD3.Capabilities.Selection where

import PSD3.Internal.Attributes.Instances (Label)
import PSD3.Internal.Types (Datum_, Element, Index_, Selector)
import PSD3.Internal.Selection.Types (Behavior, SelectionAttribute)
import Prelude (class Monad, Unit)

-- TODO things that are not handled by this (deliberately) ultra-simple grammar so far:
-- 1) say you wanted to attach to "div#hook" and then select an _already existing_ <h1> in it and apply Attrs to that h1
-- 2)...

class (Monad m) <= SelectionM selection m where
  appendTo        :: selection -> Element -> Array (SelectionAttribute) -> m selection
  selectUnder     :: selection -> Selector selection -> m selection
  attach          :: Selector selection -> m selection
  filterSelection :: selection -> Selector selection -> m selection
  mergeSelections :: selection -> selection -> m selection
  setAttributes   :: selection -> Array (SelectionAttribute) -> m Unit
  on              :: selection -> Behavior selection -> m Unit
  -- `openSelection` hopefully isn't needed and can be folded back into the UpdateJoin somehow (perhaps as a "first time only" thing or "when null")
  openSelection   :: selection -> Selector selection -> m selection
  simpleJoin      :: ∀ datum.  selection -> Element -> (Array datum) -> (Datum_ -> Index_) -> m selection
  updateJoin      :: ∀ datum.  selection -> Element -> (Array datum) -> (Datum_ -> Index_)
    -> m { enter :: selection, exit :: selection, update :: selection }
