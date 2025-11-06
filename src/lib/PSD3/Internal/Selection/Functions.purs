module PSD3.Internal.Selection.Functions where

import PSD3.Internal.Types (D3Selection_, Datum_, Element, Index_, Selector)
import PSD3.Internal.FFI (d3Append_, d3AttachZoomDefaultExtent_, d3AttachZoom_, d3DataWithFunction_, d3DataWithKeyFunction_, d3EnterAndAppend_, d3FilterSelection_, d3GetEnterSelection_, d3GetExitSelection_, d3MergeSelectionWith_, d3SelectAllInDOM_, d3SelectionSelectAll_)
import PSD3.Internal.Selection.Types (Behavior(..), SelectionAttribute, applySelectionAttributeD3)
import PSD3.Internal.Zoom (ScaleExtent(..), ZoomExtent(..))
import PSD3.Capabilities.Selection (class SelectionM)
import Data.Array (fromFoldable)
import Data.Foldable (class Foldable, foldl)
import Debug (spy)
import Prelude (Unit, discard, pure, show, unit, ($), (>>>))
import Unsafe.Coerce (unsafeCoerce)


selectionAttach :: forall d m. (SelectionM D3Selection_ m) => Selector (D3Selection_ d) -> m (D3Selection_ d)
selectionAttach selector = pure $ d3SelectAllInDOM_ selector 

selectionSelectUnder :: forall d m. (SelectionM D3Selection_ m) => D3Selection_ d -> Selector (D3Selection_ d) -> m (D3Selection_ d)
selectionSelectUnder selection selector = pure $ d3SelectionSelectAll_ selector selection

selectionAppendElement :: forall d m. (SelectionM D3Selection_ m) => D3Selection_ d -> Element -> Array (SelectionAttribute d) -> m (D3Selection_ d)
selectionAppendElement selection_ element attributes = do
  let appended_ = d3Append_ (show element) selection_
  selectionModifySelection appended_ attributes
  pure appended_

selectionFilterSelection :: forall d m. (SelectionM D3Selection_ m) => D3Selection_ d -> Selector (D3Selection_ d) -> m (D3Selection_ d)
selectionFilterSelection selection_ selector = pure $ d3FilterSelection_ selection_ selector

selectionModifySelection :: forall d m. (SelectionM D3Selection_ m) => D3Selection_ d -> Array (SelectionAttribute d) -> m Unit
selectionModifySelection selection_ attributes = do
  let _ = foldl applySelectionAttributeD3 selection_ attributes
  pure unit

selectionJoin   :: forall d datum m. (SelectionM D3Selection_ m) => D3Selection_ d -> Element -> (Array datum) -> (Datum_ -> Index_) -> m (D3Selection_ datum)
selectionJoin selection e theData keyFn = do
  let
    element         = spy "Join: " $ show e
    selectS         = d3SelectionSelectAll_ element selection
    dataSelection   = d3DataWithKeyFunction_ theData keyFn selectS
    enterSelection  = d3EnterAndAppend_ element dataSelection
  pure enterSelection

selectionNestedJoin :: forall d f datum m. Foldable f => (SelectionM D3Selection_ m) =>
  D3Selection_ d -> Element -> (Datum_ -> f datum) -> (Datum_ -> Index_) -> m (D3Selection_ datum)
selectionNestedJoin selection e extractChildren keyFn = do
  let
    element = spy "NestedJoin: " $ show e
    selectS = d3SelectionSelectAll_ element selection
    -- Convert Foldable to Array and coerce datum to Datum_ for D3
    extractFn = extractChildren >>> fromFoldable >>> unsafeCoerce
    dataSelection = d3DataWithFunction_ extractFn keyFn selectS
    enterSelection = d3EnterAndAppend_ element dataSelection
  pure enterSelection

selectionUpdateJoin   :: forall d datum m.
  (SelectionM D3Selection_ m) =>
  D3Selection_ d ->
  Element -> (Array datum) ->
  (Datum_ -> Index_) ->
  m { enter :: D3Selection_ datum, exit :: D3Selection_ d, update :: D3Selection_ datum }
selectionUpdateJoin openSelection e theData keyFn = do
  let
    -- REVIEW use these FFI function to decompose the update Selection into it's component parts
    updateSelection  = d3DataWithKeyFunction_ theData keyFn openSelection
    enterSelection   = d3GetEnterSelection_ updateSelection
    exitSelection    = d3GetExitSelection_ updateSelection
    
  pure { enter: enterSelection, exit: exitSelection, update: updateSelection }

selectionOpenSelection :: forall d m. (SelectionM D3Selection_ m) => D3Selection_ d -> Selector (D3Selection_ d) -> m (D3Selection_ d)
selectionOpenSelection selection selector = do
  let _ = spy "open selection: " $ selector
  pure $ d3SelectionSelectAll_ selector selection

selectionMergeSelections :: forall d m. (SelectionM D3Selection_ m) => D3Selection_ d -> D3Selection_ d -> m (D3Selection_ d)
selectionMergeSelections selectionA selectionB = pure $ d3MergeSelectionWith_ selectionA selectionB

selectionOn :: forall d m. (SelectionM D3Selection_ m) => D3Selection_ d -> Behavior (D3Selection_ d) -> m Unit
selectionOn selection (Drag drag) = do
-- TODO need to provide the simpler, non-simulation version here
  -- let _ = case drag of 
  --           DefaultDrag     -> defaultDrag_ selection 
  --           NoDrag          -> disableDrag_ selection
  --           (CustomDrag fn) -> defaultDrag_ selection simulation_ -- TODO no custom drag implemented yet
  pure unit

selectionOn selection (Zoom config) = do
  let
    (ScaleExtent smallest largest) = config.scale
    -- Use the target from config to "direct" the zoom to a specific element
    -- This allows zoom to be attached to one element (e.g., svg) while transforms
    -- are applied to a different element (e.g., inner group)
    target = config.target

  -- sticking to the rules of no ADT's on the JS side we case on the ZoomExtent here
    _ = case config.extent of
          DefaultZoomExtent ->
            d3AttachZoomDefaultExtent_ selection {
              scaleExtent: [ smallest, largest ]
            , name  : config.name
            , target
            }

          (ZoomExtent ze)   -> do
            d3AttachZoom_ selection {
              extent     : [ [ ze.left, ze.top ], [ ze.right, ze.bottom ] ]
            , scaleExtent: [ smallest, largest ]
            , name  : config.name
            , target
            }
  pure unit