module D3.Selection.Functions where

import D3.Data.Types (D3Selection_, Element, Selector)
import D3.FFI (d3Append_, d3AttachZoomDefaultExtent_, d3AttachZoom_, d3DataWithKeyFunction_, d3EnterAndAppend_, d3FilterSelection_, d3GetEnterSelection_, d3GetExitSelection_, d3MergeSelectionWith_, d3SelectAllInDOM_, d3SelectionSelectAll_)
import D3.Selection (Behavior(..), ChainableS, D3_Node(..), Join(..), Join(..), applyChainableSD3)
import D3.Zoom (ScaleExtent(..), ZoomExtent(..))
import D3Tagless.Capabilities (class SelectionM)
import Data.Foldable (foldl)
import Debug (spy)
import Prelude (Unit, discard, pure, show, unit, ($))


selectionAttach :: forall m. (SelectionM D3Selection_ m) => Selector D3Selection_ -> m D3Selection_
selectionAttach selector = pure $ d3SelectAllInDOM_ selector 

selectionAppendElement :: forall m. (SelectionM D3Selection_ m) => D3Selection_ -> D3_Node -> m D3Selection_
selectionAppendElement selection_ (D3_Node element attributes) = do
  let appended_ = d3Append_ (show element) selection_
  selectionModifySelection appended_ attributes
  pure appended_

selectionFilterSelection :: forall m. (SelectionM D3Selection_ m) => D3Selection_ -> Selector D3Selection_ -> m D3Selection_
selectionFilterSelection selection_ selector = pure $ d3FilterSelection_ selection_ selector

selectionModifySelection :: forall m. (SelectionM D3Selection_ m) => D3Selection_ -> Array (ChainableS) -> m Unit
selectionModifySelection selection_ attributes = do
  let _ = foldl applyChainableSD3 selection_ attributes
  pure unit

selectionJoin   :: forall datum m. (SelectionM D3Selection_ m) => D3Selection_ -> Join D3Selection_ datum -> m D3Selection_
selectionJoin selection (Join e theData keyFn) = do
  let 
    element         = spy "Join: " $ show e
    selectS         = d3SelectionSelectAll_ element selection
    dataSelection   = d3DataWithKeyFunction_ theData keyFn selectS 
    enterSelection  = d3EnterAndAppend_ element dataSelection
  pure enterSelection

selectionUpdateJoin   :: forall datum m.
  (SelectionM D3Selection_ m) =>
  D3Selection_ ->
  Join D3Selection_ datum ->
  m { enter :: D3Selection_, exit :: D3Selection_, update :: D3Selection_ }
selectionUpdateJoin openSelection (Join e theData keyFn) = do
  let
    element          = spy "Join: " $ show e
    -- REVIEW the openSelection here is not used anymore, for UpdateJoin you have to give an OpenSelection (not yet represented in the type system)
    -- openSelection = d3SelectionSelectAll_ element selection
    updateSelection  = d3DataWithKeyFunction_ theData keyFn openSelection

    enterSelection   = d3GetEnterSelection_ updateSelection
    enterSelection'  = d3Append_ element enterSelection    
    exitSelection    = d3GetExitSelection_ updateSelection
  pure { enter: enterSelection', exit: exitSelection, update: updateSelection }

selectionOpenSelection :: forall m. (SelectionM D3Selection_ m) => D3Selection_ -> Selector D3Selection_ -> m D3Selection_
selectionOpenSelection selection selector = do
  let _ = spy "open selection: " $ selector
  pure $ d3SelectionSelectAll_ selector selection

selectionMergeSelections :: forall m. (SelectionM D3Selection_ m) => D3Selection_ -> D3Selection_ -> m D3Selection_
selectionMergeSelections selectionA selectionB = pure $ d3MergeSelectionWith_ selectionA selectionB

selectionOn :: forall m. (SelectionM D3Selection_ m) => D3Selection_ -> Behavior D3Selection_ -> m Unit
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
    target = selection
    -- TODO recover the ability to "direct" the zoom to element other than the one receiving the event
    -- ie for controllers, containers etc

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