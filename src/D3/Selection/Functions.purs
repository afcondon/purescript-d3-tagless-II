module D3.Selection.Functions where

import D3.Data.Types (D3Selection_, Selector)
import D3.FFI (d3Append_, d3AttachZoomDefaultExtent_, d3AttachZoom_, d3DataWithKeyFunction_, d3EnterAndAppend_, d3FilterSelection_, d3GetEnterSelection_, d3GetExitSelection_, d3MergeSelectionWith_, d3SelectAllInDOM_, d3SelectionSelectAll_)
import D3.Selection (Behavior(..), ChainableS, D3_Node(..), Join(..), applyChainableSD3)
import D3.Zoom (ScaleExtent(..), ZoomExtent(..))
import D3Tagless.Capabilities (class SelectionM)
import Data.Foldable (foldl)
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
selectionJoin selection (Join e theData keyFn cs) = do
  let 
    element         = show e
    selectS         = d3SelectionSelectAll_ element selection
    dataSelection   = d3DataWithKeyFunction_ theData keyFn selectS 
    enterSelection  = d3EnterAndAppend_ element dataSelection
    enterSelection' = foldl applyChainableSD3 enterSelection cs
  pure enterSelection'

selectionJoin selection (UpdateJoin e theData keyFn cs) = do
  let
    element          = show e
    initialSelection = d3SelectionSelectAll_ element selection
    updateSelection  = d3DataWithKeyFunction_ theData keyFn initialSelection

    enterSelection   = d3GetEnterSelection_ updateSelection
    enterSelection'  = d3Append_ element enterSelection
    _                = foldl applyChainableSD3 enterSelection'  cs.enter
    
    exitSelection    = d3GetExitSelection_ updateSelection
    _                = foldl applyChainableSD3 exitSelection   cs.exit
    
    _                = foldl applyChainableSD3 updateSelection cs.update
  pure enterSelection -- REVIEW shouldn't this be merged selection too? how's that possibly work

selectionJoin selection (SplitJoinOpen selector) = do
  pure $ d3SelectionSelectAll_ selector selection


selectionJoin openSelection (SplitJoinClose e theData keyFn cs) = do
  let
    element         = show e -- d3 is stringy
    updateSelection = d3DataWithKeyFunction_ theData keyFn openSelection 
-- first the entering items
    enterSelection  = d3GetEnterSelection_ updateSelection
    enterSelection' = d3Append_ element enterSelection
    _               = foldl applyChainableSD3 enterSelection' cs.enter
-- next the leaving items
    exitSelection   = d3GetExitSelection_ updateSelection
    _               = foldl applyChainableSD3 exitSelection   cs.exit
-- third, the updating items
    _               = foldl applyChainableSD3 updateSelection cs.update
-- finally merge the entering and updating items
    mergedSelection = updateSelection `d3MergeSelectionWith_` enterSelection'
  pure mergedSelection


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