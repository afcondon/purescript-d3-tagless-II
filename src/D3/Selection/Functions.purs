module D3.Selection.Functions where

import D3.Data.Types (D3Selection_, Selector)
import D3.FFI (d3Append_, d3AttachZoomDefaultExtent_, d3AttachZoom_, d3Data_, d3EnterAndAppend_, d3Exit_, d3FilterSelection_, d3KeyFunction_, d3SelectAllInDOM_, d3SelectionSelectAll_, defaultDrag_, disableDrag_)
import D3.Selection (Behavior(..), ChainableS, D3_Node(..), DragBehavior(..), Join(..), applyChainableSD3)
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

selectionJoin   :: forall datum m. (SelectionM D3Selection_ m) => D3Selection_ -> Join datum -> m D3Selection_
selectionJoin selection (Join e ds cs) = do
  let 
    element = show e
    selectS = d3SelectionSelectAll_ element selection
    dataS   = d3Data_ ds selectS 
    enterS  = d3EnterAndAppend_ element dataS
    enterS' = foldl applyChainableSD3 enterS cs
  pure enterS'

selectionJoin selection (JoinWithKeyFunction e ds cs k) = do
  let 
    element = show e
    selectS = d3SelectionSelectAll_ element selection
    dataS   = d3KeyFunction_ ds k selectS 
    enterS  = d3EnterAndAppend_ element dataS
    enterS' = foldl applyChainableSD3 enterS cs
  pure enterS'

selectionJoin selection (UpdateJoin e ds cs) = do
  let
    element = show e
    selectS = d3SelectionSelectAll_ element selection
    dataS  = d3Data_ ds selectS 
    enterS = d3EnterAndAppend_ element dataS
    exitS  = d3Exit_ dataS
    _      = foldl applyChainableSD3 enterS  cs.enter
    _      = foldl applyChainableSD3 exitS   cs.exit
    _      = foldl applyChainableSD3 dataS   cs.update
  pure enterS

selectionJoin selection (UpdateJoinWithKeyFunction e ds cs k) = do
  let
    element = show e
    selectS = d3SelectionSelectAll_ element selection
    dataS  = d3KeyFunction_ ds k selectS 
    enterS = d3EnterAndAppend_ element dataS
    exitS  = d3Exit_ dataS
    _      = foldl applyChainableSD3 enterS  cs.enter
    _      = foldl applyChainableSD3 exitS   cs.exit
    _      = foldl applyChainableSD3 dataS   cs.update
  pure enterS


selectionOn :: forall m. (SelectionM D3Selection_ m) => D3Selection_ -> Behavior -> m Unit
selectionOn selection (Drag drag) = do
  let _ = case drag of 
            DefaultDrag     -> defaultDrag_ selection 
            NoDrag          -> disableDrag_ selection
            (CustomDrag fn) -> defaultDrag_ selection -- TODO no custom drag implemented yet
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