module D3.Selection.Functions where

import Control.Monad.State (gets)
import D3.Data.Types (D3Selection_, Selector)
import D3.FFI (d3Append_, d3AttachZoomDefaultExtent_, d3AttachZoom_, d3Data_, d3EnterAndAppend_, d3Exit_, d3FilterSelection_, d3DataWithKeyFunction_, d3SelectAllInDOM_, d3SelectionSelectAll_, disableDrag_)
import D3.Selection (Behavior(..), ChainableS, D3_Node(..), DragBehavior(..), Join(..), applyChainableSD3)
import D3.Simulation.Types (D3SimulationState_(..))
import D3.Zoom (ScaleExtent(..), ZoomExtent(..))
import D3Tagless.Capabilities (class SelectionM)
import Data.Foldable (foldl)
import Prelude (Unit, bind, discard, identity, pure, show, unit, ($))


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
selectionJoin selection (Join e ds k cs) = do
  let 
    element = show e
    selectS = d3SelectionSelectAll_ element selection
    dataS   = d3DataWithKeyFunction_ ds k selectS 
    enterS  = d3EnterAndAppend_ element dataS
    enterS' = foldl applyChainableSD3 enterS cs
  pure enterS'

selectionJoin selection (SplitJoinOpen selector) = do
  pure $ d3SelectionSelectAll_ selector selection


selectionJoin selection (SplitJoinClose e ds k cs) = do
  let
    dataS  = d3DataWithKeyFunction_ ds k selection 
    enterS = d3EnterAndAppend_ (show e) dataS
    exitS  = d3Exit_ dataS
    _      = foldl applyChainableSD3 enterS  cs.enter
    _      = foldl applyChainableSD3 exitS   cs.exit
    _      = foldl applyChainableSD3 dataS   cs.update
  pure enterS


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