module D3.Simulation.Functions where

import D3.FFI
import D3.Simulation.Forces
import Prelude

import Control.Monad.State (class MonadState)
import D3.Attributes.Instances (Label)
import D3.Data.Types (D3Selection_, Datum_, Index_)
import D3.Node (D3Link, D3LinkSwizzled, D3_SimulationNode)
import D3.Selection (Behavior(..), DragBehavior(..), applySelectionAttributeD3)
import D3.Simulation.Types (D3SimulationState_, Force(..), ForceStatus(..), SimVariable(..), Step(..), _alpha, _alphaDecay, _alphaMin, _alphaTarget, _d3Simulation, _force, _forceLibrary, _handle, _status, _tick, _velocityDecay)
import D3.Zoom (ScaleExtent(..), ZoomExtent(..))
import D3Tagless.Capabilities (RawData)
import Data.Array (elem, filter, intercalate)
import Data.Array as A
import Data.Lens (modifying, set, use, view, (%=))
import Data.Map (Map, toUnfoldable)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst)
import Debug (spy)
import Unsafe.Coerce (unsafeCoerce)

-- | Underlying functions which allow us to make monadic updates from OUTSIDE of a script
-- | allowing control of the simulation outside of the drawing phase which runs in D3M

reheatSimulation :: D3SimulationState_ -> D3SimulationState_
reheatSimulation = set _alpha 1.0

simulationRemoveAllForces :: forall m row. 
  (MonadState { simulation :: D3SimulationState_ | row } m) => 
  m Unit
simulationRemoveAllForces = do
  handle <- use _handle
  forces <- use _forceLibrary
  let _ = (setAsNullForceInSimulation_ handle) <$> (A.fromFoldable $ M.keys forces)
  _forceLibrary %= (const M.empty)

simulationToggleForce :: forall m row. 
  (MonadState { simulation :: D3SimulationState_ | row } m) =>
  Label -> m Unit
simulationToggleForce label = do
  maybeForce <- use (_force label)
  case maybeForce of
    Nothing -> pure unit
    Just (Force force) ->
      if (force.status == ForceActive)
      then simulationDisableForcesByLabel [force.name]
      else simulationEnableForcesByLabel [force.name]

simulationDisableForcesByLabel :: forall m row. 
  (MonadState { simulation :: D3SimulationState_ | row } m) =>
  Array Label -> m Unit
simulationDisableForcesByLabel labels = do
  handle <- use _handle
  forces <- use _forceLibrary
  _forceLibrary %= (const $ (disableByLabels handle labels) <$> forces)

simulationEnableForcesByLabel :: forall m row. 
  (MonadState { simulation :: D3SimulationState_ | row } m) =>
  Array Label  -> m Unit
simulationEnableForcesByLabel labels  = do
  handle <- use _handle
  forces <- use _forceLibrary -- TODO this is the forces table inside the simulation record
  _forceLibrary %= (const $ (enableByLabels handle labels) <$> forces)
  
listActiveForces :: Map Label ForceStatus -> Array Label
listActiveForces forceMap = fst <$> (filter (\(Tuple n s) -> s == ForceActive) $ toUnfoldable forceMap)

listActiveForcesInLibrary :: Map Label Force -> Array Label
listActiveForcesInLibrary forceMap = fst <$> (filter (\(Tuple n f) -> (view _status f) == ForceActive) $ toUnfoldable forceMap)

simulationUpdateForceStatuses :: forall m row. 
  (MonadState { simulation :: D3SimulationState_ | row } m) =>
  Map Label ForceStatus ->
  m Unit
simulationUpdateForceStatuses forceStatuses = do
  handle        <- use _handle
  -- let _ = spy "forceStatuses on update: " $ listActiveForces forceStatuses
  _forceLibrary %= (putStatusMap forceStatuses)
  forceLibrary  <- use _forceLibrary -- now use the updated force
  -- let _ = spy "forceLibrary after status update: " $ listActiveForcesInLibrary forceLibrary
  let _ = (updateForceInSimulation handle) <$> forceLibrary
  pure unit

simulationSetVariable :: forall m row.
  (MonadState { simulation :: D3SimulationState_ | row } m) => 
  SimVariable -> m Unit
simulationSetVariable v = do
  handle <- use _handle
  case v of
    (Alpha n)         -> do
      let _ = setAlpha_ handle n
      _d3Simulation <<< _alpha %= (const n)
    (AlphaTarget n)   -> do
      let _ = setAlphaTarget_ handle n
      _d3Simulation <<< _alphaTarget %= (const n)
    (AlphaMin n)      -> do
      let _ = setAlphaMin_ handle n
      _d3Simulation <<< _alphaMin %= (const n)
    (AlphaDecay n)    -> do
      let _ = setAlphaDecay_ handle n
      _d3Simulation <<< _alphaDecay %= (const n)
    (VelocityDecay n) -> do
      let _ = setVelocityDecay_ handle n
      _d3Simulation <<< _velocityDecay %= (const n)

simulationStart :: forall m row. 
  (MonadState { simulation :: D3SimulationState_ | row } m) =>
  m Unit
simulationStart = do
  handle <- use _handle
  _d3Simulation <<< _alpha %= (const 1.0)
  pure $ startSimulation_ handle

simulationStop :: forall m row. 
  (MonadState { simulation :: D3SimulationState_ | row } m) => 
  m Unit
simulationStop = do
  handle <- use _handle
  let _ = stopSimulation_ handle
  pure unit

simulationShowForces :: forall m row. 
  (MonadState { simulation :: D3SimulationState_ | row } m) =>
  m String
simulationShowForces = do
  forces <- use _forceLibrary
  let forceTuples = M.toUnfoldable forces
      showTuple (Tuple label force) = show label <> " " <> show force
  pure $ intercalate "\n" $ showTuple <$> forceTuples

simulationPreservePositions ::
  forall id d r m row. 
  Bind m =>
  MonadState { simulation :: D3SimulationState_ | row } m =>
  D3Selection_ ->
  RawData d r id -> 
  (Datum_ -> Index_) ->
  m (Array (D3_SimulationNode d))
simulationPreservePositions selection rawdata key = do
  let updatedData = d3PreserveSimulationPositions_ selection rawdata.nodes key
  pure updatedData

simulationPreserveLinkReferences ::
  forall d id r m row. 
  Eq id => 
  Bind m =>
  MonadState { simulation :: D3SimulationState_ | row } m =>
  D3Selection_ ->
  RawData d r id -> 
  (Datum_ -> Index_) ->
  m (Array (D3Link id r))
simulationPreserveLinkReferences selection rawdata keyFn = do
  let
    nodeIDs :: Array id 
    nodeIDs       = getIDsFromNodes_ rawdata.nodes keyFn
    validLink :: D3Link id r -> Boolean
    validLink l = do
      let { sourceID, targetID } = getLinkIDs_ keyFn l
      (sourceID `elem` nodeIDs) && (targetID `elem` nodeIDs)
    validNewLinks = filter validLink rawdata.links 
    updatedData = d3PreserveLinkReferences_ selection validNewLinks
  pure updatedData

simulationSwizzleLinks ::
  forall d r row id m. 
  Bind m =>
  MonadState { simulation :: D3SimulationState_ | row } m =>
  Array (D3Link id r) ->
  Array (D3_SimulationNode d) ->
  (Datum_ -> Index_) ->
  m (Array (D3LinkSwizzled (D3_SimulationNode d) r))
simulationSwizzleLinks links nodes keyFn = do
  handle <- use _handle
  pure $ swizzleLinks_ links nodes keyFn 

-- | the situation with General Update Pattern for simulations is MUCH more complicated than for non-simulation data
-- | this function takes care of all that complexity and adds both links and nodes and takes care of ensuring that
-- | existing nodes preserve their positions and that all links are to nodes that are still in the simulation
simulationMergeNewData :: forall d r row id m.
  Eq id =>
  Bind m =>
  MonadState { simulation :: D3SimulationState_ | row } m =>
  D3Selection_ -> -- nodes selection
  (Datum_ -> Index_) -> -- nodes keyFn
  D3Selection_ -> -- links selection
  (Datum_ -> Index_) -> -- links KeyFn
  RawData d r id -> -- links and nodes raw data
  m { links :: (Array (D3LinkSwizzled (D3_SimulationNode d) r)), nodes :: (Array (D3_SimulationNode d))}
simulationMergeNewData nodeSelection nodeKeyFn linkSelection linkKeyFn rawdata = do
  let updatedNodeData = d3PreserveSimulationPositions_ nodeSelection rawdata.nodes nodeKeyFn
      nodeIDs :: Array id 
      nodeIDs       = getIDsFromNodes_ rawdata.nodes nodeKeyFn
      validLink :: D3Link id r -> Boolean
      validLink l = do
        let { sourceID, targetID } = getLinkIDs_ linkKeyFn l
        (sourceID `elem` nodeIDs) && (targetID `elem` nodeIDs)
      validNewLinks = filter validLink rawdata.links 
      updatedLinkData  = d3PreserveLinkReferences_ linkSelection validNewLinks
      swizzledLinkData = swizzleLinks_ updatedLinkData updatedNodeData nodeKeyFn

  pure { nodes: updatedNodeData, links: swizzledLinkData }

  -- | all the above should be the moral equivalent of this: 
  -- updatedNodeData <- simulationPreservePositions nodeSelection rawdata nodeKeyFn
  -- updatedLinkData <- simulationPreserveLinkReferences linkSelection rawdata linkKeyFn
  -- swizzledLinkData <- simulationSwizzleLinks updatedLinkData updatedNodeData nodeKeyFn
  -- pure { nodes: updatedNodeData, links: swizzledLinkData }

simulationSetNodes :: 
  forall d row m.
  Bind m =>
  MonadState { simulation :: D3SimulationState_ | row } m =>
  Array (D3_SimulationNode d) ->
  m (Array (D3_SimulationNode d))
simulationSetNodes nodes = do
  handle <- use _handle
  let _ = setNodes_ handle nodes
  let opaqueNodes = getNodes_ handle
  pure $ unsafeCoerce opaqueNodes

simulationSetLinks :: -- now with 100% more swizzling!!
  forall d r id row m. -- TODO might need to check whether _unnecessary swizzle_ breaks things?
  Eq id =>
  Bind m =>
  MonadState { simulation :: D3SimulationState_ | row } m =>
  Array (D3Link id r) -> Array (D3_SimulationNode d) -> (Datum_ -> Index_ ) ->
  m (Array (D3LinkSwizzled (D3_SimulationNode d) r))
simulationSetLinks links nodes keyFn = do
  handle <- use _handle
  let _ = setLinks_ handle (swizzleLinks_ links nodes keyFn)
  let swizzledLinks = getLinksFromSimulation_ handle
  pure swizzledLinks

simulationSetNodesFromSelection :: 
  forall row m.
  Bind m =>
  MonadState { simulation :: D3SimulationState_ | row } m =>
  D3Selection_ -> m Unit
simulationSetNodesFromSelection nodeSelection = do
  handle <- use _handle
  let _ = setNodes_ handle (unsafeCoerce $ d3GetSelectionData_ nodeSelection)
  pure unit

simulationSetLinksFromSelection :: 
  forall row m.
  Bind m =>
  MonadState { simulation :: D3SimulationState_ | row } m =>
  D3Selection_ -> (Datum_ -> Boolean) -> m Unit
simulationSetLinksFromSelection linkSelection filterFn = do
  handle <- use _handle
  let _ = setLinks_ handle (unsafeCoerce $ filter filterFn $ d3GetSelectionData_ linkSelection)
  pure unit


simulationCreateTickFunction :: forall selection row m. 
  (MonadState { simulation :: D3SimulationState_ | row } m) =>
  Label -> Step selection -> m Unit
simulationCreateTickFunction label tick@(StepTransformFFI selection fn) = pure unit
simulationCreateTickFunction label tick@(Step selection chain) = do
  handle <- use _handle
  let 
    makeTick _ = do
      -- TODO this coerce is forced upon us here due to forall selection in SimulationM
      -- going to have to parameterize simulation with selection or hide the type dep somehow
      let _ = (applySelectionAttributeD3 (unsafeCoerce selection)) <$> chain
      unit
    _ = onTick_ handle label makeTick  -- actually put this tick function into the simulation
    (tick' :: Step D3Selection_) = unsafeCoerce tick
  -- finally, update the State to track this tick function (not actually used for anything at present, tho)
  modifying (_d3Simulation <<< _tick label) (liftA1 $ const tick')
  
-- the price of being able to treat Drag, Zoom, Click etc the same in SimulationM and SelectionM instances is some duplication here
-- Drag has to behave differently in the simulation case
simulationOn :: forall row m. 
  (MonadState { simulation :: D3SimulationState_ | row } m) =>
   D3Selection_ -> Behavior D3Selection_ -> m Unit
simulationOn selection (Drag drag) = do
  handle <- use _handle
  let _ = case drag of 
            DefaultDrag      -> simulationDrag_ "default" selection handle simdrag
            NoDrag           -> disableDrag_ selection
            (CustomDrag name fn)  -> simulationDrag_ name selection handle fn
  pure unit

simulationOn selection (Zoom config) = do
  let 
    (ScaleExtent smallest largest) = config.scale

  -- sticking to the rules of no ADT's on the JS side we case on the ZoomExtent here
    _ = case config.extent of
          DefaultZoomExtent -> 
            d3AttachZoomDefaultExtent_ selection {
              scaleExtent: [ smallest, largest ]
            , name  : config.name
            , target: config.target
            } 

          (ZoomExtent ze)   -> do
            d3AttachZoom_ selection { 
              extent     : [ [ ze.left, ze.top ], [ ze.right, ze.bottom ] ]
            , scaleExtent: [ smallest, largest ]
            , name  : config.name
            , target: config.target
            }
  pure unit