module PSD3.Internal.Simulation.Functions where

import PSD3.Internal.FFI
import PSD3.Internal.Simulation.Forces
import Prelude

import Control.Monad.State (class MonadState)
import PSD3.Internal.Attributes.Instances (Label)
import PSD3.Internal.Types (D3Selection_, Datum_, Index_)
import PSD3.Data.Node (Link, SwizzledLink, SimulationNode)
import PSD3.Internal.Selection.Types (Behavior(..), DragBehavior(..), applySelectionAttributeD3)
import PSD3.Internal.Simulation.Types (D3SimulationState_(..), Force(..), ForceStatus(..), SimVariable(..), Step(..), _alpha, _alphaDecay, _alphaMin, _alphaTarget, _d3Simulation, _force, _forceLibrary, _handle, _name, _status, _tick, _velocityDecay)
import PSD3.Internal.Zoom (ScaleExtent(..), ZoomExtent(..))
import PSD3.Capabilities.Simulation (RawData)
import Data.Array (elem, filter, intercalate)
import Data.Array as A
import Data.Lens (modifying, set, use, view, (%=))
import Data.Map (Map, toUnfoldable)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), fst)
import Debug (spy)
import Unsafe.Coerce (unsafeCoerce)

-- | Underlying functions which allow us to make monadic updates from OUTSIDE of a script
-- | allowing control of the simulation outside of the drawing phase which runs in D3M

reheatSimulation :: forall d. D3SimulationState_ d -> D3SimulationState_ d
reheatSimulation = set _alpha 1.0

-- | Actualize forces without MonadState - for use in Effect callbacks
-- | This function updates the force library and syncs to the D3 simulation
actualizeForcesDirect :: forall d. Set Label -> D3SimulationState_ d -> D3SimulationState_ d
actualizeForcesDirect activeForces simState =
  let SimState_ record = simState
      handle = record.handle_
      library = record.forceLibrary
      allLabels = M.keys library
      enableLabels = A.fromFoldable $ Set.intersection activeForces (Set.fromFoldable allLabels)
      disableLabels = A.fromFoldable $ Set.difference (Set.fromFoldable allLabels) activeForces
      -- Update statuses in the force library using the existing helper functions
      updatedLibrary = (enableByLabels handle enableLabels) <$> ((disableByLabels handle disableLabels) <$> library)
  in SimState_ $ record { forceLibrary = updatedLibrary }

simulationRemoveAllForces :: forall d m row.
  (MonadState { simulation :: D3SimulationState_ d | row } m) =>
  m Unit
simulationRemoveAllForces = do
  handle <- use _handle
  forces <- use _forceLibrary
  let _ = (setAsNullForceInSimulation_ handle) <$> (A.fromFoldable $ M.keys forces)
  _forceLibrary %= (const M.empty)

-- | Replace all forces with a new array
-- |
-- | This is the core implementation for the setForces capability.
-- | It removes all existing forces and adds the new ones.
simulationSetForces :: forall d m row.
  (MonadState { simulation :: D3SimulationState_ d | row } m) =>
  Array (Force d) -> m Unit
simulationSetForces newForces = do
  -- 1. Remove all existing forces
  simulationRemoveAllForces

  -- 2. Build new force library from array
  let newLibrary = M.fromFoldable $ newForces <#> \f ->
        Tuple (view _name f) (enableForce f)  -- All new forces start active
  _forceLibrary %= const newLibrary

  -- 3. Add all forces to D3 simulation
  handle <- use _handle
  let _ = (putForceInSimulation <$> newForces) <*> pure handle
  pure unit

simulationToggleForce :: forall d m row.
  (MonadState { simulation :: D3SimulationState_ d | row } m) =>
  Label -> m Unit
simulationToggleForce label = do
  maybeForce <- use (_force label)
  case maybeForce of
    Nothing -> pure unit
    Just (Force force) ->
      if (force.status == ForceActive)
      then simulationDisableForcesByLabel [force.name]
      else simulationEnableForcesByLabel [force.name]

simulationDisableForcesByLabel :: forall d m row.
  (MonadState { simulation :: D3SimulationState_ d | row } m) =>
  Array Label -> m Unit
simulationDisableForcesByLabel labels = do
  handle <- use _handle
  forces <- use _forceLibrary
  _forceLibrary %= (const $ (disableByLabels handle labels) <$> forces)

simulationEnableForcesByLabel :: forall d m row.
  (MonadState { simulation :: D3SimulationState_ d | row } m) =>
  Array Label  -> m Unit
simulationEnableForcesByLabel labels  = do
  handle <- use _handle
  forces <- use _forceLibrary -- TODO this is the forces table inside the simulation record
  _forceLibrary %= (const $ (enableByLabels handle labels) <$> forces)

-- | Enable only the forces in the Set, disable all others
simulationActualizeForces :: forall d m row.
  (MonadState { simulation :: D3SimulationState_ d | row } m) =>
  Set Label ->
  m Unit
simulationActualizeForces activeForces = do
  handle <- use _handle
  library <- use _forceLibrary
  let allLabels = M.keys library
      enableLabels = A.fromFoldable $ Set.intersection activeForces (Set.fromFoldable allLabels)
      disableLabels = A.fromFoldable $ Set.difference (Set.fromFoldable allLabels) activeForces
      _ = spy "🔧 actualizeForces - enabling" enableLabels
      _ = spy "🔧 actualizeForces - disabling" disableLabels
  -- Update status in PureScript state
  simulationEnableForcesByLabel enableLabels
  simulationDisableForcesByLabel disableLabels
  -- Sync to D3 simulation (this is the critical step!)
  updatedLibrary <- use _forceLibrary
  let _ = (updateForceInSimulation handle) <$> updatedLibrary
  pure unit
  
listActiveForces :: Map Label ForceStatus -> Array Label
listActiveForces forceMap = fst <$> (filter (\(Tuple n s) -> s == ForceActive) $ toUnfoldable forceMap)

listActiveForcesInLibrary :: forall d. Map Label (Force d) -> Array Label
listActiveForcesInLibrary forceMap = fst <$> (filter (\(Tuple n f) -> (view _status f) == ForceActive) $ toUnfoldable forceMap)

simulationUpdateForceStatuses :: forall d m row.
  (MonadState { simulation :: D3SimulationState_ d | row } m) =>
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

simulationSetVariable :: forall d m row.
  (MonadState { simulation :: D3SimulationState_ d | row } m) => 
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

simulationStart :: forall d m row.
  (MonadState { simulation :: D3SimulationState_ d | row } m) =>
  m Unit
simulationStart = do
  handle <- use _handle
  _d3Simulation <<< _alpha %= (const 1.0)
  pure $ startSimulation_ handle

simulationStop :: forall d m row.
  (MonadState { simulation :: D3SimulationState_ d | row } m) => 
  m Unit
simulationStop = do
  handle <- use _handle
  let _ = stopSimulation_ handle
  pure unit

simulationShowForces :: forall d m row.
  (MonadState { simulation :: D3SimulationState_ d | row } m) =>
  m String
simulationShowForces = do
  forces <- use _forceLibrary
  let forceTuples = M.toUnfoldable forces
      showTuple (Tuple label force) = show label <> " " <> show force
  pure $ intercalate "\n" $ showTuple <$> forceTuples

simulationPreservePositions ::
  forall simStateType nodeData id linkRow key m row.
  Bind m =>
  MonadState { simulation :: D3SimulationState_ simStateType | row } m =>
  D3Selection_ (SimulationNode nodeData) ->
  RawData nodeData id linkRow ->
  (SimulationNode nodeData -> key) ->
  m (Array (SimulationNode nodeData))
simulationPreservePositions selection rawdata key = do
  let updatedData = d3PreserveSimulationPositions_ selection rawdata.nodes key
  pure updatedData

simulationPreserveLinkReferences ::
  forall simStateType nodeData id linkRow m row.
  Eq id =>
  Bind m =>
  MonadState { simulation :: D3SimulationState_ simStateType | row } m =>
  D3Selection_ (SimulationNode nodeData) ->
  Array (Link id linkRow) ->
  Array (SimulationNode nodeData) ->
  (SimulationNode nodeData -> id) ->  -- Key function must return same type as link source/target
  m (Array (Link id linkRow))
simulationPreserveLinkReferences selection links nodes keyFn = do
  let
    nodeIDs :: Array id
    nodeIDs       = getIDsFromNodes_ nodes keyFn
    validLink :: Link id linkRow -> Boolean
    validLink l = do
      let { sourceID, targetID } = getLinkIDs_ l
      (sourceID `elem` nodeIDs) && (targetID `elem` nodeIDs)
    validNewLinks = filter validLink links
    updatedData = d3PreserveLinkReferences_ selection validNewLinks
  pure updatedData

simulationSwizzleLinks ::
  forall d nodeData id linkRow key m row.
  Bind m =>
  MonadState { simulation :: D3SimulationState_ d | row } m =>
  Array (Link id linkRow) ->
  Array (SimulationNode nodeData) ->
  (SimulationNode nodeData -> key) ->
  m (Array (SwizzledLink nodeData linkRow))
simulationSwizzleLinks links nodes keyFn = do
  handle <- use _handle
  pure $ swizzleLinks_ links nodes keyFn

-- | the situation with General Update Pattern for simulations is MUCH more complicated than for non-simulation data
-- | this function takes care of all that complexity and adds both links and nodes and takes care of ensuring that
-- | existing nodes preserve their positions and that all links are to nodes that are still in the simulation
simulationMergeNewData :: forall simStateType nodeData id linkRow linkKey m row.
  Eq id =>
  Bind m =>
  MonadState { simulation :: D3SimulationState_ simStateType | row } m =>
  D3Selection_ (SimulationNode nodeData) -> -- nodes selection
  (SimulationNode nodeData -> id) -> -- nodes keyFn (must return same type as link source/target)
  D3Selection_ Datum_ -> -- links selection
  (Link id linkRow -> linkKey) -> -- links KeyFn (typed!)
  Array (Link id linkRow) -> -- links raw data
  Array (SimulationNode nodeData) -> -- nodes raw data
  m { links :: Array (SwizzledLink nodeData linkRow), nodes :: Array (SimulationNode nodeData)}
simulationMergeNewData nodeSelection nodeKeyFn linkSelection linkKeyFn links nodes = do
  let updatedNodeData = d3PreserveSimulationPositions_ nodeSelection nodes nodeKeyFn
      nodeIDs :: Array id
      nodeIDs       = getIDsFromNodes_ nodes nodeKeyFn
      validLink :: Link id linkRow -> Boolean
      validLink l = do
        let { sourceID, targetID } = getLinkIDs_ l
        (sourceID `elem` nodeIDs) && (targetID `elem` nodeIDs)
      validNewLinks = filter validLink links
      updatedLinkData  = d3PreserveLinkReferences_ linkSelection validNewLinks
      swizzledLinkData = swizzleLinks_ updatedLinkData updatedNodeData nodeKeyFn

  pure { nodes: updatedNodeData, links: swizzledLinkData }

  -- | all the above should be the moral equivalent of this: 
  -- updatedNodeData <- simulationPreservePositions nodeSelection rawdata nodeKeyFn
  -- updatedLinkData <- simulationPreserveLinkReferences linkSelection rawdata linkKeyFn
  -- swizzledLinkData <- simulationSwizzleLinks updatedLinkData updatedNodeData nodeKeyFn
  -- pure { nodes: updatedNodeData, links: swizzledLinkData }

simulationSetNodes ::
  forall d nodeData row m.
  Bind m =>
  MonadState { simulation :: D3SimulationState_ d | row } m =>
  Array (SimulationNode nodeData) ->
  m (Array (SimulationNode nodeData))
simulationSetNodes nodes = do
  handle <- use _handle
  let _ = setNodes_ handle nodes
  let opaqueNodes = getNodes_ handle
  pure $ unsafeCoerce opaqueNodes

simulationSetLinks :: -- now with 100% more swizzling!!
  forall d nodeData id linkRow key m row.
  Bind m =>
  MonadState { simulation :: D3SimulationState_ d | row } m =>
  Array (Link id linkRow) -> Array (SimulationNode nodeData) -> (SimulationNode nodeData -> key) ->
  m (Array (SwizzledLink nodeData linkRow))
simulationSetLinks links nodes keyFn = do
  handle <- use _handle
  -- Swizzle links (convert source/target indices to node objects)
  let swizzledLinks = swizzleLinks_ links nodes keyFn
  -- Set in simulation (for links force if enabled)
  let _ = setLinks_ handle swizzledLinks
  -- Return the swizzled links directly, not from simulation
  -- (getLinksFromSimulation_ returns [] when links force is disabled)
  pure swizzledLinks

simulationSetNodesFromSelection ::
  forall d row m.
  Bind m =>
  MonadState { simulation :: D3SimulationState_ d | row } m =>
  D3Selection_ d -> m Unit
simulationSetNodesFromSelection nodeSelection = do
  handle <- use _handle
  let _ = setNodes_ handle (unsafeCoerce $ d3GetSelectionData_ nodeSelection)
  pure unit

simulationSetLinksFromSelection ::
  forall d row m.
  Bind m =>
  MonadState { simulation :: D3SimulationState_ d | row } m =>
  D3Selection_ d -> (Datum_ -> Boolean) -> m Unit
simulationSetLinksFromSelection linkSelection filterFn = do
  handle <- use _handle
  let _ = setLinks_ handle (unsafeCoerce $ filter filterFn $ d3GetSelectionData_ linkSelection)
  pure unit


simulationCreateTickFunction :: forall d selection row m.
  (MonadState { simulation :: D3SimulationState_ d | row } m) =>
  Label -> Step selection d -> m Unit
simulationCreateTickFunction label tick@(StepTransformFFI selection fn) = pure unit
simulationCreateTickFunction label tick@(Step selection chain) = do
  handle <- use _handle
  let 
    makeTick _ = do
      -- TODO this coerce is forced upon us here due to forall selection in SimulationM2
      -- going to have to parameterize simulation with selection or hide the type dep somehow
      let _ = (applySelectionAttributeD3 (unsafeCoerce selection)) <$> chain
      unit
    _ = onTick_ handle label makeTick  -- actually put this tick function into the simulation
    (tick' :: Step D3Selection_ d) = unsafeCoerce tick
  -- finally, update the State to track this tick function (not actually used for anything at present, tho)
  modifying (_d3Simulation <<< _tick label) (liftA1 $ const tick')
  
-- the price of being able to treat Drag, Zoom, Click etc the same in SimulationM2 and SelectionM instances is some duplication here
-- Drag has to behave differently in the simulation case
simulationOn :: forall d row m.
  (MonadState { simulation :: D3SimulationState_ d | row } m) =>
   D3Selection_ d -> Behavior (D3Selection_ d) -> m Unit
simulationOn selection (Drag drag) = do
  handle <- use _handle
  let _ = case drag of 
            DefaultDrag      -> simulationDrag_ "default" selection handle simdrag_
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