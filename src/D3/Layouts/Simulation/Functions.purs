module D3.Simulation.Functions where

import Prelude

import Control.Monad.State (class MonadState)
import D3.Attributes.Instances (Label)
import D3.Data.Types (D3Selection_, Datum_, Index_)
import D3.FFI (d3AttachZoomDefaultExtent_, d3AttachZoom_, d3PreserveLinkReferences_, d3PreserveSimulationPositions_, defaultSimulationDrag_, disableDrag_, getIDsFromNodes_, getLinkIDs_, getLinksFromSimulation_, getNodes_, onTick_, setAlphaDecay_, setAlphaMin_, setAlphaTarget_, setAlpha_, setAsNullForceInSimulation_, setLinks_, setNodes_, setVelocityDecay_, startSimulation_, stopSimulation_, swizzleLinks_)
import D3.Node (D3Link, D3LinkSwizzled, D3_SimulationNode)
import D3.Selection (Behavior(..), DragBehavior(..), applySelectionAttributeD3)
import D3.Simulation.Forces (disableByLabels, enableByLabels, enableOnlyTheseLabels, putForceInSimulation, setForceAttr)
import D3.Simulation.Types 
import D3.Zoom (ScaleExtent(..), ZoomExtent(..))
import D3Tagless.Capabilities (RawData)
import Data.Array (elem, filter, intercalate)
import Data.Array as A
import Data.Foldable (traverse_)
import Data.Lens (modifying, set, use, view, (%=))
import Data.Lens.At (at)
import Data.Map (Map, toUnfoldable)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..), fst, snd)
import Stories.Spago.State (_d3Simulation)
import Unsafe.Coerce (unsafeCoerce)

-- | Underlying functions which allow us to make monadic updates from OUTSIDE of a script
-- | allowing control of the simulation outside of the drawing phase which runs in D3M

insertForce :: Force -> D3SimulationState_ -> D3SimulationState_
-- insertForce force (SimState_ ss_) = SimState_ ss_ { forces = M.insert (getLabel force) force ss_.forces }
insertForce force = set (_force (view _name force)) (Just force)

reheatSimulation :: D3SimulationState_ -> D3SimulationState_
reheatSimulation = set _alpha 1.0

-- type SimulationStateRow row = ( simulation :: D3SimulationState_ | row )

simulationAddForces :: forall m row. 
  (MonadState { simulation :: D3SimulationState_ | row } m) => 
  Array Force -> m Unit
simulationAddForces forces = do
  traverse_ simulationAddForce forces -- effectfully put the forces in the simulation
  let (updateMap :: M.Map Label Force) = M.fromFoldable $ forceTuples forces
  modifying (_d3Simulation <<< _forces) (\m -> M.union updateMap m )

simulationRemoveAllForces :: forall m row. 
  (MonadState { simulation :: D3SimulationState_ | row } m) => 
  m Unit
simulationRemoveAllForces = do
  handle <- use (_d3Simulation <<< _handle)
  forces <- use (_d3Simulation <<< _forces)
  let _ = (setAsNullForceInSimulation_ handle) <$> (A.fromFoldable $ M.keys forces)
  modifying (_d3Simulation <<< _forces) (const M.empty)

simulationAddForce :: forall m row. 
  (MonadState { simulation :: D3SimulationState_ | row } m) =>
  Force -> m Unit
simulationAddForce (Force force) = do 
  let _ = (\a -> setForceAttr force.force_ force.filter (unwrap a)) <$> force.attributes -- side-effecting function that sets force's attributes
  handle <- use (_d3Simulation <<< _handle)
  let _ = if force.status == ForceActive
          then putForceInSimulation (Force force) handle
          else handle          
  modifying (_d3Simulation <<< _force force.name) (liftA1 $ const $ Force force)

simulationToggleForce :: forall m row. 
  (MonadState { simulation :: D3SimulationState_ | row } m) =>
  Label -> m Unit
simulationToggleForce label = do
  maybeForce <- use (_d3Simulation <<< _forces <<< at label)
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
  handle <- use (_d3Simulation <<< _handle)
  forces <- use (_d3Simulation <<< _forces)
  (_d3Simulation <<< _forces) %= (const $ (disableByLabels handle labels) <$> forces)

simulationEnableOnlyTheseForces :: forall m row. 
  (MonadState { simulation :: D3SimulationState_ | row } m) =>
  Array Label -> m Unit
simulationEnableOnlyTheseForces labels = do
  handle <- use (_d3Simulation <<< _handle)
  forces <- use (_d3Simulation <<< _forces)
  let updatedForces = (enableOnlyTheseLabels handle labels) <$> forces -- REVIEW can't we traversed (optic) this update?
  modifying (_d3Simulation <<< _forces) (const updatedForces)

simulationEnableForcesByLabel :: forall m row. 
  (MonadState { simulation :: D3SimulationState_ | row } m) =>
  Array Label  -> m Unit
simulationEnableForcesByLabel labels  = do
  handle <- use (_d3Simulation <<< _handle)
  forces <- use (_d3Simulation <<< _forces) -- TODO this is the forces table inside the simulation record
  let updatedForces = (enableByLabels handle labels) <$> forces -- REVIEW can't we traversed (optic) this update?
  modifying (_d3Simulation <<< _forces) (const updatedForces)
  
simulationSetActiveForces :: forall m row. 
  (MonadState { simulation :: D3SimulationState_ | row } m) =>
  Map Label ForceStatus -> m Unit
simulationSetActiveForces stagingforces = do
  handle <- use (_d3Simulation <<< _handle)
  forces <- use (_d3Simulation <<< _forces) -- TODO this is the forces table inside the simulation record
  let labels = fst <$> (filter (\f -> snd f == ForceActive) $ toUnfoldable stagingforces) -- only forces that are (label, ForceActive)
  let updatedForces = (enableOnlyTheseLabels handle labels) <$> forces -- REVIEW can't we traversed (optic) this update?
  modifying (_d3Simulation <<< _forces) (const updatedForces)

simulationSetVariable :: forall m row.
  (MonadState { simulation :: D3SimulationState_ | row } m) => 
  SimVariable -> m Unit
simulationSetVariable v = do
  handle <- use (_d3Simulation <<< _handle)
  case v of
    (Alpha n)         -> do
      let _ = setAlpha_ handle n
      modifying (_d3Simulation <<< _alpha) (const n)
    (AlphaTarget n)   -> do
      let _ = setAlphaTarget_ handle n
      modifying (_d3Simulation <<< _alphaTarget) (const n)
    (AlphaMin n)      -> do
      let _ = setAlphaMin_ handle n
      modifying (_d3Simulation <<< _alphaMin) (const n)
    (AlphaDecay n)    -> do
      let _ = setAlphaDecay_ handle n
      modifying (_d3Simulation <<< _alphaDecay) (const n)
    (VelocityDecay n) -> do
      let _ = setVelocityDecay_ handle n
      modifying (_d3Simulation <<< _velocityDecay) (const n)

simulationStart :: forall m row. 
  (MonadState { simulation :: D3SimulationState_ | row } m) =>
  m Unit
simulationStart = do
  handle <- use (_d3Simulation <<< _handle)
-- let newAlpha = 1.0
-- modifying (_d3Simulation <<< _alpha) (const newAlpha)
  pure $ startSimulation_ handle

-- simulationStop :: forall m row. 
--   (MonadState { simulation :: D3SimulationState_ | row } m) => 
--   m Unit
simulationStop :: forall t34 t48.
  Bind t34 => MonadState
                { simulation :: D3SimulationState_
                | t48
                }
                t34
               => t34 Unit
simulationStop = do
  handle <- use (_d3Simulation <<< _handle)
  let _ = stopSimulation_ handle
  pure unit

simulationShowForces :: forall m row. 
  (MonadState { simulation :: D3SimulationState_ | row } m) =>
  m String
simulationShowForces = do
  forces <- use (_d3Simulation <<< _forces)
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

simulationSetNodes :: 
  forall d row m.
  Bind m =>
  MonadState { simulation :: D3SimulationState_ | row } m =>
  Array (D3_SimulationNode d) -> m Unit
simulationSetNodes nodes = do
  handle <- use (_d3Simulation <<< _handle)
  let _ = setNodes_ handle nodes
  pure unit

simulationSetLinks :: 
  forall d r row m.
  Bind m =>
  MonadState { simulation :: D3SimulationState_ | row } m =>
  Array (D3LinkSwizzled (D3_SimulationNode d) r) -> m Unit
simulationSetLinks links = do
  handle <- use (_d3Simulation <<< _handle)
  let _ = setLinks_ handle links
  pure unit

simulationSwizzleLinks ::
  forall d r row id m. 
  Bind m =>
  MonadState { simulation :: D3SimulationState_ | row } m =>
  Array (D3Link id r) ->
  Array (D3_SimulationNode d) ->
  (Datum_ -> Index_) ->
  m (Array (D3LinkSwizzled (D3_SimulationNode d) r))
simulationSwizzleLinks links nodes keyFn = do
  handle <- use (_d3Simulation <<< _handle)
  pure $ swizzleLinks_ links nodes keyFn 

simulationGetNodes :: forall m row d.
  (MonadState { simulation :: D3SimulationState_ | row } m) => 
  m (Array (D3_SimulationNode d))
simulationGetNodes = do
  handle <- use (_d3Simulation <<< _handle)
  let opaqueNodes = getNodes_ handle
  pure $ unsafeCoerce opaqueNodes

simulationGetLinks :: forall m row d r. 
  (MonadState { simulation :: D3SimulationState_ | row } m) => 
  m (Array (D3LinkSwizzled (D3_SimulationNode d) r))
simulationGetLinks = do
  handle <- use (_d3Simulation <<< _handle)
  let links = getLinksFromSimulation_ handle
  pure links

simulationCreateTickFunction :: forall selection row m. 
  (MonadState { simulation :: D3SimulationState_ | row } m) =>
  Label -> Step selection -> m Unit
simulationCreateTickFunction label tick@(StepTransformFFI selection fn) = pure unit
simulationCreateTickFunction label tick@(Step selection chain) = do
  handle <- use (_d3Simulation <<< _handle)
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
  handle <- use (_d3Simulation <<< _handle)
  let _ = case drag of 
            DefaultDrag     -> defaultSimulationDrag_ selection handle
            NoDrag          -> disableDrag_ selection
            (CustomDrag fn) -> defaultSimulationDrag_ selection handle -- TODO no custom drag implemented yet
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