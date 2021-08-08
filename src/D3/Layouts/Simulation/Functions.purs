module D3.Simulation.Functions where

import Prelude

import Control.Monad.State (class MonadState, State, get, gets, modify_)
import D3.Attributes.Instances (Label)
import D3.Data.Types (D3Selection_, Datum_, Index_)
import D3.FFI (d3AttachZoomDefaultExtent_, d3AttachZoom_, defaultSimulationDrag_, disableDrag_, getLinksFromSimulation_, getLinks_, getNodes_, onTick_, setAlphaDecay_, setAlphaMin_, setAlphaTarget_, setAlpha_, setAsNullForceInSimulation_, setLinks_, setNodes_, setVelocityDecay_, startSimulation_, stopSimulation_)
import D3.Node (D3_Link, D3_SimulationNode, NodeID)
import D3.Selection (Behavior(..), DragBehavior(..), applyChainableSD3)
import D3.Simulation.Forces (createForce, disableByLabels, enableByLabels, enableForce, enableOnlyTheseLabels, getHandle, putForceInSimulation, setForceAttr, setForceAttrWithFilter)
import D3.Simulation.Types (Force(..), ForceFilter(..), ForceStatus(..), ForceType(..), SimVariable(..), D3SimulationState_(..), Step(..))
import D3.Zoom (ScaleExtent(..), ZoomExtent(..))
import D3Tagless.Capabilities (setForcesByLabel)
import Data.Array (intercalate)
import Data.Array as A
import Data.Foldable (traverse_)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Debug (spy, trace)
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

-- | Underlying functions which allow us to make monadic updates from OUTSIDE of a script
-- | allowing control of the simulation outside of the drawing phase which runs in D3M
setForcesEmpty :: D3SimulationState_ -> D3SimulationState_
setForcesEmpty (SimState_ ss_) = SimState_ ss_ { forces = M.empty }

setForces :: M.Map Label Force -> D3SimulationState_ -> D3SimulationState_
setForces forces (SimState_ ss_) = SimState_ ss_ { forces = forces }

insertForce :: Force -> D3SimulationState_ -> D3SimulationState_
insertForce force@(Force l status t f attrs h_) (SimState_ ss_) = SimState_ ss_ { forces = M.insert l force ss_.forces }

reheatSimulation :: D3SimulationState_ -> D3SimulationState_
reheatSimulation (SimState_ ss_) = SimState_ ss_ { alpha = 1.0 }

-- type SimulationStateRow row = ( simulation :: D3SimulationState_ | row )

simulationAddForces :: forall m row. 
  (MonadState { simulationState :: D3SimulationState_ | row } m) => 
  Array Force -> m Unit
simulationAddForces forces = traverse_ simulationAddForce forces

simulationRemoveAllForces :: forall m row. 
  (MonadState { simulationState :: D3SimulationState_ | row } m) => 
  m Unit
simulationRemoveAllForces = do
  { simulationState: SimState_ ss_} <- get
  let _ = (setAsNullForceInSimulation_ ss_.simulation_) <$> (A.fromFoldable $ M.keys ss_.forces)
      updatedSimulation = SimState_ ss_ { forces = M.empty }
  modify_ (\s -> s { simulationState = updatedSimulation })

simulationAddForce :: forall m row. 
  (MonadState { simulationState :: D3SimulationState_ | row } m) =>
  Force -> m Unit
simulationAddForce force@(Force label status t f attrs h_) = do 
  -- TODO this is where the filter has to wrap the strength
  let _ = 
        case f of
          Nothing       -> (\a -> setForceAttr h_ (unwrap a)) <$> attrs 
          (Just (FilterNodes _ filter)) -> (\a -> setForceAttrWithFilter h_ filter (unwrap a)) <$> attrs 
  { simulationState: SimState_ ss_} <- get
  let _ = if status == ForceActive
          then putForceInSimulation force ss_.simulation_
          else ss_.simulation_
      updatedSimulation = SimState_ ss_ { forces = M.insert label force ss_.forces }
  -- if the force isn't active then we just keep it in map, with label as key
  modify_ (\s -> s { simulationState = updatedSimulation } )

simulationToggleForce :: forall m row. 
  (MonadState { simulationState :: D3SimulationState_ | row } m) =>
  Label -> m Unit
simulationToggleForce label = do
  { simulationState: SimState_ ss_ } <- get
  case M.lookup label ss_.forces of
    Nothing -> pure unit
    (Just (Force _ ForceActive _ _ _ _))   -> simulationDisableForcesByLabel [ label ]
    (Just (Force _ ForceDisabled _ _ _ _)) -> simulationEnableForcesByLabel [ label ]

simulationDisableForcesByLabel :: forall m row. 
  (MonadState { simulationState :: D3SimulationState_ | row } m) =>
  Array Label -> m Unit
simulationDisableForcesByLabel labels = do
  { simulationState: SimState_ ss_} <- get
  let updatedForces = (disableByLabels ss_.simulation_ labels) <$> ss_.forces
      updatedSimulation = SimState_ ss_ { forces = updatedForces }
  modify_ (\s -> s { simulationState = updatedSimulation })

simulationEnableOnlyTheseForces :: forall m row. 
  (MonadState { simulationState :: D3SimulationState_ | row } m) =>
  Array Label -> m Unit
simulationEnableOnlyTheseForces labels = do
  { simulationState: SimState_ ss_} <- get
  let updatedForces = (enableOnlyTheseLabels ss_.simulation_ labels) <$> ss_.forces
      updatedSimulation = SimState_ ss_ { forces = updatedForces }
  modify_ (\s -> s { simulationState = updatedSimulation })


simulationEnableForcesByLabel :: forall m row. 
  (MonadState { simulationState :: D3SimulationState_ | row } m) =>
  Array Label  -> m Unit
simulationEnableForcesByLabel labels  = do
  { simulationState: SimState_ ss_} <- get
  let updatedForces = (enableByLabels ss_.simulation_ labels) <$> ss_.forces
      updatedSimulation = SimState_ ss_ { forces = updatedForces }
  modify_ (\s -> s { simulationState = updatedSimulation })
  
simulationSetVariable :: forall m row.
  (MonadState { simulationState :: D3SimulationState_ | row } m) => 
  SimVariable -> m Unit
simulationSetVariable v = do
  { simulationState: SimState_ ss_} <- get
  let updatedSimulation =
        case v of
          (Alpha n)         -> do
            let _ = setAlpha_ ss_.simulation_ n
            SimState_ ss_ { alpha = n }
          (AlphaTarget n)   -> do
            let _ = setAlphaTarget_ ss_.simulation_ n
            SimState_ ss_ { alphaTarget = n }
          (AlphaMin n)      -> do
            let _ = setAlphaMin_ ss_.simulation_ n
            SimState_ ss_ { alphaMin = n }
          (AlphaDecay n)    -> do
            let _ = setAlphaDecay_ ss_.simulation_ n
            SimState_ ss_ { alphaDecay = n }
          (VelocityDecay n) -> do
            let _ = setVelocityDecay_ ss_.simulation_ n
            SimState_ ss_ { velocityDecay = n }
  modify_ (\s -> s { simulationState = updatedSimulation })

simulationStart :: forall m row. 
  (MonadState { simulationState :: D3SimulationState_ | row } m) =>
  m Unit
simulationStart = do
  { simulationState: SimState_ ss_} <- get
  let _ = startSimulation_ ss_.simulation_
      _ = setAlpha_ ss_.simulation_ 1.0
      updatedSimulation = SimState_ ss_ { alpha = 1.0 }
  modify_ (\state -> state { simulationState = updatedSimulation } )

simulationStop :: forall m row. 
  (MonadState { simulationState :: D3SimulationState_ | row } m) => 
  m Unit
simulationStop = do
  { simulationState: SimState_ ss_} <- get
  let _ = stopSimulation_ ss_.simulation_
  pure unit

simulationShowForces :: forall m row. 
  (MonadState { simulationState :: D3SimulationState_ | row } m) =>
  m String
simulationShowForces = do
  { simulationState: SimState_ ss_} <- get
  let forceTuples = M.toUnfoldable ss_.forces
      showTuple (Tuple label force) = show label <> " " <> show force
  pure $ intercalate "\n" $ showTuple <$> forceTuples

simulationSetNodesAndLinks :: 
  forall id d obj r m row. 
  Bind m =>
  MonadState { simulationState :: D3SimulationState_ | row } m =>
  Array (D3_SimulationNode d) -> Array (D3_Link id r) -> (Datum_ -> id)
    -> m (Tuple (Array (D3_SimulationNode d)) (Array (D3_Link (D3_SimulationNode d) r)))
simulationSetNodesAndLinks nodes links indexFn = do
  { simulationState: SimState_ ss_} <- get
  let opaqueNodes    = ss_.simulation_ `setNodes_` nodes
      maybelinkForce = M.lookup "links" ss_.forces
  case maybelinkForce of
    Nothing -> do
      let newLinksForce = enableForce $ createForce "links" ForceLink Nothing [] -- links force doesn't accept ForceFilter (it kind of already is a force filter by it's nature)
      simulationAddForce newLinksForce
      let updatedLinks = setLinks_ (getHandle newLinksForce) links indexFn -- indexFn is needed to tell D3 how to swizzle the NodeIDs to get object references
      pure (Tuple opaqueNodes updatedLinks)
    (Just linkForce) -> do
      let updatedLinks = setLinks_ (getHandle linkForce) links indexFn -- indexFn is needed to tell D3 how to swizzle the NodeIDs to get object references
      pure (Tuple opaqueNodes updatedLinks)

simulationSetNodes :: forall m row d. 
  (MonadState { simulationState :: D3SimulationState_ | row } m) => 
  Array (D3_SimulationNode d) -> m (Array (D3_SimulationNode d))
simulationSetNodes nodes = do
  { simulationState: SimState_ ss_} <- get
  let opaqueNodes = ss_.simulation_ `setNodes_` nodes
  -- modify_ (\s -> s { simulationState = (SimState_ ss_ { nodes = (unsafeCoerce opaqueNodes) })})
  pure nodes

simulationSetLinks :: forall id m row datum r. 
  (MonadState { simulationState :: D3SimulationState_ | row } m) => 
  Array (D3_Link id r) -> (datum -> id) -> m (Array (D3_Link datum r))
simulationSetLinks links keyFn = do
  { simulationState: SimState_ ss_} <- get
  let newLinksForce = enableForce $ createForce "links" ForceLink Nothing [] -- links force doesn't accept ForceFilter (it kind of already is a force filter by it's nature)
  let updatedLinks = setLinks_ (getHandle newLinksForce) links keyFn -- keyFn is needed to tell D3 how to swizzle the NodeIDs to get object references
  simulationAddForce newLinksForce
  pure (unsafeCoerce updatedLinks) -- TODO notice the coerce here

simulationGetNodes :: forall m row d.
  (MonadState { simulationState :: D3SimulationState_ | row } m) => 
  m (Array (D3_SimulationNode d))
simulationGetNodes = do
  { simulationState: SimState_ ss_} <- get
  let opaqueNodes = getNodes_ ss_.simulation_
  pure $ unsafeCoerce opaqueNodes

simulationGetLinks :: forall m row datum r. 
  (MonadState { simulationState :: D3SimulationState_ | row } m) => 
  m (Array (D3_Link datum r))
simulationGetLinks = do
  { simulationState: SimState_ ss_} <- get
  let opaqueLinks = getLinksFromSimulation_ ss_.simulation_
  pure $ unsafeCoerce opaqueLinks -- TODO notice the coerce here

simulationAddSelection :: forall m row.  -- NB not polymorphic in selection because D3SimulationState_ isn't
  (MonadState { simulationState :: D3SimulationState_ | row } m) =>
  Label -> D3Selection_ -> m Unit
simulationAddSelection label selection = do
  { simulationState: SimState_ ss_} <- get
  modify_ (\s -> s { simulationState = (SimState_ ss_ { selections = M.insert label selection ss_.selections } )})

simulationGetSelection :: forall m row. 
  (MonadState { simulationState :: D3SimulationState_ | row } m) =>
  Label -> m (Maybe D3Selection_)
simulationGetSelection label = do
  { simulationState: SimState_ ss_} <- get
  pure $ M.lookup label ss_.selections


simulationCreateTickFunction :: forall selection row m. 
  (MonadState { simulationState :: D3SimulationState_ | row } m) =>
  Label -> Step selection -> m Unit
simulationCreateTickFunction label tick@(StepTransformFFI selection fn) = pure unit
simulationCreateTickFunction label tick@(Step selection chain) = do
  { simulationState: SimState_ ss_} <- get
  let makeTick _ = do
        -- TODO this coerce is forced upon us here due to forall selection in SimulationM
        -- going to have to parameterize simulation with selection or hide the type dep somehow
        let _ = (applyChainableSD3 (unsafeCoerce selection)) <$> chain
        unit
      updatedTicks      = M.insert label (unsafeCoerce tick) ss_.ticks
      updatedSimulation = SimState_ ss_ { ticks = updatedTicks }
      _                 = onTick_ ss_.simulation_ label makeTick
  modify_ (\s -> s { simulationState = updatedSimulation} )
  
-- the price of being able to treat Drag, Zoom, Click etc the same in SimulationM and SelectionM instances is some duplication here
-- Drag has to behave differently in the simulation case
simulationOn :: forall selection row m. 
  (MonadState { simulationState :: D3SimulationState_ | row } m) =>
   D3Selection_ -> Behavior D3Selection_ -> m Unit
simulationOn selection (Drag drag) = do
  (SimState_ { simulation_ }) <- gets _.simulationState
  let _ = case drag of 
            DefaultDrag     -> defaultSimulationDrag_ selection  simulation_
            NoDrag          -> disableDrag_ selection
            (CustomDrag fn) -> defaultSimulationDrag_ selection simulation_ -- TODO no custom drag implemented yet
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