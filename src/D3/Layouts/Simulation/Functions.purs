module D3.Simulation.Functions where

import Prelude

import Control.Monad.State (class MonadState, State, get, gets, modify_)
import D3.Attributes.Instances (Label)
import D3.Data.Types (D3Selection_, Index_)
import D3.FFI (d3AttachZoomDefaultExtent_, d3AttachZoom_, defaultSimulationDrag_, disableDrag_, onTick_, setAlphaDecay_, setAlphaMin_, setAlphaTarget_, setAlpha_, setAsNullForceInSimulation_, setLinks_, setNodes_, setVelocityDecay_, startSimulation_, stopSimulation_)
import D3.Node (D3_Link, D3_SimulationNode, NodeID)
import D3.Selection (Behavior(..), DragBehavior(..), applyChainableSD3)
import D3.Simulation.Forces (createForce, disableByLabels, enableByLabels, enableForce, getHandle, putForceInSimulation, setForceAttr)
import D3.Simulation.Types (Force(..), ForceStatus(..), ForceType(..), SimVariable(..), SimulationState_(..), Step(..))
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
setForcesEmpty :: SimulationState_ -> SimulationState_
setForcesEmpty (SS_ ss_) = SS_ ss_ { forces = M.empty }

setForces :: M.Map Label Force -> SimulationState_ -> SimulationState_
setForces forces (SS_ ss_) = SS_ ss_ { forces = forces }

insertForce :: Force -> SimulationState_ -> SimulationState_
insertForce force@(Force l status t attrs h_) (SS_ ss_) = SS_ ss_ { forces = M.insert l force ss_.forces }

reheatSimulation :: SimulationState_ -> SimulationState_
reheatSimulation (SS_ ss_) = SS_ ss_ { running = true, alpha = 1.0 }

stopSimulation :: SimulationState_ -> SimulationState_
stopSimulation (SS_ ss_) = SS_ ss_ { running = false }

-- type SimulationStateRow row = ( simulation :: SimulationState_ | row )

simulationAddForces :: forall m row. 
  (MonadState { simulationState :: SimulationState_ | row } m) => 
  Array Force -> m Unit
simulationAddForces forces = traverse_ simulationAddForce forces

simulationRemoveAllForces :: forall m row. 
  (MonadState { simulationState :: SimulationState_ | row } m) => 
  m Unit
simulationRemoveAllForces = do
  { simulationState: SS_ ss_} <- get
  let _ = (setAsNullForceInSimulation_ ss_.simulation_) <$> (A.fromFoldable $ M.keys ss_.forces)
      updatedSimulation = SS_ ss_ { forces = M.empty }
  modify_ (\s -> s { simulationState = updatedSimulation })

simulationAddForce :: forall m row. 
  (MonadState { simulationState :: SimulationState_ | row } m) =>
  Force -> m Unit
simulationAddForce force@(Force label status t attrs h_) = do
  let _ = (\a -> setForceAttr h_ (unwrap a)) <$> attrs 
  { simulationState: SS_ ss_} <- get
  let _ = if status == ForceActive
          then putForceInSimulation force ss_.simulation_
          else ss_.simulation_
      updatedSimulation = SS_ ss_ { forces = M.insert label force ss_.forces }
  -- if the force isn't active then we just keep it in map, with label as key
  modify_ (\s -> s { simulationState = updatedSimulation } )

simulationToggleForce :: forall m row. 
  (MonadState { simulationState :: SimulationState_ | row } m) =>
  Label -> m Unit
simulationToggleForce label = do
  { simulationState: SS_ ss_ } <- get
  case M.lookup label ss_.forces of
    Nothing -> pure unit
    (Just (Force _ ForceActive _ _ _))   -> simulationDisableForcesByLabel [ label ]
    (Just (Force _ ForceDisabled _ _ _)) -> simulationEnableForcesByLabel [ label ]

simulationDisableForcesByLabel :: forall m row. 
  (MonadState { simulationState :: SimulationState_ | row } m) =>
  Array Label -> m Unit
simulationDisableForcesByLabel labels = do
  { simulationState: SS_ ss_} <- get
  let updatedForces = (disableByLabels ss_.simulation_ labels) <$> ss_.forces
      updatedSimulation = SS_ ss_ { forces = updatedForces }
  modify_ (\s -> s { simulationState = updatedSimulation })

simulationEnableForcesByLabel :: forall m row. 
  (MonadState { simulationState :: SimulationState_ | row } m) =>
  Array Label  -> m Unit
simulationEnableForcesByLabel labels  = do
  { simulationState: SS_ ss_} <- get
  let updatedForces = (enableByLabels ss_.simulation_ labels) <$> ss_.forces
      updatedSimulation = SS_ ss_ { forces = updatedForces }
  modify_ (\s -> s { simulationState = updatedSimulation })
  
simulationSetVariable :: forall m row.
  (MonadState { simulationState :: SimulationState_ | row } m) => 
  SimVariable -> m Unit
simulationSetVariable v = do
  { simulationState: SS_ ss_} <- get
  let updatedSimulation =
        case v of
          (Alpha n)         -> do
            let _ = setAlpha_ ss_.simulation_ n
            SS_ ss_ { alpha = n }
          (AlphaTarget n)   -> do
            let _ = setAlphaTarget_ ss_.simulation_ n
            SS_ ss_ { alphaTarget = n }
          (AlphaMin n)      -> do
            let _ = setAlphaMin_ ss_.simulation_ n
            SS_ ss_ { alphaMin = n }
          (AlphaDecay n)    -> do
            let _ = setAlphaDecay_ ss_.simulation_ n
            SS_ ss_ { alphaDecay = n }
          (VelocityDecay n) -> do
            let _ = setVelocityDecay_ ss_.simulation_ n
            SS_ ss_ { velocityDecay = n }
  modify_ (\s -> s { simulationState = updatedSimulation })

simulationStart :: forall m row. 
  (MonadState { simulationState :: SimulationState_ | row } m) =>
  m Unit
simulationStart = do
  { simulationState: SS_ ss_} <- get
  let _ = startSimulation_ ss_.simulation_
      _ = setAlpha_ ss_.simulation_ 1.0
      updatedSimulation = SS_ ss_ { running = true, alpha = 1.0 }
  modify_ (\state -> state { simulationState = updatedSimulation } )

simulationStop :: forall m row. 
  (MonadState { simulationState :: SimulationState_ | row } m) => 
  m Unit
simulationStop = do
  { simulationState: SS_ ss_} <- get
  let _ = stopSimulation_ ss_.simulation_
      updatedSimulation = SS_ ss_ { running = false }
  modify_ (\state -> state { simulationState = updatedSimulation } )

simulationShowForces :: forall m row. 
  (MonadState { simulationState :: SimulationState_ | row } m) =>
  m String
simulationShowForces = do
  { simulationState: SS_ ss_} <- get
  let forceTuples = M.toUnfoldable ss_.forces
      showTuple (Tuple label force) = show label <> " " <> show force
  pure $ intercalate "\n" $ showTuple <$> forceTuples

simulationSetNodes :: forall m row d. 
  (MonadState { simulationState :: SimulationState_ | row } m) => 
  Array (D3_SimulationNode d) -> m (Array (D3_SimulationNode d))
simulationSetNodes nodes = do
  { simulationState: SS_ ss_} <- get
  let opaqueNodes = ss_.simulation_ `setNodes_` nodes
  modify_ (\s -> s { simulationState = (SS_ ss_ { nodes = (unsafeCoerce opaqueNodes) })})
  pure nodes

simulationSetLinks :: forall id m row datum r. 
  (MonadState { simulationState :: SimulationState_ | row } m) => 
  Array (D3_Link id r) -> (datum -> id) -> m (Array (D3_Link datum r))
simulationSetLinks links keyFn = do
  { simulationState: SS_ ss_} <- get
  let newLinksForce = enableForce $ createForce "links" ForceLink []
  let updatedLinks = setLinks_ (getHandle newLinksForce) links keyFn
  simulationAddForce newLinksForce
  pure (unsafeCoerce updatedLinks) -- TODO notice the coerce here

simulationCreateTickFunction :: forall selection row m. 
  (MonadState { simulationState :: SimulationState_ | row } m) =>
  Label -> Step selection -> m Unit
simulationCreateTickFunction label tick@(StepTransformFFI selection fn) = pure unit
simulationCreateTickFunction label tick@(Step selection chain) = do
  { simulationState: SS_ ss_} <- get
  let makeTick _ = do
        -- TODO this coerce is forced upon us here due to forall selection in SimulationM
        -- going to have to parameterize simulation with selection or hide the type dep somehow
        let _ = (applyChainableSD3 (unsafeCoerce selection)) <$> chain
        unit
      updatedTicks      = M.insert label (unsafeCoerce tick) ss_.ticks
      updatedSimulation = SS_ ss_ { ticks = updatedTicks }
      _                 = onTick_ ss_.simulation_ label makeTick
  modify_ (\s -> s { simulationState = updatedSimulation} )
  
-- the price of being able to treat Drag, Zoom, Click etc the same in SimulationM and SelectionM instances is some duplication here
-- Drag has to behave differently in the simulation case
simulationOn :: forall selection row m. 
  (MonadState { simulationState :: SimulationState_ | row } m) =>
   D3Selection_ -> Behavior -> m Unit
simulationOn selection (Drag drag) = do
  (SS_ { simulation_ }) <- gets _.simulationState
  let _ = case drag of 
            DefaultDrag     -> defaultSimulationDrag_ selection  simulation_
            NoDrag          -> disableDrag_ selection
            (CustomDrag fn) -> defaultSimulationDrag_ selection simulation_ -- TODO no custom drag implemented yet
  pure unit

simulationOn selection (Zoom config) = do
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