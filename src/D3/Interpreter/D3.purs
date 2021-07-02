module D3.Interpreter.D3 where

import D3.FFI
import D3.Interpreter
import Prelude hiding (append)

import Control.Monad.State (class MonadState, State, StateT, get, modify_, runStateT)
import D3.Attributes.Instances (Attribute(..), Label, unbox)
import D3.Attributes.Sugar (classed, viewBox)
import D3.Data.Types (D3Selection_, D3Simulation_, Element(..))
import D3.Node (D3_SimulationNode)
import D3.Selection (Behavior(..), ChainableS(..), D3_Node(..), DragBehavior(..), Join(..), Keys(..), OrderingAttribute(..), node)
import D3.Simulation.Config (ChainableF(..), D3ForceHandle_, defaultConfigSimulation)
import D3.Simulation.Forces (Force(..), ForceStatus(..), disableByLabels, enableByLabels, putForceInSimulation, setForceAttr)
import D3.Zoom (ScaleExtent(..), ZoomExtent(..))
import Data.Array (intercalate)
import Data.Array as A
import Data.Foldable (foldl, traverse_)
import Data.Identity (Identity(..))
import Data.Map as M
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Unsafe.Coerce (unsafeCoerce)

-- not actually using Effect in foreign fns to keep sigs simple (for now)

-- newtype D3M :: forall k. k -> Type -> Type
newtype D3M state selection a = D3M (StateT state Effect a) 

derive newtype instance functorD3M     :: Functor           (D3M state selection)
derive newtype instance applyD3M       :: Apply             (D3M state selection)
derive newtype instance applicativeD3M :: Applicative       (D3M state selection)
derive newtype instance bindD3M        :: Bind              (D3M state selection)
derive newtype instance monadD3M       :: Monad             (D3M state selection)
derive newtype instance monadStateD3M  :: MonadState  state (D3M state selection) 
derive newtype instance monadEffD3M    :: MonadEffect       (D3M state selection)

runD3M :: forall a. D3M Unit D3Selection_ a -> Effect (Tuple a Unit)
runD3M (D3M state_T) = runStateT state_T unit

eval_D3M :: forall a. D3M Unit D3Selection_ a -> Effect a
eval_D3M (D3M state_T) = liftA1 fst $ runStateT state_T unit

exec_D3M :: forall a. D3M Unit D3Selection_ a -> Effect Unit
exec_D3M (D3M state_T) = liftA1 snd $ runStateT state_T unit

data SimulationState_ = SS_ { -- TODO move back to Simulation.purs ?
    simulation    :: D3Simulation_
  , running       :: Boolean
  , forces        :: M.Map Label Force
  , ticks         :: M.Map Label (Array ChainableS)

  , alpha         :: Number
  , alphaTarget   :: Number
  , alphaMin      :: Number
  , alphaDecay    :: Number
  , velocityDecay :: Number
}

initialSimulationState = SS_
   {  simulation   : initSimulation_ defaultConfigSimulation  
    , alpha        : defaultConfigSimulation.alpha
    , alphaTarget  : defaultConfigSimulation.alphaTarget
    , alphaMin     : defaultConfigSimulation.alphaMin
    , alphaDecay   : defaultConfigSimulation.alphaDecay
    , velocityDecay: defaultConfigSimulation.velocityDecay
    , running      : defaultConfigSimulation.running
    , forces       : M.empty
    , ticks        : M.empty
  }

run_D3M_Simulation :: forall a. SimulationState_ -> D3M SimulationState_ D3Selection_ a -> Effect (Tuple a SimulationState_)
run_D3M_Simulation simulation (D3M state_T) = runStateT state_T simulation

eval_D3M_Simulation :: forall a. SimulationState_ -> D3M SimulationState_ D3Selection_ a -> Effect a
eval_D3M_Simulation simulation (D3M state_T) = liftA1 fst $ runStateT state_T simulation

exec_D3M_Simulation :: forall a. SimulationState_ -> D3M SimulationState_ D3Selection_ a -> Effect SimulationState_
exec_D3M_Simulation simulation (D3M state_T) = liftA1 snd $ runStateT state_T simulation

-- | ====================================================
-- | Selection instance (capability) for the D3 interpreter
-- | ====================================================
instance d3TaglessD3M :: SelectionM D3Selection_ (D3M state D3Selection_) where
  attach selector = pure $ d3SelectAllInDOM_ selector 

  appendElement selection_ (D3_Node element attributes) = do
    let appended_ = d3Append_ (show element) selection_
    modifySelection appended_ attributes -- this modify is NOT stateT modify
    pure appended_

  filterSelection selection_ selector = pure $ d3FilterSelection_ selection_ selector

  modifySelection selection_ attributes = do
    let _ = foldl applyChainableSD3 selection_ attributes
    pure unit

  join selection (Join j) = do
    let 
      selectS = d3SelectionSelectAll_ (show j.element) selection
      dataS   = case j.key of
                  UseDatumAsKey    -> d3Data_        j.data    selectS 
                  (ComputeKey fn)  -> d3KeyFunction_ j.data fn selectS 
      enterS  = d3EnterAndAppend_ (show j.element) dataS
      enterS' = foldl applyChainableSD3 enterS j.behaviour
    pure enterS'

  join selection (JoinGeneral j) = do
    let
      selectS = d3SelectionSelectAll_ (show j.element) selection
      dataS  = case j.key of
                UseDatumAsKey    -> d3Data_        j.data    selectS 
                (ComputeKey fn)  -> d3KeyFunction_ j.data fn selectS 
      enterS = d3EnterAndAppend_ (show j.element) dataS
      exitS  = d3Exit_ dataS
      _      = foldl applyChainableSD3 enterS  j.behaviour.enter
      _      = foldl applyChainableSD3 exitS   j.behaviour.exit
      _      = foldl applyChainableSD3 dataS   j.behaviour.update
    pure enterS
  
  on selection (Drag drag) = do
    let _ = case drag of 
              DefaultDrag     -> defaultDrag_ selection 
              NoDrag          -> disableDrag_ selection
              (CustomDrag fn) -> defaultDrag_ selection -- TODO no custom drag implemented yet
    pure unit

  on selection (Zoom config) = do
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


applyChainableSD3 :: D3Selection_ -> ChainableS -> D3Selection_
applyChainableSD3 selection_ (AttrT (ToAttribute label attr)) = 
  d3SetAttr_ label (unbox attr) selection_

-- NB only protection against non-text attribute for Text field is in the helper function
-- and similarly for Property and HTML
applyChainableSD3 selection_ (TextT (ToAttribute label attr))     = d3SetText_     (unbox attr) selection_ 
applyChainableSD3 selection_ (PropertyT (ToAttribute label attr)) = d3SetProperty_ (unbox attr) selection_ 
applyChainableSD3 selection_ (HTMLT (ToAttribute label attr))     = d3SetHTML_     (unbox attr) selection_ 

-- NB this remove call will have no effect on elements with active or pending transitions
-- and this gives rise to very counter-intuitive misbehaviour as subsequent enters clash with 
-- elements that should have been removed
-- also NB "selection" here will often be a "transition" but this distinction won't matter (i think)
-- TODO remove is not like other chainables, in fact it's not chainable since it returns unit
applyChainableSD3 selection_ RemoveT = do
  let _ = d3RemoveSelection_ selection_ 
  selection_

-- for transition in D3 we must use .call(selection, transition) so that chain continues
-- in this interpreter it's enought to just return the selection instead of the transition
applyChainableSD3 selection_ (TransitionT chain transition) = do
  let tHandler = d3AddTransition_ selection_ transition
      _        = foldl applyChainableSD3 tHandler chain
  selection_ -- NB we return selection, not transition

applyChainableSD3 selection_ (OnT event listener) = selectionOn_ selection_ (show event) listener

applyChainableSD3 selection_ (OrderingT oAttr) =
  case oAttr of
    Order          -> d3OrderSelection_ selection_
    (Sort compare) -> d3SortSelection_ selection_ compare
    Raise          -> d3RaiseSelection_ selection_
    Lower          -> d3LowerSelection_ selection_


-- TODO reuse existing SVG if it's the right one
removeExistingSVG :: forall m. SelectionM D3Selection_ m => String -> m D3Selection_
removeExistingSVG rootSelector = do
  let
    root     = d3SelectFirstInDOM_ rootSelector
    -- check for an svg element under the given root
    previous = d3SelectionSelect_ (rootSelector <> " svg") root
  pure $ case d3SelectionIsEmpty_ previous of -- 
          true  -> previous
          false -> d3RemoveSelection_ previous 

-- | ====================================================
-- | Simulation instance (capability) for the D3 interpreter
-- | ====================================================
instance simulationD3M :: SimulationM (D3M SimulationState_ D3Selection_) where
  removeAllForces = do
    sim <- get
    let (Identity tuple) = runStateT simulationRemoveAllForces sim
    pure unit

  loadForces forces = do
    sim <- get
    let (Identity tuple) = runStateT (simulationLoadForces forces) sim
    pure unit
  
  addForce force = do
    sim <- get
    let (Identity tuple) = runStateT (simulationAddForce force) sim
    pure unit

  disableForcesByLabel labels = do
    sim <- get
    let (Identity tuple) = runStateT (simulationDisableForcesByLabel labels) sim
    pure unit

  enableForcesByLabel labels  = do
    sim <- get
    let (Identity tuple) = runStateT (simulationEnableForcesByLabel labels) sim
    pure unit
    
  setAlpha v = do
    sim <- get
    let (Identity tuple) = runStateT (simulationSetAlpha v) sim
    pure unit

  setAlphaTarget v = do
    sim <- get
    let (Identity tuple) = runStateT (simulationSetAlphaTarget v) sim
    pure unit

  setAlphaMin v = do
    sim <- get
    let (Identity tuple) = runStateT (simulationSetAlphaMin v) sim
    pure unit

  setAlphaDecay v = do
    sim <- get
    let (Identity tuple) = runStateT (simulationSetAlphaDecay v) sim
    pure unit

  setVelocityDecay v = do
    sim <- get
    let (Identity tuple) = runStateT (simulationSetVelocityDecay v) sim
    pure unit

  start = do
    sim <- get
    let (Identity tuple) = runStateT simulationStart sim
    pure unit

  stop = do
    sim <- get
    let (Identity tuple) = runStateT simulationStop sim
    pure unit

  setNodes nodes = do
    sim <- get
    let (Identity tuple) = runStateT (simulationSetNodes nodes) sim
    pure $ fst tuple

  createTickFunction (Step label selection chain) = do
    (SS_ sim) <- get
    let makeTick _ = do
          -- TODO this coerce is forced upon us here due to forall selection in SimulationM
          let _ = (applyChainableSD3 (unsafeCoerce selection)) <$> chain
          unit
    pure $ onTick_ sim.simulation label makeTick

-- | Underlying functions which allow us to make monadic updates from OUTSIDE of a script
-- | allowing control of the simulation outside of the drawing phase which runs in D3M
simulationLoadForces :: Array Force -> State SimulationState_ Unit
simulationLoadForces forces = do
  simulationRemoveAllForces
  traverse_ simulationAddForce forces

simulationRemoveAllForces :: State SimulationState_ Unit
simulationRemoveAllForces = do
    (SS_ sim) <- get
    let _ = (setAsNullForceInSimulation_ sim.simulation) <$> (A.fromFoldable $ M.keys sim.forces)
    modify_ (\(SS_ s) -> SS_ s { forces = (M.empty :: M.Map Label Force) } )

simulationAddForce :: Force -> State SimulationState_ Unit
simulationAddForce force@(Force l status t attrs h_) = do
    -- TODO this should be a traverse_ eventually
    let _ = (\a -> setForceAttr h_ (unwrap a)) <$> attrs 
    (SS_ sim) <- get
    let _ = if status == ForceActive
            then putForceInSimulation force sim.simulation
            else sim.simulation
    -- if the force isn't active then we just keep it in map, with label as key
    modify_ $ (\s -> SS_ sim { forces = M.insert l force sim.forces })

simulationDisableForcesByLabel :: Array Label -> State SimulationState_ Unit
simulationDisableForcesByLabel labels = do
  (SS_ sim) <- get
  let updatedForces = (disableByLabels sim.simulation labels) <$> sim.forces
  modify_ (\s -> SS_ sim { forces = updatedForces } )

simulationEnableForcesByLabel :: Array Label  -> State SimulationState_ Unit
simulationEnableForcesByLabel labels  = do
  (SS_ sim) <- get
  let updatedForces = (enableByLabels sim.simulation labels) <$> sim.forces
  modify_ (\s -> SS_ sim { forces = updatedForces } )
  
simulationSetAlpha :: Number -> State SimulationState_ Unit
simulationSetAlpha v = do
  (SS_ sim) <- get
  let _ = setAlpha_ sim.simulation v
  modify_ (\s -> SS_ sim { alpha = v } )

simulationSetAlphaTarget :: Number -> State SimulationState_ Unit
simulationSetAlphaTarget v = do
  (SS_ sim) <- get
  let _ = setAlphaTarget_ sim.simulation v
  modify_  (\s -> SS_ sim { alphaTarget = v })

simulationSetAlphaMin :: Number -> State SimulationState_ Unit
simulationSetAlphaMin v = do
  (SS_ sim) <- get
  let _ = setAlphaMin_ sim.simulation v
  modify_  (\s -> SS_ sim { alphaMin = v })

simulationSetAlphaDecay :: Number -> State SimulationState_ Unit
simulationSetAlphaDecay v = do
  (SS_ sim) <- get
  let _ = setAlphaDecay_ sim.simulation v
  modify_  (\s -> SS_ sim { alphaDecay = v })

simulationSetVelocityDecay :: Number -> State SimulationState_ Unit
simulationSetVelocityDecay v = do
  (SS_ sim) <- get
  let _ = setVelocityDecay_ sim.simulation v
  modify_  (\s -> SS_ sim { velocityDecay = v })

simulationStart :: State SimulationState_ Unit
simulationStart = do
  (SS_ sim) <- get
  let _ = startSimulation_  sim.simulation
      _ = setAlpha_ sim.simulation 1.0
  modify_ (\s -> SS_ sim { running = true, alpha = 1.0 } )

simulationStop :: State SimulationState_ Unit
simulationStop = do
  (SS_ sim) <- get
  let _ = stopSimulation_ sim.simulation
  modify_ (\s -> SS_ sim { running = false } )

simulationShowForces :: State SimulationState_ String
simulationShowForces = do
  (SS_ sim) <- get
  let forceTuples = M.toUnfoldable sim.forces
      showTuple (Tuple label force) = show label <> " " <> show force
  pure $ intercalate "\n" $ showTuple <$> forceTuples

simulationSetNodes :: forall d. Array (D3_SimulationNode d) -> State SimulationState_ (Array (D3_SimulationNode d))
simulationSetNodes nodes = do
  (SS_ sim) <- get
  pure (sim.simulation `setNodes_` nodes)

simulationCreateTickFunction :: forall selection. Step selection -> State SimulationState_ Unit
simulationCreateTickFunction (Step label selection chain) = do
  (SS_ sim) <- get
  let makeTick _ = do
        -- TODO this coerce is forced upon us here due to forall selection in SimulationM
        let _ = (applyChainableSD3 (unsafeCoerce selection)) <$> chain
        unit
  pure $ onTick_ sim.simulation label makeTick