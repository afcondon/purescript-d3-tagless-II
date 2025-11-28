-- | OrchestrationV2 - Scene transitions using our own force engine
-- |
-- | This replaces the original Orchestration.purs with a cleaner architecture:
-- | - Uses SimulationManager (our own force loop) instead of D3 simulation
-- | - SceneConfigs define forces per scene
-- | - Tick callback handles DOM updates
-- | - We control swizzling
module Component.CodeExplorerV2.OrchestrationV2
  ( -- * Initialization
    initialize
    -- * Scene Transitions
  , transitionToOrbit
  , transitionToTree
  , transitionToForceGraph
  , transitionToBubblePack
    -- * Simulation Control
  , startSimulation
  , stopSimulation
  , reheatSimulation
  ) where

import Prelude

import Component.CodeExplorerV2.SceneConfigs as Scenes
import Component.CodeExplorerV2.SimulationManager as Sim
import D3.Viz.Spago.Files (LinkType(..), SpagoLink)
import D3.Viz.Spago.Model (SpagoModel, SpagoSimNode, isPackage, isUsedModule, nodesToCircle, nodesToRadialTree, unpinAllNodes)
import Data.Array (filter) as Array
import Data.Array as Array
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (notNull, null, toMaybe)
import Data.Number (cos, sin)
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref

-- =============================================================================
-- FFI for DOM Updates
-- =============================================================================

-- | Update node transforms in the DOM
foreign import updateNodeTransforms_ :: Array SpagoSimNode -> Effect Unit

-- | Update link paths in the DOM (expects swizzled links)
foreign import updateLinkPaths_ :: forall linkData.
  Array (Sim.SwizzledLink SpagoSimNode linkData) -> Effect Unit

-- | Update link paths from raw links (does swizzling internally)
foreign import updateLinkPathsFromRaw_ :: Array SpagoSimNode -> Array SpagoLink -> Effect Unit

-- | Create SVG structure and return refs to node/link groups
foreign import createSvgStructure_ :: String -> Number -> Number -> Effect Unit

-- | Join nodes to DOM (D3 enter/update/exit)
foreign import joinNodesToDOM_ :: Array SpagoSimNode -> Effect Unit

-- | Join links to DOM
foreign import joinLinksToDOM_ :: Array SpagoLink -> Effect Unit

-- | Clear links from DOM
foreign import clearLinks_ :: Effect Unit

-- | Get window dimensions
foreign import getWindowSize_ :: Effect { width :: Number, height :: Number }

-- | Staged tree transition (D3 animation + pin + links + simulation)
foreign import transitionToTreePositions_ ::
  Array SpagoSimNode
  -> Array SpagoLink
  -> Effect Unit  -- onComplete callback
  -> Effect Unit

-- =============================================================================
-- State
-- =============================================================================

-- | Global simulation ref (stored in window via SimulationManager)
-- | We also keep a local ref for nodes/links since SimulationManager is generic
type OrchestraState =
  { simRef :: Ref Sim.SimState
  , model :: SpagoModel
  , currentLinks :: Array SpagoLink  -- Raw links (source/target as IDs)
  }

-- Store orchestration state in a module-level ref
foreign import setOrchestraState_ :: OrchestraState -> Effect Unit
foreign import getOrchestraState_ :: Effect OrchestraState

-- =============================================================================
-- Tick Callback
-- =============================================================================

-- | Create the tick callback that updates the DOM
makeTickCallback :: OrchestraState -> Effect Unit
makeTickCallback state = do
  -- Read current nodes from simulation
  simState <- Ref.read state.simRef
  let nodes = simState.nodes

  -- Update node transforms
  updateNodeTransforms_ nodes

  -- Update link paths (using raw links, swizzling done in JS)
  updateLinkPathsFromRaw_ nodes state.currentLinks

-- =============================================================================
-- Initialization
-- =============================================================================

-- | Initialize the visualization
initialize :: SpagoModel -> String -> Effect (Ref Sim.SimState)
initialize model selector = do
  log "[OrchV2] Initializing"

  -- Get window size
  { width, height } <- getWindowSize_

  -- Create SVG structure
  createSvgStructure_ selector width height

  -- Create simulation
  simRef <- Sim.createSimulation

  -- Position nodes for Orbit scene (packages on circle, modules at package positions)
  let positionedNodes = nodesToCircle isPackage 1120.0 model.nodes

  -- Set nodes in simulation
  Sim.setNodes positionedNodes simRef

  -- Create orchestra state
  let orchestraState =
        { simRef
        , model: model { nodes = positionedNodes }
        , currentLinks: []
        }
  setOrchestraState_ orchestraState

  -- Join nodes to DOM
  joinNodesToDOM_ positionedNodes

  -- Set tick callback
  Sim.setTickCallback (makeTickCallback orchestraState) simRef

  -- Apply Orbit scene forces
  Scenes.applyScene Scenes.orbitScene simRef

  -- Start simulation
  Sim.start simRef

  log "[OrchV2] Initialized with Orbit scene"
  pure simRef

-- =============================================================================
-- Scene Transitions
-- =============================================================================

-- | Transition to Orbit scene
transitionToOrbit :: Effect Unit
transitionToOrbit = do
  log "[OrchV2] Transitioning to Orbit"
  state <- getOrchestraState_

  -- Stop simulation
  Sim.stop state.simRef

  -- Clear links
  clearLinks_

  -- Reposition nodes for orbit
  let positionedNodes = nodesToCircle isPackage 1120.0 state.model.nodes

  -- Update nodes in simulation
  Sim.setNodes positionedNodes state.simRef

  -- Update links state
  let newState = state { currentLinks = [] }
  setOrchestraState_ newState

  -- Re-join nodes to DOM
  joinNodesToDOM_ positionedNodes

  -- Update tick callback
  Sim.setTickCallback (makeTickCallback newState) state.simRef

  -- Apply Orbit scene forces
  Scenes.applyScene Scenes.orbitScene state.simRef

  -- Start simulation
  Sim.reheat state.simRef

  log "[OrchV2] Orbit scene active"

-- | Transition to Tree scene (staged: animate -> pin -> links -> simulate)
transitionToTree :: Effect Unit
transitionToTree = do
  log "[OrchV2] Transitioning to Tree (staged)"
  state <- getOrchestraState_

  -- Stop any running simulation
  Sim.stop state.simRef

  -- Get tree links
  let treeLinks = Array.filter (\l -> l.linktype == M2M_Tree) state.model.links

  -- Update state with tree links
  let newState = state { currentLinks = treeLinks }
  setOrchestraState_ newState

  -- Start the staged transition
  -- Phase 1: D3 animates nodes to tree positions (staggered by layer)
  -- Phase 2: Pins nodes at final positions
  -- Phase 3: Adds bezier tree links
  -- Phase 4: After delay, swaps to straight links and calls onComplete
  transitionToTreePositions_ state.model.nodes treeLinks (onTreeTransitionComplete newState)

  where
  -- Called after D3 transition + link swap is complete
  onTreeTransitionComplete :: OrchestraState -> Effect Unit
  onTreeTransitionComplete newState = do
    log "[OrchV2] Tree transition complete, engaging simulation"

    -- Read the nodes (they've been updated in place by JS)
    simState <- Ref.read newState.simRef

    -- Update simulation with pinned nodes
    Sim.setNodes simState.nodes newState.simRef

    -- Create and initialize link force
    let linkForce = Sim.createLink { distance: 125.0, strength: 0.3, iterations: 3.0 }
    _ <- Sim.initializeLinkForce linkForce simState.nodes newState.currentLinks

    -- Update tick callback
    Sim.setTickCallback (makeTickCallback newState) newState.simRef

    -- Apply Tree scene forces
    Scenes.applyScene Scenes.treeScene newState.simRef
    Sim.addForce "links" linkForce newState.simRef

    -- Unpin nodes so they can be pulled by forces
    unpinAllNodesInPlace simState.nodes

    -- Start simulation with moderate energy
    Sim.reheat newState.simRef

    log "[OrchV2] Tree scene active with simulation"

-- | Unpin nodes in place (mutates)
unpinAllNodesInPlace :: Array SpagoSimNode -> Effect Unit
unpinAllNodesInPlace nodes = unpinNodes_ nodes

foreign import unpinNodes_ :: Array SpagoSimNode -> Effect Unit

-- | Transition to Force Graph scene
transitionToForceGraph :: Effect Unit
transitionToForceGraph = do
  log "[OrchV2] Transitioning to ForceGraph"
  state <- getOrchestraState_

  -- Stop simulation
  Sim.stop state.simRef

  -- Clear existing links
  clearLinks_

  -- Unpin all nodes so forces can move them
  let unpinnedNodes = unpinAllNodes state.model.nodes

  -- Update nodes in simulation
  Sim.setNodes unpinnedNodes state.simRef

  -- Get tree links for force graph
  let treeLinks = Array.filter (\l -> l.linktype == M2M_Tree) state.model.links

  -- Update state
  let newState = state { currentLinks = treeLinks }
  setOrchestraState_ newState

  -- Create link force and initialize with links
  let linkForce = Sim.createLink { distance: 125.0, strength: 1.0, iterations: 6.0 }
  _ <- Sim.initializeLinkForce linkForce unpinnedNodes treeLinks

  -- Join to DOM
  joinNodesToDOM_ unpinnedNodes
  joinLinksToDOM_ treeLinks

  -- Update tick callback
  Sim.setTickCallback (makeTickCallback newState) state.simRef

  -- Apply ForceGraph scene (includes link force)
  Scenes.applyScene Scenes.forceGraphScene state.simRef

  -- Also need to add the initialized link force
  Sim.addForce "links" linkForce state.simRef

  -- Reheat to full energy
  Sim.reheat state.simRef

  log "[OrchV2] ForceGraph scene active"

-- | Transition to Bubble Pack scene
transitionToBubblePack :: Effect Unit
transitionToBubblePack = do
  log "[OrchV2] Transitioning to BubblePack"
  state <- getOrchestraState_

  -- Stop simulation
  Sim.stop state.simRef

  -- Clear links
  clearLinks_

  -- Use current node positions but unpin
  simState <- Ref.read state.simRef
  let unpinnedNodes = unpinAllNodes simState.nodes

  -- Update nodes
  Sim.setNodes unpinnedNodes state.simRef

  -- Get tree links
  let treeLinks = Array.filter (\l -> l.linktype == M2M_Tree) state.model.links

  -- Update state
  let newState = state { currentLinks = treeLinks }
  setOrchestraState_ newState

  -- Create and initialize link force
  let linkForce = Sim.createLink { distance: 50.0, strength: 0.5, iterations: 2.0 }
  _ <- Sim.initializeLinkForce linkForce unpinnedNodes treeLinks

  -- Join to DOM
  joinNodesToDOM_ unpinnedNodes
  joinLinksToDOM_ treeLinks

  -- Update tick callback
  Sim.setTickCallback (makeTickCallback newState) state.simRef

  -- Apply BubblePack scene
  Scenes.applyScene Scenes.bubblePackScene state.simRef
  Sim.addForce "links" linkForce state.simRef

  -- Reheat
  Sim.reheat state.simRef

  log "[OrchV2] BubblePack scene active"

-- =============================================================================
-- Simulation Control
-- =============================================================================

-- | Start the simulation
startSimulation :: Effect Unit
startSimulation = do
  state <- getOrchestraState_
  Sim.start state.simRef

-- | Stop the simulation
stopSimulation :: Effect Unit
stopSimulation = do
  state <- getOrchestraState_
  Sim.stop state.simRef

-- | Reheat the simulation
reheatSimulation :: Effect Unit
reheatSimulation = do
  state <- getOrchestraState_
  Sim.reheat state.simRef
