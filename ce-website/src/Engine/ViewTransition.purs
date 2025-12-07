-- | View Transition - GUP-style enter/exit/update for view changes
-- |
-- | Based on LesMisV3/GUPDemo pattern: tick-driven transitions with progress tracking.
-- | Nodes not in a view are removed from DOM entirely.
-- |
-- | Key insight: Simulation tick drives EVERYTHING - positions AND transitions.
-- | No CSS transitions needed. All visual properties interpolated in PureScript.
module Engine.ViewTransition
  ( -- * Types
    ViewNodes(..)
  , TransitionState
  , RenderNode
  , TransitionDelta
  , defaultDelta
    -- * State management
  , mkTransitionState
  , computeTransition
  , tickTransitionState
    -- * Node filtering
  , getViewNodes
  , getNodesForView
  , viewNeedsPackages
  , viewNeedsModules
    -- * Visual interpolation
  , nodeOpacity
  , nodeRadius
    -- * FFI for DOM updates
  , updateCircleTransitions
  , removeCompletedExits
  , addEnteringNodes
  , applyViewTransition
    -- * Package labels for TopoGraph
  , renderPackageLabels
  , clearPackageLabels
  ) where

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Engine.ViewState (ViewState(..), OverviewView(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import PSD3.Transition.Tick as Tick
import Types (SimNode, NodeType(..))

-- =============================================================================
-- Types
-- =============================================================================

-- | Which nodes a view needs in the DOM
data ViewNodes
  = AllNodes          -- ^ Treemap: packages + all modules
  | UsedModulesOnly   -- ^ Tree, Force: modules in spanning tree only
  | PackagesOnly      -- ^ Topo: just packages

derive instance eqViewNodes :: Eq ViewNodes

-- | Progress increment per tick (at 60fps: 0.04 = 25 ticks = ~0.4 seconds)
type TransitionDelta = Number

-- | Default transition speed
defaultDelta :: TransitionDelta
defaultDelta = 0.04

-- | State for tracking node transitions
type TransitionState =
  { enteringProgress :: Map Int Tick.Progress   -- nodeId -> progress (0->1)
  , exitingNodes :: Array (Tick.Transitioning SimNode)  -- nodes being animated out
  , currentViewNodes :: ViewNodes               -- what the current view expects
  }

-- | Node ready for rendering with computed visual state
type RenderNode =
  { node :: SimNode
  , enterProgress :: Maybe Number  -- Just 0.0-1.0 if entering
  , exitProgress :: Maybe Number   -- Just 0.0-1.0 if exiting
  }

-- =============================================================================
-- State Management
-- =============================================================================

-- | Create initial transition state (all nodes entering)
mkTransitionState :: ViewNodes -> Array SimNode -> TransitionState
mkTransitionState viewNodes nodes =
  let
    nodeIds = map _.id nodes
    enteringProgress = Tick.startProgress nodeIds Map.empty
  in
    { enteringProgress
    , exitingNodes: []
    , currentViewNodes: viewNodes
    }

-- | Compute transition when changing views
-- | Returns new state with enter/exit sets populated
computeTransition
  :: ViewState
  -> Array SimNode  -- All available nodes
  -> TransitionState
  -> TransitionState
computeTransition targetView allNodes state =
  let
    targetViewNodes = getViewNodes targetView

    -- Skip if view type unchanged
    _ = if targetViewNodes == state.currentViewNodes
      then state
      else state

    -- Get which nodes should be visible in target vs current view
    currentVisible = Set.fromFoldable $ map _.id $ filterNodesForViewType state.currentViewNodes allNodes
    targetVisible = Set.fromFoldable $ map _.id $ filterNodesForViewType targetViewNodes allNodes

    -- Compute enter/exit sets
    enteringIds = Set.difference targetVisible currentVisible
    exitingIds = Set.difference currentVisible targetVisible

    -- Get exiting node data (freeze their positions for animation)
    exitingNodeData = Array.filter (\n -> Set.member n.id exitingIds) allNodes
    newExiting = Tick.startTransitions exitingNodeData

    -- Start entering progress for new nodes
    newEnteringProgress = Tick.startProgress (Set.toUnfoldable enteringIds) state.enteringProgress

    -- Remove any nodes that are now entering from the exiting list
    -- (in case of rapid view switches)
    filteredExiting = Array.filter (\t -> not (Set.member t.item.id enteringIds))
                        (state.exitingNodes <> newExiting)
  in
    { enteringProgress: newEnteringProgress
    , exitingNodes: filteredExiting
    , currentViewNodes: targetViewNodes
    }

-- | Advance transition state on each tick
-- | Returns updated state with completed transitions removed
tickTransitionState :: TransitionDelta -> TransitionState -> TransitionState
tickTransitionState delta state =
  let
    { active: stillEntering } = Tick.tickProgressMap delta state.enteringProgress
    { active: stillExiting } = Tick.tickTransitions delta state.exitingNodes
  in
    state
      { enteringProgress = stillEntering
      , exitingNodes = stillExiting
      }

-- =============================================================================
-- Node Filtering
-- =============================================================================

-- | Get ViewNodes type for a ViewState
getViewNodes :: ViewState -> ViewNodes
getViewNodes (Overview TreemapView) = AllNodes
getViewNodes (Overview TreeView) = UsedModulesOnly
getViewNodes (Overview ForceView) = UsedModulesOnly
getViewNodes (Overview TopoView) = PackagesOnly
getViewNodes (Detail _) = UsedModulesOnly  -- Detail views show used modules

-- | Filter nodes based on view type
filterNodesForViewType :: ViewNodes -> Array SimNode -> Array SimNode
filterNodesForViewType AllNodes nodes = nodes
filterNodesForViewType UsedModulesOnly nodes =
  Array.filter (\n -> n.nodeType == ModuleNode && n.isInTree) nodes
filterNodesForViewType PackagesOnly nodes =
  Array.filter (\n -> n.nodeType == PackageNode) nodes

-- | Get nodes for rendering in a specific view
-- | Includes both visible nodes and exiting nodes (for fade-out animation)
getNodesForView :: ViewState -> Array SimNode -> TransitionState -> Array RenderNode
getNodesForView viewState allNodes state =
  let
    viewNodes = getViewNodes viewState
    visibleNodes = filterNodesForViewType viewNodes allNodes

    -- Build render nodes for visible nodes (may be entering)
    renderVisible = map (\n ->
      { node: n
      , enterProgress: Map.lookup n.id state.enteringProgress
      , exitProgress: Nothing
      }) visibleNodes

    -- Add exiting nodes with exit progress
    renderExiting = map (\t ->
      { node: t.item
      , enterProgress: Nothing
      , exitProgress: Just t.progress
      }) state.exitingNodes
  in
    renderVisible <> renderExiting

-- | Does this view show packages?
viewNeedsPackages :: ViewState -> Boolean
viewNeedsPackages (Overview TreemapView) = true
viewNeedsPackages (Overview TopoView) = true
viewNeedsPackages _ = false

-- | Does this view show modules?
viewNeedsModules :: ViewState -> Boolean
viewNeedsModules (Overview TreemapView) = true
viewNeedsModules (Overview TreeView) = true
viewNeedsModules (Overview ForceView) = true
viewNeedsModules (Overview TopoView) = false
viewNeedsModules (Detail _) = true

-- =============================================================================
-- Visual Interpolation (Tick-Driven)
-- =============================================================================

-- | Get opacity for a node based on transition state
-- | Entering: fades in from 0 -> 1
-- | Exiting: fades out from 1 -> 0
-- | Normal: 1.0
nodeOpacity :: RenderNode -> Number
nodeOpacity { enterProgress, exitProgress } =
  case enterProgress, exitProgress of
    Just p, _ -> Tick.lerp 0.0 1.0 (Tick.easeOut p)  -- Fade in
    _, Just p -> Tick.lerp 1.0 0.0 (Tick.easeOut p)  -- Fade out
    _, _ -> 1.0

-- | Get radius multiplier for a node based on transition state
-- | Entering: pops in from small
-- | Exiting: shrinks out
-- | Normal: 1.0 (no change to base radius)
nodeRadius :: RenderNode -> Number -> Number
nodeRadius { enterProgress, exitProgress } baseRadius =
  case enterProgress, exitProgress of
    Just p, _ -> Tick.lerp 0.0 baseRadius (Tick.easeOut p)  -- Grow in
    _, Just p -> Tick.lerp baseRadius 0.0 (Tick.easeIn p)   -- Shrink out
    _, _ -> baseRadius

-- =============================================================================
-- FFI for DOM Updates
-- =============================================================================

-- | Type for transition state passed to JavaScript
type TransitionMapEntry =
  { opacity :: Number
  , radius :: Number
  , exitComplete :: Boolean
  }

-- | Update circle visual properties (opacity, radius) based on transition state
foreign import updateCircleTransitions_ :: String -> Effect Unit

-- | Remove circles that have completed their exit animation
foreign import removeCompletedExits_ :: String -> Effect Unit

-- | Add entering nodes to the DOM
foreign import addEnteringNodes_ :: String -> Array SimNode -> Effect Unit

-- | Update node transition state on bound data
foreign import updateNodeTransitionState_ :: String -> Object TransitionMapEntry -> Effect Unit

-- | Wrapped version with selector
updateCircleTransitions :: String -> Effect Unit
updateCircleTransitions = updateCircleTransitions_

-- | Wrapped version with selector
removeCompletedExits :: String -> Effect Unit
removeCompletedExits = removeCompletedExits_

-- | Wrapped version with selector
addEnteringNodes :: String -> Array SimNode -> Effect Unit
addEnteringNodes = addEnteringNodes_

-- | Apply view transition: update __data__, then update visuals, then remove exited
-- | This is the main function to call from the tick handler
applyViewTransition :: String -> ViewState -> Array SimNode -> TransitionState -> Effect Unit
applyViewTransition selector viewState allNodes transitionState = do
  -- Get render nodes with transition progress
  let renderNodes = getNodesForView viewState allNodes transitionState

  -- Build transition map for JavaScript: nodeId -> { opacity, radius, exitComplete }
  let transitionMap = buildTransitionMap renderNodes

  -- Update bound data with transition values
  updateNodeTransitionState_ selector transitionMap

  -- Update visual properties from bound data
  updateCircleTransitions_ selector

  -- Remove nodes that have completed exiting
  removeCompletedExits_ selector

-- | Build transition map from render nodes
buildTransitionMap :: Array RenderNode -> Object TransitionMapEntry
buildTransitionMap renderNodes =
  Object.fromFoldable $ map mkEntry renderNodes
  where
  mkEntry :: RenderNode -> Tuple String TransitionMapEntry
  mkEntry rn =
    let
      opacity = nodeOpacity rn
      radius = nodeRadius rn rn.node.r
      exitComplete = case rn.exitProgress of
        Just p -> p >= 1.0
        Nothing -> false
    in
      Tuple (show rn.node.id) { opacity, radius, exitComplete }

-- =============================================================================
-- Package Labels for TopoGraph
-- =============================================================================

-- | Render package labels in TopoGraph view
foreign import renderPackageLabels_ :: Array SimNode -> Effect Unit

-- | Clear package labels
foreign import clearPackageLabels_ :: Effect Unit

-- | Render package labels for TopoGraph view
renderPackageLabels :: Array SimNode -> Effect Unit
renderPackageLabels = renderPackageLabels_

-- | Clear package labels when leaving TopoGraph
clearPackageLabels :: Effect Unit
clearPackageLabels = clearPackageLabels_
