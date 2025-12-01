-- | Code Explorer Visualization
-- |
-- | Manages the force simulation and rendering for all scenes.
-- | Uses tick-driven transitions for smooth scene changes.
module Viz.Explorer
  ( initExplorer
  , transitionToScene
  , getTransitionProgress
  ) where

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number (cos, sin)
import Data.Nullable (Nullable, null, notNull)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Data.Loader (LoadedModel)
import PSD3.ForceEngine.Simulation as Sim
import PSD3.ForceEngine.Core as Core
import PSD3.ForceEngine.Types (ForceSpec(..))
import PSD3.Transition.Tick as Tick
import PSD3v2.Attribute.Types (cx, cy, fill, stroke, strokeWidth, radius, id_, class_, width, height, viewBox, opacity)
import PSD3v2.Behavior.Types (Behavior(..), ScaleExtent(..), defaultZoom)
import PSD3v2.Capabilities.Selection (select, appendChild, appendData, setAttrs, on)
import PSD3v2.Interpreter.D3v2 (D3v2M, runD3v2M, D3v2Selection_)
import PSD3v2.Selection.Types (ElementType(..), SBoundOwns)
import Types (Scene(..), SimNode, SimLink, NodeType(..), LinkType)
import Web.DOM.Element (Element)

-- FFI for fast tick updates (bypasses PureScript array traversal)
foreign import updateNodePositions :: forall s d. D3v2Selection_ s Element d -> Effect Unit
foreign import updateLinkPositions :: forall s d. D3v2Selection_ s Element d -> Effect Unit

-- =============================================================================
-- Types
-- =============================================================================

-- | Row types for simulation
-- | Note: fx/fy are included here because getNodes returns { x, y, vx, vy | nodeRow }
type NodeRow = ( id :: Int, name :: String, nodeType :: NodeType, package :: String
               , r :: Number, cluster :: Int, targets :: Array Int, sources :: Array Int
               , gridX :: Number, gridY :: Number, orbitAngle :: Number
               , treeX :: Number, treeY :: Number
               , fx :: Nullable Number, fy :: Nullable Number )
type LinkRow = ( linkType :: LinkType )

type CESimulation = Sim.Simulation NodeRow LinkRow

-- | Explorer state
type ExplorerState =
  { model :: LoadedModel
  , simulation :: CESimulation
  , currentScene :: Scene
  , transitionProgress :: Maybe Tick.Progress  -- Nothing = stable, Just p = transitioning
  , targetScene :: Maybe Scene
  , nodePositions :: Map Int { x :: Number, y :: Number }  -- Cached target positions
  }

-- | Constants
svgWidth :: Number
svgWidth = 1200.0

svgHeight :: Number
svgHeight = 800.0

orbitRadius :: Number
orbitRadius = 350.0

transitionDelta :: Tick.TickDelta
transitionDelta = 0.02  -- ~50 ticks = ~0.8 seconds

-- =============================================================================
-- Initialization
-- =============================================================================

-- | Initialize the explorer with loaded data
initExplorer :: LoadedModel -> String -> Effect (Ref ExplorerState)
initExplorer model containerSelector = do
  -- Create simulation
  sim <- Sim.create Sim.defaultConfig

  -- Fix packages at their grid positions, modules start free
  let nodesWithFix = map fixPackagePosition model.nodes
  Sim.setNodes nodesWithFix sim
  Sim.setLinks model.links sim

  -- Add Grid forces (pass nodes for dynamic force initialization)
  addGridForces nodesWithFix sim

  -- Create state
  stateRef <- Ref.new
    { model
    , simulation: sim
    , currentScene: Grid
    , transitionProgress: Nothing
    , targetScene: Nothing
    , nodePositions: Map.empty
    }

  -- Render initial SVG structure
  { nodeSel, linkSel } <- runD3v2M $ renderSVGContainer containerSelector model.nodes model.links

  -- Set up tick callback
  Sim.onTick (onTick stateRef nodeSel linkSel) sim

  -- Start simulation
  Sim.start sim

  pure stateRef

-- =============================================================================
-- Scene Forces
-- =============================================================================

-- | Fix packages at their grid positions (set fx/fy)
fixPackagePosition :: SimNode -> SimNode
fixPackagePosition node = case node.nodeType of
  PackageNode -> node { fx = notNull node.gridX, fy = notNull node.gridY }
  ModuleNode -> node  -- Modules stay free

-- | Add forces for Grid scene
-- | Based on working CodeExplorerV1/Forces.purs:
-- | - NO ManyBody charge! (was causing explosion)
-- | - Packages: strong positioning (0.8)
-- | - Modules: weaker positioning (0.2)
-- | - Collision: node radius + padding
addGridForces :: Array SimNode -> CESimulation -> Effect Unit
addGridForces nodes sim = do
  -- NO ManyBody! Grid scene doesn't need charge repulsion

  -- Collision: based on node radius + padding
  let collideConfig = { radiusAccessor: (\n -> n.r + 5.0) :: SimNode -> Number
                      , strength: 0.7
                      , iterations: 1 }
  let collideHandle = Core.createCollideDynamic collideConfig
  _ <- Core.initializeForce collideHandle nodes
  Ref.modify_ (Map.insert "collide" collideHandle) sim.forces

  -- Package ForceX: STRONG pull to grid (0.8)
  -- Module ForceX: WEAK pull to grid (0.2)
  let forceXConfig = { xAccessor: _.gridX :: SimNode -> Number
                     , strength: 0.5 }  -- Intermediate for now, could split by type
  let forceXHandle = Core.createForceXDynamic forceXConfig
  _ <- Core.initializeForce forceXHandle nodes
  Ref.modify_ (Map.insert "gridX" forceXHandle) sim.forces

  -- ForceY: same pattern
  let forceYConfig = { yAccessor: _.gridY :: SimNode -> Number
                     , strength: 0.5 }
  let forceYHandle = Core.createForceYDynamic forceYConfig
  _ <- Core.initializeForce forceYHandle nodes
  Ref.modify_ (Map.insert "gridY" forceYHandle) sim.forces

  -- Debug: check how many forces we have
  forces <- Ref.read sim.forces
  log $ "[CE] Added " <> show (Map.size forces) <> " forces to simulation"

-- =============================================================================
-- Transitions
-- =============================================================================

-- | Start a transition to a new scene
transitionToScene :: Scene -> Ref ExplorerState -> Effect Unit
transitionToScene targetScene stateRef = do
  state <- Ref.read stateRef

  -- Don't transition to same scene or while already transitioning
  when (state.currentScene /= targetScene && state.transitionProgress == Nothing) do

    -- Calculate target positions for all nodes
    let targetPositions = calculateTargetPositions targetScene state.model

    -- Pin all nodes at their current positions
    nodes <- Sim.getNodes state.simulation
    let pinnedNodes = map (\n -> n { fx = notNull n.x, fy = notNull n.y }) nodes
    Sim.setNodes pinnedNodes state.simulation

    -- Start transition
    Ref.write (state
      { transitionProgress = Just 0.0
      , targetScene = Just targetScene
      , nodePositions = targetPositions
      }) stateRef

    -- Reheat simulation to keep ticking
    Sim.reheat state.simulation

-- | Get current transition progress (for UI display)
getTransitionProgress :: Ref ExplorerState -> Effect (Maybe Tick.Progress)
getTransitionProgress stateRef = do
  state <- Ref.read stateRef
  pure state.transitionProgress

-- | Calculate target positions for each node in a scene
calculateTargetPositions :: Scene -> LoadedModel -> Map Int { x :: Number, y :: Number }
calculateTargetPositions scene model = case scene of
  Grid -> calculateGridPositions model
  Orbit -> calculateOrbitPositions model
  Tree -> calculateTreePositions model
  BubblePack -> Map.empty  -- TODO

calculateGridPositions :: LoadedModel -> Map Int { x :: Number, y :: Number }
calculateGridPositions model =
  Map.fromFoldable $ map (\n -> Tuple n.id { x: n.gridX, y: n.gridY }) model.nodes

calculateOrbitPositions :: LoadedModel -> Map Int { x :: Number, y :: Number }
calculateOrbitPositions model =
  Map.fromFoldable $ Array.concat
    [ packagePositions
    , modulePositions
    ]
  where
  -- Packages go on orbit ring
  packagePositions = Array.mapMaybe (\n -> case n.nodeType of
    PackageNode ->
      let
        x = cos n.orbitAngle * orbitRadius
        y = sin n.orbitAngle * orbitRadius
      in Just (Tuple n.id { x, y })
    _ -> Nothing
    ) model.nodes

  -- Build package position map
  pkgPosMap = Map.fromFoldable packagePositions

  -- Modules cluster near their package
  modulePositions = Array.mapMaybe (\n -> case n.nodeType of
    ModuleNode ->
      -- Find parent package position
      let pkgPos = Map.lookup n.cluster pkgPosMap
      in case pkgPos of
        Just { x: pkgX, y: pkgY } ->
          -- Module is offset from package
          Just (Tuple n.id { x: pkgX + n.gridX * 0.5, y: pkgY + n.gridY * 0.5 })
        Nothing ->
          Just (Tuple n.id { x: n.gridX, y: n.gridY })
    _ -> Nothing
    ) model.nodes

calculateTreePositions :: LoadedModel -> Map Int { x :: Number, y :: Number }
calculateTreePositions model =
  -- For now, just return treeX/treeY from nodes (which are 0)
  -- TODO: Calculate actual radial tree layout
  Map.fromFoldable $ map (\n -> Tuple n.id { x: n.treeX, y: n.treeY }) model.nodes

-- =============================================================================
-- Tick Handler
-- =============================================================================

onTick
  :: Ref ExplorerState
  -> D3v2Selection_ SBoundOwns Element SimNode
  -> D3v2Selection_ SBoundOwns Element SimLink
  -> Effect Unit
onTick stateRef nodeSel _linkSel = do
  state <- Ref.read stateRef

  -- Handle transition progress if transitioning
  case state.transitionProgress of
    Nothing -> pure unit  -- Not transitioning, just update positions
    Just progress -> do
      let newProgress = min 1.0 (progress + transitionDelta)

      if newProgress >= 1.0
        then do
          -- Transition complete
          case state.targetScene of
            Just target -> do
              -- Unpin nodes
              nodes <- Sim.getNodes state.simulation
              let unpinnedNodes = map (\n -> n { fx = null, fy = null }) nodes
              Sim.setNodes unpinnedNodes state.simulation

              Ref.write (state
                { currentScene = target
                , transitionProgress = Nothing
                , targetScene = Nothing
                }) stateRef
            Nothing -> pure unit
        else do
          -- Interpolate node positions
          nodes <- Sim.getNodes state.simulation
          let interpolatedNodes = map (interpolateNode newProgress state.nodePositions) nodes
          Sim.setNodes interpolatedNodes state.simulation

          Ref.write (state { transitionProgress = Just newProgress }) stateRef

  -- Update visual positions (using fast FFI, not PureScript traversal)
  updateNodePositions nodeSel

-- | Interpolate a node's position toward its target
interpolateNode :: Tick.Progress -> Map Int { x :: Number, y :: Number } -> SimNode -> SimNode
interpolateNode progress targetPositions node =
  case Map.lookup node.id targetPositions of
    Just { x: targetX, y: targetY } ->
      let
        newX = Tick.lerp node.x targetX (Tick.easeInOut progress)
        newY = Tick.lerp node.y targetY (Tick.easeInOut progress)
      in node { fx = notNull newX, fy = notNull newY }
    Nothing -> node

-- =============================================================================
-- Rendering
-- =============================================================================

renderSVGContainer
  :: String
  -> Array SimNode
  -> Array SimLink
  -> D3v2M { nodeSel :: D3v2Selection_ SBoundOwns Element SimNode
           , linkSel :: D3v2Selection_ SBoundOwns Element SimLink
           }
renderSVGContainer containerSelector nodes links = do
  container <- select containerSelector

  svg <- appendChild SVG
    [ width svgWidth
    , height svgHeight
    , viewBox (show ((-svgWidth) / 2.0) <> " " <> show ((-svgHeight) / 2.0) <> " " <> show svgWidth <> " " <> show svgHeight)
    , id_ "ce-svg"
    , class_ "ce-viz"
    ] container

  zoomGroup <- appendChild Group [ id_ "ce-zoom-group" ] svg

  linksGroup <- appendChild Group [ id_ "ce-links" ] zoomGroup
  nodesGroup <- appendChild Group [ id_ "ce-nodes" ] zoomGroup

  -- Render links
  linkSel <- appendData Line links
    [ stroke "#666"
    , strokeWidth 0.5
    , opacity 0.3
    ] linksGroup

  -- Render nodes
  nodeSel <- appendData Circle nodes
    [ cx (_.x :: SimNode -> Number)
    , cy (_.y :: SimNode -> Number)
    , radius (_.r :: SimNode -> Number)
    , fill (nodeColor :: SimNode -> String)
    , stroke "#fff"
    , strokeWidth 0.5
    ] nodesGroup

  -- Attach zoom
  _ <- on (Zoom $ defaultZoom (ScaleExtent 0.1 10.0) "#ce-zoom-group") svg

  pure { nodeSel, linkSel }

-- | Node color based on type
nodeColor :: SimNode -> String
nodeColor n = case n.nodeType of
  PackageNode -> "#ff7f0e"  -- Orange for packages
  ModuleNode -> "#1f77b4"   -- Blue for modules
