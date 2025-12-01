-- | Spago Data with Grid Forces Test
-- |
-- | Loads actual spago data (1003 nodes) and uses grid forces.
-- | This isolates whether the spago data itself or Halogen causes slowdown.
module Viz.SpagoGridTest
  ( initSpagoGridTest
  ) where

import Prelude

import Data.Nullable as Data.Nullable
import Data.Tuple as Data.Tuple
import Data.Array as Array
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Random (random)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Data.Loader (loadModel, LoadedModel)
import PSD3.ForceEngine.Simulation as Sim
import PSD3.ForceEngine.Core as Core
import PSD3.ForceEngine.Render (GroupId(..), updateCirclePositions)
import PSD3v2.Attribute.Types (cx, cy, fill, stroke, strokeWidth, radius, id_, class_, width, height, viewBox, opacity, x1, y1, x2, y2)
import PSD3v2.Behavior.Types (Behavior(..), ScaleExtent(..), defaultZoom)
import PSD3v2.Capabilities.Selection (select, appendChild, appendData, on)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2M, D3v2Selection_)
import PSD3v2.Selection.Types (ElementType(..), SBoundOwns)
import Types (SimNode, SimLink, NodeType(..), LinkType)
import Web.DOM.Element (Element)

-- =============================================================================
-- Types
-- =============================================================================

type NodeRow = ( id :: Int, name :: String, nodeType :: NodeType, package :: String
               , r :: Number, cluster :: Int, targets :: Array Int, sources :: Array Int
               , gridX :: Number, gridY :: Number, orbitAngle :: Number
               , treeX :: Number, treeY :: Number
               , fx :: Data.Nullable.Nullable Number, fy :: Data.Nullable.Nullable Number )
type LinkRow = ( linkType :: LinkType )

type SpagoSimulation = Sim.Simulation NodeRow LinkRow

type SpagoTestState =
  { simulation :: SpagoSimulation
  }

-- =============================================================================
-- Constants
-- =============================================================================

svgWidth :: Number
svgWidth = 1200.0

svgHeight :: Number
svgHeight = 800.0

nodesGroupId :: GroupId
nodesGroupId = GroupId "#spago-test-nodes"

-- =============================================================================
-- Initialization
-- =============================================================================

-- | Initialize the spago grid test
-- | Loads actual spago data and uses grid forces (same as Code Explorer)
initSpagoGridTest :: String -> Effect Unit
initSpagoGridTest containerSelector = launchAff_ do
  log "[SpagoGridTest] Loading spago data..."
  result <- loadModel
  case result of
    Left err -> liftEffect $ log $ "[SpagoGridTest] Error: " <> err
    Right model -> do
      liftEffect $ log $ "[SpagoGridTest] Loaded: " <> show model.moduleCount <> " modules, " <> show model.packageCount <> " packages"
      liftEffect $ initWithModel model containerSelector

initWithModel :: LoadedModel -> String -> Effect Unit
initWithModel model containerSelector = do
  log "[SpagoGridTest] BUILD: 2024-12-01 18:50 - NO links (Grid scene baseline)"
  log "[SpagoGridTest] Initializing with grid forces..."

  -- Randomize module positions (same as Code Explorer)
  randomizedNodes <- randomizeModulePositions model.nodes
  let nodesWithFix = map fixPackagePosition randomizedNodes

  log $ "[SpagoGridTest] Nodes prepared: " <> show (Array.length nodesWithFix)

  -- Create simulation
  sim <- Sim.create Sim.defaultConfig
  Sim.setNodes nodesWithFix sim
  -- NO LINKS in simulation - just like we tested with grid test

  -- Add Grid forces
  let collideHandle = Core.createCollideGrid 5.0 0.7 1
  _ <- Core.initializeForce collideHandle nodesWithFix
  Ref.modify_ (Map.insert "collide" collideHandle) sim.forces

  let forceXHandle = Core.createForceXGrid 0.5
  _ <- Core.initializeForce forceXHandle nodesWithFix
  Ref.modify_ (Map.insert "gridX" forceXHandle) sim.forces

  let forceYHandle = Core.createForceYGrid 0.5
  _ <- Core.initializeForce forceYHandle nodesWithFix
  Ref.modify_ (Map.insert "gridY" forceYHandle) sim.forces

  -- Get nodes from simulation
  simNodes <- Sim.getNodes sim
  log $ "[SpagoGridTest] Sim nodes: " <> show (Array.length simNodes)

  -- Render SVG with links (limit to 3000 to test if count is the issue)
  let limitedLinks = Array.take 3000 model.links
  log $ "[SpagoGridTest] Links: " <> show (Array.length limitedLinks) <> " (limited from " <> show (Array.length model.links) <> ")"
  _ <- runD3v2M $ renderSVG containerSelector simNodes limitedLinks

  -- Create state
  stateRef <- Ref.new { simulation: sim }

  -- Set up tick callback - MINIMAL, just like grid test
  Sim.onTick (onTick stateRef) sim

  -- Start simulation
  Sim.start sim
  log "[SpagoGridTest] Simulation started"

-- =============================================================================
-- Helpers (copied from Explorer.purs)
-- =============================================================================

fixPackagePosition :: SimNode -> SimNode
fixPackagePosition node = case node.nodeType of
  PackageNode -> node { fx = Data.Nullable.notNull node.gridX, fy = Data.Nullable.notNull node.gridY }
  ModuleNode -> node

randomizeModulePositions :: Array SimNode -> Effect (Array SimNode)
randomizeModulePositions nodes = traverse randomizeIfModule nodes
  where
  packagePositions = Map.fromFoldable $ Array.mapMaybe getPackagePos nodes

  getPackagePos n = case n.nodeType of
    PackageNode -> Just (Data.Tuple.Tuple n.id { x: n.gridX, y: n.gridY })
    ModuleNode -> Nothing

  randomizeIfModule n = case n.nodeType of
    PackageNode -> pure n
    ModuleNode -> do
      rx <- random
      ry <- random
      let { x: pkgX, y: pkgY } = case Map.lookup n.cluster packagePositions of
            Just pos -> pos
            Nothing -> { x: 0.0, y: 0.0 }
      let jitterX = (rx - 0.5) * 100.0
      let jitterY = (ry - 0.5) * 100.0
      pure $ n { x = pkgX + jitterX, y = pkgY + jitterY }

-- =============================================================================
-- Tick Handler
-- =============================================================================

onTick :: Ref SpagoTestState -> Effect Unit
onTick stateRef = do
  -- Test: Add Ref.read like Explorer.purs does
  _state <- Ref.read stateRef

  -- MINIMAL: Just update positions via FFI
  updateCirclePositions nodesGroupId

-- =============================================================================
-- Rendering
-- =============================================================================

renderSVG
  :: String
  -> Array SimNode
  -> Array SimLink
  -> D3v2M { nodeSel :: D3v2Selection_ SBoundOwns Element SimNode }
renderSVG containerSelector nodes links = do
  container <- select containerSelector

  svg <- appendChild SVG
    [ width svgWidth
    , height svgHeight
    , viewBox (show ((-svgWidth) / 2.0) <> " " <> show ((-svgHeight) / 2.0) <> " " <> show svgWidth <> " " <> show svgHeight)
    , id_ "spago-test-svg"
    , class_ "spago-test"
    ] container

  zoomGroup <- appendChild Group [ id_ "spago-test-zoom-group" ] svg
  linksGroup <- appendChild Group [ id_ "spago-test-links" ] zoomGroup
  nodesGroup <- appendChild Group [ id_ "spago-test-nodes" ] zoomGroup

  -- NO links in Grid scene - they'll be added when transitioning to Tree/Orbit
  pure unit

  -- Bind node data
  nodeSel <- appendData Circle nodes
    [ cx (_.x :: SimNode -> Number)
    , cy (_.y :: SimNode -> Number)
    , radius (_.r :: SimNode -> Number)
    , fill (nodeColor :: SimNode -> String)
    , stroke "#fff"
    , strokeWidth 0.5
    ] nodesGroup

  -- Attach zoom
  _ <- on (Zoom $ defaultZoom (ScaleExtent 0.1 10.0) "#spago-test-zoom-group") svg

  pure { nodeSel }

nodeColor :: SimNode -> String
nodeColor n = case n.nodeType of
  PackageNode -> "#ff7f0e"
  ModuleNode -> "#1f77b4"
