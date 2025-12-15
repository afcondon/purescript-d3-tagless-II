-- | Simple Force Layout for Network Datasets - STATELESS ARCHITECTURE
-- |
-- | This module provides stateless functions for force-directed graph visualization.
-- | All state lives in Halogen; this module only provides:
-- | - Simulation creation
-- | - Scene data construction
-- | - Rendering
-- | - Force configuration helpers
-- |
-- | The Halogen component owns:
-- | - The model (nodes/links data)
-- | - The simulation handle (opaque)
-- | - Enabled forces set
-- | - Link weight toggle
-- | - Visible node IDs
-- | - Enter/exit transition state
module D3.Viz.ForcePlayground.Simple
  ( -- Simulation creation
    createSimulation
  , createSimulationWithCallbacks
  , NetworkSimulation
  , NetworkNodeRow
  , NetworkLinkRow
  -- Scene data
  , SceneData
  , RenderNode
  , SwizzledLink
  , ExitingNode
  , buildSceneData
  -- Rendering
  , renderSVGContainer
  , renderScene
  -- Force configuration
  , ForceId(..)
  , forceIdName
  , forceIdSpec
  , allForces
  -- Visual helpers (exported for use in expressions if needed)
  , nodeRadius
  , categoryColor
  , linkTypeColor
  -- Transition timing
  , transitionDelta
  -- Re-export for Halogen component
  , scheduleAfterRender
  ) where

import Prelude

import Data.Array (filter, elem, index)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Effect (Effect)
import D3.Viz.ForcePlayground.Model (NetworkModel, NetworkNode)
import PSD3.ForceEngine.Simulation as Sim
import PSD3.ForceEngine.Types (ForceSpec(..), defaultManyBody, defaultCollide, defaultLink, ForceXConfig, ForceYConfig)
import PSD3.ForceEngine.Links (swizzleLinksByIndex, filterLinksToSubset)
import PSD3.ForceEngine.Events (SimulationCallbacks)
import PSD3.Transition.Tick as Tick
import PSD3v3.Integration (v3Attr, v3AttrStr)
import PSD3v3.Expr (lit, str)
import PSD3v2.Behavior.Types (Behavior(..), ScaleExtent(..), defaultZoom)
import PSD3v2.Capabilities.Selection (select, renderTree)
import PSD3v2.Interpreter.D3v2 (runD3v2M)
import PSD3v2.Selection.Types (ElementType(..)) as ET
import PSD3v2.VizTree.Tree as T

-- =============================================================================
-- Type Definitions
-- =============================================================================

-- | Row types for the simulation (must match extra fields in NetworkNode/NetworkLink)
type NetworkNodeRow = (name :: String, group :: Int, sizeClass :: Int, importance :: Number, subgraph :: Int)
type NetworkLinkRow = (weight :: Number, linkType :: Int, distance :: Number)

-- | Concrete simulation type
type NetworkSimulation = Sim.Simulation NetworkNodeRow NetworkLinkRow

-- | Exiting node with frozen position and transition progress
type ExitingNode = Tick.Transitioning NetworkNode

-- | Node ready for rendering with computed visual state
type RenderNode =
  { node :: NetworkNode
  , enterProgress :: Maybe Number  -- Just 0.0-1.0 if entering, Nothing if not
  , exitProgress :: Maybe Number   -- Just 0.0-1.0 if exiting, Nothing if not
  }

-- | Swizzled link for rendering (with full node refs and link attributes)
type SwizzledLink =
  { source :: NetworkNode
  , target :: NetworkNode
  , weight :: Number
  , linkType :: Int
  , index :: Int
  }

-- | Scene data for rendering - pure data structure
type SceneData =
  { nodes :: Array RenderNode
  , links :: Array SwizzledLink
  }

-- =============================================================================
-- Force Configuration
-- =============================================================================

-- | Identifiers for the forces we use
data ForceId
  = ForceCharge
  | ForceCollide
  | ForceLink
  | ForceX
  | ForceY

derive instance eqForceId :: Eq ForceId
derive instance ordForceId :: Ord ForceId

-- | Get the string name used in the simulation
forceIdName :: ForceId -> String
forceIdName = case _ of
  ForceCharge -> "charge"
  ForceCollide -> "collision"
  ForceLink -> "links"
  ForceX -> "forceX"
  ForceY -> "forceY"

-- | Default forceX config (weak pull toward center)
defaultForceX :: ForceXConfig
defaultForceX = { x: 0.0, strength: 0.1 }

-- | Default forceY config (weak pull toward center)
defaultForceY :: ForceYConfig
defaultForceY = { y: 0.0, strength: 0.1 }

-- | Get the ForceSpec for a force
forceIdSpec :: ForceId -> ForceSpec
forceIdSpec = case _ of
  ForceCharge -> ManyBody "charge" defaultManyBody { strength = -30.0, distanceMax = 200.0 }
  ForceCollide -> Collide "collision" defaultCollide { radius = 10.0, strength = 0.7, iterations = 1 }
  ForceLink -> Link "links" defaultLink { distance = 40.0, strength = 0.4, iterations = 1 }
  ForceX -> PositionX "forceX" defaultForceX
  ForceY -> PositionY "forceY" defaultForceY

-- | All forces enabled by default
allForces :: Set ForceId
allForces = Set.fromFoldable [ForceX, ForceY, ForceCharge, ForceCollide, ForceLink]

-- =============================================================================
-- Constants
-- =============================================================================

-- | SVG dimensions
svgWidth :: Number
svgWidth = 800.0

svgHeight :: Number
svgHeight = 600.0

-- | Transition speed: progress increment per tick
-- | At 60fps, 0.03 ≈ 33 ticks ≈ 0.55 seconds (snappy)
transitionDelta :: Tick.TickDelta
transitionDelta = 0.03

-- =============================================================================
-- Simulation Creation
-- =============================================================================

-- | Create a simulation without callbacks (for simple usage)
createSimulation :: NetworkModel -> Effect NetworkSimulation
createSimulation model = do
  sim <- Sim.create Sim.defaultConfig
  Sim.setNodes model.nodes sim
  Sim.setLinks model.links sim

  -- Add all forces initially
  Sim.addForce (forceIdSpec ForceX) sim
  Sim.addForce (forceIdSpec ForceY) sim
  Sim.addForce (forceIdSpec ForceCharge) sim
  Sim.addForce (forceIdSpec ForceCollide) sim
  Sim.addForce (forceIdSpec ForceLink) sim

  Sim.start sim
  pure sim

-- | Create a simulation with callbacks (for Halogen subscription pattern)
createSimulationWithCallbacks :: SimulationCallbacks -> NetworkModel -> Effect NetworkSimulation
createSimulationWithCallbacks callbacks model = do
  sim <- Sim.createWithCallbacks Sim.defaultConfig callbacks
  Sim.setNodes model.nodes sim
  Sim.setLinks model.links sim

  -- Add all forces initially
  Sim.addForce (forceIdSpec ForceX) sim
  Sim.addForce (forceIdSpec ForceY) sim
  Sim.addForce (forceIdSpec ForceCharge) sim
  Sim.addForce (forceIdSpec ForceCollide) sim
  Sim.addForce (forceIdSpec ForceLink) sim

  Sim.start sim
  pure sim

-- =============================================================================
-- Scene Data Construction (Pure)
-- =============================================================================

-- | Build scene data from current simulation state and transition maps
-- | This is a PURE function - all state comes from parameters
buildSceneData
  :: Array NetworkNode           -- Current nodes from simulation (with positions)
  -> Array Int                   -- Visible node IDs
  -> Map Int Tick.Progress       -- Entering progress by node ID
  -> Array ExitingNode           -- Exiting nodes with frozen positions
  -> Array { source :: Int, target :: Int, weight :: Number, linkType :: Int, distance :: Number }  -- Links from model
  -> SceneData
buildSceneData currentNodes visibleIds enteringProgress exitingNodes modelLinks =
  let
    -- Filter to visible nodes only
    visibleNodes = filter (\n -> elem n.id visibleIds) currentNodes

    -- Build render nodes with transition state
    renderNodes = map
      (\n ->
        { node: n
        , enterProgress: Map.lookup n.id enteringProgress
        , exitProgress: Nothing
        }
      )
      visibleNodes

    -- Add exiting nodes (with frozen positions and exit progress)
    exitingRenderNodes = map
      (\e ->
        { node: e.item
        , enterProgress: Nothing
        , exitProgress: Just e.progress
        }
      )
      exitingNodes

    allRenderNodes = renderNodes <> exitingRenderNodes

    -- Create links only between visible nodes (not exiting)
    visibleLinks = filterLinksToSubset _.id visibleNodes modelLinks
    swizzledLinks = swizzleLinksByIndex _.id visibleNodes visibleLinks \src tgt i link ->
      { source: src, target: tgt, weight: link.weight, linkType: link.linkType, index: i }
  in
    { nodes: allRenderNodes, links: swizzledLinks }

-- =============================================================================
-- Rendering (Side-effecting but stateless)
-- =============================================================================

-- | Render the SVG container structure
renderSVGContainer :: String -> Effect Unit
renderSVGContainer containerSelector = runD3v2M do
  container <- select containerSelector

  let containerTree :: T.Tree Unit
      containerTree =
        T.named ET.SVG "svg"
          [ v3Attr "width" (lit svgWidth)
          , v3Attr "height" (lit svgHeight)
          , v3AttrStr "viewBox" (str (show ((-svgWidth) / 2.0) <> " " <> show ((-svgHeight) / 2.0) <> " " <> show svgWidth <> " " <> show svgHeight))
          , v3AttrStr "id" (str "network-force-svg")
          , v3AttrStr "class" (str "network-force")
          ]
          `T.withBehaviors` [ Zoom $ defaultZoom (ScaleExtent 0.1 10.0) "#network-zoom-group" ]
          `T.withChild`
            T.named ET.Group "zoom-group" [ v3AttrStr "id" (str "network-zoom-group"), v3AttrStr "class" (str "zoom-group") ]
              `T.withChildren`
                [ T.named ET.Group "links" [ v3AttrStr "id" (str "network-links"), v3AttrStr "class" (str "links") ]
                , T.named ET.Group "nodes" [ v3AttrStr "id" (str "network-nodes"), v3AttrStr "class" (str "nodes") ]
                ]

  _ <- renderTree container containerTree
  pure unit

-- | Render the scene (nodes and links)
renderScene :: SceneData -> Effect Unit
renderScene scene = runD3v2M do
  -- Render links
  linksGroup <- select "#network-links"
  let linksTree = createLinksTree scene
  _ <- renderTree linksGroup linksTree

  -- Render nodes
  nodesGroup <- select "#network-nodes"
  let nodesTree = createNodesTree scene
  _ <- renderTree nodesGroup nodesTree

  pure unit

-- =============================================================================
-- Visual Property Helpers
-- =============================================================================

-- | Color palette for node categories (D3 category10)
categoryColors :: Array String
categoryColors =
  [ "#1f77b4"  -- blue (Research)
  , "#ff7f0e"  -- orange (Industry)
  , "#2ca02c"  -- green (Government)
  , "#d62728"  -- red (Community)
  , "#9467bd"  -- purple
  , "#8c564b"  -- brown
  , "#e377c2"  -- pink
  , "#7f7f7f"  -- gray
  , "#bcbd22"  -- olive
  , "#17becf"  -- cyan
  ]

-- | Get color for a category/group
categoryColor :: Int -> String
categoryColor g = fromMaybe "#69b3a2" (index categoryColors (g `mod` 10))

-- | Color palette for link types (muted)
linkTypeColors :: Array String
linkTypeColors =
  [ "#6baed6"  -- light blue (Collaboration)
  , "#fd8d3c"  -- light orange (Citation)
  , "#74c476"  -- light green (Funding)
  , "#e377c2"  -- pink (Communication)
  ]

-- | Get color for a link type
linkTypeColor :: Int -> String
linkTypeColor t = fromMaybe "#999" (index linkTypeColors (t `mod` 4))

-- | Calculate node radius based on sizeClass and importance
nodeRadius :: NetworkNode -> Number
nodeRadius node =
  let
    baseSize = case node.sizeClass of
      0 -> 3.0   -- small
      1 -> 5.0   -- medium
      _ -> 7.0   -- large
    -- Importance adds 0-3 extra pixels
    importanceBonus = node.importance * 3.0
  in
    baseSize + importanceBonus

-- =============================================================================
-- Tree Construction for Rendering
-- =============================================================================

-- | Create nodes tree with transition-aware visual properties
createNodesTree :: SceneData -> T.Tree SceneData
createNodesTree scene =
  T.sceneNestedJoin "nodes" "circle"
    [ scene ]
    (_.nodes)
    ( \rn -> T.elem ET.Circle
        [ v3Attr "cx" (lit rn.node.x)
        , v3Attr "cy" (lit rn.node.y)
        , v3Attr "r" (lit (radiusForRenderNode rn))
        , v3AttrStr "fill" (str (fillForRenderNode rn))
        , v3AttrStr "stroke" (str (strokeForRenderNode rn))
        , v3Attr "stroke-width" (lit (strokeWidthForRenderNode rn))
        , v3Attr "opacity" (lit (opacityForRenderNode rn))
        ]
    )
    { enterBehavior: Nothing
    , updateBehavior: Nothing
    , exitBehavior: Nothing
    }

-- | Create links tree with rich visual encoding
createLinksTree :: SceneData -> T.Tree SceneData
createLinksTree scene =
  T.sceneNestedJoin "links" "line"
    [ scene ]
    (_.links)
    ( \link -> T.elem ET.Line
        [ v3Attr "x1" (lit link.source.x)
        , v3Attr "y1" (lit link.source.y)
        , v3Attr "x2" (lit link.target.x)
        , v3Attr "y2" (lit link.target.y)
        , v3Attr "stroke-width" (lit (0.5 + link.weight * 2.0))  -- 0.5-2.5 based on weight
        , v3AttrStr "stroke" (str (linkTypeColor link.linkType))
        , v3Attr "opacity" (lit (0.3 + link.weight * 0.4))      -- 0.3-0.7 based on weight
        ]
    )
    { enterBehavior: Nothing
    , updateBehavior: Nothing
    , exitBehavior: Nothing
    }

-- =============================================================================
-- Visual Property Interpolation (Tick-Driven)
-- =============================================================================

-- | Get radius for a node based on transition state
radiusForRenderNode :: RenderNode -> Number
radiusForRenderNode rn =
  let baseRadius = nodeRadius rn.node
  in case rn.enterProgress, rn.exitProgress of
    Just p, _ -> Tick.lerp (baseRadius * 3.0) baseRadius p  -- Entering: large → normal
    _, Just p -> Tick.lerp baseRadius (baseRadius * 3.0) p  -- Exiting: normal → large (pop)
    _, _ -> baseRadius

-- | Get fill color for a node based on transition state
fillForRenderNode :: RenderNode -> String
fillForRenderNode rn =
  case rn.enterProgress, rn.exitProgress of
    Just _, _ -> "#2ca02c"  -- Green flash for entering
    _, Just _ -> "#d62728"  -- Red flash for exiting
    _, _ -> categoryColor rn.node.group  -- Normal category color

-- | Get stroke color for a node based on transition state
strokeForRenderNode :: RenderNode -> String
strokeForRenderNode rn =
  case rn.enterProgress, rn.exitProgress of
    Just _, _ -> "#98df8a"  -- Light green for entering
    _, Just _ -> "#ff9896"  -- Light red for exiting
    _, _ -> "#fff"

-- | Get stroke width for a node based on transition state
strokeWidthForRenderNode :: RenderNode -> Number
strokeWidthForRenderNode rn =
  case rn.enterProgress, rn.exitProgress of
    Just p, _ -> Tick.lerp 4.0 1.5 p  -- Thick → normal
    _, Just p -> Tick.lerp 1.5 4.0 p  -- Normal → thick
    _, _ -> 1.5

-- | Get opacity for a node based on transition state
opacityForRenderNode :: RenderNode -> Number
opacityForRenderNode rn =
  let baseOpacity = 0.7 + rn.node.importance * 0.3
  in case rn.exitProgress of
    Just p -> Tick.lerp baseOpacity 0.0 p  -- Fade out
    _ -> baseOpacity

-- =============================================================================
-- FFI for scheduling
-- =============================================================================

-- | Schedule a callback after render (used for drag attachment)
foreign import scheduleAfterRender :: Effect Unit -> Effect Unit
