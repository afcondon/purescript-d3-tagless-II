-- | Draw module for CodeExplorerV2
-- |
-- | Handles D3 visualization using genericUpdateSimulation following LesMisGUP patterns.
-- | Uses consistent Int IDs for nodes and links.
module Component.CodeExplorerV2.Draw where

import Prelude

import Component.CodeExplorerV2.Types (Scene(..))
import D3.Viz.Spago.Files (LinkType(..), SpagoLinkData, SpagoNodeRow, SpagoLink, D3_Radius)
import D3.Viz.Spago.Model (SpagoModel, SpagoSimNode, isPackage, isUsedModule, packagesNodesToPhyllotaxis)
import Data.Array (filter, take, length) as Array
import D3.Viz.Spago.Render (spagoRenderCallbacks)
import D3.Viz.Spago.Draw.Attributes (SpagoSceneAttributes, graphSceneAttributes, treeSceneAttributes, svgAttrs)
import Data.Maybe (Maybe(..))
import Data.Nullable as Nullable
import Data.Set (Set)
import Data.Set as Set
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import PSD3.Data.Node (D3_FocusXY, SwizzledLink)
import PSD3.Internal.Attributes.Instances (Label)
import PSD3.Internal.FFI (keyIsID_)
import PSD3.Internal.Simulation.Types (Force)
import PSD3.Internal.Types (PointXY)
import PSD3v2.Attribute.Types (class_)
import PSD3v2.Behavior.Types (Behavior(..), defaultDrag, defaultZoom, ScaleExtent(..))
import PSD3v2.Capabilities.Selection (class SelectionM, appendChild, select, on)
import PSD3v2.Capabilities.Simulation (class SimulationM2, init, start)
import PSD3v2.Interpreter.D3v2 (D3v2Selection_)
import Data.Map as Map
import PSD3v2.Selection.Types (ElementType(..), SBoundOwns, SEmpty)
import PSD3v2.Simulation.Update (DeclarativeUpdateConfig, genericUpdateSimulation)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (Element)
import Effect.Class (liftEffect)
import Utility (getWindowWidthHeight)
import Data.Tuple (Tuple(..))

-- Type alias uses D3_Radius from D3.Viz.Spago.Files

-- | Type alias for node row (matching Spago)
type NodeRow = SpagoNodeRow (D3_FocusXY (D3_Radius ()))

-- | Initialize the visualization
initialize :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3v2Selection_ m =>
  SimulationM2 (D3v2Selection_ SBoundOwns Element) m =>
  Array (Force SpagoSimNode) ->
  Set Label ->
  SpagoModel ->
  Array SpagoLink ->  -- Unswizzled links (deep copy before D3 mutates them)
  String ->
  Scene ->
  m Unit
initialize forcesArray activeForces model unswizzledLinks selector initialScene = do
  log "CodeExplorerV2.Draw: Initializing"

  -- Debug: Show link counts by type
  let allP2PLinks = Array.filter (\l -> l.linktype == P2P) unswizzledLinks
      packageNodes = Array.filter isPackage model.nodes
      packageIDs = map _.id packageNodes

  log $ "DEBUG: Total links: " <> show (Array.length unswizzledLinks)
  log $ "DEBUG: P2P links: " <> show (Array.length allP2PLinks)
  log $ "DEBUG: Package nodes: " <> show (Array.length packageNodes)
  log $ "DEBUG: First 5 package IDs: " <> show (Array.take 5 packageIDs)
  log $ "DEBUG: First 5 P2P links (source->target): " <> show (map (\l -> { s: l.source, t: l.target }) (Array.take 5 allP2PLinks))

  -- Get viewport dimensions
  (Tuple w h) <- liftEffect getWindowWidthHeight

  -- Create SVG structure
  root <- select selector
  svg <- appendChild SVG (svgAttrs w h) root
  inner <- appendChild Group [] svg
  _ <- on (Drag defaultDrag) inner
  _ <- on (Zoom (defaultZoom (ScaleExtent 0.1 4.0) "g")) svg

  -- Create groups for links (under) and nodes (over)
  linksGroup <- appendChild Group [ class_ "links" ] inner
  nodesGroup <- appendChild Group [ class_ "nodes" ] inner

  -- Initialize simulation with forces
  _ <- init
    { nodes: model.nodes
    , links: model.links
    , forces: forcesArray
    , activeForces: activeForces
    , config:
        { alpha: 1.0
        , alphaTarget: 0.0
        , alphaMin: 0.001
        , alphaDecay: 0.0228
        , velocityDecay: 0.4
        }
    , keyFn: (\n -> n.id)
    , ticks: Map.empty
    }

  -- Get scene configuration (pass unswizzled links explicitly)
  let sceneConfig = getSceneConfig model unswizzledLinks initialScene

  -- Call genericUpdateSimulation with render callbacks
  genericUpdateSimulation
    { nodes: nodesGroup
    , links: unsafeCoerce linksGroup
    }
    Group   -- Node element type (Spago uses groups for circle+text)
    Path    -- Link element type (paths for bezier/diagonal)
    sceneConfig
    (\n -> n.id)  -- Node key function (Int)
    keyIsID_      -- Link key function (unused - library uses swizzledLinkKey_)
    graphSceneAttributes  -- Use graph styling for initial scene
    spagoRenderCallbacks

  -- Start simulation
  start

  log "CodeExplorerV2.Draw: Initialization complete"

-- | Update scene (switch between PackageGraph and HorizontalTree)
updateScene :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3v2Selection_ m =>
  SimulationM2 (D3v2Selection_ SBoundOwns Element) m =>
  SpagoModel ->
  Array SpagoLink ->  -- Unswizzled links
  Scene ->
  Set Label ->
  m Unit
updateScene model links targetScene _activeForces = do
  log $ "CodeExplorerV2.Draw: Updating to scene"

  -- Get the DOM selections (they persist between updates)
  nodesGroup <- select "g.nodes"
  linksGroup <- select "g.links"

  -- Get scene configuration (library handles link copying internally)
  let sceneConfig = getSceneConfig model links targetScene

  -- Get appropriate attributes for scene
  let attrs = case targetScene of
        PackageGraph -> graphSceneAttributes
        HorizontalTree -> treeSceneAttributes

  -- Call genericUpdateSimulation
  genericUpdateSimulation
    { nodes: unsafeCoerce nodesGroup
    , links: unsafeCoerce linksGroup
    }
    Group
    Path
    sceneConfig
    (\n -> n.id)
    keyIsID_
    attrs
    spagoRenderCallbacks

  -- Restart simulation
  start

-- | Get scene configuration for a given scene
-- | Takes unswizzled links explicitly since D3 mutates links in place after init
getSceneConfig :: SpagoModel -> Array SpagoLink -> Scene -> DeclarativeUpdateConfig NodeRow Int SpagoLinkData
getSceneConfig model unswizzledLinks scene = case scene of
  PackageGraph ->
    { allNodes: model.nodes
    , allLinks: unswizzledLinks
    , nodeFilter: isPackage  -- Only packages
    , linkFilter: Just (\l -> l.linktype == P2P)  -- Only package-to-package links
    , nodeInitializers: [ packagesNodesToPhyllotaxis ]  -- Arrange in sunflower spiral
    , activeForces: Just $ Set.fromFoldable ["charge", "collision", "center", "links"]
    , config: Nothing
    }

  HorizontalTree ->
    { allNodes: model.nodes
    , allLinks: unswizzledLinks
    , nodeFilter: isUsedModule  -- Only modules in the tree
    , linkFilter: Just (\l -> l.linktype == M2M_Tree)  -- Only tree links
    , nodeInitializers: [ treePositionInitializer ]  -- Position nodes by tree coordinates
    , activeForces: Just $ Set.fromFoldable ["collision"]
    , config: Nothing
    }

-- | Node initializer that positions nodes by their tree coordinates
treePositionInitializer :: Array SpagoSimNode -> Array SpagoSimNode
treePositionInitializer nodes = map positionByTree nodes
  where
    positionByTree n = case Nullable.toMaybe n.treeXY of
      Nothing -> n
      Just pos -> n { x = pos.x, y = pos.y, fx = Nullable.notNull pos.x, fy = Nullable.notNull pos.y }
