module D3.Viz.Spago.DrawV2 where

import Prelude

import D3.Viz.Spago.Model (SpagoSimNode)
import D3.Viz.Spago.Draw.Attributes (SpagoSceneAttributes)
import PSD3.Internal.Simulation.Types (Force)
import PSD3.Internal.FFI (keyIsID_, SimulationVariables)
import PSD3.Data.Node (D3Link_Unswizzled)
import PSD3v2.Attribute.Types (width, height, viewBox, id_, class_)
import PSD3v2.Behavior.Types (Behavior(..), defaultDrag, defaultZoom, ScaleExtent(..))
import PSD3v2.Capabilities.Selection (select, appendChild, on)
import PSD3v2.Capabilities.Simulation (init, addTickFunction, start, Step(..))
import PSD3v2.Interpreter.D3v2 (D3v2SimM, D3v2Selection_)
import PSD3v2.Selection.Types (ElementType(..), SBound, SEmpty)
import PSD3v2.Attribute.Types (transform, x1, y1, x2, y2)
import D3.Viz.Spago.RenderV2 as RenderV2
import D3.Viz.Spago.RenderV2 (IndexedLink)
import D3.Viz.Spago.Draw.Attributes (translateNode)
import Data.Map as Map
import Data.Set (Set)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Utility (getWindowWidthHeight)
import Unsafe.Coerce (unsafeCoerce)

-- | Initialize the SVG structure for Spago visualization using PSD3v2
-- | Creates the zoom/pan container and group elements for nodes and links
type SpagoSelections =
  { nodesGroup :: D3v2Selection_ SEmpty ElementType SpagoSimNode
  , linksGroup :: D3v2Selection_ SEmpty ElementType IndexedLink
  }

initialize :: forall row.
  String ->
  D3v2SimM row SpagoSimNode SpagoSelections
initialize containerSelector = do
  -- Get window dimensions
  (Tuple w h) <- liftEffect getWindowWidthHeight

  -- Create SVG container
  container <- select containerSelector
  svg <- appendChild SVG
    [ width w
    , height h
    , viewBox (show ((-w) / 2.1) <> " " <> show ((-h) / 2.05) <> " " <> show w <> " " <> show h)
    , id_ "spago-v2-svg"
    , class_ "spago-v2 overlay"
    ]
    container

  -- Create inner zoom group
  zoomGroup <- appendChild Group
    [ id_ "zoom-group"
    , class_ "zoom-group"
    ]
    svg

  -- Attach drag behavior to zoom group (allows panning)
  _ <- on (Drag defaultDrag) zoomGroup

  -- Attach zoom behavior to SVG
  _ <- on (Zoom $ defaultZoom (ScaleExtent 0.1 4.0) "#zoom-group") svg

  -- Create groups for links and nodes (links first so they render under nodes)
  linksGroup <- appendChild Group
    [ id_ "links"
    , class_ "links"
    ]
    zoomGroup

  nodesGroup <- appendChild Group
    [ id_ "nodes"
    , class_ "nodes"
    ]
    zoomGroup

  -- Return the actual selections for later use
  pure { nodesGroup, linksGroup }

-- | Initialize simulation with nodes and links
-- | This is the core initialization that sets up the force simulation
initializeSimulation :: forall row.
  { nodes :: Array SpagoSimNode
  , links :: Array D3Link_Unswizzled
  , forces :: Array (Force SpagoSimNode)
  , activeForces :: Set String
  , config :: SimulationVariables
  } ->
  D3v2SimM row SpagoSimNode
    { nodes :: Array SpagoSimNode
    , links :: Array (D3Link_Unswizzled)
    }
initializeSimulation { nodes, links, forces, activeForces, config } = do
  -- Initialize simulation using PSD3v2 SimulationM
  { nodes: nodesInSim, links: linksInSim } <- init
    { nodes: nodes
    , links: links
    , forces: forces
    , activeForces: activeForces
    , config: config
    , keyFn: keyIsID_
    , ticks: Map.empty
    }

  pure { nodes: nodesInSim, links: linksInSim }

-- | Update simulation with new scene configuration
-- | This renders nodes and links, then registers tick functions and starts the simulation
updateSimulation :: forall row.
  SpagoSelections ->
  { nodes :: Array SpagoSimNode
  , links :: Array D3Link_Unswizzled
  , forces :: Array (Force SpagoSimNode)
  , activeForces :: Set String
  , config :: SimulationVariables
  , attrs :: SpagoSceneAttributes
  } ->
  D3v2SimM row SpagoSimNode Unit
updateSimulation selections { nodes, links, forces, activeForces, config, attrs } = do
  -- Initialize simulation
  { nodes: nodesInSim, links: linksInSim } <- initializeSimulation
    { nodes, links, forces, activeForces, config }

  -- Render nodes (creates Group → Circle + Text structure)
  nodeGroups <- RenderV2.renderNodes nodesInSim selections.nodesGroup attrs

  -- Render links
  linkLines <- RenderV2.renderLinks linksInSim selections.linksGroup

  -- Register tick function for nodes (updates transform on parent group)
  addTickFunction "nodes" $ Step nodeGroups
    [ transform (\(d :: SpagoSimNode) -> translateNode d)
    ]

  -- Register tick function for links (updates x1, y1, x2, y2)
  addTickFunction "links" $ Step linkLines
    [ x1 (\(RenderV2.IndexedLink il) -> (unsafeCoerce il.link).source.x :: Number)
    , y1 (\(RenderV2.IndexedLink il) -> (unsafeCoerce il.link).source.y :: Number)
    , x2 (\(RenderV2.IndexedLink il) -> (unsafeCoerce il.link).target.x :: Number)
    , y2 (\(RenderV2.IndexedLink il) -> (unsafeCoerce il.link).target.y :: Number)
    ]

  -- Start simulation
  start

  pure unit
