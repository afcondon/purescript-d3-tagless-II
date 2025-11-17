module D3.Viz.LesMis.LesMisGUP where

-- | Fully declarative force-directed graph using genericUpdateSimulation
-- |
-- | This demonstrates the "impossible to mess up" API design:
-- | - No manual GUP steps
-- | - Automatic link filtering
-- | - Scene-based transitions
-- | - Zero boilerplate

import Prelude

import D3.Viz.LesMiserables.Model (LesMisNodeRow, LesMisRawModel, LesMisSimNode)
import D3.Viz.LesMis.LesMisRenderCallbacks (LesMisAttributes, lesMisRenderCallbacks)
import D3.Viz.LesMis.LesMisScenes (fullGraphScene, filteredGraphScene, gridScene, phylotaxisScene)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import PSD3.Data.Node (D3Link_Swizzled, D3Link_Unswizzled)
import PSD3.Internal.Attributes.Instances (Label)
import PSD3.Internal.FFI (keyIsID_)
import PSD3.Internal.Simulation.Types (Force)
import PSD3v2.Attribute.Types (class_, height, id_, viewBox, width)
import PSD3v2.Behavior.Types (Behavior(..), ScaleExtent(..), defaultDrag, defaultZoom)
import PSD3v2.Capabilities.Selection (on, renderTree, select)
import PSD3v2.Capabilities.Simulation (init, start, stop, reheat)
import PSD3v2.Interpreter.D3v2 (D3v2SimM, D3v2Selection_, reselectD3v2)
import PSD3v2.Selection.Types (ElementType(..), SBound, SEmpty)
import PSD3v2.Simulation.Scene (SceneConfig)
import PSD3v2.Simulation.Update (genericUpdateSimulation)
import PSD3v2.VizTree.Tree as T
import Unsafe.Coerce (unsafeCoerce)
import Utility (getWindowWidthHeight)
import Web.DOM.Element (Element)

-- | Update the LesMis simulation with a new scene configuration
-- |
-- | This is the visualization-specific wrapper around genericUpdateSimulation
updateLesMisSimulation :: forall row.
  { nodes :: D3v2Selection_ SEmpty Element LesMisSimNode
  , links :: D3v2Selection_ SEmpty Element D3Link_Swizzled
  } ->
  { allNodes :: Array LesMisSimNode
  , allLinks :: Array D3Link_Unswizzled
  , scene :: SceneConfig LesMisNodeRow LesMisAttributes
  } ->
  D3v2SimM row LesMisSimNode Unit
updateLesMisSimulation selections { allNodes, allLinks, scene } = do
  genericUpdateSimulation
    selections
    Circle  -- Node element (circles for each node)
    Line    -- Link element
    { allNodes: allNodes
    , allLinks: allLinks
    , nodeFilter: scene.chooseNodes
    , linkFilter: Just scene.linksShown
    , nodeInitializers: scene.nodeInitializerFunctions
    , activeForces: Just scene.activeForces
    , config: Nothing
    }
    (\n -> (unsafeCoerce n :: { id :: String }).id)  -- Node key function
    (\l -> (unsafeCoerce l :: { source :: { id :: String }, target :: { id :: String } }).source.id <> "-" <> (unsafeCoerce l :: { source :: { id :: String }, target :: { id :: String } }).target.id)  -- Link key function (combine source-target IDs)
    scene.attributes
    lesMisRenderCallbacks

-- | Initial setup of the force-directed graph using declarative pattern
-- |
-- | Returns group selections for future scene updates
drawLesMisGUP :: forall row.
  Array (Force LesMisSimNode) ->
  Set.Set Label ->
  LesMisRawModel ->
  String ->
  D3v2SimM row LesMisSimNode
    { nodesGroup :: D3v2Selection_ SEmpty Element LesMisSimNode
    , linksGroup :: D3v2Selection_ SEmpty Element D3Link_Swizzled
    , model :: LesMisRawModel
    }
drawLesMisGUP forcesArray activeForces model containerSelector = do
  -- Get window dimensions
  (Tuple w h) <- liftEffect getWindowWidthHeight

  -- Select container
  container <- select containerSelector

  -- Declarative tree structure for force graph
  let forceGraphTree :: T.Tree Unit
      forceGraphTree =
        T.named SVG "svg"
          [ width w
          , height h
          , viewBox (show ((-w) / 2.0) <> " " <> show ((-h) / 2.0) <> " " <> show w <> " " <> show h)
          , id_ "lesmis-gup-svg"
          , class_ "lesmis-gup"
          ]
          `T.withChild`
            (T.named Group "zoomGroup"
              [ id_ "zoom-group"
              , class_ "zoom-group"
              ]
              `T.withChildren`
                [ T.named Group "linksGroup"
                    [ id_ "links"
                    , class_ "links"
                    ]
                , T.named Group "nodesGroup"
                    [ id_ "nodes"
                    , class_ "nodes"
                    ]
                ])

  -- Render the structure tree
  selections <- renderTree container forceGraphTree

  -- Extract selections for behaviors and updates
  svg <- liftEffect $ reselectD3v2 "svg" selections
  zoomGroup <- liftEffect $ reselectD3v2 "zoomGroup" selections
  linksGroup <- liftEffect $ reselectD3v2 "linksGroup" selections
  nodesGroup <- liftEffect $ reselectD3v2 "nodesGroup" selections

  -- Attach drag behavior to zoom group (allows panning)
  _ <- on (Drag defaultDrag) zoomGroup

  -- Attach zoom behavior to SVG
  _ <- on (Zoom $ defaultZoom (ScaleExtent 0.5 4.0) "#zoom-group") svg

  -- Initialize simulation
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
    , keyFn: keyIsID_
    , ticks: Map.empty
    }

  -- Use declarative update to render initial scene
  let scene = fullGraphScene activeForces

  updateLesMisSimulation
    { nodes: nodesGroup, links: linksGroup }
    { allNodes: model.nodes, allLinks: model.links, scene }

  -- Start simulation
  start

  -- Return selections and model for future scene switches
  pure { nodesGroup, linksGroup, model }

-- | Switch to full graph scene
switchToFullGraph :: forall row.
  { nodes :: D3v2Selection_ SEmpty Element LesMisSimNode
  , links :: D3v2Selection_ SEmpty Element D3Link_Swizzled
  } ->
  LesMisRawModel ->
  Set.Set Label ->
  D3v2SimM row LesMisSimNode Unit
switchToFullGraph selections model activeForces = do
  stop
  let scene = fullGraphScene activeForces
  updateLesMisSimulation selections { allNodes: model.nodes, allLinks: model.links, scene }
  reheat 0.8
  start

-- | Switch to filtered graph scene (only nodes with group >= minGroup)
switchToFilteredGraph :: forall row.
  { nodes :: D3v2Selection_ SEmpty Element LesMisSimNode
  , links :: D3v2Selection_ SEmpty Element D3Link_Swizzled
  } ->
  LesMisRawModel ->
  Int ->
  Set.Set Label ->
  D3v2SimM row LesMisSimNode Unit
switchToFilteredGraph selections model minGroup activeForces = do
  stop
  let scene = filteredGraphScene minGroup activeForces
  updateLesMisSimulation selections { allNodes: model.nodes, allLinks: model.links, scene }
  reheat 0.8
  start

-- | Switch to grid layout scene
switchToGrid :: forall row.
  { nodes :: D3v2Selection_ SEmpty Element LesMisSimNode
  , links :: D3v2Selection_ SEmpty Element D3Link_Swizzled
  } ->
  LesMisRawModel ->
  Number ->
  D3v2SimM row LesMisSimNode Unit
switchToGrid selections model gridSpacing = do
  stop
  let scene = gridScene gridSpacing
  updateLesMisSimulation selections { allNodes: model.nodes, allLinks: model.links, scene }
  start  -- Start immediately (pinned layout)

-- | Switch to phylotaxis (sunflower spiral) layout scene
switchToPhylotaxis :: forall row.
  { nodes :: D3v2Selection_ SEmpty Element LesMisSimNode
  , links :: D3v2Selection_ SEmpty Element D3Link_Swizzled
  } ->
  LesMisRawModel ->
  D3v2SimM row LesMisSimNode Unit
switchToPhylotaxis selections model = do
  stop
  let scene = phylotaxisScene
  updateLesMisSimulation selections { allNodes: model.nodes, allLinks: model.links, scene }
  start  -- Start immediately (pinned layout)
