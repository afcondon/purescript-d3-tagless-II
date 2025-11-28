-- | Les Misérables Force-Directed Graph (V3 Architecture)
-- |
-- | A clean, simple force-directed graph using the ForceEngine library.
-- | Uses the high-level Simulation API and PSD3v2 TreeAPI for rendering.
-- |
-- | NO FFI in this demo - all D3 interaction goes through library modules.
module D3.Viz.LesMisV3.Draw
  ( startLesMis
  ) where

import Prelude

import D3.Viz.LesMisV3.Model (LesMisLink, LesMisModel, LesMisNode)
import Data.Array as Array
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number (sqrt)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Partial.Unsafe (unsafeCrashWith)
import PSD3.ForceEngine.Core as Core
import PSD3.ForceEngine.Simulation as Sim
import PSD3.ForceEngine.Types (ForceSpec(..), defaultManyBody, defaultCollide, defaultLink, defaultCenter)
import PSD3.Internal.Scales.Scales (d3SchemeCategory10N_)
import PSD3v2.Attribute.Types (Attribute, cx, cy, fill, stroke, strokeWidth, x1, x2, y1, y2, radius, id_, class_, width, height, viewBox, opacity)
import PSD3v2.Behavior.Types (Behavior(..), ScaleExtent(..), defaultZoom)
import PSD3v2.Capabilities.Selection (select, renderTree, on, setAttrs)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2Selection_, reselectD3v2, getElementsD3v2)
import PSD3v2.Selection.Types (ElementType(..), SEmpty, SBoundOwns)
import PSD3v2.VizTree.Tree as T
import Web.DOM.Element (Element)

-- =============================================================================
-- Types
-- =============================================================================

-- | Swizzled link (source/target are node references, not indices)
type SwizzledLink =
  { source :: LesMisNode
  , target :: LesMisNode
  , value :: Number
  , index :: Int
  }

-- | State for the visualization
type VizState =
  { nodes :: Array LesMisNode
  , swizzledLinks :: Array SwizzledLink
  , nodesSel :: D3v2Selection_ SBoundOwns Element LesMisNode
  , linksSel :: D3v2Selection_ SBoundOwns Element SwizzledLink
  }

-- =============================================================================
-- Constants
-- =============================================================================

svgWidth :: Number
svgWidth = 900.0

svgHeight :: Number
svgHeight = 600.0

-- =============================================================================
-- Entry Point
-- =============================================================================

-- | Start the Les Misérables force-directed graph
-- | Returns a cleanup function to stop the simulation
startLesMis :: LesMisModel -> String -> Effect (Effect Unit)
startLesMis model containerSelector = do
  -- Swizzle links (replace indices with node references)
  let swizzledLinks = swizzleLinks model.nodes model.links

  -- Create simulation using library API
  sim <- Sim.create Sim.defaultConfig

  -- Set nodes (this initializes them with vx, vy, index)
  Sim.setNodes model.nodes sim

  -- Set links
  Sim.setLinks model.links sim

  -- Add forces using declarative ForceSpec
  Sim.addForce (ManyBody "charge" defaultManyBody { strength = -100.0, distanceMax = 500.0 }) sim
  Sim.addForce (Collide "collision" defaultCollide { radius = 5.0, strength = 1.0, iterations = 1 }) sim
  Sim.addForce (Center "center" defaultCenter { x = 0.0, y = 0.0, strength = 0.1 }) sim
  Sim.addForce (Link "links" defaultLink { distance = 30.0, strength = 0.5, iterations = 1 }) sim

  -- Render initial DOM and get selections for updates
  vizStateRef <- renderInitialDOM containerSelector model.nodes swizzledLinks

  -- Attach drag behavior to nodes with simulation reheat
  state <- Ref.read vizStateRef
  let nodeElements = getElementsD3v2 state.nodesSel
  Core.attachDragWithReheat nodeElements (Sim.reheat sim)

  -- Set tick callback to update DOM
  Sim.onTick (updateDOM vizStateRef) sim

  -- Start simulation
  Sim.start sim

  -- Return cleanup function
  pure (Sim.stop sim)

-- =============================================================================
-- DOM Rendering
-- =============================================================================

-- | Render the initial SVG structure and elements
-- | Returns a ref containing selections for later updates
renderInitialDOM :: String -> Array LesMisNode -> Array SwizzledLink -> Effect (Ref VizState)
renderInitialDOM containerSelector nodes swizzledLinks = runD3v2M do
  -- Select container
  container <- select containerSelector :: _ (D3v2Selection_ SEmpty Element Unit)

  -- Build SVG tree
  let svgTree :: T.Tree Unit
      svgTree =
        T.named SVG "svg"
          [ width svgWidth
          , height svgHeight
          , viewBox (show ((-svgWidth) / 2.0) <> " " <> show ((-svgHeight) / 2.0) <> " " <> show svgWidth <> " " <> show svgHeight)
          , id_ "lesmis-v3-svg"
          , class_ "lesmis-v3"
          ]
          `T.withChild`
            (T.named Group "zoomGroup"
              [ id_ "lesmis-zoom-group"
              , class_ "zoom-group"
              ]
              `T.withChildren`
                [ T.named Group "linksGroup" [ class_ "links" ]
                , T.named Group "nodesGroup" [ class_ "nodes" ]
                ])

  -- Render structure
  selections <- renderTree container svgTree

  -- Get groups for data joins
  linksGroupSel <- liftEffect $ reselectD3v2 "linksGroup" selections
  nodesGroupSel <- liftEffect $ reselectD3v2 "nodesGroup" selections
  svgSel <- liftEffect $ reselectD3v2 "svg" selections

  -- Render links
  let linksTree :: T.Tree SwizzledLink
      linksTree =
        T.joinData "linkElements" "line" swizzledLinks $ \link ->
          T.elem Line
            [ x1 link.source.x
            , y1 link.source.y
            , x2 link.target.x
            , y2 link.target.y
            , strokeWidth (sqrt link.value)
            , stroke "#999"
            , opacity 0.6
            ]

  linksSelections <- renderTree linksGroupSel linksTree

  -- Render nodes
  let nodesTree :: T.Tree LesMisNode
      nodesTree =
        T.joinData "nodeElements" "circle" nodes $ \node ->
          T.elem Circle
            [ cx node.x
            , cy node.y
            , radius 5.0
            , fill (d3SchemeCategory10N_ (toNumber node.group))
            , stroke "#fff"
            , strokeWidth 1.5
            ]

  nodesSelections <- renderTree nodesGroupSel nodesTree

  -- Attach zoom behavior
  _ <- on (Zoom $ defaultZoom (ScaleExtent 0.1 10.0) "#lesmis-zoom-group") svgSel

  -- Get the bound selections for later updates
  let nodesSel = case Map.lookup "nodeElements" nodesSelections of
        Just sel -> sel
        Nothing -> unsafeCrashWith "nodeElements not found"

  let linksSel = case Map.lookup "linkElements" linksSelections of
        Just sel -> sel
        Nothing -> unsafeCrashWith "linkElements not found"

  -- Create state ref
  liftEffect $ Ref.new
    { nodes
    , swizzledLinks
    , nodesSel
    , linksSel
    }

-- =============================================================================
-- DOM Updates (on each tick)
-- =============================================================================

-- | Update DOM positions based on current node positions
-- | Called on each simulation tick
updateDOM :: Ref VizState -> Effect Unit
updateDOM vizStateRef = runD3v2M do
  state <- liftEffect $ Ref.read vizStateRef

  -- Update node positions (cx, cy)
  _ <- setAttrs nodeAttrs state.nodesSel

  -- Update link positions (x1, y1, x2, y2)
  _ <- setAttrs linkAttrs state.linksSel

  pure unit

  where
  -- Attributes for nodes - data functions read current x,y
  nodeAttrs :: Array (Attribute LesMisNode)
  nodeAttrs =
    [ cx ((_.x) :: LesMisNode -> Number)
    , cy ((_.y) :: LesMisNode -> Number)
    ]

  -- Attributes for links - read positions from swizzled source/target
  linkAttrs :: Array (Attribute SwizzledLink)
  linkAttrs =
    [ x1 ((\link -> link.source.x) :: SwizzledLink -> Number)
    , y1 ((\link -> link.source.y) :: SwizzledLink -> Number)
    , x2 ((\link -> link.target.x) :: SwizzledLink -> Number)
    , y2 ((\link -> link.target.y) :: SwizzledLink -> Number)
    ]

-- =============================================================================
-- Link Swizzling
-- =============================================================================

-- | Replace link indices with node references
swizzleLinks :: Array LesMisNode -> Array LesMisLink -> Array SwizzledLink
swizzleLinks nodes links =
  Array.mapWithIndex swizzle links
  where
  swizzle i link =
    { source: unsafeArrayIndex nodes link.source
    , target: unsafeArrayIndex nodes link.target
    , value: link.value
    , index: i
    }

-- Safe-ish array index (we know indices are valid from processRawModel)
unsafeArrayIndex :: forall a. Array a -> Int -> a
unsafeArrayIndex arr i = case Array.index arr i of
  Just x -> x
  Nothing -> unsafeCrashWith ("Array index out of bounds: " <> show i)
