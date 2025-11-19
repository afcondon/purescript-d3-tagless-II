module D3.Viz.ModuleGraph.Viz where

-- | Simple force-directed module graph using Tree API
-- | Based on LesMisGUPV2 but simplified - no GUP, no dynamic layouts

import Prelude

import D3.Viz.ModuleGraph.Model (ModuleGraph, ModuleSimNode)
import Data.Array as Array
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import PSD3.Data.Node (D3Link_Swizzled)
import PSD3.Internal.FFI (keyIsID_)
import PSD3.Internal.Scales.Scales (d3SchemeCategory10N_)
import PSD3.Internal.Simulation.Config as F
import PSD3.Internal.Simulation.Forces (createForce, createLinkForce, initialize)
import PSD3.Internal.Simulation.Types (Force)
import PSD3v2.Attribute.Types (class_, cx, cy, fill, fontSize, height, id_, radius, stroke, strokeOpacity, strokeWidth, textAnchor, textContent, viewBox, width, x, x1, x2, y, y1, y2)
import PSD3v2.Behavior.Types (Behavior(..), ScaleExtent(..), defaultZoom, simulationDrag)
import PSD3v2.Capabilities.Selection (on, renderTree, select)
import PSD3v2.Capabilities.Simulation (Step(..), addTickFunction, init, start)
import PSD3v2.Interpreter.D3v2 (D3v2SimM, D3v2Selection_, reselectD3v2)
import PSD3v2.Selection.Types (ElementType(..), SBoundOwns, SBoundInherits)
import Partial.Unsafe (unsafePartial, unsafeCrashWith)
import Web.DOM.Element (Element)
import PSD3v2.VizTree.Tree as T
import Unsafe.Coerce (unsafeCoerce)
import Utility (getWindowWidthHeight)

-- | Indexed link for data join
newtype IndexedLink = IndexedLink { index :: Int, link :: D3Link_Swizzled }

instance Eq IndexedLink where
  eq (IndexedLink a) (IndexedLink b) = a.index == b.index

instance Ord IndexedLink where
  compare (IndexedLink a) (IndexedLink b) = compare a.index b.index

-- | Draw the module graph visualization
drawModuleGraph :: forall row.
  Array (Force ModuleSimNode) ->
  Set.Set String ->
  ModuleGraph ->
  String ->
  D3v2SimM row ModuleSimNode Unit
drawModuleGraph forcesArray activeForces graph containerSelector = do
  -- Get container dimensions
  Tuple w h <- liftEffect getWindowWidthHeight

  container <- select containerSelector

  -- Declarative tree structure for force graph (using Tree API)
  let forceGraphTree :: T.Tree Unit
      forceGraphTree =
        T.named SVG "svg"
          [ width w
          , height h
          , viewBox (show ((-w) / 2.0) <> " " <> show ((-h) / 2.0) <> " " <> show w <> " " <> show h)
          , id_ "module-graph-svg"
          , class_ "module-graph"
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
                , T.named Group "labelsGroup"
                    [ id_ "labels"
                    , class_ "labels"
                    ]
                ])

  -- Render the structure tree
  selections <- renderTree container forceGraphTree

  -- Extract selections for behaviors
  svg <- liftEffect $ reselectD3v2 "svg" selections
  linksGroup <- liftEffect $ reselectD3v2 "linksGroup" selections
  nodesGroup <- liftEffect $ reselectD3v2 "nodesGroup" selections
  labelsGroup <- liftEffect $ reselectD3v2 "labelsGroup" selections

  -- Setup zoom behavior on SVG, targeting zoomGroup
  _ <- on (Zoom $ defaultZoom (ScaleExtent 0.1 10.0) "#zoom-group") svg

  -- Initialize simulation
  { nodes: nodesInSim, links: linksInSim } <- init
    { nodes: graph.nodes
    , links: graph.links
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

  -- Swizzle links (after init)
  let linksSwizzled = unsafeCoerce linksInSim :: Array D3Link_Swizzled

  -- Create indexed links for data join
  let indexedLinks = Array.mapWithIndex (\i link -> IndexedLink { index: i, link }) linksSwizzled

  -- Render links tree into linksGroup using Tree API
  let linksTree :: T.Tree IndexedLink
      linksTree =
        T.joinData "linkElements" "line" indexedLinks $ \(IndexedLink il) ->
          let link = unsafeCoerce il.link
          in T.elem Line
            [ x1 ((\(_ :: IndexedLink) -> link.source.x) :: IndexedLink -> Number)
            , y1 ((\(_ :: IndexedLink) -> link.source.y) :: IndexedLink -> Number)
            , x2 ((\(_ :: IndexedLink) -> link.target.x) :: IndexedLink -> Number)
            , y2 ((\(_ :: IndexedLink) -> link.target.y) :: IndexedLink -> Number)
            , strokeWidth 1.0
            , stroke "#999"
            , strokeOpacity 0.6
            ]

  linksSelections <- renderTree linksGroup linksTree

  -- Render nodes tree into nodesGroup using Tree API
  let nodesTree :: T.Tree ModuleSimNode
      nodesTree =
        T.joinData "nodeElements" "circle" (unsafeCoerce nodesInSim) $ \(d :: ModuleSimNode) ->
          T.elem Circle
            [ cx d.x
            , cy d.y
            , radius 5.0
            , fill (d3SchemeCategory10N_ (toNumber d.group))
            , stroke "#fff"
            , strokeWidth 1.5
            ]

  nodesSelections <- renderTree nodesGroup nodesTree

  -- Render labels tree into labelsGroup using Tree API
  let labelsTree :: T.Tree ModuleSimNode
      labelsTree =
        T.joinData "labelElements" "text" (unsafeCoerce nodesInSim) $ \(d :: ModuleSimNode) ->
          T.elem Text
            [ x d.x
            , y (d.y + 3.0)  -- Offset slightly below center
            , textAnchor "middle"
            , fontSize 10.0
            , fill "#000"
            , strokeOpacity 1.0
            , class_ "node-label"
            , textContent d.name
            ]

  labelsSelections <- renderTree labelsGroup labelsTree

  -- Extract bound selections for behaviors and tick functions
  let nodeCircles :: D3v2Selection_ SBoundOwns Element ModuleSimNode
      nodeCircles = case Map.lookup "nodeElements" nodesSelections of
        Just sel -> sel
        Nothing -> unsafePartial $ unsafeCrashWith "nodeElements not found"

      linkLines :: D3v2Selection_ SBoundOwns Element IndexedLink
      linkLines = case Map.lookup "linkElements" linksSelections of
        Just sel -> sel
        Nothing -> unsafePartial $ unsafeCrashWith "linkElements not found"

      labelTexts :: D3v2Selection_ SBoundOwns Element ModuleSimNode
      labelTexts = case Map.lookup "labelElements" labelsSelections of
        Just sel -> sel
        Nothing -> unsafePartial $ unsafeCrashWith "labelElements not found"

  -- Setup drag behavior on nodes
  _ <- on (Drag $ simulationDrag "module-graph") nodeCircles

  -- Add tick function to update positions
  addTickFunction "links" $ Step linkLines
    [ x1 ((\(IndexedLink il) -> let link = unsafeCoerce il.link in link.source.x) :: IndexedLink -> Number)
    , y1 ((\(IndexedLink il) -> let link = unsafeCoerce il.link in link.source.y) :: IndexedLink -> Number)
    , x2 ((\(IndexedLink il) -> let link = unsafeCoerce il.link in link.target.x) :: IndexedLink -> Number)
    , y2 ((\(IndexedLink il) -> let link = unsafeCoerce il.link in link.target.y) :: IndexedLink -> Number)
    ]

  addTickFunction "nodes" $ Step nodeCircles
    [ cx ((\(d :: ModuleSimNode) -> d.x) :: ModuleSimNode -> Number)
    , cy ((\(d :: ModuleSimNode) -> d.y) :: ModuleSimNode -> Number)
    ]

  addTickFunction "labels" $ Step labelTexts
    [ x ((\(d :: ModuleSimNode) -> d.x) :: ModuleSimNode -> Number)
    , y ((\(d :: ModuleSimNode) -> d.y + 3.0) :: ModuleSimNode -> Number)
    ]

  -- Start simulation
  start

  pure unit
