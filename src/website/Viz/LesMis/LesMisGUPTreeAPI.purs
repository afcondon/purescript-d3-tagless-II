module D3.Viz.LesMisGUPTreeAPI where

-- | Force-directed graph using TreeAPI + Simulation integration
-- |
-- | Demonstrates the new renderTreeWithSimulation pattern that combines:
-- | - Declarative tree structure for SVG hierarchy
-- | - Imperative callback for simulation setup
-- |
-- | This is a simplified version of LesMisGUPV2 to demonstrate the pattern.

import Prelude

import D3.Viz.LesMiserables.Model (LesMisRawModel, LesMisSimNode, LesMisNodeRow, LesMisLinkRow, LesMisLink)
import PSD3.Data.Node (SwizzledLink)
import Data.Array as Array
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import PSD3.Internal.Simulation.Types (Force)
import PSD3.Internal.Scales.Scales (d3SchemeCategory10N_)
import PSD3.Internal.FFI (keyIsID_)
import PSD3v2.Attribute.Types (cx, cy, fill, radius, stroke, strokeWidth, x1, x2, y1, y2, id_, class_, width, height, viewBox)
import PSD3v2.Behavior.Types (Behavior(..), defaultDrag, defaultZoom, simulationDrag, ScaleExtent(..))
import PSD3v2.Capabilities.Selection (select, on, renderTreeWithSimulation, renderTree)
import PSD3v2.Capabilities.Simulation (init, addTickFunction, start, Step(..))
import PSD3v2.Interpreter.D3v2 (D3v2SimM, D3v2Selection_)
import PSD3v2.Interpreter.D3v2 as D3v2
import PSD3v2.Selection.Types (ElementType(..), SBoundOwns, SEmpty)
import Partial.Unsafe (unsafePartial, unsafeCrashWith)
import Web.DOM.Element (Element)
import PSD3v2.VizTree.Tree as T
import Unsafe.Coerce (unsafeCoerce)
import Utility (getWindowWidthHeight)
import Effect (Effect)
import Effect.Class.Console (log)
import PSD3.Internal.FFI (linksForceName_)
import PSD3.Internal.Simulation.Config as F
import PSD3.Internal.Simulation.Forces (createForce, createLinkForce)
import PSD3.Internal.Simulation.Types (Force, ForceType(..), RegularForceType(..), allNodes, initialSimulationState)
import PSD3.Internal.Simulation.Forces (initialize) as Forces

-- | Swizzled link type alias for LesMis
type LesMisSwizzledLink = SwizzledLink LesMisNodeRow LesMisLinkRow

-- | Indexed link for data join
newtype IndexedLink = IndexedLink { index :: Int, link :: LesMisSwizzledLink }

instance Eq IndexedLink where
  eq (IndexedLink a) (IndexedLink b) =
    let linkA = unsafeCoerce a.link :: { source :: { id :: String }, target :: { id :: String } }
        linkB = unsafeCoerce b.link :: { source :: { id :: String }, target :: { id :: String } }
    in linkA.source.id == linkB.source.id && linkA.target.id == linkB.target.id

instance Ord IndexedLink where
  compare (IndexedLink a) (IndexedLink b) =
    let linkA = unsafeCoerce a.link :: { source :: { id :: String }, target :: { id :: String } }
        linkB = unsafeCoerce b.link :: { source :: { id :: String }, target :: { id :: String } }
        sourceComp = compare linkA.source.id linkB.source.id
    in if sourceComp == EQ
       then compare linkA.target.id linkB.target.id
       else sourceComp

-- | Keyed node for data join
newtype KeyedNode = KeyedNode LesMisSimNode

instance Eq KeyedNode where
  eq (KeyedNode a) (KeyedNode b) =
    let nodeA = unsafeCoerce a :: { id :: String }
        nodeB = unsafeCoerce b :: { id :: String }
    in nodeA.id == nodeB.id

instance Ord KeyedNode where
  compare (KeyedNode a) (KeyedNode b) =
    let nodeA = unsafeCoerce a :: { id :: String }
        nodeB = unsafeCoerce b :: { id :: String }
    in compare nodeA.id nodeB.id

-- | Main entry point
main :: LesMisRawModel -> Effect Unit
main rawModel = launchAff_ do
  liftEffect $ log "LesMisGUPTreeAPI: Starting"
  Tuple w h <- liftEffect getWindowWidthHeight

  -- Create initial simulation state with empty force library
  -- (forces will be created inside renderWithTreeAPI)
  let initState = { simulation: initialSimulationState Map.empty }

  _ <- D3v2.runD3v2SimM initState (renderWithTreeAPI rawModel w h)
  liftEffect $ log "LesMisGUPTreeAPI: Complete"

-- | Render using TreeAPI + renderTreeWithSimulation pattern
renderWithTreeAPI :: forall row.
  LesMisRawModel
  -> Number
  -> Number
  -> D3v2SimM row LesMisSimNode Unit
renderWithTreeAPI rawModel w h = do
  log "LesMisGUPTreeAPI: renderWithTreeAPI"

  -- Define the tree structure declaratively
  -- The SVG hierarchy is separate from the simulation logic
  let vb = "0 0 " <> show w <> " " <> show h
      svgTree :: T.Tree Unit
      svgTree =
        T.named SVG "svg" [ id_ "lesmis-gup-treeapi-svg"
                          , width w
                          , height h
                          , viewBox vb
                          ] `T.withChild`
          T.named Group "inner" [] `T.withChildren`
            [ T.named Group "linksGroup" [ class_ "links" ]
            , T.named Group "nodesGroup" [ class_ "nodes" ]
            ]

  -- Select container
  container <- select "#lesmis-gup-treeapi"

  -- Use renderTreeWithSimulation to combine declarative tree with simulation setup
  _ <- renderTreeWithSimulation container svgTree \selections -> do
    -- Extract the groups we need from the tree
    let inner = case Map.lookup "inner" selections of
          Just sel -> sel
          Nothing -> unsafePartial $ unsafeCrashWith "inner not found"

    -- Attach zoom/drag to inner group
    svg <- select "#lesmis-gup-treeapi-svg"
    _ <- on (Drag defaultDrag) inner
    _ <- on (Zoom (defaultZoom (ScaleExtent 0.1 4.0) "g")) svg

    -- Create forces
    let manyBodyForce = createForce "charge" (RegularForce ForceManyBody) allNodes [ F.strengthVal (-100.0) ]
        collisionForce = createForce "collision" (RegularForce ForceCollide) allNodes [ F.radiusVal 6.0 ]
        centerForce = createForce "center" (RegularForce ForceCenter) allNodes [ F.xVal 0.0, F.yVal 0.0, F.strengthVal 0.1 ]
        linksForce = createLinkForce allNodes []
        forcesArray = [ manyBodyForce, collisionForce, centerForce, linksForce ]

    -- Initialize simulation and get nodes/links (links are now swizzled)
    { nodes: nodesInSim, links: linksInSim } <- init
      { nodes: rawModel.nodes
      , links: rawModel.links
      , forces: forcesArray
      , activeForces: Set.fromFoldable ["charge", "collision", "center", linksForceName_]
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

    -- Create indexed links and keyed nodes for data joins
    let indexedLinks = Array.mapWithIndex (\i l -> IndexedLink { index: i, link: l }) linksInSim
        keyedNodes = nodesInSim <#> KeyedNode

    -- Get fresh empty selections for data binding
    linksGroupEmpty <- select "g.links"
    nodesGroupEmpty <- select "g.nodes"

    -- Render links into linksGroup using TreeAPI
    let linksTree :: T.Tree IndexedLink
        linksTree =
          T.joinData "linkElements" "line" indexedLinks $ \(IndexedLink il) ->
            T.elem Line
              [ x1 ((unsafeCoerce il.link).source.x :: Number)
              , y1 ((unsafeCoerce il.link).source.y :: Number)
              , x2 ((unsafeCoerce il.link).target.x :: Number)
              , y2 ((unsafeCoerce il.link).target.y :: Number)
              , stroke "#999"
              , strokeWidth 1.0
              ]

    linksSelections <- renderTree linksGroupEmpty linksTree

    -- Render nodes into nodesGroup using TreeAPI
    let nodesTree :: T.Tree KeyedNode
        nodesTree =
          T.joinData "nodeElements" "circle" keyedNodes $ \(KeyedNode d) ->
            T.elem Circle
              [ cx d.x
              , cy d.y
              , radius 5.0
              , fill (d3SchemeCategory10N_ (toNumber d.group))
              , stroke "#fff"
              , strokeWidth 2.0
              ]

    nodesSelections <- renderTree nodesGroupEmpty nodesTree

    -- Extract bound selections for tick functions
    let nodeCircles = case Map.lookup "nodeElements" nodesSelections of
          Just sel -> sel
          Nothing -> unsafePartial $ unsafeCrashWith "nodeElements not found"

    let linkLines = case Map.lookup "linkElements" linksSelections of
          Just sel -> sel
          Nothing -> unsafePartial $ unsafeCrashWith "linkElements not found"

    -- Attach simulation drag
    _ <- on (Drag $ simulationDrag "lesmis-gup-treeapi") nodeCircles

    -- Add tick functions
    addTickFunction "nodes" $ Step nodeCircles
      [ cx (\(KeyedNode d) -> d.x)
      , cy (\(KeyedNode d) -> d.y)
      ]

    addTickFunction "links" $ Step linkLines
      [ x1 (\(IndexedLink il) -> (unsafeCoerce il.link).source.x :: Number)
      , y1 (\(IndexedLink il) -> (unsafeCoerce il.link).source.y :: Number)
      , x2 (\(IndexedLink il) -> (unsafeCoerce il.link).target.x :: Number)
      , y2 (\(IndexedLink il) -> (unsafeCoerce il.link).target.y :: Number)
      ]

    -- Start simulation
    start

  log "LesMisGUPTreeAPI: Initialization complete"
