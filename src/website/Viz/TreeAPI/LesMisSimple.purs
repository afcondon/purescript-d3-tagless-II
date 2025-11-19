module D3.Viz.TreeAPI.LesMisSimple where

-- | Simplified Les Misérables force-directed graph
-- | For Tour Motion page - just zoom and drag, no other controls

import Prelude

import D3.Viz.LesMiserables.Model (LesMisRawModel, LesMisSimNode)
import Data.Array as Array
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number (sqrt, cos, sin, pi)
import Data.Set as Set
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Partial.Unsafe (unsafePartial, unsafeCrashWith)
import PSD3.Data.Node (D3Link_Swizzled)
import PSD3.Internal.FFI (keyIsID_, linksForceName_)
import PSD3.Internal.Scales.Scales (d3SchemeCategory10N_)
import PSD3.Internal.Simulation.Types (Force, ForceType(..), RegularForceType(..), allNodes, initialSimulationState)
import PSD3.Internal.Simulation.Config as F
import PSD3.Internal.Simulation.Forces (createForce, createLinkForce, initialize)
import PSD3v2.Attribute.Types (cx, cy, fill, radius, stroke, strokeWidth, x1, x2, y1, y2, id_, class_, width, height, viewBox)
import PSD3v2.Behavior.Types (Behavior(..), defaultZoom, simulationDrag, ScaleExtent(..))
import PSD3v2.Capabilities.Selection (select, renderTree, on)
import PSD3v2.Capabilities.Simulation (init, addTickFunction, start, Step(..))
import PSD3v2.Interpreter.D3v2 (runD3v2SimM, D3v2Selection_, D3v2SimM, reselectD3v2)
import PSD3v2.Selection.Types (ElementType(..), SEmpty, SBoundOwns, SBoundInherits)
import PSD3v2.VizTree.Tree as T
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (Element)

-- | Indexed link for data join
newtype IndexedLink = IndexedLink { index :: Int, link :: D3Link_Swizzled }

instance Eq IndexedLink where
  eq (IndexedLink a) (IndexedLink b) = a.index == b.index

instance Ord IndexedLink where
  compare (IndexedLink a) (IndexedLink b) = compare a.index b.index

-- | Phylotaxis layout constants (sunflower spiral pattern)
initialRadius :: Number
initialRadius = 10.0

initialAngle :: Number
initialAngle = pi * (3.0 - sqrt 5.0)  -- Golden angle

-- | Apply phylotaxis positions to nodes
setPhyllotaxisPositions :: forall r. Array (Record (x :: Number, y :: Number | r)) -> Array (Record (x :: Number, y :: Number | r))
setPhyllotaxisPositions nodes = Array.mapWithIndex setPosition nodes
  where
    setPosition :: Int -> Record (x :: Number, y :: Number | r) -> Record (x :: Number, y :: Number | r)
    setPosition index node =
      let
        i = toNumber index
        radius' = initialRadius * sqrt (0.5 + i)
        angle = i * initialAngle
      in
        node { x = radius' * cos angle, y = radius' * sin angle }

-- | Draw Les Mis force graph with minimal controls (zoom + drag only)
drawLesMisSimple :: forall row.
  LesMisRawModel ->
  String ->
  Number ->
  Number ->
  D3v2SimM row LesMisSimNode Unit
drawLesMisSimple model containerSelector w h = do
  liftEffect $ Console.log $ "=== Drawing LesMis Simple ==="

  -- Select container
  container <- select containerSelector :: _ (D3v2Selection_ SEmpty Element Unit)

  -- Apply phylotaxis initial positions
  let nodesWithPositions = setPhyllotaxisPositions model.nodes

  -- Create forces
  let manyBodyForce = createForce "many body negative" (RegularForce ForceManyBody) allNodes [ F.strengthVal (-100.0) ]
  let centerForce = createForce "center" (RegularForce ForceCenter) allNodes [ F.xVal 0.0, F.yVal 0.0, F.strengthVal 0.1 ]
  let linksForce = createLinkForce allNodes []
  let collisionForce = createForce "collision" (RegularForce ForceCollide) allNodes [ F.radiusVal 6.0 ]

  -- Build force library
  let forceLibrary = initialize [ manyBodyForce, centerForce, linksForce, collisionForce ]
  let forcesArray = [ manyBodyForce, centerForce, linksForce, collisionForce ]
  let activeForces = Set.fromFoldable ["many body negative", "center", linksForceName_, "collision"]

  -- Initialize simulation
  { nodes: nodesInSim, links: linksInSim } <- init
    { nodes: nodesWithPositions
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

  -- Wrap links with indices
  let indexedLinks = Array.mapWithIndex (\i link -> IndexedLink { index: i, link }) linksInSim

  -- Declarative tree structure
  let forceGraphTree :: T.Tree Unit
      forceGraphTree =
        T.named SVG "svg"
          [ width w
          , height h
          , viewBox (show ((-w) / 2.0) <> " " <> show ((-h) / 2.0) <> " " <> show w <> " " <> show h)
          , id_ "lesmis-simple-svg"
          , class_ "lesmis-simple"
          ]
          `T.withChild`
            (T.named Group "zoomGroup"
              [ id_ "zoom-group"
              , class_ "zoom-group"
              ]
              `T.withChildren`
                [ T.named Group "linksGroup" [ class_ "links" ]
                , T.named Group "nodesGroup" [ class_ "nodes" ]
                ])

  -- Render structure
  selections <- renderTree container forceGraphTree

  -- Reselect groups for data rendering
  linksGroupSel <- liftEffect $ reselectD3v2 "linksGroup" selections
  nodesGroupSel <- liftEffect $ reselectD3v2 "nodesGroup" selections

  -- Render links
  let linksTree :: T.Tree IndexedLink
      linksTree =
        T.joinData "linkElements" "line" indexedLinks $ \(IndexedLink il) ->
          let link = unsafeCoerce il.link
          in T.elem Line
            [ x1 ((\(_ :: IndexedLink) -> link.source.x) :: IndexedLink -> Number)
            , y1 ((\(_ :: IndexedLink) -> link.source.y) :: IndexedLink -> Number)
            , x2 ((\(_ :: IndexedLink) -> link.target.x) :: IndexedLink -> Number)
            , y2 ((\(_ :: IndexedLink) -> link.target.y) :: IndexedLink -> Number)
            , strokeWidth ((\(_ :: IndexedLink) -> sqrt link.value) :: IndexedLink -> Number)
            , stroke ((\(_ :: IndexedLink) -> d3SchemeCategory10N_ (toNumber link.target.group)) :: IndexedLink -> String)
            ]

  linksSelections <- renderTree linksGroupSel linksTree

  -- Render nodes
  let nodesTree :: T.Tree LesMisSimNode
      nodesTree =
        T.joinData "nodeElements" "circle" nodesInSim $ \(d :: LesMisSimNode) ->
          T.elem Circle
            [ cx d.x
            , cy d.y
            , radius 5.0
            , fill (d3SchemeCategory10N_ (toNumber d.group))
            , stroke "#fff"
            , strokeWidth 2.0
            ]

  nodesSelections <- renderTree nodesGroupSel nodesTree

  -- Extract selections for behaviors
  svgSel <- liftEffect $ reselectD3v2 "svg" selections

  let nodesSel :: D3v2Selection_ SBoundOwns Element LesMisSimNode
      nodesSel = case Map.lookup "nodeElements" nodesSelections of
        Just sel -> sel
        Nothing -> unsafePartial $ unsafeCrashWith "nodeElements not found"

  let linksSel :: D3v2Selection_ SBoundOwns Element IndexedLink
      linksSel = case Map.lookup "linkElements" linksSelections of
        Just sel -> sel
        Nothing -> unsafePartial $ unsafeCrashWith "linkElements not found"

  -- Attach zoom behavior
  _ <- on (Zoom $ defaultZoom (ScaleExtent 0.1 10.0) ".zoom-group") svgSel

  -- Attach drag behavior to nodes (simulation-aware when in D3v2SimM monad)
  _ <- on (Drag $ simulationDrag "lesmis-simple") nodesSel

  -- Register tick functions to update positions
  addTickFunction "update-nodes" $ Step nodesSel
    [ cx ((_.x) :: LesMisSimNode -> Number)
    , cy ((_.y) :: LesMisSimNode -> Number)
    ]

  addTickFunction "update-links" $ Step linksSel
    [ x1 ((\(IndexedLink il) -> (unsafeCoerce il.link).source.x) :: IndexedLink -> Number)
    , y1 ((\(IndexedLink il) -> (unsafeCoerce il.link).source.y) :: IndexedLink -> Number)
    , x2 ((\(IndexedLink il) -> (unsafeCoerce il.link).target.x) :: IndexedLink -> Number)
    , y2 ((\(IndexedLink il) -> (unsafeCoerce il.link).target.y) :: IndexedLink -> Number)
    ]

  -- Start simulation
  start

  liftEffect $ Console.log "Les Mis Simple: simulation started"

-- | Entry point with fixed dimensions for Tour page
startLesMisSimple :: LesMisRawModel -> String -> Effect Unit
startLesMisSimple model containerSelector = launchAff_ do
  -- Fixed dimensions for embedded view
  let w = 900.0
  let h = 600.0

  -- Create forces and initial simulation state
  let manyBodyForce = createForce "many body negative" (RegularForce ForceManyBody) allNodes [ F.strengthVal (-100.0) ]
  let centerForce = createForce "center" (RegularForce ForceCenter) allNodes [ F.xVal 0.0, F.yVal 0.0, F.strengthVal 0.1 ]
  let linksForce = createLinkForce allNodes []
  let collisionForce = createForce "collision" (RegularForce ForceCollide) allNodes [ F.radiusVal 6.0 ]

  let forceLibrary = initialize [ manyBodyForce, centerForce, linksForce, collisionForce ]
  let initialState = { simulation: initialSimulationState forceLibrary }

  -- Run the viz
  void $ runD3v2SimM initialState $ drawLesMisSimple model containerSelector w h
