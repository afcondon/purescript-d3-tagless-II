module D3.Viz.TreeAPI.LesMisTreeExample where

-- | Force-directed graph using declarative Tree API
-- |
-- | Demonstrates:
-- | - Tree API for structure declaration
-- | - Integration with force simulation
-- | - Automatic position updates on tick

import Prelude

import D3.Viz.LesMiserables.Model (LesMisRawModel, LesMisSimNode)
import Data.Array as Array
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number (sqrt, cos, sin, pi)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web as AJAX
import D3.Viz.LesMiserables.File (readGraphFromFileContents)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff.Class (liftAff)
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
import PSD3v2.Behavior.Types (Behavior(..), defaultDrag, defaultZoom, simulationDrag, ScaleExtent(..))
import PSD3v2.Capabilities.Selection (select, renderTree, on)
import PSD3v2.Capabilities.Simulation (init, addTickFunction, start, Step(..))
import PSD3v2.Interpreter.D3v2 (runD3v2SimM, D3v2Selection_, D3v2SimM, reselectD3v2)
import PSD3v2.Selection.Types (ElementType(..), SEmpty, SBound)
import PSD3v2.VizTree.Tree as T
import Unsafe.Coerce (unsafeCoerce)
import Utility (getWindowWidthHeight)
import Web.DOM.Element (Element)

-- | Indexed link for data join (links don't have Ord instance, so we use index)
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
        radius = initialRadius * sqrt (0.5 + i)
        angle = i * initialAngle
      in
        node { x = radius * cos angle, y = radius * sin angle }

-- | Test function - loads real Les Misérables data
testLesMisTree :: Effect Unit
testLesMisTree = launchAff_ do
  -- Load the Les Misérables dataset
  liftEffect $ Console.log "Loading Les Misérables data..."
  response <- AJAX.get ResponseFormat.string "./data/miserables.json"

  let model = readGraphFromFileContents response
  liftEffect $ Console.log $ "Loaded " <> show (Array.length model.nodes) <> " nodes and " <> show (Array.length model.links) <> " links"

  -- Create forces for the simulation
  let manyBodyForce = createForce "many body negative" (RegularForce ForceManyBody) allNodes [ F.strengthVal (-100.0) ]
  let centerForce = createForce "center" (RegularForce ForceCenter) allNodes [ F.xVal 0.0, F.yVal 0.0, F.strengthVal 0.1 ]
  let linksForce = createLinkForce allNodes []
  let collisionForce = createForce "collision" (RegularForce ForceCollide) allNodes [ F.radiusVal 6.0 ]

  -- Build force library
  let forceLibrary = initialize [ manyBodyForce, centerForce, linksForce, collisionForce ]

  let forcesArray = [ manyBodyForce, centerForce, linksForce, collisionForce ]
  let activeForces = Set.fromFoldable ["many body negative", "center", linksForceName_, "collision"]

  -- Create initial simulation state with force library
  let initialState = { simulation: initialSimulationState forceLibrary }

  -- Run with simulation state management
  void $ liftAff $ runD3v2SimM initialState do
    drawLesMisTree forcesArray activeForces model "#viz"

-- | Draw LesMis using Tree API
drawLesMisTree :: forall row.
  Array (Force LesMisSimNode) ->
  Set.Set String ->
  LesMisRawModel ->
  String ->
  D3v2SimM row LesMisSimNode Unit
drawLesMisTree forcesArray activeForces model containerSelector = do
  liftEffect $ Console.log $ "=== Drawing LesMis with Tree API ==="
  liftEffect $ Console.log $ "Model has " <> show (Array.length model.nodes) <> " nodes, " <> show (Array.length model.links) <> " links"

  -- Get window dimensions
  (Tuple w h) <- liftEffect getWindowWidthHeight
  liftEffect $ Console.log $ "Window dimensions: " <> show w <> " x " <> show h

  -- Select container
  container <- select containerSelector :: _ (D3v2Selection_ SEmpty Element Unit)

  -- Apply phylotaxis initial positions
  let nodesWithPositions = setPhyllotaxisPositions model.nodes

  -- Initialize simulation first (so we have node/link data with positions)
  liftEffect $ Console.log "Initializing simulation..."
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

  liftEffect $ Console.log $ "Simulation initialized with " <> show (Array.length nodesInSim) <> " nodes, " <> show (Array.length linksInSim) <> " links"

  -- Wrap links with indices for data join
  let indexedLinks = Array.mapWithIndex (\i link -> IndexedLink { index: i, link }) linksInSim
  liftEffect $ Console.log $ "Created " <> show (Array.length indexedLinks) <> " indexed links"

  -- Declarative tree structure for force graph
  let forceGraphTree :: T.Tree Unit
      forceGraphTree =
        T.named SVG "svg"
          [ width w
          , height h
          , viewBox (show ((-w) / 2.0) <> " " <> show ((-h) / 2.0) <> " " <> show w <> " " <> show h)
          , id_ "lesmis-tree-svg"
          , class_ "lesmis-tree"
          ]
          `T.withChild`
            (T.named Group "zoomGroup"
              [ id_ "zoom-group"
              , class_ "zoom-group"
              ]
              `T.withChildren`
                [ -- Links group (structural, no data)
                  T.named Group "linksGroup"
                    [ id_ "links"
                    , class_ "links"
                    ]
                , -- Nodes group (structural, no data)
                  T.named Group "nodesGroup"
                    [ id_ "nodes"
                    , class_ "nodes"
                    ]
                ])

  -- Render the structure tree
  liftEffect $ Console.log "Rendering structure tree..."
  selections <- renderTree container forceGraphTree
  liftEffect $ Console.log $ "Structure rendered, selections: " <> show (Map.keys selections)

  -- Extract the linksGroup and nodesGroup for rendering data
  -- Use reselectD3v2 to convert from SBound Unit to SEmpty datumOut
  linksGroupSel <- liftEffect $ reselectD3v2 "linksGroup" selections
  nodesGroupSel <- liftEffect $ reselectD3v2 "nodesGroup" selections
  liftEffect $ Console.log "Extracted linksGroup and nodesGroup selections"

  -- Render links tree into linksGroup
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

  liftEffect $ Console.log $ "Rendering " <> show (Array.length indexedLinks) <> " links into linksGroup..."
  linksSelections <- renderTree linksGroupSel linksTree
  liftEffect $ Console.log $ "Links rendered, selections: " <> show (Map.keys linksSelections)

  -- Render nodes tree into nodesGroup
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

  liftEffect $ Console.log $ "Rendering " <> show (Array.length nodesInSim) <> " nodes into nodesGroup..."
  nodesSelections <- renderTree nodesGroupSel nodesTree
  liftEffect $ Console.log $ "Nodes rendered, selections: " <> show (Map.keys nodesSelections)

  -- Extract final selections for behaviors
  zoomGroupSel <- liftEffect $ reselectD3v2 "zoomGroup" selections
  svgSel <- liftEffect $ reselectD3v2 "svg" selections

  -- Extract bound selections for drag and tick
  -- IMPORTANT: Must use `case` pattern matching, NOT `fromMaybe`, when default might throw!
  --
  -- ROOT CAUSE: PureScript is strict, so `fromMaybe (unsafeCrashWith "error") maybe_value`
  -- evaluates unsafeCrashWith BEFORE calling fromMaybe (JavaScript evaluates all arguments
  -- before calling functions). The crash happens even if maybe_value is Just!
  --
  -- Pattern matching with `case` is lazy in unevaluated branches, so the Nothing branch
  -- only executes if we actually match Nothing. This is the correct pattern when the
  -- default might throw an exception or have side effects.
  let nodesSel :: D3v2Selection_ SBound Element LesMisSimNode
      nodesSel = case Map.lookup "nodeElements" nodesSelections of
        Just sel -> sel
        Nothing -> unsafePartial $ unsafeCrashWith "nodeElements not found"

  let linksSel :: D3v2Selection_ SBound Element IndexedLink
      linksSel = case Map.lookup "linkElements" linksSelections of
        Just sel -> sel
        Nothing -> unsafePartial $ unsafeCrashWith "linkElements not found"

  -- Attach behaviors
  _ <- on (Drag defaultDrag) zoomGroupSel
  _ <- on (Zoom $ defaultZoom (ScaleExtent 0.5 4.0) "#zoom-group") svgSel
  _ <- on (Drag $ simulationDrag "lesmis-tree") nodesSel

  -- Add tick functions to update positions
  addTickFunction "nodes" $ Step nodesSel
    [ cx (\(d :: LesMisSimNode) -> d.x)
    , cy (\(d :: LesMisSimNode) -> d.y)
    ]

  addTickFunction "links" $ Step linksSel
    [ x1 (\(IndexedLink il) -> (unsafeCoerce il.link).source.x :: Number)
    , y1 (\(IndexedLink il) -> (unsafeCoerce il.link).source.y :: Number)
    , x2 (\(IndexedLink il) -> (unsafeCoerce il.link).target.x :: Number)
    , y2 (\(IndexedLink il) -> (unsafeCoerce il.link).target.y :: Number)
    ]

  -- Start the simulation
  start

  pure unit
