module D3.Viz.FPFTW.LesMisGUPAuto where

-- | Auto-cycling Les Misérables GUP demo based on LesMisTreeExample
-- | Shows enter/update/exit pattern with random node cycling

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.State (get)
import Data.Array as Array
import Data.Foldable (for_)
import Data.Int (toNumber, floor)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number (sqrt, cos, sin, pi)
import Data.Set (Set)
import Data.Set as Set
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, delay, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Random (randomInt)
import Effect.Ref as Ref
import D3.Viz.LesMiserables.Model (LesMisRawModel, LesMisSimNode)
import PSD3.Data.Node (D3Link_Swizzled, D3Link_Unswizzled)
import PSD3.Internal.FFI (keyIsID_, linksForceName_)
import PSD3.Internal.Scales.Scales (d3SchemeCategory10N_)
import PSD3.Internal.Simulation.Config as F
import PSD3.Internal.Simulation.Forces (createForce, createLinkForce)
import PSD3.Internal.Simulation.Types (D3SimulationState_, Force, ForceType(..), RegularForceType(..), allNodes, initialSimulationState)
import PSD3v2.Attribute.Types (class_, cx, cy, fill, height, id_, radius, stroke, strokeWidth, viewBox, width, x1, x2, y1, y2)
import PSD3v2.Behavior.Types (Behavior(..), ScaleExtent(..), defaultDrag, defaultZoom, simulationDrag)
import PSD3v2.Capabilities.Selection (append, joinData, merge, on, remove, renderTree, select, selectAll, setAttrs, setAttrsExit)
import PSD3v2.Capabilities.Simulation (Step(..), addTickFunction, init, reheat, start, update)
import PSD3v2.Interpreter.D3v2 (D3v2Selection_, D3v2SimM, reselectD3v2, runD3v2SimM, execD3v2SimM)
import PSD3v2.Selection.Types (ElementType(..), JoinResult(..), SBoundOwns, SBoundInherits, SEmpty)
import PSD3v2.VizTree.Tree as T
import Partial.Unsafe (unsafePartial, unsafeCrashWith)
import Unsafe.Coerce (unsafeCoerce)
import Utility (getWindowWidthHeight)
import Web.DOM.Element (Element)

-- | Phylotaxis constants
initialRadius :: Number
initialRadius = 10.0

goldenAngle :: Number
goldenAngle = pi * (3.0 - sqrt 5.0)

-- | Apply phylotaxis position to a single node
phylotaxisPosition :: Int -> { x :: Number, y :: Number }
phylotaxisPosition i =
  let
    i' = toNumber i
    r = initialRadius * sqrt (0.5 + i')
    angle = i' * goldenAngle
  in
    { x: r * cos angle, y: r * sin angle }

-- | Randomly select a subset of node IDs
randomSubset :: Array String -> Int -> Int -> Effect (Set String)
randomSubset allIds minSize maxSize = do
  targetSize <- randomInt minSize maxSize
  shuffled <- shuffleArray allIds
  pure $ Set.fromFoldable $ Array.take targetSize shuffled

-- | Fisher-Yates shuffle
shuffleArray :: forall a. Array a -> Effect (Array a)
shuffleArray arr = do
  ref <- Ref.new arr
  let len = Array.length arr
  for_ (Array.range 0 (len - 2)) \i -> do
    j <- randomInt i (len - 1)
    current <- Ref.read ref
    case Array.index current i, Array.index current j of
      Just vi, Just vj -> do
        let swapped = Array.updateAt i vj current >>= Array.updateAt j vi
        case swapped of
          Just s -> Ref.write s ref
          Nothing -> pure unit
      _, _ -> pure unit
  Ref.read ref

-- | Indexed link wrapper for data join
newtype IndexedLink = IndexedLink { index :: Int, link :: D3Link_Swizzled }

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

-- | Keyed node wrapper for data join (uses ID for equality)
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

-- | Draw LesMis with auto-cycling GUP
-- | Returns the simulation state for use in auto-cycling
drawLesMisGUPAuto :: forall row.
  Array LesMisSimNode ->
  Array D3Link_Unswizzled ->
  String ->
  Number ->
  Number ->
  D3v2SimM row LesMisSimNode { simulation :: D3SimulationState_ LesMisSimNode | row }
drawLesMisGUPAuto nodes links containerSelector w h = do
  liftEffect $ Console.log "=== LesMis GUP Auto: Initializing ==="

  -- Select container
  container <- select containerSelector :: _ (D3v2Selection_ SEmpty Element Unit)

  -- Build structural tree (SVG + groups)
  let forceGraphTree :: T.Tree Unit
      forceGraphTree =
        T.named SVG "svg"
          [ width w
          , height h
          , viewBox (show (-w / 2.0) <> " " <> show (-h / 2.0) <> " " <> show w <> " " <> show h)
          , id_ "lesmis-gup-svg"
          , class_ "lesmis-gup"
          ]
          `T.withChild`
            (T.named Group "zoomGroup"
              [ id_ "zoom-group"
              , class_ "zoom-group"
              ]
              `T.withChildren`
                [ T.named Group "linksGroup" [ id_ "links", class_ "links" ]
                , T.named Group "nodesGroup" [ id_ "nodes", class_ "nodes" ]
                ])

  -- Render structure
  selections <- renderTree container forceGraphTree

  -- Extract group selections
  linksGroupSel <- liftEffect $ reselectD3v2 "linksGroup" selections
  nodesGroupSel <- liftEffect $ reselectD3v2 "nodesGroup" selections
  zoomGroupSel <- liftEffect $ reselectD3v2 "zoomGroup" selections
  svgSel <- liftEffect $ reselectD3v2 "svg" selections

  -- Pick initial random subset (1/3 to 2/3 of nodes)
  let allNodeIds = nodes <#> _.id
      minNodes = max 20 (floor $ toNumber (Array.length nodes) / 3.0)
      maxNodes = floor $ toNumber (Array.length nodes) * 2.0 / 3.0

  initialIds <- liftEffect $ randomSubset allNodeIds minNodes maxNodes

  let initialNodes = Array.filter (\n -> Set.member n.id initialIds) nodes
      initialLinks = Array.filter (\l ->
        let link = unsafeCoerce l :: { source :: String, target :: String }
        in Set.member link.source initialIds && Set.member link.target initialIds
      ) links
      nodesWithPositions = Array.mapWithIndex (\i node ->
        node { x = (phylotaxisPosition i).x, y = (phylotaxisPosition i).y }
      ) initialNodes

  liftEffect $ Console.log $ "Initial subset: " <> show (Array.length initialNodes) <> " nodes"

  -- Create forces
  let forcesArray =
        [ createForce "charge" (RegularForce ForceManyBody) allNodes [ F.strengthVal (-30.0) ]
        , createForce "collision" (RegularForce ForceCollide) allNodes [ F.radiusVal 6.0 ]
        , createForce "center" (RegularForce ForceCenter) allNodes [ F.xVal 0.0, F.yVal 0.0, F.strengthVal 0.1 ]
        , createLinkForce allNodes [ F.distanceVal 30.0 ]
        ]
      activeForces = Set.fromFoldable ["charge", "collision", "center", linksForceName_]

  -- Initialize simulation
  { nodes: nodesInSim, links: linksInSim } <- init
    { nodes: nodesWithPositions
    , links: initialLinks
    , forces: forcesArray
    , activeForces: activeForces
    , config: { alpha: 1.0, alphaTarget: 0.0, alphaMin: 0.001, alphaDecay: 0.0228, velocityDecay: 0.4 }
    , keyFn: keyIsID_
    , ticks: Map.empty
    }

  -- Wrap for data joins
  let indexedLinks = Array.mapWithIndex (\i link -> IndexedLink { index: i, link }) linksInSim
      keyedNodes = nodesInSim <#> KeyedNode

  -- Create data trees
  let linksTree :: T.Tree IndexedLink
      linksTree =
        T.joinData "linkElements" "line" indexedLinks $ \(IndexedLink il) ->
          let link = unsafeCoerce il.link
          in T.elem Line
            [ x1 ((\(_ :: IndexedLink) -> link.source.x) :: IndexedLink -> Number)
            , y1 ((\(_ :: IndexedLink) -> link.source.y) :: IndexedLink -> Number)
            , x2 ((\(_ :: IndexedLink) -> link.target.x) :: IndexedLink -> Number)
            , y2 ((\(_ :: IndexedLink) -> link.target.y) :: IndexedLink -> Number)
            , stroke "#999"
            , strokeWidth 1.0
            ]

      nodesTree :: T.Tree KeyedNode
      nodesTree =
        T.joinData "nodeElements" "circle" keyedNodes $ \(KeyedNode d :: KeyedNode) ->
          T.elem Circle
            [ cx d.x
            , cy d.y
            , radius 5.0
            , fill (d3SchemeCategory10N_ (toNumber d.group))
            , stroke "#fff"
            , strokeWidth 1.5
            ]

  -- Render data
  linksSelections <- renderTree linksGroupSel linksTree
  nodesSelections <- renderTree nodesGroupSel nodesTree

  -- Extract bound selections
  let nodesSel :: D3v2Selection_ SBoundOwns Element KeyedNode
      nodesSel = case Map.lookup "nodeElements" nodesSelections of
        Just sel -> sel
        Nothing -> unsafePartial $ unsafeCrashWith "nodeElements not found"

      linksSel :: D3v2Selection_ SBoundOwns Element IndexedLink
      linksSel = case Map.lookup "linkElements" linksSelections of
        Just sel -> sel
        Nothing -> unsafePartial $ unsafeCrashWith "linkElements not found"

  -- Attach behaviors
  _ <- on (Drag defaultDrag) zoomGroupSel
  _ <- on (Zoom $ defaultZoom (ScaleExtent 0.1 10.0) "#zoom-group") svgSel
  _ <- on (Drag $ simulationDrag "lesmis-gup") nodesSel

  -- Add tick functions
  liftEffect $ Console.log "Adding tick functions for initial render"
  addTickFunction "nodes" $ Step nodesSel
    [ cx (\(KeyedNode d :: KeyedNode) -> d.x)
    , cy (\(KeyedNode d :: KeyedNode) -> d.y)
    ]

  addTickFunction "links" $ Step linksSel
    [ x1 (\(IndexedLink il) -> (unsafeCoerce il.link).source.x :: Number)
    , y1 (\(IndexedLink il) -> (unsafeCoerce il.link).source.y :: Number)
    , x2 (\(IndexedLink il) -> (unsafeCoerce il.link).target.x :: Number)
    , y2 (\(IndexedLink il) -> (unsafeCoerce il.link).target.y :: Number)
    ]

  -- Start simulation
  liftEffect $ Console.log "Starting initial simulation"
  start
  liftEffect $ Console.log "Initial simulation started"

  liftEffect $ Console.log "=== LesMis GUP Auto: Initial render complete ==="

  -- Get current simulation state
  simState <- get

  -- Create Ref to hold simulation state across cycles
  simStateRef <- liftEffect $ Ref.new simState

  -- Launch auto-cycling in background (like letters GUP but with Aff delay)
  liftEffect $ launchAff_ $ autoCycleGraph simStateRef nodes links linksGroupSel nodesGroupSel allNodeIds minNodes maxNodes

  pure simState

-- | Auto-cycle through random subsets (similar to letters GUP pattern)
-- | This runs indefinitely, updating the graph every 3 seconds
autoCycleGraph ::
  forall row.
  Ref.Ref { simulation :: D3SimulationState_ LesMisSimNode | row } ->
  Array LesMisSimNode ->
  Array D3Link_Unswizzled ->
  D3v2Selection_ SEmpty Element Unit ->
  D3v2Selection_ SEmpty Element Unit ->
  Array String ->
  Int ->
  Int ->
  Aff Unit
autoCycleGraph simStateRef allNodes allLinks linksGroupSel nodesGroupSel allNodeIds minNodes maxNodes =
  tailRecM (const cycle) unit
  where
    cycle = do
      -- Wait 3 seconds between updates (like letters GUP timing)
      delay (Milliseconds 3000.0)

      -- Pick new random subset
      activeIds <- liftEffect $ randomSubset allNodeIds minNodes maxNodes

      liftEffect $ Console.log $ "GUP Cycle: Showing " <> show (Set.size activeIds) <> " / " <> show (Array.length allNodes) <> " nodes"

      -- Filter nodes and links (like letters GUP filters to new text)
      let activeNodes = Array.filter (\n -> Set.member n.id activeIds) allNodes
          activeLinks = Array.filter (\l ->
            let link = unsafeCoerce l :: { source :: String, target :: String }
            in Set.member link.source activeIds && Set.member link.target activeIds
          ) allLinks
          nodesWithPositions = Array.mapWithIndex (\i node ->
            node { x = (phylotaxisPosition i).x, y = (phylotaxisPosition i).y }
          ) activeNodes

      -- Update the graph (like updateText in letters GUP)
      -- Read current simulation state from Ref
      currentState <- liftEffect $ Ref.read simStateRef

      -- Run update and get new state
      newState <- liftAff $ execD3v2SimM currentState do
        updateGraph nodesWithPositions activeLinks linksGroupSel nodesGroupSel

      -- Store updated state back in Ref
      liftEffect $ Ref.write newState simStateRef

      pure $ Loop unit

-- | Update graph with new subset (similar to updateText in letters GUP)
-- |
-- | This is the core GUP function that handles:
-- | 1. Update simulation data
-- | 2. Re-render trees with T.joinData (handles enter/update/exit automatically)
-- | 3. Reheat simulation to animate to new positions
updateGraph :: forall row.
  Array LesMisSimNode ->
  Array D3Link_Unswizzled ->
  D3v2Selection_ SEmpty Element Unit ->
  D3v2Selection_ SEmpty Element Unit ->
  D3v2SimM row LesMisSimNode Unit
updateGraph nodes links linksGroupSel nodesGroupSel = do
  -- Update simulation with new filtered data
  { nodes: nodesInSim, links: linksInSim } <- update
    { nodes: Just nodes
    , links: Just links
    , nodeFilter: Nothing
    , linkFilter: Nothing
    , activeForces: Nothing
    , config: Nothing
    , keyFn: keyIsID_
    }

  -- Reheat simulation to animate to new positions (like transitions in letters GUP)
  reheat 0.3

  -- Wrap for data joins
  let indexedLinks = Array.mapWithIndex (\i link -> IndexedLink { index: i, link }) linksInSim
      keyedNodes = nodesInSim <#> KeyedNode

  -- Re-render links using Tree API (links don't need enter/update/exit colors)
  -- BUT: Tree API crashes if array is empty, so only render if we have links
  when (Array.length indexedLinks > 0) do
    let linksTree :: T.Tree IndexedLink
        linksTree =
          T.joinData "linkElements" "line" indexedLinks $ \(IndexedLink il) ->
            let link = unsafeCoerce il.link
            in T.elem Line
              [ x1 ((\(_ :: IndexedLink) -> link.source.x) :: IndexedLink -> Number)
              , y1 ((\(_ :: IndexedLink) -> link.source.y) :: IndexedLink -> Number)
              , x2 ((\(_ :: IndexedLink) -> link.target.x) :: IndexedLink -> Number)
              , y2 ((\(_ :: IndexedLink) -> link.target.y) :: IndexedLink -> Number)
              , stroke "#999"
              , strokeWidth 1.0
              ]

    _ <- renderTree linksGroupSel linksTree
    pure unit

  -- Manual GUP for nodes to get enter/update/exit colors (like letters GUP)
  liftEffect $ Console.log $ "GUP: Updating " <> show (Array.length keyedNodes) <> " nodes"

  JoinResult { enter, update: updateSel, exit } <- joinData keyedNodes "circle" nodesGroupSel

  -- ENTER: New nodes start green
  enterNodes <- append Circle
    [ class_ "enter"
    , fill "green"  -- Enter nodes are green
    , cx (\(KeyedNode d :: KeyedNode) -> d.x)
    , cy (\(KeyedNode d :: KeyedNode) -> d.y)
    , radius 5.0
    , stroke "#fff"
    , strokeWidth 1.5
    ]
    enter

  liftEffect $ Console.log $ "GUP: Created enter nodes (green)"

  -- UPDATE: Existing nodes keep group color
  _ <- setAttrs
    [ class_ "update"
    , fill (\(KeyedNode d :: KeyedNode) -> d3SchemeCategory10N_ (toNumber d.group))
    , cx (\(KeyedNode d :: KeyedNode) -> d.x)
    , cy (\(KeyedNode d :: KeyedNode) -> d.y)
    , radius 5.0
    , stroke "#fff"
    , strokeWidth 1.5
    ]
    updateSel

  liftEffect $ Console.log $ "GUP: Updated existing nodes (group colors)"

  -- EXIT: Removing nodes turn brown and then remove them
  _ <- setAttrsExit
    [ class_ "exit"
    , fill "brown"  -- Exit nodes are brown
    ]
    exit

  liftEffect $ Console.log $ "GUP: Styled exit nodes (brown)"

  -- IMPORTANT: Remove exit nodes! (letters GUP does this via withTransitionExit + automatic removal)
  _ <- remove exit

  liftEffect $ Console.log $ "GUP: Removed exit nodes from DOM"

  -- CRITICAL: Re-register tick functions with updated selections
  -- The original tick functions captured the initial DOM elements, so they don't
  -- know about the new nodes we just created via GUP. We need to reselect ALL
  -- current nodes and re-register the tick functions.

  liftEffect $ Console.log "GUP: Re-registering tick functions with updated DOM"

  -- Merge enter + update to get all active nodes
  allActiveNodes <- merge enterNodes updateSel

  -- Re-register tick function for nodes
  addTickFunction "nodes" $ Step allActiveNodes
    [ cx (\(KeyedNode d :: KeyedNode) -> d.x)
    , cy (\(KeyedNode d :: KeyedNode) -> d.y)
    ]

  liftEffect $ Console.log "GUP: Tick functions re-registered"

  pure unit
