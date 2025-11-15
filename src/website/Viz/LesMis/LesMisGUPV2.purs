module D3.Viz.LesMisGUPV2 where

-- | Force-directed graph with General Update Pattern (GUP) using PSD3v2
-- | Demonstrates SimulationM2 with dynamic layout transitions
-- |
-- | Features:
-- | - Grid layout transition
-- | - Phylotaxis (sunflower spiral) layout transition
-- | - Zoom/pan/drag interactions
-- | - Clean PSD3v2 API

import Prelude

import Control.Monad.State (get)
import D3.Viz.LesMiserables.Model (LesMisRawModel, LesMisSimNode)
import Data.Array as Array
import Data.Int (toNumber, floor)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Data.Number (sqrt, cos, sin, pi, ceil, floor, (%)) as Number
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import PSD3.Data.Node (D3Link_Swizzled)
import PSD3.Internal.Simulation.Types (Force)
import PSD3.Internal.Scales.Scales (d3SchemeCategory10N_)
import PSD3.Internal.FFI (keyIsID_)
import PSD3v2.Attribute.Types (cx, cy, fill, radius, stroke, strokeWidth, x1, x2, y1, y2, id_, class_, width, height, viewBox)
import PSD3v2.Behavior.Types (Behavior(..), defaultDrag, defaultZoom, simulationDrag, ScaleExtent(..))
import PSD3v2.Capabilities.Selection (select, on, renderTree)
import PSD3v2.Capabilities.Simulation (init, addTickFunction, start, stop, Step(..), update, reheat)
import PSD3v2.Interpreter.D3v2 (D3v2SimM, D3v2Selection_, reselectD3v2)
import PSD3v2.Selection.Types (ElementType(..), SBound)
import Partial.Unsafe (unsafePartial, unsafeCrashWith)
import Web.DOM.Element (Element)
import PSD3v2.VizTree.Tree as T
import Unsafe.Coerce (unsafeCoerce)
import Utility (getWindowWidthHeight)
import Effect (Effect)

-- | FFI functions for mutating node positions
foreign import setNodesGridPositions_ :: forall a. Array a -> Number -> Effect (Array a)
foreign import setNodesPhyllotaxisPositions_ :: forall a. Array a -> Effect (Array a)
foreign import clearNodesFxFy_ :: forall a. Array a -> Effect (Array a)

-- | FFI function to transition nodes/links to fx/fy positions with D3 transitions
foreign import transitionToFxFyPositions_ :: forall a. String -> String -> String -> Array a -> Effect Unit -> Effect Unit

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
initialAngle = Number.pi * (3.0 - Number.sqrt 5.0)  -- Golden angle

-- | Apply phylotaxis positions to nodes for initial layout (does NOT pin with fx/fy)
setPhyllotaxisInitialPositions :: forall r. Array (Record (x :: Number, y :: Number | r)) -> Array (Record (x :: Number, y :: Number | r))
setPhyllotaxisInitialPositions nodes = Array.mapWithIndex setPosition nodes
  where
    setPosition :: Int -> Record (x :: Number, y :: Number | r) -> Record (x :: Number, y :: Number | r)
    setPosition index node =
      let
        i = toNumber index
        rad = initialRadius * Number.sqrt (0.5 + i)
        angle = i * initialAngle
        newX = rad * Number.cos angle
        newY = rad * Number.sin angle
      in
        node { x = newX, y = newY }

-- | Apply phylotaxis positions to nodes (sets fx/fy to pin them)
setPhyllotaxisPositions :: forall r. Array (Record (x :: Number, y :: Number, fx :: Nullable Number, fy :: Nullable Number | r)) -> Array (Record (x :: Number, y :: Number, fx :: Nullable Number, fy :: Nullable Number | r))
setPhyllotaxisPositions nodes = Array.mapWithIndex setPosition nodes
  where
    setPosition :: Int -> Record (x :: Number, y :: Number, fx :: Nullable Number, fy :: Nullable Number | r) -> Record (x :: Number, y :: Number, fx :: Nullable Number, fy :: Nullable Number | r)
    setPosition index node =
      let
        i = toNumber index
        rad = initialRadius * Number.sqrt (0.5 + i)
        angle = i * initialAngle
        newX = rad * Number.cos angle
        newY = rad * Number.sin angle
      in
        node { x = newX, y = newY, fx = toNullable (Just newX), fy = toNullable (Just newY) }

-- | Calculate grid position from index
numberToGridPoint :: Int -> Int -> { x :: Number, y :: Number }
numberToGridPoint columns i = do
  let
    c = toNumber columns
    d = toNumber i
    x = (d Number.% c)
    y = Number.floor (d / c)
  { x, y }

-- | Apply grid positions to nodes (sets fx/fy to pin them)
setGridPositions :: forall r. Number -> Array (Record (x :: Number, y :: Number, fx :: Nullable Number, fy :: Nullable Number | r)) -> Array (Record (x :: Number, y :: Number, fx :: Nullable Number, fy :: Nullable Number | r))
setGridPositions gridSpacing nodes = Array.mapWithIndex setPosition nodes
  where
    nodeCount = Array.length nodes
    columns = floor $ Number.ceil $ Number.sqrt $ toNumber nodeCount
    offset = -(toNumber columns * gridSpacing) / 2.0

    setPosition i node =
      let gridPt = numberToGridPoint columns i
          newX = gridPt.x * gridSpacing + offset
          newY = gridPt.y * gridSpacing + offset
      in node { x = newX, y = newY, fx = toNullable (Just newX), fy = toNullable (Just newY) }

-- | Unpin all nodes (clear fx/fy to allow forces to control them)
unpinAllNodes :: forall r. Array (Record (fx :: Nullable Number, fy :: Nullable Number | r)) -> Array (Record (fx :: Nullable Number, fy :: Nullable Number | r))
unpinAllNodes nodes = nodes <#> \node -> node { fx = toNullable Nothing, fy = toNullable Nothing }

-- | Initial draw of LesMis with GUP support
-- |
-- | Returns the group selections for nodes and links so they can be updated later
drawLesMisGUPV2 :: forall row.
  Array (Force LesMisSimNode) ->
  Set.Set String ->
  LesMisRawModel ->
  String ->
  D3v2SimM row LesMisSimNode { nodesGroup :: ElementType, linksGroup :: ElementType }
drawLesMisGUPV2 forcesArray activeForces model containerSelector = do
  -- Get window dimensions
  (Tuple w h) <- liftEffect getWindowWidthHeight

  -- Select container
  container <- select containerSelector

  -- Declarative tree structure for force graph (using Tree API)
  let forceGraphTree :: T.Tree Unit
      forceGraphTree =
        T.named SVG "svg"
          [ width w
          , height h
          , viewBox (show ((-w) / 2.0) <> " " <> show ((-h) / 2.0) <> " " <> show w <> " " <> show h)
          , id_ "lesmis-gup-v2-svg"
          , class_ "lesmis-gup-v2"
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

  -- Extract selections for behaviors
  svg <- liftEffect $ reselectD3v2 "svg" selections
  zoomGroup <- liftEffect $ reselectD3v2 "zoomGroup" selections
  linksGroup <- liftEffect $ reselectD3v2 "linksGroup" selections
  nodesGroup <- liftEffect $ reselectD3v2 "nodesGroup" selections

  -- Attach drag behavior to zoom group (allows panning)
  _ <- on (Drag defaultDrag) zoomGroup

  -- Attach zoom behavior to SVG
  _ <- on (Zoom $ defaultZoom (ScaleExtent 0.5 4.0) "#zoom-group") svg

  -- Apply phylotaxis initial positions (without pinning, so simulation can run)
  let nodesWithPositions = setPhyllotaxisInitialPositions model.nodes

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

  -- Wrap links with indices for data join
  let indexedLinks = Array.mapWithIndex (\i link -> IndexedLink { index: i, link }) linksInSim

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
            , strokeWidth ((\(_ :: IndexedLink) -> Number.sqrt link.value) :: IndexedLink -> Number)
            , stroke ((\(_ :: IndexedLink) -> d3SchemeCategory10N_ (toNumber link.target.group)) :: IndexedLink -> String)
            ]

  linksSelections <- renderTree linksGroup linksTree

  -- Render nodes tree into nodesGroup using Tree API
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

  nodesSelections <- renderTree nodesGroup nodesTree

  -- Extract bound selections for behaviors and tick functions
  -- Use case pattern matching (NOT fromMaybe) when default might throw
  let nodeCircles :: D3v2Selection_ SBound Element LesMisSimNode
      nodeCircles = case Map.lookup "nodeElements" nodesSelections of
        Just sel -> sel
        Nothing -> unsafePartial $ unsafeCrashWith "nodeElements not found"

  let linkLines :: D3v2Selection_ SBound Element IndexedLink
      linkLines = case Map.lookup "linkElements" linksSelections of
        Just sel -> sel
        Nothing -> unsafePartial $ unsafeCrashWith "linkElements not found"

  -- Attach simulation drag to node circles
  _ <- on (Drag $ simulationDrag "lesmis-gup") nodeCircles

  -- Add tick functions
  addTickFunction "nodes" $ Step nodeCircles
    [ cx (\(d :: LesMisSimNode) -> d.x)
    , cy (\(d :: LesMisSimNode) -> d.y)
    ]

  addTickFunction "links" $ Step linkLines
    [ x1 (\(IndexedLink il) -> (unsafeCoerce il.link).source.x :: Number)
    , y1 (\(IndexedLink il) -> (unsafeCoerce il.link).source.y :: Number)
    , x2 (\(IndexedLink il) -> (unsafeCoerce il.link).target.x :: Number)
    , y2 (\(IndexedLink il) -> (unsafeCoerce il.link).target.y :: Number)
    ]

  -- Start simulation
  start

  -- Return group element types for updates
  pure { nodesGroup: Group, linksGroup: Group }

-- | Move nodes to grid layout
-- |
-- | Stops simulation, mutates nodes to set fx/fy, then uses D3 transitions
-- | to smoothly animate nodes to their grid positions.
moveToGrid :: forall row.
  Number ->
  D3v2SimM row LesMisSimNode Unit
moveToGrid gridSpacing = do
  -- Stop the simulation (we'll use transitions instead)
  stop

  -- Get current nodes from simulation
  { nodes: currentNodes } <- update
    { nodes: Nothing
    , links: Nothing
    , nodeFilter: Nothing
    , linkFilter: Nothing
    , activeForces: Nothing
    , config: Nothing
    , keyFn: keyIsID_
    }

  -- Mutate nodes in-place to set fx/fy to grid positions
  _ <- liftEffect $ setNodesGridPositions_ currentNodes gridSpacing

  -- Transition nodes and links to fx/fy positions (1.5s animation)
  liftEffect $ transitionToFxFyPositions_
    "#lesmis-gup-v2-svg"  -- SVG selector
    ".nodes circle"        -- Node selector
    ".links line"          -- Link selector
    currentNodes
    (pure unit)            -- Completion callback (do nothing)

-- | Move nodes to phylotaxis layout
-- |
-- | Stops simulation, mutates nodes to set fx/fy, then uses D3 transitions
-- | to smoothly animate nodes to their phylotaxis (sunflower spiral) positions.
moveToPhylotaxis :: forall row.
  D3v2SimM row LesMisSimNode Unit
moveToPhylotaxis = do
  -- Stop the simulation (we'll use transitions instead)
  stop

  -- Get current nodes from simulation
  { nodes: currentNodes } <- update
    { nodes: Nothing
    , links: Nothing
    , nodeFilter: Nothing
    , linkFilter: Nothing
    , activeForces: Nothing
    , config: Nothing
    , keyFn: keyIsID_
    }

  -- Mutate nodes in-place to set fx/fy to phylotaxis positions
  _ <- liftEffect $ setNodesPhyllotaxisPositions_ currentNodes

  -- Transition nodes and links to fx/fy positions (1.5s animation)
  liftEffect $ transitionToFxFyPositions_
    "#lesmis-gup-v2-svg"  -- SVG selector
    ".nodes circle"        -- Node selector
    ".links line"          -- Link selector
    currentNodes
    (pure unit)            -- Completion callback (do nothing)

-- | Unpin nodes and let forces take over
unpinNodes :: forall row.
  D3v2SimM row LesMisSimNode Unit
unpinNodes = do
  -- Get current nodes
  { nodes: currentNodes } <- update
    { nodes: Nothing
    , links: Nothing
    , nodeFilter: Nothing
    , linkFilter: Nothing
    , activeForces: Nothing
    , config: Nothing
    , keyFn: keyIsID_
    }

  -- Mutate nodes in-place to clear fx/fy
  _ <- liftEffect $ clearNodesFxFy_ currentNodes

  -- Reheat and restart
  reheat 0.5
  start
