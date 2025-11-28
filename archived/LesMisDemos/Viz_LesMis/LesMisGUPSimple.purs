module D3.Viz.LesMis.LesMisGUPSimple where

-- | Minimal LesMis GUP demo using declarative SceneNestedJoin
-- |
-- | This demonstrates the power of the declarative Tree API:
-- | - Define the scene structure once with enter/update/exit behaviors
-- | - Update by just calling renderTree with new data
-- | - SceneNestedJoin handles all the GUP transitions automatically!

import Prelude

import D3.Viz.LesMiserables.Model (LesMisRawModel, LesMisSimNode, LesMisNodeRow, LesMisLinkRow)
import Data.Array as Array
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number (sqrt, cos, sin, pi) as Number
import Data.Set as Set
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import PSD3.Data.Node (SwizzledLink)
import PSD3.Internal.FFI (keyIsID_)
import PSD3.Internal.Scales.Scales (d3SchemeCategory10N_)
import PSD3.Internal.Simulation.Types (Force)
import PSD3v2.Attribute.Types (cx, cy, fill, radius, stroke, strokeWidth, x1, x2, y1, y2, id_, class_, width, height, viewBox)
import PSD3v2.Behavior.Types (Behavior(..), defaultDrag, defaultZoom, simulationDrag, ScaleExtent(..))
import PSD3v2.Capabilities.Selection (select, on, renderTree)
import PSD3v2.Capabilities.Simulation (init, addTickFunction, start, Step(..), update)
import PSD3v2.Interpreter.D3v2 (D3v2SimM, reselectD3v2)
import PSD3v2.Selection.Types (ElementType(..), SBoundOwns, SBoundInherits)
import PSD3v2.Selection.Types as ST
import PSD3v2.Transition.Types (Easing(..))
import PSD3v2.VizTree.Tree as T
import Partial.Unsafe (unsafePartial, unsafeCrashWith)
import Unsafe.Coerce (unsafeCoerce)
import Utility (getWindowWidthHeight)

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

-- | Scene data - just contains nodes, no functions!
-- | Color is determined by datum properties and GUP phase, not scene config
newtype SceneData = SceneData { nodes :: Array LesMisSimNode }

-- Dummy instances - SceneData is never actually compared, but renderTree requires Ord
instance Eq SceneData where
  eq _ _ = true

instance Ord SceneData where
  compare _ _ = EQ

-- | Phylotaxis layout (sunflower spiral)
phylotaxisPosition :: Int -> { x :: Number, y :: Number }
phylotaxisPosition i =
  let i' = toNumber i
      r = 10.0 * Number.sqrt (0.5 + i')
      angle = i' * Number.pi * (3.0 - Number.sqrt 5.0)  -- Golden angle
  in { x: r * Number.cos angle, y: r * Number.sin angle }

setPhyllotaxisPositions :: forall r. Array (Record (x :: Number, y :: Number | r)) -> Array (Record (x :: Number, y :: Number | r))
setPhyllotaxisPositions nodes = Array.mapWithIndex (\i n -> n { x = (phylotaxisPosition i).x, y = (phylotaxisPosition i).y }) nodes

-- | The key function: declarative tree with SceneNestedJoin for GUP
-- |
-- | This defines ONCE how nodes should appear, update, and exit.
-- | Color is based on datum properties (group field), not scene config.
-- | To update the visualization, just call renderTree with new SceneData!
createNodesTree :: SceneData -> T.Tree SceneData
createNodesTree (SceneData scene) =
  T.sceneNestedJoin "nodeElements" "circle"
    [SceneData scene]                       -- Outer data: SceneData
    (\(SceneData s) -> map KeyedNode s.nodes)  -- Decompose: extract and wrap nodes
    (\(KeyedNode node) -> T.elem ST.Circle  -- Template for each node (unwrap)
      [ cx node.x
      , cy node.y
      , radius 5.0
      , fill (\(KeyedNode n) -> d3SchemeCategory10N_ (toNumber n.group))
      , stroke "#fff"
      , strokeWidth 2.0
      ])
    { enterBehavior: Just             -- New nodes enter from center
        { initialAttrs:
            [ cx 0.0
            , cy 0.0
            , radius 0.0
            ]
        , transition: Just
            { duration: Milliseconds 600.0
            , delay: Nothing
            , easing: Just CubicInOut
            }
        }
    , updateBehavior: Nothing  -- No update behavior - positions updated by tick function
    , exitBehavior: Just              -- Exiting nodes shrink away
        { attrs: [ radius 0.0 ]
        , transition: Just
            { duration: Milliseconds 500.0
            , delay: Nothing
            , easing: Just CubicIn
            }
        }
    }

-- | Swizzled link type alias for LesMis
type LesMisSwizzledLink = SwizzledLink LesMisNodeRow LesMisLinkRow

-- | Indexed link wrapper for data join
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
    in if sourceComp == EQ then compare linkA.target.id linkB.target.id else sourceComp

-- | Draw initial force graph with declarative GUP
-- |
-- | Sets up the visualization structure and returns the simulation state.
-- | To update: just call updateNodes with new node data!
drawLesMisGUPSimple ::
  Array (Force LesMisSimNode) ->
  Set.Set String ->
  LesMisRawModel ->
  String ->
  D3v2SimM () LesMisSimNode Unit
drawLesMisGUPSimple forcesArray activeForces model containerSelector = do
  -- Get window dimensions
  (Tuple w h) <- liftEffect getWindowWidthHeight

  -- Select container and build SVG structure
  container <- select containerSelector
  let svgTree :: T.Tree Unit
      svgTree = T.named SVG "svg"
        [ width w, height h
        , viewBox (show (-w/2.0) <> " " <> show (-h/2.0) <> " " <> show w <> " " <> show h)
        , id_ "lesmis-gup-simple-svg", class_ "lesmis-gup-simple"
        ]
        `T.withChild`
          (T.named Group "zoomGroup" [ id_ "zoom-group", class_ "zoom-group" ]
            `T.withChildren`
              [ T.named Group "linksGroup" [ id_ "links", class_ "links" ]
              , T.named Group "nodesGroup" [ id_ "nodes", class_ "nodes" ]
              ])

  selections <- renderTree container svgTree

  -- Extract groups and attach behaviors
  svg <- liftEffect $ reselectD3v2 "svg" selections
  zoomGroup <- liftEffect $ reselectD3v2 "zoomGroup" selections
  linksGroup <- liftEffect $ reselectD3v2 "linksGroup" selections
  nodesGroup <- liftEffect $ reselectD3v2 "nodesGroup" selections

  _ <- on (Drag defaultDrag) zoomGroup
  _ <- on (Zoom $ defaultZoom (ScaleExtent 0.5 4.0) "#zoom-group") svg

  -- Initialize simulation with phylotaxis positions
  let nodesWithPositions = setPhyllotaxisPositions model.nodes
  { nodes: nodesInSim, links: linksInSim } <- init
    { nodes: nodesWithPositions, links: model.links
    , forces: forcesArray, activeForces: activeForces
    , config: { alpha: 1.0, alphaTarget: 0.0, alphaMin: 0.001, alphaDecay: 0.0228, velocityDecay: 0.4 }
    , keyFn: keyIsID_, ticks: Map.empty
    }

  -- Render links (simple join)
  let indexedLinks = Array.mapWithIndex (\i link -> IndexedLink { index: i, link }) linksInSim
  let linksTree :: T.Tree IndexedLink
      linksTree = T.joinData "linkElements" "line" indexedLinks $ \(IndexedLink il) ->
        let link = unsafeCoerce il.link
        in T.elem Line
          [ x1 ((\(_ :: IndexedLink) -> link.source.x) :: IndexedLink -> Number)
          , y1 ((\(_ :: IndexedLink) -> link.source.y) :: IndexedLink -> Number)
          , x2 ((\(_ :: IndexedLink) -> link.target.x) :: IndexedLink -> Number)
          , y2 ((\(_ :: IndexedLink) -> link.target.y) :: IndexedLink -> Number)
          , strokeWidth 1.0, stroke "#999"
          ]
  linksSelections <- renderTree linksGroup linksTree
  let linkLines = case Map.lookup "linkElements" linksSelections of
        Just sel -> sel
        Nothing -> unsafePartial $ unsafeCrashWith "linkElements not found"

  -- Render nodes using SceneNestedJoin (all nodes colored by group)
  let scene = SceneData { nodes: nodesInSim }
  nodesSelections <- renderTree nodesGroup (createNodesTree scene)
  let nodeCircles = case Map.lookup "nodeElements" nodesSelections of
        Just sel -> sel
        Nothing -> unsafePartial $ unsafeCrashWith "nodeElements not found"

  -- SceneNestedJoin returns Selection SceneData, but DOM elements are bound to KeyedNode
  -- This is a library limitation - the type signature doesn't capture the inner datum type
  -- We use unsafeCoerce because we know the runtime datum type is KeyedNode
  let nodeCirclesKeyed = unsafeCoerce nodeCircles :: _ SBoundOwns _ KeyedNode

  -- Attach simulation drag and tick functions
  _ <- on (Drag $ simulationDrag "lesmis-gup-simple") nodeCirclesKeyed

  addTickFunction "nodes" $ Step nodeCirclesKeyed
    [ cx (\(KeyedNode d) -> d.x)
    , cy (\(KeyedNode d) -> d.y)
    ]
  addTickFunction "links" $ Step linkLines
    [ x1 (\(IndexedLink il) -> (unsafeCoerce il.link).source.x :: Number)
    , y1 (\(IndexedLink il) -> (unsafeCoerce il.link).source.y :: Number)
    , x2 (\(IndexedLink il) -> (unsafeCoerce il.link).target.x :: Number)
    , y2 (\(IndexedLink il) -> (unsafeCoerce il.link).target.y :: Number)
    ]

  start

-- | Update nodes with new data - this is where the magic happens!
-- |
-- | Just call renderTree with new SceneData, and SceneNestedJoin automatically:
-- | - Adds entering nodes with transitions (colored by group)
-- | - Updates existing nodes with transitions (colored by group)
-- | - Removes exiting nodes with transitions
updateNodes :: Array LesMisSimNode -> D3v2SimM () LesMisSimNode Unit
updateNodes newNodes = do
  -- Update simulation with new node set
  { nodes: updatedNodes } <- update
    { nodes: Just newNodes, links: Nothing, nodeFilter: Nothing, linkFilter: Nothing
    , activeForces: Nothing, config: Nothing, keyFn: keyIsID_
    }

  -- Render new scene - SceneNestedJoin does all the GUP work!
  nodesGroup <- select "#nodes"
  let scene = SceneData { nodes: updatedNodes }
  _ <- renderTree nodesGroup (createNodesTree scene)
  pure unit
