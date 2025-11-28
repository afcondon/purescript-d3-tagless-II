module D3.Viz.LesMis.LesMisGUPClean where

-- | Clean LesMis GUP using existing primitives (no SceneNestedJoin, no wrappers!)
-- |
-- | This uses renderData + joinDataWithKey directly on the raw arrays.

import Prelude

import D3.Viz.LesMiserables.Model (LesMisRawModel, LesMisSimNode, LesMisNodeRow, LesMisLinkRow)
import Data.Array as Array
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number (sqrt, cos, sin, pi) as Number
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import PSD3.Data.Node (SwizzledLink)
import PSD3.Internal.FFI (keyIsID_)
import PSD3.Internal.Scales.Scales (d3SchemeCategory10N_)
import PSD3.Internal.Simulation.Types (Force)
import PSD3v2.Attribute.Types (cx, cy, fill, radius, stroke, strokeWidth, x1, x2, y1, y2, id_, class_, width, height, viewBox)
import PSD3v2.Behavior.Types (Behavior(..), defaultDrag, defaultZoom, simulationDrag, ScaleExtent(..))
import PSD3v2.Capabilities.Selection (select, on, renderTree, joinDataWithKey, append, setAttrs, remove, merge)
import PSD3v2.Capabilities.Simulation (init, addTickFunction, start, Step(..), update)
import PSD3v2.Interpreter.D3v2 (D3v2SimM, reselectD3v2)
import PSD3v2.Selection.Types (ElementType(..), SBoundOwns, SBoundInherits, JoinResult(..))
import PSD3v2.VizTree.Tree as T
import Partial.Unsafe (unsafePartial, unsafeCrashWith)
import Unsafe.Coerce (unsafeCoerce)
import Utility (getWindowWidthHeight)

-- | Phylotaxis layout (sunflower spiral)
phylotaxisPosition :: Int -> { x :: Number, y :: Number }
phylotaxisPosition i =
  let i' = toNumber i
      r = 10.0 * Number.sqrt (0.5 + i')
      angle = i' * Number.pi * (3.0 - Number.sqrt 5.0)  -- Golden angle
  in { x: r * Number.cos angle, y: r * Number.sin angle }

setPhyllotaxisPositions :: forall r. Array (Record (x :: Number, y :: Number | r)) -> Array (Record (x :: Number, y :: Number | r))
setPhyllotaxisPositions nodes = Array.mapWithIndex (\i n -> n { x = (phylotaxisPosition i).x, y = (phylotaxisPosition i).y }) nodes

-- | Swizzled link type alias for LesMis
type LesMisSwizzledLink = SwizzledLink LesMisNodeRow LesMisLinkRow

-- | Indexed link wrapper (needed because SwizzledLink doesn't have Ord)
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

-- | Draw initial force graph - NO SceneNestedJoin, NO SceneData wrapper!
drawLesMisGUPClean ::
  Array (Force LesMisSimNode) ->
  Set.Set String ->
  LesMisRawModel ->
  String ->
  D3v2SimM () LesMisSimNode Unit
drawLesMisGUPClean forcesArray activeForces model containerSelector = do
  -- Get window dimensions
  (Tuple w h) <- liftEffect getWindowWidthHeight

  -- Select container and build SVG structure (using Tree API for static structure)
  container <- select containerSelector
  let svgTree :: T.Tree Unit
      svgTree = T.named SVG "svg"
        [ width w, height h
        , viewBox (show (-w/2.0) <> " " <> show (-h/2.0) <> " " <> show w <> " " <> show h)
        , id_ "lesmis-gup-clean-svg", class_ "lesmis-gup-clean"
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

  -- Render links using joinDataWithKey (links don't have Ord, so we need key function)
  let indexedLinks = Array.mapWithIndex (\i link -> IndexedLink { index: i, link }) linksInSim
  JoinResult { enter: linksEnter, update: linksUpdate, exit: linksExit } <-
    joinDataWithKey indexedLinks (\(IndexedLink il) -> show il.index) "line" linksGroup

  linksEnterBound <- append Line [] linksEnter
  _ <- setAttrs
    [ strokeWidth 1.0, stroke "#999"
    , x1 (\(IndexedLink il) -> (unsafeCoerce il.link).source.x :: Number)
    , y1 (\(IndexedLink il) -> (unsafeCoerce il.link).source.y :: Number)
    , x2 (\(IndexedLink il) -> (unsafeCoerce il.link).target.x :: Number)
    , y2 (\(IndexedLink il) -> (unsafeCoerce il.link).target.y :: Number)
    ] linksEnterBound
  _ <- remove linksExit
  linkLines <- merge linksEnterBound linksUpdate

  -- Render nodes using joinDataWithKey (use node ID as key)
  JoinResult { enter: nodesEnter, update: nodesUpdate, exit: nodesExit } <-
    joinDataWithKey nodesInSim (_.id) "circle" nodesGroup

  nodesEnterBound <- append Circle [] nodesEnter
  _ <- setAttrs
    [ cx ((_.x) :: LesMisSimNode -> Number)
    , cy ((_.y) :: LesMisSimNode -> Number)
    , radius (5.0 :: Number)
    , fill ((\n -> d3SchemeCategory10N_ (toNumber n.group)) :: LesMisSimNode -> String)
    , stroke ("#fff" :: String)
    , strokeWidth (2.0 :: Number)
    ] nodesEnterBound
  _ <- remove nodesExit
  nodeCircles <- merge nodesEnterBound nodesUpdate

  -- Attach simulation drag and tick functions
  _ <- on (Drag $ simulationDrag "lesmis-gup-clean") nodeCircles

  addTickFunction "nodes" $ Step nodeCircles
    [ cx ((_.x) :: LesMisSimNode -> Number)
    , cy ((_.y) :: LesMisSimNode -> Number)
    ]
  addTickFunction "links" $ Step linkLines
    [ x1 (\(IndexedLink il) -> (unsafeCoerce il.link).source.x :: Number)
    , y1 (\(IndexedLink il) -> (unsafeCoerce il.link).source.y :: Number)
    , x2 (\(IndexedLink il) -> (unsafeCoerce il.link).target.x :: Number)
    , y2 (\(IndexedLink il) -> (unsafeCoerce il.link).target.y :: Number)
    ]

  start

-- | Update nodes - just rejoin the data!
updateNodes :: Array LesMisSimNode -> D3v2SimM () LesMisSimNode Unit
updateNodes newNodes = do
  -- Update simulation with new node set
  { nodes: updatedNodes } <- update
    { nodes: Just newNodes, links: Nothing, nodeFilter: Nothing, linkFilter: Nothing
    , activeForces: Nothing, config: Nothing, keyFn: keyIsID_
    }

  -- Rejoin data (GUP happens automatically!)
  nodesGroup <- select "#nodes"
  JoinResult { enter, update: updateSel, exit } <-
    joinDataWithKey updatedNodes (_.id) "circle" nodesGroup

  enterBound <- append Circle [] enter
  _ <- setAttrs
    [ cx ((_.x) :: LesMisSimNode -> Number)
    , cy ((_.y) :: LesMisSimNode -> Number)
    , radius (5.0 :: Number)
    , fill ((\n -> d3SchemeCategory10N_ (toNumber n.group)) :: LesMisSimNode -> String)
    , stroke ("#fff" :: String)
    , strokeWidth (2.0 :: Number)
    ] enterBound
  _ <- remove exit
  nodeCircles <- merge enterBound updateSel

  -- Update tick function with new selection
  addTickFunction "nodes" $ Step nodeCircles
    [ cx (_.x :: LesMisSimNode -> Number)
    , cy (_.y :: LesMisSimNode -> Number)
    ]

  pure unit
