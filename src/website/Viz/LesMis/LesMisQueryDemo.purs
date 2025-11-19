module D3.Viz.LesMis.QueryDemo where

-- | Demo of PSD3v2 Selection Query Language
-- |
-- | This demonstrates:
-- | - Using query language to dynamically select nodes
-- | - CSS class-based filtering
-- | - Data-based filtering
-- | - Dynamic attribute updates based on queries
-- |
-- | Features:
-- | - Force-directed Les Mis network
-- | - Buttons to toggle large size for each group
-- | - Uses queryAll + filterByData to select nodes by group

import Prelude

import D3.Viz.LesMis.QueryDemoFFI (createGroupButtons)
import D3.Viz.LesMiserables.Model (LesMisRawModel, LesMisSimNode)
import Data.Array as Array
import Data.Int (toNumber)
import Data.Map as Map
import Data.Number (sqrt, cos, sin, pi)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import PSD3.Data.Node (D3Link_Swizzled)
import PSD3.Internal.FFI (keyIsID_)
import PSD3.Internal.Scales.Scales (d3SchemeCategory10N_)
import PSD3.Internal.Simulation.Types (Force)
import PSD3v2.Attribute.Types (cx, cy, fill, radius, stroke, strokeWidth, x1, x2, y1, y2, id_, class_, width, height, viewBox)
import PSD3v2.Behavior.Types (Behavior(..), defaultDrag, defaultZoom, simulationDrag, ScaleExtent(..))
import PSD3v2.Capabilities.Selection (select, appendChild, joinData, append, on, renderTree)
import PSD3v2.Selection.Operations as Ops
import PSD3v2.Capabilities.Simulation (init, addTickFunction, start, Step(..))
import PSD3v2.Interpreter.D3v2 (D3v2SimM, reselectD3v2)
import PSD3v2.Selection.Query (queryAll)
import PSD3v2.Selection.Types (ElementType(..), JoinResult(..))
import PSD3v2.VizTree.Tree as T
import Unsafe.Coerce (unsafeCoerce)
import Utility (getWindowWidthHeight)

-- | Indexed link for data join
newtype IndexedLink = IndexedLink { index :: Int, link :: D3Link_Swizzled }

instance Eq IndexedLink where
  eq (IndexedLink a) (IndexedLink b) = a.index == b.index

instance Ord IndexedLink where
  compare (IndexedLink a) (IndexedLink b) = compare a.index b.index

-- | Phylotaxis layout
initialRadius :: Number
initialRadius = 10.0

initialAngle :: Number
initialAngle = pi * (3.0 - sqrt 5.0)

setPhyllotaxisPositions :: forall r. Array (Record (x :: Number, y :: Number | r)) -> Array (Record (x :: Number, y :: Number | r))
setPhyllotaxisPositions nodes = Array.mapWithIndex setPosition nodes
  where
    setPosition :: Int -> Record (x :: Number, y :: Number | r) -> Record (x :: Number, y :: Number | r)
    setPosition index node =
      let
        i = toNumber index
        rad = initialRadius * sqrt (0.5 + i)
        angle = i * initialAngle
      in
        node { x = rad * cos angle, y = rad * sin angle }

-- | Main visualization with query demo
drawLesMisQueryDemo :: forall row.
  Array (Force LesMisSimNode) ->
  Set.Set String ->
  LesMisRawModel ->
  String ->
  D3v2SimM row LesMisSimNode Unit
drawLesMisQueryDemo forcesArray activeForces model containerSelector = do
  -- Get window dimensions
  (Tuple w h) <- liftEffect getWindowWidthHeight

  -- Find unique groups in the data
  let groups = Array.nub $ map _.group model.nodes
  let sortedGroups = Array.sort groups

  -- Create container
  container <- select containerSelector

  -- Create tree structure for the viz
  let vizTree :: T.Tree LesMisSimNode
      vizTree =
        T.named SVG "svg"
          [ width w
          , height h
          , viewBox (show ((-w) / 2.0) <> " " <> show ((-h) / 2.0) <> " " <> show w <> " " <> show h)
          , id_ "lesmis-query-demo-svg"
          , class_ "lesmis-query-demo"
          ]
          `T.withChild`
            T.named Group "zoomGroup" [id_ "zoom-group", class_ "zoom-group"]
              `T.withChildren`
                [ T.named Group "linksGroup" [id_ "links", class_ "links"]
                , T.named Group "nodesGroup" [id_ "nodes", class_ "nodes"]
                ]

  selections <- renderTree container vizTree

  -- Extract selections
  svg <- liftEffect $ reselectD3v2 "svg" selections
  zoomGroup <- liftEffect $ reselectD3v2 "zoomGroup" selections
  linksGroup <- liftEffect $ reselectD3v2 "linksGroup" selections
  nodesGroup <- liftEffect $ reselectD3v2 "nodesGroup" selections

  -- Attach behaviors
  _ <- on (Drag defaultDrag) zoomGroup
  _ <- on (Zoom $ defaultZoom (ScaleExtent 0.5 4.0) "#zoom-group") svg

  -- Apply phylotaxis positions
  let nodesWithPositions = setPhyllotaxisPositions model.nodes

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

  -- Join links
  JoinResult { enter: linkEnter } <- joinData indexedLinks "line" linksGroup
  linkLines <- append Line
    [ x1 (\(IndexedLink il) -> (unsafeCoerce il.link).source.x :: Number)
    , y1 (\(IndexedLink il) -> (unsafeCoerce il.link).source.y :: Number)
    , x2 (\(IndexedLink il) -> (unsafeCoerce il.link).target.x :: Number)
    , y2 (\(IndexedLink il) -> (unsafeCoerce il.link).target.y :: Number)
    , strokeWidth (\(IndexedLink il) -> sqrt (unsafeCoerce il.link).value :: Number)
    , stroke (\(IndexedLink il) -> d3SchemeCategory10N_ (toNumber (unsafeCoerce il.link).target.group) :: String)
    ]
    linkEnter

  -- Join nodes
  JoinResult { enter: nodeEnter } <- joinData nodesInSim "circle" nodesGroup
  nodeCircles <- append Circle
    [ cx (\(d :: LesMisSimNode) -> d.x)
    , cy (\(d :: LesMisSimNode) -> d.y)
    , radius 5.0
    , fill (\(d :: LesMisSimNode) -> d3SchemeCategory10N_ (toNumber d.group))
    , stroke "#fff"
    , strokeWidth 2.0
    , id_ (\(d :: LesMisSimNode) -> "node-" <> d.id)
    , class_ (\(d :: LesMisSimNode) -> "node group-" <> show d.group)
    ]
    nodeEnter

  -- Attach simulation drag
  _ <- on (Drag $ simulationDrag "lesmis-query") nodeCircles

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

  -- Create interactive buttons using the query language!
  liftEffect $ createGroupButtons
    sortedGroups
    (\group -> d3SchemeCategory10N_ (toNumber group))
    (\group isLarge -> do
      -- Unwrap D3v2Selection_ to Selection for query language (using unsafeCoerce)
      let unwrappedSelections = unsafeCoerce selections

      -- Query circles for this specific group using CSS class selector!
      -- This demonstrates the power of queryAll with CSS selectors
      groupCircles <- queryAll (".group-" <> show group) unwrappedSelections

      -- Note: queryAll returns SEmpty but we know these have data bound
      -- Safe to coerce since they were created via joinData
      let boundCircles = unsafeCoerce groupCircles

      -- Update radius based on whether group is large
      let newRadius = if isLarge then 10.0 else 5.0
      void $ Ops.setAttrs [radius newRadius] boundCircles
    )

  -- Start simulation
  start

  pure unit
