-- | Tree Visualization - Animated Tree with Tick-Based Interpolation
-- |
-- | Shows "used" modules (those in the spanning tree) as a vertical tree.
-- | Animation: used module circles move from root position to tree positions.
-- | Tree links (bezier curves) grow from root position outward.
-- |
-- | Architecture:
-- | - Computes tree layout on first frame using Data.TreeLayout
-- | - Treemap circles stay in DOM, we just move the "used" ones
-- | - Tree links (paths) are added on first frame, then animated
-- | - Unused modules stay in their treemap positions (low-key in blueprint)
module Viz.TreeView
  ( renderAnimated
  , AnimationState
  , initAnimation
  , tickAnimation
  , isAnimating
  , Config
  ) where

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Types (SimNode, SimLink, NodeType(..))
import Data.TreeLayout as TreeLayout
import PSD3v2.Capabilities.Selection (select, renderTree)
import PSD3v2.Interpreter.D3v2 (runD3v2M)
import PSD3v2.Transform (transformCircles, transformPaths, clearContainer)
import PSD3v2.VizTree.Tree as T
import PSD3v2.Selection.Types (ElementType(..))
import PSD3v3.Integration (v3AttrFn, v3AttrFnStr, v3AttrStr)
import PSD3v3.Expr (str)
import PSD3.Transition.Tick as Tick
import DataViz.Layout.Hierarchy.Link (linkBezierVertical)

-- =============================================================================
-- Types
-- =============================================================================

-- | Rendering configuration
type Config =
  { containerSelector :: String
  }

-- | Animation state for tree growth
-- | Includes computed tree layout (computed once on first frame)
type AnimationState =
  { progress :: Tick.Progress  -- 0.0 = at root position, 1.0 = at tree positions
  , isComplete :: Boolean
  , isFirstFrame :: Boolean    -- True on first frame (need to compute layout + add links)
  , treeLayout :: Maybe TreeLayout.TreeLayoutResult  -- Computed on first frame
  }

-- | Link element for the tree visualization
type LinkElement =
  { -- Source node final tree position
    sourceTreeX :: Number
  , sourceTreeY :: Number
    -- Target node final tree position
  , targetTreeX :: Number
  , targetTreeY :: Number
    -- Root position (for animation start)
  , rootX :: Number
  , rootY :: Number
  }

-- =============================================================================
-- Animation State Management
-- =============================================================================

-- | Initialize animation state
initAnimation :: AnimationState
initAnimation =
  { progress: 0.0
  , isComplete: false
  , isFirstFrame: true
  , treeLayout: Nothing
  }

-- | Advance animation by one tick
-- | Returns updated state
tickAnimation :: Tick.TickDelta -> AnimationState -> AnimationState
tickAnimation delta state =
  if state.isComplete then state
  else
    let newProgress = min 1.0 (state.progress + delta)
    in state
       { progress = newProgress
       , isComplete = newProgress >= 1.0
       , isFirstFrame = false
       }

-- | Check if animation is still running
isAnimating :: AnimationState -> Boolean
isAnimating state = not state.isComplete

-- =============================================================================
-- Rendering
-- =============================================================================

-- | Render tree with animation
-- | First frame: compute layout, add link elements, position circles at root
-- | Subsequent frames: interpolate circles and links toward tree positions
-- |
-- | Returns updated AnimationState with computed layout
renderAnimated :: Config -> AnimationState -> Array SimNode -> Array SimLink -> Effect AnimationState
renderAnimated config animState nodes links = do
  log $ "[TreeView] renderAnimated called, isFirstFrame=" <> show animState.isFirstFrame <> ", progress=" <> show animState.progress

  -- Get or compute tree layout
  layout <- case animState.treeLayout of
    Just l -> do
      log "[TreeView] Using cached layout"
      pure l
    Nothing -> do
      log "[TreeView] Computing tree layout..."
      -- Compute tree layout from raw nodes/links
      let rootId = TreeLayout.findRootModule nodes
      log $ "[TreeView] Found root: " <> show rootId
      let result = TreeLayout.computeTreeLayout rootId nodes links
      log $ "[TreeView] Layout computed: root=" <> show result.rootId
          <> " at (" <> show result.rootX <> ", " <> show result.rootY <> ")"
          <> ", " <> show (Array.length result.treeNodes) <> " nodes"
      pure result

  let rootX = layout.rootX
  let rootY = layout.rootY

  -- First frame: add links to DOM
  if animState.isFirstFrame then do
    log $ "[TreeView] First frame - root at (" <> show rootX <> ", " <> show rootY <> ")"
    addTreeLinks config layout rootX rootY
  else
    pure unit

  -- Update positions on every frame
  updatePositions config animState layout

  -- Return updated state with layout cached
  pure $ animState { treeLayout = Just layout }

-- | Add tree links to a new group in the SVG
-- | Called once on first frame
addTreeLinks :: Config -> TreeLayout.TreeLayoutResult -> Number -> Number -> Effect Unit
addTreeLinks config layout rootX rootY = do
  -- Build link elements from tree edges
  let nodeMap = buildTreeNodeMap layout.treeNodes
  let linkElements = Array.mapMaybe (buildLinkElement nodeMap rootX rootY) (Set.toUnfoldable layout.treeEdges)

  log $ "[TreeView] Adding " <> show (Array.length linkElements) <> " tree links as bezier paths"

  -- First remove any existing tree-links group (in case we're re-entering tree view)
  clearContainer (config.containerSelector <> " g.tree-links-group")

  -- Add link elements to SVG
  _ <- runD3v2M do
    svg <- select (config.containerSelector <> " svg")
    renderTree svg (buildLinksVizTree linkElements rootX rootY)

  pure unit

-- | Update positions on existing elements (for animation ticks)
-- | Moves "used" circles from root to tree positions
-- | Grows bezier paths from root outward
updatePositions :: Config -> AnimationState -> TreeLayout.TreeLayoutResult -> Effect Unit
updatePositions config animState layout = do
  -- Apply easing to progress
  let easedProgress = Tick.easeOutCubic animState.progress
  let rootX = layout.rootX
  let rootY = layout.rootY

  -- Build lookup for tree positions by ID
  let treeNodeMap = Map.fromFoldable $ layout.treeNodes <#> \tn -> Tuple tn.id tn

  -- Update circles - only move "used" ones (isInTree), leave others alone
  transformCircles config.containerSelector \rect ->
    case rect.simNode of
      Just sn ->
        -- Look up this node's tree position
        case Map.lookup sn.id treeNodeMap of
          Just tn | tn.isInTree ->
            -- Interpolate from root position to tree position
            { cx: Tick.lerp rootX tn.treeX easedProgress
            , cy: Tick.lerp rootY tn.treeY easedProgress
            }
          _ ->
            -- Not in tree - stay at treemap position
            { cx: rect.x + rect.width / 2.0
            , cy: rect.y + rect.height / 2.0
            }
      Nothing ->
        -- No simNode bound - stay at treemap position
        { cx: rect.x + rect.width / 2.0
        , cy: rect.y + rect.height / 2.0
        }

  -- Update tree links (bezier paths)
  transformPaths (config.containerSelector <> " g.tree-links-group") \el ->
    let
      srcX = Tick.lerp rootX el.sourceTreeX easedProgress
      srcY = Tick.lerp rootY el.sourceTreeY easedProgress
      tgtX = Tick.lerp rootX el.targetTreeX easedProgress
      tgtY = Tick.lerp rootY el.targetTreeY easedProgress
    in
      linkBezierVertical srcX srcY tgtX tgtY

-- =============================================================================
-- Helpers
-- =============================================================================

-- | Build a map from node ID to TreeNode for quick lookup
buildTreeNodeMap :: Array TreeLayout.TreeNode -> Map Int TreeLayout.TreeNode
buildTreeNodeMap nodes = Map.fromFoldable $ nodes <#> \n -> Tuple n.id n

-- | Build a link element from a tree edge (source ID, target ID)
buildLinkElement :: Map Int TreeLayout.TreeNode -> Number -> Number -> Tuple Int Int -> Maybe LinkElement
buildLinkElement nodeMap rootX rootY (Tuple srcId tgtId) = do
  sourceNode <- Map.lookup srcId nodeMap
  targetNode <- Map.lookup tgtId nodeMap

  pure
    { sourceTreeX: sourceNode.treeX
    , sourceTreeY: sourceNode.treeY
    , targetTreeX: targetNode.treeX
    , targetTreeY: targetNode.treeY
    , rootX
    , rootY
    }

-- =============================================================================
-- VizTree for Links
-- =============================================================================

-- | Build VizTree for tree links only (appended to existing SVG)
buildLinksVizTree :: Array LinkElement -> Number -> Number -> T.Tree LinkElement
buildLinksVizTree linkElements rootX rootY =
  T.named Group "tree-links-group"
    [ v3AttrFnStr "class" (\_ -> "tree-links-group")
    ]
  `T.withChild`
    T.joinData "tree-links" "path" linkElements (linkTemplate rootX rootY)

-- | Template for tree link path (bezier curve, starts at root position)
linkTemplate :: Number -> Number -> LinkElement -> T.Tree LinkElement
linkTemplate rootX rootY _el =
  T.elem Path
    [ v3AttrFnStr "d" (\_ -> linkBezierVertical rootX rootY rootX rootY)
    , v3AttrStr "fill" (str "none")
    , v3AttrStr "stroke" (str "rgba(255, 255, 255, 0.5)")
    , v3AttrFn "stroke-width" (\_ -> 1.5)
    , v3AttrFnStr "class" (\_ -> "tree-link")
    ]
