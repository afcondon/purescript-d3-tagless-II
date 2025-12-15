-- | Treemap Visualization - Stateless Rendering
-- |
-- | Clean, functional implementation with no Refs and no FFI in user code.
-- | All D3 access is through the PSD3 library's declarative API.
-- |
-- | Architecture:
-- | - Halogen calls render with data
-- | - This module builds a declarative VizTree
-- | - Library interprets the tree into D3 commands
-- | - D3 manages its own DOM mutations (hidden from us)
module Viz.Treemap
  ( render
  , highlightDependencies
  , clearHighlight
  , Config
  , Callbacks
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Class.Console (log)
import Types (SimNode, NodeType(..), Package)
import ViewState (ViewState)
import Data.Set as Set
import PSD3v2.Capabilities.Selection (select, renderTree)
import PSD3v2.Classify (classifyElements, clearClasses)
import PSD3v2.Interpreter.D3v2 (runD3v2M)
import PSD3.AST as T
import PSD3v2.Behavior.Types (onMouseEnter, onMouseLeave, onClickWithDatum)
import PSD3v2.Selection.Types (ElementType(..))
import PSD3v3.Integration (v3AttrFn, v3AttrFnStr, v3AttrStr)
import PSD3v3.Expr (str)
import DataViz.Layout.Hierarchy.Core (hierarchy, sum) as Hier
import DataViz.Layout.Hierarchy.Treemap (treemap, defaultTreemapConfig, TreemapNode(..)) as TM

-- =============================================================================
-- Types
-- =============================================================================

-- | Rendering configuration
type Config =
  { containerSelector :: String
  , viewState :: ViewState
  , packageCount :: Int  -- For color palette
  , packages :: Array Package  -- Package metadata for hierarchy
  }

-- | Callbacks for user interactions
type Callbacks =
  { onNodeClick :: SimNode -> Effect Unit
  , onNodeHover :: Maybe SimNode -> Effect Unit
  }

-- | Leaf data for treemap hierarchy
-- | We store the original SimNode so we can access all its properties
type TreemapLeaf =
  { name :: String
  , nodeType :: NodeType
  , loc :: Number      -- Value for treemap sizing
  , simNode :: Maybe SimNode  -- Original node (Nothing for root/package containers)
  , cluster :: Int     -- For coloring
  }

-- | Node with computed treemap position
type PositionedNode =
  { simNode :: SimNode
  , cx :: Number       -- Center X of treemap rectangle
  , cy :: Number       -- Center Y of treemap rectangle
  , width :: Number    -- Rectangle width (for radius calculation)
  , height :: Number   -- Rectangle height
  }

-- | Package rectangle for background
-- | Also used for modules (with simNode populated for hover callbacks)
type PackageRect =
  { name :: String
  , x :: Number
  , y :: Number
  , width :: Number
  , height :: Number
  , cluster :: Int
  , isUsed :: Boolean  -- True if module has importers (sources)
  , simNode :: Maybe SimNode  -- Original node for callbacks (modules only)
  }

-- =============================================================================
-- Rendering
-- =============================================================================

-- | Render the treemap visualization
-- |
-- | This is a pure rendering function - no internal state, no FFI.
-- | We build a declarative VizTree and let the library render it.
render :: Config -> Callbacks -> Array SimNode -> Effect Unit
render config callbacks nodes = do
  log $ "[Treemap] Rendering " <> show (Array.length nodes) <> " nodes"

  -- Filter nodes for treemap: packages and modules
  let packageNodes = Array.filter (\n -> n.nodeType == PackageNode) nodes
  let moduleNodes = Array.filter (\n -> n.nodeType == ModuleNode) nodes

  log $ "[Treemap] Packages: " <> show (Array.length packageNodes)
      <> ", Modules: " <> show (Array.length moduleNodes)

  -- Compute treemap layout
  let positioned = computeTreemapLayout config packageNodes moduleNodes

  log $ "[Treemap] Positioned: " <> show (Array.length positioned.packages)
      <> " packages, " <> show (Array.length positioned.modules) <> " modules"

  -- Build the visualization tree declaratively
  let vizTree = buildVizTree config callbacks positioned

  -- Render it - the library handles all D3 interaction
  _ <- runD3v2M do
    container <- select config.containerSelector
    renderTree container vizTree

  log "[Treemap] Render complete"

-- =============================================================================
-- Treemap Layout Computation
-- =============================================================================

type PositionedNodes =
  { packages :: Array PositionedNode
  , modules :: Array PositionedNode
  , packageRects :: Array PackageRect  -- For background rectangles
  }

-- | Compute treemap layout using the library
computeTreemapLayout :: Config -> Array SimNode -> Array SimNode -> PositionedNodes
computeTreemapLayout _config packageNodes moduleNodes =
  let
    -- Build module lookup by package name
    modulesByPackage :: Map String (Array SimNode)
    modulesByPackage = foldl addModule Map.empty moduleNodes
      where
        addModule acc mod =
          let existing = fromMaybe [] (Map.lookup mod.package acc)
          in Map.insert mod.package (Array.snoc existing mod) acc

    -- Build hierarchical data for treemap
    -- Root -> Packages -> Modules
    rootData :: TreemapLeaf
    rootData =
      { name: "root"
      , nodeType: PackageNode  -- doesn't matter for root
      , loc: 0.0  -- will be computed by sum
      , simNode: Nothing
      , cluster: 0
      }

    -- Children accessor for hierarchy
    getChildren :: TreemapLeaf -> Maybe (Array TreemapLeaf)
    getChildren leaf = case leaf.name of
      "root" ->
        -- Root's children are packages
        Just $ packageNodes <#> \pkg ->
          { name: pkg.name
          , nodeType: PackageNode
          , loc: 0.0  -- sum will aggregate from modules
          , simNode: Just pkg
          , cluster: pkg.cluster
          }
      _ ->
        -- Package's children are its modules
        case leaf.nodeType of
          PackageNode ->
            let mods = fromMaybe [] (Map.lookup leaf.name modulesByPackage)
            in if Array.null mods then Nothing
               else Just $ mods <#> \mod ->
                 { name: mod.name
                 , nodeType: ModuleNode
                 , loc: mod.r * mod.r  -- Use radius squared as proxy for LOC
                 , simNode: Just mod
                 , cluster: mod.cluster
                 }
          ModuleNode -> Nothing  -- Modules are leaves

    -- Build hierarchy
    hier = Hier.hierarchy rootData getChildren

    -- Compute values (sum of LOC)
    valued = Hier.sum hier _.loc

    -- Configure treemap layout
    -- Use 95% of a 16:10 viewport for better fill
    -- ViewBox will be 0 0 width height, then we'll center it
    treemapWidth = 1900.0   -- 95% of ~2000
    treemapHeight = 1140.0  -- 95% of ~1200 (16:10 ratio)

    treemapConfig = TM.defaultTreemapConfig
      { size = { width: treemapWidth, height: treemapHeight }
      , paddingInner = 3.0
      , paddingOuter = 6.0
      }

    -- Run treemap layout
    treemapResult = TM.treemap treemapConfig valued

    -- Offset to center the treemap in viewbox
    offsetX = treemapWidth / 2.0
    offsetY = treemapHeight / 2.0

    -- Extract positioned nodes from treemap result
    -- We need to walk the tree and extract package and module positions
    extractPositions :: TM.TreemapNode TreemapLeaf -> PositionedNodes
    extractPositions (TM.TNode root) =
      let
        -- Packages are depth 1, modules are depth 2
        processChildren :: Array (TM.TreemapNode TreemapLeaf) -> PositionedNodes -> PositionedNodes
        processChildren children acc = foldl processNode acc children

        processNode :: PositionedNodes -> TM.TreemapNode TreemapLeaf -> PositionedNodes
        processNode acc (TM.TNode node) =
          let
            leaf = node.data_
            -- Center coordinates (offset to center viewbox)
            cx = (node.x0 + node.x1) / 2.0 - offsetX
            cy = (node.y0 + node.y1) / 2.0 - offsetY
            -- Rectangle bounds (also offset)
            rx = node.x0 - offsetX
            ry = node.y0 - offsetY
            width = node.x1 - node.x0
            height = node.y1 - node.y0
          in case leaf.simNode of
            Nothing -> acc  -- Root node, skip
            Just simNode ->
              let
                positioned =
                  { simNode
                  , cx
                  , cy
                  , width
                  , height
                  }
              in case leaf.nodeType of
                PackageNode ->
                  -- Add package circle, package rect, and recurse into modules
                  let
                    rect =
                      { name: leaf.name
                      , x: rx
                      , y: ry
                      , width
                      , height
                      , cluster: leaf.cluster
                      , isUsed: true  -- Packages always considered "used"
                      , simNode: leaf.simNode  -- Include for hover callbacks
                      }
                    newAcc = acc
                      { packages = Array.snoc acc.packages positioned
                      , packageRects = Array.snoc acc.packageRects rect
                      }
                  in processChildren node.children newAcc
                ModuleNode ->
                  acc { modules = Array.snoc acc.modules positioned }
      in
        processChildren root.children { packages: [], modules: [], packageRects: [] }

  in
    extractPositions treemapResult

-- =============================================================================
-- Visualization Tree Building
-- =============================================================================

-- | Build the declarative visualization tree
-- |
-- | Structure:
-- |   SVG (root)
-- |     └─ Group "package-rects" (background rectangles)
-- |         └─ Join for rects
-- |         └─ Join for labels
-- |     └─ Group "packages"
-- |         └─ Join "package-circles" (one circle per package)
-- |     └─ Group "modules"
-- |         └─ Join "module-circles" (one circle per module)
buildVizTree :: Config -> Callbacks -> PositionedNodes -> T.Tree PackageRect
buildVizTree _config callbacks positioned =
  -- Root SVG element - viewBox centered at origin, matching treemap dimensions
  T.named SVG "root"
    [ v3AttrStr "viewBox" (str "-950 -570 1900 1140")
    , v3AttrFnStr "class" (\_ -> "treemap-svg")
    ]
  `T.withChildren`
    [ -- Package rectangles (blueprint background)
      T.named Group "package-rects"
        [ v3AttrFnStr "class" (\_ -> "package-rects-group")
        ]
      `T.withChildren`
        [ T.joinData "pkg-rects" "rect" positioned.packageRects (packageRectTemplate callbacks)
        , T.joinData "pkg-labels" "text" positioned.packageRects packageLabelTemplate
        ]

      -- Package circles group (hidden for now - modules fill the space)
    -- , T.named Group "packages" []
    --   `T.withChild`
    --     T.joinData "package-circles" "circle" positioned.packages (packageCircleTemplate config.viewState)

      -- Module circles group
    , T.named Group "modules"
        [ v3AttrFnStr "class" (\_ -> "modules-group")
        ]
      `T.withChild`
        T.joinData "module-circles" "circle" (map toPackageRect positioned.modules) (moduleCircleTemplate callbacks)
    ]
  where
    -- Convert PositionedNode to PackageRect for type compatibility
    toPackageRect :: PositionedNode -> PackageRect
    toPackageRect pn =
      { name: pn.simNode.name
      , x: pn.cx - pn.width / 2.0
      , y: pn.cy - pn.height / 2.0
      , width: pn.width
      , height: pn.height
      , cluster: pn.simNode.cluster
      , isUsed: not (Array.null pn.simNode.sources)  -- Has importers?
      , simNode: Just pn.simNode  -- Keep for hover callbacks
      }

-- | Template for package background rectangle (blueprint style)
-- | Includes hover behaviors for package dependency highlighting
packageRectTemplate :: Callbacks -> PackageRect -> T.Tree PackageRect
packageRectTemplate callbacks _rect =
  T.elem Rect
    [ v3AttrFn "x" (_.x)
    , v3AttrFn "y" (_.y)
    , v3AttrFn "width" (_.width)
    , v3AttrFn "height" (_.height)
    , v3AttrStr "fill" (str "none")
    , v3AttrStr "stroke" (str "rgba(255, 255, 255, 0.3)")
    , v3AttrFn "stroke-width" (\_ -> 1.0)
    , v3AttrStr "pointer-events" (str "all")  -- Capture events despite no fill
    , v3AttrFnStr "class" (\_ -> "package-rect")
    ]
  `T.withBehaviors`
    [ onMouseEnter (\r -> callbacks.onNodeHover r.simNode)
    , onMouseLeave (\_ -> callbacks.onNodeHover Nothing)
    ]

-- | Template for package label
packageLabelTemplate :: PackageRect -> T.Tree PackageRect
packageLabelTemplate _rect =
  T.elem Text
    [ v3AttrFn "x" (\r -> r.x + 4.0)  -- Small offset from left edge
    , v3AttrFn "y" (\r -> r.y + 12.0) -- Small offset from top
    , v3AttrStr "fill" (str "rgba(255, 255, 255, 0.5)")
    , v3AttrStr "font-size" (str "10px")
    , v3AttrStr "font-family" (str "monospace")
    , v3AttrFnStr "class" (\_ -> "package-label")
    , v3AttrFnStr "textContent" (_.name)  -- Text content via attribute
    ]

-- | Template for a single module circle
-- | Uses PackageRect for data consistency across joins
-- | Used modules: white stroke, Unused modules: thin black stroke
-- | Fill with background blue to capture mouse events
moduleCircleTemplate :: Callbacks -> PackageRect -> T.Tree PackageRect
moduleCircleTemplate callbacks _node =
  T.elem Circle
    [ v3AttrFn "cx" (\r -> r.x + r.width / 2.0)   -- Center from rect bounds
    , v3AttrFn "cy" (\r -> r.y + r.height / 2.0)
    , v3AttrFn "r" (\r -> min r.width r.height / 2.0 * 0.8)  -- Fit in rect with margin
    , v3AttrStr "fill" (str "#0E4C8A")  -- Background blue to capture mouse events
    , v3AttrFnStr "stroke" (\r -> if r.isUsed then "rgba(255, 255, 255, 0.7)" else "black")
    , v3AttrFn "stroke-width" (\r -> if r.isUsed then 1.0 else 0.5)
    , v3AttrFnStr "class" (\_ -> "module-node")
    ]
  `T.withBehaviors`
    [ onMouseEnter (\r -> callbacks.onNodeHover r.simNode)
    , onMouseLeave (\_ -> callbacks.onNodeHover Nothing)
    , onClickWithDatum (\r -> case r.simNode of
        Just sn -> callbacks.onNodeClick sn
        Nothing -> pure unit)
    ]

-- =============================================================================
-- Highlight API (Ephemeral DOM updates without re-render)
-- =============================================================================

-- | CSS classes used for highlighting
highlightClasses :: Array String
highlightClasses = ["highlighted", "is-target", "is-source", "dimmed"]

-- | Highlight dependencies of the hovered node
-- |
-- | Applies CSS classes to elements based on their relationship
-- | to the hovered node:
-- | - "highlighted": the hovered node itself
-- | - "is-target": nodes that this node depends on (imports)
-- | - "is-source": nodes that depend on this node (importers)
highlightDependencies :: String -> SimNode -> Effect Unit
highlightDependencies containerSelector node = do
  log $ "[Treemap] Highlighting dependencies for: " <> node.name
  log $ "[Treemap]   nodeType: " <> (if node.nodeType == PackageNode then "PackageNode" else "ModuleNode")
  log $ "[Treemap]   targets (depends on): " <> show node.targets
  log $ "[Treemap]   sources (depended on by): " <> show node.sources
  let
    targetSet = Set.fromFoldable node.targets
    sourceSet = Set.fromFoldable node.sources

    classifier :: PackageRect -> String
    classifier rect = case rect.simNode of
      Nothing -> ""  -- Skip if no SimNode
      Just sn ->
        if sn.id == node.id then "highlighted"
        else if Set.member sn.id targetSet then "is-target"
        else if Set.member sn.id sourceSet then "is-source"
        else ""  -- No dimming - leave as-is

  -- Classify both circles (modules) and rects (packages)
  classifyElements containerSelector "circle" classifier
  classifyElements containerSelector "rect" classifier

-- | Clear all highlight classes from elements
clearHighlight :: String -> Effect Unit
clearHighlight containerSelector = do
  log "[Treemap] Clearing highlights"
  clearClasses containerSelector "circle" highlightClasses
  clearClasses containerSelector "rect" highlightClasses
