-- | Engine.Treemap
-- |
-- | Treemap visualization for Code Explorer.
-- | Static layout showing modules grouped by package, sized by LOC.
module CodeExplorer.Treemap
  ( renderTreemap
  , renderWatermark
  , clearTreemap
  , clearWatermark
  , TreemapData
  , buildTreemapData
  , computeTreemapPositions
  , recalculateTreemapPositions
  -- * Hover sync
  , highlightWatermarkPackage
  , clearWatermarkHighlight
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Foreign.Object as Object
import DataViz.Layout.Hierarchy.Core as H
import DataViz.Layout.Hierarchy.Treemap (TreemapNode(..), TreemapConfig, treemap, squarify, defaultTreemapConfig)
import PSD3.Scale (interpolateTurbo)
import PSD3.Interpreter.D3 (runD3v2M, D3v2M, D3v2Selection_, reselectD3v2)
import PSD3.Capabilities.Selection (select, renderTree)
import PSD3.Selection.Types (ElementType(..), SEmpty)
import PSD3.AST as T
import PSD3.Expr.Integration (evalAttr, evalAttrStr)
import PSD3.Expr.Expr (lit, str)
import Effect.Class (liftEffect)
import Types (SimNode, NodeType(..))
import CodeExplorer.ViewBox as ViewBox
import Web.DOM.Element (Element)
import Web.DOM.ParentNode (querySelector, QuerySelector(..))
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument (toParentNode)

-- =============================================================================
-- Types
-- =============================================================================

-- | Hierarchical data for treemap
-- | Structure: Root -> Packages -> Modules
data TreemapData
  = TMRoot { children :: Array TreemapData }
  | TMPackage { name :: String, packageIndex :: Int, children :: Array TreemapData }
  | TMModule { name :: String, packageIndex :: Int, loc :: Number }

-- | Get children of a treemap node
tmChildren :: TreemapData -> Maybe (Array TreemapData)
tmChildren (TMRoot { children }) = Just children
tmChildren (TMPackage { children }) = Just children
tmChildren (TMModule _) = Nothing

-- | Get LOC value for a node (modules only)
tmValue :: TreemapData -> Number
tmValue (TMRoot _) = 0.0 -- Internal nodes get value from children
tmValue (TMPackage _) = 0.0 -- Internal nodes get value from children
tmValue (TMModule { loc }) = loc

-- | Get package index for coloring
tmPackageIndex :: TreemapData -> Int
tmPackageIndex (TMRoot _) = -1
tmPackageIndex (TMPackage { packageIndex }) = packageIndex
tmPackageIndex (TMModule { packageIndex }) = packageIndex

-- | Get node name
tmName :: TreemapData -> String
tmName (TMRoot _) = "root"
tmName (TMPackage { name }) = name
tmName (TMModule { name }) = name

-- | Check if node is a module (leaf)
tmIsModule :: TreemapData -> Boolean
tmIsModule (TMModule _) = true
tmIsModule _ = false

-- | Check if node is a package
tmIsPackage :: TreemapData -> Boolean
tmIsPackage (TMPackage _) = true
tmIsPackage _ = false

-- =============================================================================
-- Hierarchy Building
-- =============================================================================

-- | Build treemap data from simulation nodes
-- | Filters to project modules only and groups by package
buildTreemapData :: Array SimNode -> TreemapData
buildTreemapData nodes =
  let
    -- Get modules only (not packages)
    modules = Array.filter (\n -> n.nodeType == ModuleNode) nodes

    -- Get packages only
    packages = Array.filter (\n -> n.nodeType == PackageNode) nodes

    -- Build package name -> index map for coloring
    packageIndexMap = Object.fromFoldable $ Array.mapWithIndex
      (\idx pkg -> Tuple pkg.name idx)
      packages

    -- Group modules by package
    modulesByPackage :: Object.Object (Array SimNode)
    modulesByPackage = foldl groupByPackage Object.empty modules

    groupByPackage acc mod =
      let
        existing = case Object.lookup mod.package acc of
          Just arr -> arr
          Nothing -> []
      in
        Object.insert mod.package (Array.snoc existing mod) acc

    -- Convert to TreemapData structure
    packageNodes = Array.mapMaybe mkPackageNode packages

    mkPackageNode pkg =
      case Object.lookup pkg.name modulesByPackage of
        Nothing -> Nothing -- Skip packages with no modules
        Just mods ->
          let
            pkgIdx = case Object.lookup pkg.name packageIndexMap of
              Just i -> i
              Nothing -> 0
            moduleNodes = map (mkModuleNode pkgIdx) mods
          in
            Just $ TMPackage
              { name: pkg.name
              , packageIndex: pkgIdx
              , children: moduleNodes
              }

    mkModuleNode pkgIdx mod = TMModule
      { name: mod.name
      , packageIndex: pkgIdx
      , loc: mod.r * mod.r -- r is sqrt(LOC), so r^2 gives LOC
      }
  in
    TMRoot { children: packageNodes }

-- =============================================================================
-- Layout Computation
-- =============================================================================

-- | Treemap layout configuration
treemapConfig :: TreemapConfig TreemapData
treemapConfig = defaultTreemapConfig
  { size = { width: ViewBox.viewBoxWidth, height: ViewBox.viewBoxHeight }
  , tile = squarify 1.0 -- Golden ratio gives nice proportions
  , paddingInner = 2.0
  , paddingOuter = 4.0
  , paddingTop = 20.0 -- Extra space at top for package labels
  }

-- | Compute treemap layout from data
computeLayout :: TreemapData -> TreemapNode TreemapData
computeLayout rootData =
  let
    -- Build hierarchy
    hier = H.hierarchy rootData tmChildren
    -- Compute values (sum up LOC)
    valued = H.sum hier tmValue
    -- Apply treemap layout
    layout = treemap treemapConfig valued
  in
    layout

-- =============================================================================
-- Rendering
-- =============================================================================

-- | Flatten treemap to get leaf nodes with bounds
type TreemapLeaf =
  { name :: String
  , packageIndex :: Int
  , x0 :: Number
  , y0 :: Number
  , x1 :: Number
  , y1 :: Number
  , depth :: Int
  }

-- | Get all nodes (both packages and modules) with their bounds
flattenTreemap :: TreemapNode TreemapData -> Array TreemapLeaf
flattenTreemap (TNode n) =
  let
    thisNode =
      { name: tmName n.data_
      , packageIndex: tmPackageIndex n.data_
      , x0: n.x0
      , y0: n.y0
      , x1: n.x1
      , y1: n.y1
      , depth: n.depth
      }
    childNodes = Array.concatMap flattenTreemap n.children
  in
    -- Include packages (depth 1) and modules (depth 2), skip root
    if n.depth > 0 then Array.cons thisNode childNodes
    else childNodes

-- | Render treemap visualization
renderTreemap :: Array SimNode -> Effect Unit
renderTreemap nodes = do
  log "[Treemap] Rendering treemap..."

  -- Build data and compute layout
  let tmData = buildTreemapData nodes
  let layout = computeLayout tmData
  let allNodes = flattenTreemap layout
  let packageNodes = Array.filter (\n -> n.depth == 1) allNodes
  let moduleNodes = Array.filter (\n -> n.depth == 2) allNodes

  log $ "[Treemap] Layout computed: " <> show (Array.length packageNodes) <> " packages, " <> show (Array.length moduleNodes) <> " modules"

  -- Clear existing content and render
  clearNodesGroup
  _ <- runD3v2M $ renderTreemapSVG packageNodes moduleNodes
  log "[Treemap] Render complete"

-- | Render the treemap using TreeAPI
renderTreemapSVG :: Array TreemapLeaf -> Array TreemapLeaf -> D3v2M Unit
renderTreemapSVG packageNodes moduleNodes = do
  nodesGroup <- select "#explorer-nodes" :: _ (D3v2Selection_ SEmpty Element Unit)

  -- Create treemap container structure
  let
    translateX = (-ViewBox.viewBoxWidth) / 2.0
    translateY = (-ViewBox.viewBoxHeight) / 2.0

    structureTree :: T.Tree Unit
    structureTree =
      T.named Group "treemap-group"
        [ evalAttrStr "id" (str "treemap-group")
        , evalAttrStr "transform" (str ("translate(" <> show translateX <> "," <> show translateY <> ")"))
        ]
        `T.withChildren`
          [ T.named Group "treemap-packages" []
          , T.named Group "treemap-modules" []
          , T.named Group "treemap-labels" []
          ]

  -- Render structure
  selections <- renderTree nodesGroup structureTree

  -- Reselect groups
  packagesGroup <- liftEffect $ reselectD3v2 "treemap-packages" selections
  modulesGroup <- liftEffect $ reselectD3v2 "treemap-modules" selections
  labelsGroup <- liftEffect $ reselectD3v2 "treemap-labels" selections

  -- Render package backgrounds (depth 1) - Blueprint style: white lines, no fill
  let packagesTree :: T.Tree TreemapLeaf
      packagesTree =
        T.joinData "package-rects" "rect" packageNodes $ \n ->
          T.elem Rect
            [ evalAttr "x" (lit n.x0)
            , evalAttr "y" (lit n.y0)
            , evalAttr "width" (lit (n.x1 - n.x0))
            , evalAttr "height" (lit (n.y1 - n.y0))
            , evalAttrStr "fill" (str "none")
            , evalAttrStr "stroke" (str "rgba(255, 255, 255, 0.6)")
            , evalAttr "stroke-width" (lit 1.5)
            , evalAttr "opacity" (lit 1.0)
            , evalAttrStr "class" (str "treemap-package")
            ]
  _ <- renderTree packagesGroup packagesTree

  -- Render module rectangles (depth 2) - Blueprint style: white lines, no fill
  let modulesTree :: T.Tree TreemapLeaf
      modulesTree =
        T.joinData "module-rects" "rect" moduleNodes $ \n ->
          T.elem Rect
            [ evalAttr "x" (lit n.x0)
            , evalAttr "y" (lit n.y0)
            , evalAttr "width" (lit (n.x1 - n.x0))
            , evalAttr "height" (lit (n.y1 - n.y0))
            , evalAttrStr "fill" (str "none")
            , evalAttrStr "stroke" (str "rgba(255, 255, 255, 0.4)")
            , evalAttr "stroke-width" (lit 0.5)
            , evalAttrStr "class" (str "treemap-module")
            ]
  _ <- renderTree modulesGroup modulesTree

  -- Render package labels - Blueprint style: white text
  liftEffect $ log $ "[Treemap] Rendering labels for " <> show (Array.length packageNodes) <> " packages"
  let labelsTree :: T.Tree TreemapLeaf
      labelsTree =
        T.joinData "package-labels" "text" packageNodes $ \n ->
          T.elem Text
            [ evalAttr "x" (lit ((n.x0 + n.x1) / 2.0))
            , evalAttr "y" (lit (n.y0 + 14.0))
            , evalAttrStr "text-anchor" (str "middle")
            , evalAttrStr "fill" (str "rgba(255, 255, 255, 0.7)")
            , evalAttrStr "font-family" (str "Monaco, 'Courier New', monospace")
            , evalAttr "font-size" (lit 10.0)
            , evalAttr "opacity" (lit 1.0)
            , evalAttrStr "class" (str "treemap-package-label")
            , evalAttrStr "textContent" (str n.name)
            ]
  _ <- renderTree labelsGroup labelsTree
  liftEffect $ log "[Treemap] Labels rendered"

  pure unit

-- | Color for package background (monochromatic - light gray)
packageColor :: TreemapLeaf -> String
packageColor _ = "#f8f8f8"

-- | Color for module rectangle (monochromatic - slightly darker gray)
moduleColor :: TreemapLeaf -> String
moduleColor _ = "#ececec"

-- | Helper for golden ratio distribution
numMod :: Number -> Number -> Number
numMod a b = a - b * toNumber (floor (a / b))

-- =============================================================================
-- Cleanup
-- =============================================================================

-- | Clear treemap (called when switching to other views)
clearTreemap :: Effect Unit
clearTreemap = clearNodesGroup

-- | Clear nodes group (same as Explorer)
clearNodesGroup :: Effect Unit
clearNodesGroup = do
  win <- window
  doc <- document win
  let parentNode = toParentNode doc
  mElement <- querySelector (QuerySelector "#explorer-nodes") parentNode
  case mElement of
    Just nodesGroup -> clearElement nodesGroup
    Nothing -> log "[Treemap] Could not find #explorer-nodes"

-- | Clear all children of an element
foreign import clearElement :: Element -> Effect Unit

-- | Set up hover interactions for watermark labels
foreign import setupWatermarkHover :: Effect Unit

-- =============================================================================
-- Treemap-Anchored Positioning
-- =============================================================================

-- | Apply treemap-based positions to all nodes
-- | Packages get positioned at treemap rectangle centers
-- | Modules inherit their package's position
-- | Also sets x/y to the position (for initialization)
recalculateTreemapPositions :: Array SimNode -> Array SimNode
recalculateTreemapPositions nodes =
  let
    -- Compute package positions
    posMap = computeTreemapPositions nodes

    -- Build package name -> cluster id map
    packages = Array.filter (\n -> n.nodeType == PackageNode) nodes
    clusterMap = Object.fromFoldable $ map (\p -> Tuple (show p.id) p.name) packages

    -- Update each node
    updateNode node =
      case node.nodeType of
        PackageNode ->
          case Object.lookup node.name posMap of
            Just pos -> node { gridX = pos.x, gridY = pos.y, x = pos.x, y = pos.y }
            Nothing -> node
        ModuleNode ->
          -- Look up the package name via cluster, then get position
          case Object.lookup (show node.cluster) clusterMap of
            Just pkgName ->
              case Object.lookup pkgName posMap of
                Just pos -> node { gridX = pos.x, gridY = pos.y, x = pos.x, y = pos.y }
                Nothing -> node
            Nothing -> node
  in
    map updateNode nodes

-- | Compute treemap layout and return package center positions
-- | Returns a map of package name -> { x, y } in viewBox coordinates (centered at origin)
computeTreemapPositions :: Array SimNode -> Object.Object { x :: Number, y :: Number }
computeTreemapPositions nodes =
  let
    -- Build treemap data and compute layout
    tmData = buildTreemapData nodes
    layout = computeLayout tmData
    allNodes = flattenTreemap layout

    -- Get package nodes (depth 1)
    packageNodes = Array.filter (\n -> n.depth == 1) allNodes

    -- Convert to position map (centered coordinates)
    -- ViewBox is centered at origin, so offset by (-width/2, -height/2)
    offsetX = (-ViewBox.viewBoxWidth) / 2.0
    offsetY = (-ViewBox.viewBoxHeight) / 2.0

    toPositionEntry leaf =
      let
        centerX = (leaf.x0 + leaf.x1) / 2.0 + offsetX
        centerY = (leaf.y0 + leaf.y1) / 2.0 + offsetY
      in
        Tuple leaf.name { x: centerX, y: centerY }
  in
    Object.fromFoldable $ map toPositionEntry packageNodes

-- =============================================================================
-- Watermark Rendering
-- =============================================================================

-- | Render treemap as a muted background watermark
-- | This is rendered into a separate group that persists across scene changes
renderWatermark :: Array SimNode -> Effect Unit
renderWatermark nodes = do
  log "[Treemap] Rendering watermark..."

  -- Build data and compute layout
  let tmData = buildTreemapData nodes
  let layout = computeLayout tmData
  let allNodes = flattenTreemap layout
  let packageNodes = Array.filter (\n -> n.depth == 1) allNodes

  log $ "[Treemap] Watermark: " <> show (Array.length packageNodes) <> " packages"

  -- Render into watermark group
  _ <- runD3v2M $ renderWatermarkSVG packageNodes

  -- Set up hover interactions (must be done after DOM is ready)
  setupWatermarkHover

  log "[Treemap] Watermark render complete"

-- | Render the watermark using TreeAPI
renderWatermarkSVG :: Array TreemapLeaf -> D3v2M Unit
renderWatermarkSVG packageNodes = do
  -- Select existing watermark group (created during SVG setup)
  watermarkGroup <- select "#treemap-watermark" :: _ (D3v2Selection_ SEmpty Element Unit)

  -- Create structure
  let
    translateX = (-ViewBox.viewBoxWidth) / 2.0
    translateY = (-ViewBox.viewBoxHeight) / 2.0

    structureTree :: T.Tree Unit
    structureTree =
      T.named Group "watermark-inner"
        [ evalAttrStr "transform" (str ("translate(" <> show translateX <> "," <> show translateY <> ")"))
        , evalAttrStr "id" (str "watermark-inner")
        ]
        `T.withChildren`
          [ T.named Group "watermark-rects" []
          , T.named Group "watermark-labels" []
          ]

  -- Render structure
  selections <- renderTree watermarkGroup structureTree

  -- Reselect groups
  rectsGroup <- liftEffect $ reselectD3v2 "watermark-rects" selections
  labelsGroup <- liftEffect $ reselectD3v2 "watermark-labels" selections

  -- Render package rectangles - Blueprint watermark: very faded white lines
  let rectsTree :: T.Tree TreemapLeaf
      rectsTree =
        T.joinData "watermark-package-rects" "rect" packageNodes $ \n ->
          T.elem Rect
            [ evalAttr "x" (lit n.x0)
            , evalAttr "y" (lit n.y0)
            , evalAttr "width" (lit (n.x1 - n.x0))
            , evalAttr "height" (lit (n.y1 - n.y0))
            , evalAttrStr "fill" (str "rgba(255, 255, 255, 0)")
            , evalAttrStr "stroke" (str "rgba(255, 255, 255, 0.15)")
            , evalAttr "stroke-width" (lit 1.0)
            , evalAttr "opacity" (lit 1.0)
            , evalAttrStr "class" (str "watermark-package")
            ]
  _ <- renderTree rectsGroup rectsTree

  -- Render package labels - Blueprint watermark: very faded white text
  liftEffect $ log $ "[Treemap] Rendering watermark labels for " <> show (Array.length packageNodes) <> " packages"
  let labelsTree :: T.Tree TreemapLeaf
      labelsTree =
        T.joinData "watermark-package-labels" "text" packageNodes $ \n ->
          T.elem Text
            [ evalAttr "x" (lit ((n.x0 + n.x1) / 2.0))
            , evalAttr "y" (lit (n.y0 + 14.0))
            , evalAttrStr "text-anchor" (str "middle")
            , evalAttrStr "fill" (str "rgba(255, 255, 255, 0.2)")
            , evalAttrStr "font-family" (str "Monaco, 'Courier New', monospace")
            , evalAttr "font-size" (lit 10.0)
            , evalAttr "opacity" (lit 1.0)
            , evalAttrStr "class" (str "watermark-package-label")
            , evalAttrStr "textContent" (str n.name)
            ]
  _ <- renderTree labelsGroup labelsTree
  liftEffect $ log "[Treemap] Watermark labels rendered"

  pure unit

-- | Muted color for watermark packages (monochromatic)
watermarkColor :: TreemapLeaf -> String
watermarkColor _ = "#f0f0f0"

-- | Clear the watermark
clearWatermark :: Effect Unit
clearWatermark = do
  win <- window
  doc <- document win
  let parentNode = toParentNode doc
  mElement <- querySelector (QuerySelector "#treemap-watermark") parentNode
  case mElement of
    Just watermarkGroup -> clearElement watermarkGroup
    Nothing -> pure unit -- No watermark to clear

-- =============================================================================
-- Hover Sync
-- =============================================================================

-- | Highlight a package in the treemap watermark
foreign import highlightWatermarkPackage_ :: String -> Effect Unit

-- | Clear all watermark highlights
foreign import clearWatermarkHighlight_ :: Effect Unit

-- | Wrapped version for export
highlightWatermarkPackage :: String -> Effect Unit
highlightWatermarkPackage = highlightWatermarkPackage_

-- | Wrapped version for export
clearWatermarkHighlight :: Effect Unit
clearWatermarkHighlight = clearWatermarkHighlight_
