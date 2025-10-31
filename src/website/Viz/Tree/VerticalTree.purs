module D3.Viz.Tree.VerticalTree where

import Prelude

import PSD3.Shared.TreeHelpers (treeDatum_)
import Data.Number (abs)
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect, liftEffect)
import PSD3 (class SelectionM, D3Selection_, Datum_, Selector, Element(..), appendTo, attach, setAttributes, simpleJoin)
import PSD3.Data.Tree (TreeJson_, TreeLayoutFn_, TreeType(..))
import PSD3.Internal.Attributes.Sugar (classed, dy, fill, fontFamily, fontSize, radius, strokeColor, strokeOpacity, strokeWidth, text, textAnchor, transform, x)
import PSD3.Internal.FFI (descendants_, hierarchyFromJSON_, hNodeHeight_, keyIsID_, links_, runLayoutFn_, treeMinMax_, treeSetNodeSize_)
import PSD3.Internal.Hierarchical (verticalClusterLink, verticalLink)
import PSD3.Shared.ZoomableViewbox (ZoomableSVGConfig, zoomableSVG)
import Utility (getWindowWidthHeight)

-- Get layout algorithm from D3 (from shared TreeHelpers)
foreign import getLayout :: TreeType -> TreeLayoutFn_

-- Transform function for vertical trees (x and y are in normal positions)
positionXY :: Datum_ -> String
positionXY d = "translate(" <> show (treeDatum_.x d) <> "," <> show (treeDatum_.y d) <> ")"

-- | Draw a vertical tree (top-down orientation)
-- | TreeType parameter controls Tidy vs Dendrogram layout
-- Snippet_Start
-- Name: VerticalTree
drawVerticalTree :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  TreeType ->          -- TidyTree or Dendrogram
  TreeJson_ ->         -- The tree data
  Selector D3Selection_ -> -- Where to draw (e.g., "div.viz")
  m D3Selection_
drawVerticalTree treeType json selector = do
  -- Get window dimensions
  Tuple w h <- liftEffect getWindowWidthHeight

  -- Build D3 hierarchy from JSON
  let root = hierarchyFromJSON_ json
      numberOfLevels = (hNodeHeight_ root) + 1.0
      spacing = { interChild: 10.0, interLevel: h / numberOfLevels }

      -- Configure layout algorithm (tidy tree or dendrogram)
      layout = (getLayout treeType) `treeSetNodeSize_` [ spacing.interChild, spacing.interLevel ]
      laidOutRoot = layout `runLayoutFn_` root

      -- Calculate extents for viewBox
      { xMin, xMax, yMin, yMax } = treeMinMax_ laidOutRoot
      xExtent = abs $ xMax - xMin
      yExtent = abs $ yMax - yMin
      pad n = n * 1.2
      vtreeYOffset = (abs (h - yExtent)) / 2.0
      vtreeXOffset = xMin

      -- Choose link style based on tree type
      linkPath = case treeType of
        Dendrogram -> verticalClusterLink spacing.interLevel
        TidyTree -> verticalLink

      -- Consistent colors across all tree layouts
      linkColor = "#94a3b8"      -- Slate gray for links
      nodeColor = "#0ea5e9"      -- Sky blue for nodes
      textColor = "#0c4a6e"      -- Dark blue for text

  -- Build the SVG structure with zoom
  rootSel <- attach selector
  let zoomConfig :: ZoomableSVGConfig
      zoomConfig = {
        minX: vtreeXOffset,
        minY: -vtreeYOffset,
        width: pad xExtent,
        height: pad yExtent,
        svgClass: "tree vertical-tree",
        innerClass: "tree-container",
        innerWidth: w,
        innerHeight: h
      }
  { svg, zoomGroup } <- zoomableSVG rootSel zoomConfig

  -- Create container for tree elements
  container <- appendTo zoomGroup Group [ fontFamily "sans-serif", fontSize 10.0 ]
  linksGroup <- appendTo container Group [ classed "links" ]
  nodesGroup <- appendTo container Group [ classed "nodes" ]

  -- Draw links (paths between nodes)
  theLinks <- simpleJoin linksGroup Path (links_ laidOutRoot) keyIsID_
  setAttributes theLinks
    [ strokeWidth 1.5
    , strokeColor linkColor
    , strokeOpacity 0.6
    , fill "none"
    , linkPath
    ]

  -- Draw nodes (groups containing circles and text labels)
  nodeGroups <- simpleJoin nodesGroup Group (descendants_ laidOutRoot) keyIsID_
  setAttributes nodeGroups
    [ transform [ positionXY ] ]

  -- Add circles to nodes
  _ <- appendTo nodeGroups Circle
    [ fill nodeColor
    , radius 3.0
    , strokeColor "white"
    , strokeWidth 1.5
    ]

  -- Add text labels to nodes
  _ <- appendTo nodeGroups Text
    [ dy 0.31
    , x (\d -> if treeDatum_.hasChildren d then 8.0 else (-8.0))
    , textAnchor (\d -> if treeDatum_.hasChildren d then "start" else "end")
    , text treeDatum_.name
    , fill textColor
    , fontSize 11.0
    ]

  pure svg
-- Snippet_End
