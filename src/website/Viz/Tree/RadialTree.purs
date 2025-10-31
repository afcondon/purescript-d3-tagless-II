module D3.Viz.Tree.RadialTree where

import Prelude

import PSD3.Shared.TreeHelpers (treeDatum_)
import Data.Number (pi)
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect, liftEffect)
import PSD3 (class SelectionM, D3Selection_, Datum_, Selector, Element(..), appendTo, attach, setAttributes, simpleJoin)
import PSD3.Data.Tree (TreeJson_, TreeLayoutFn_, TreeType)
import PSD3.Internal.Attributes.Sugar (classed, dy, fill, fontFamily, fontSize, radius, strokeColor, strokeOpacity, strokeWidth, text, textAnchor, transform, x)
import PSD3.Internal.FFI (descendants_, hierarchyFromJSON_, keyIsID_, links_, runLayoutFn_, treeMinMax_, treeSetSeparation_, treeSetSize_)
import PSD3.Internal.Hierarchical (radialLink, radialSeparation)
import PSD3.Shared.ZoomableViewbox (ZoomableSVGConfig, zoomableSVG)
import Utility (getWindowWidthHeight)

-- Get layout algorithm from D3 (from shared TreeHelpers)
foreign import getLayout :: TreeType -> TreeLayoutFn_

-- Radial transform functions
radialRotate :: Number -> String
radialRotate x = show $ (x * 180.0 / pi - 90.0)

radialRotateCommon :: Datum_ -> String
radialRotateCommon d = "rotate(" <> radialRotate (treeDatum_.x d) <> ")"

radialTranslate :: Datum_ -> String
radialTranslate d = "translate(" <> show (treeDatum_.y d) <> ",0)"

rotateRadialLabels :: Datum_ -> String
rotateRadialLabels d =
  "rotate(" <>
    (if treeDatum_.x d >= pi
    then "180"
    else "0")
    <> ")"

-- | Draw a radial tree (emanating from center in polar coordinates)
-- | TreeType parameter controls Tidy vs Dendrogram layout
-- Snippet_Start
-- Name: RadialTree
drawRadialTree :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  TreeType ->          -- TidyTree or Dendrogram
  TreeJson_ ->         -- The tree data
  Selector D3Selection_ -> -- Where to draw (e.g., "div.viz")
  m D3Selection_
drawRadialTree treeType json selector = do
  -- Get window dimensions
  Tuple w h <- liftEffect getWindowWidthHeight

  -- Build D3 hierarchy from JSON
  let root = hierarchyFromJSON_ json

      -- Configure layout algorithm for radial (uses polar coordinates)
      layout = ((getLayout treeType) `treeSetSize_` [ 2.0 * pi, (w / 2.0) - 100.0 ])
                                     `treeSetSeparation_` radialSeparation
      laidOutRoot = layout `runLayoutFn_` root

      -- Calculate extents for viewBox (radial uses yMax as radius)
      { yMax } = treeMinMax_ laidOutRoot
      radialRadius = yMax
      radialExtent = 2.0 * radialRadius

      -- Consistent colors across all tree layouts
      linkColor = "#94a3b8"      -- Slate gray for links
      nodeColor = "#0ea5e9"      -- Sky blue for nodes
      textColor = "#0c4a6e"      -- Dark blue for text

  -- Build the SVG structure with zoom
  rootSel <- attach selector
  let zoomConfig :: ZoomableSVGConfig
      zoomConfig = {
        minX: -radialRadius * 1.2,
        minY: -radialRadius * 1.2,
        width: radialExtent * 1.2,
        height: radialExtent * 1.2,
        svgClass: "tree radial-tree",
        innerClass: "tree-container",
        innerWidth: w,
        innerHeight: h
      }
  { svg, zoomGroup } <- zoomableSVG rootSel zoomConfig

  -- Create container for tree elements
  container <- appendTo zoomGroup Group [ fontFamily "sans-serif", fontSize 10.0 ]
  linksGroup <- appendTo container Group [ classed "links" ]
  nodesGroup <- appendTo container Group [ classed "nodes" ]

  -- Draw links (paths between nodes) using radial link generator
  theLinks <- simpleJoin linksGroup Path (links_ laidOutRoot) keyIsID_
  setAttributes theLinks
    [ strokeWidth 1.5
    , strokeColor linkColor
    , strokeOpacity 0.6
    , fill "none"
    , radialLink treeDatum_.x treeDatum_.y
    ]

  -- Draw nodes (groups containing circles and text labels)
  nodeGroups <- simpleJoin nodesGroup Group (descendants_ laidOutRoot) keyIsID_
  setAttributes nodeGroups
    [ transform [ radialRotateCommon, radialTranslate, rotateRadialLabels ] ]

  -- Add circles to nodes
  _ <- appendTo nodeGroups Circle
    [ fill nodeColor
    , radius 3.0
    , strokeColor "white"
    , strokeWidth 1.5
    ]

  -- Add text labels to nodes (positioned differently for left vs right side)
  _ <- appendTo nodeGroups Text
    [ dy 0.31
    , x (\d ->
        if (treeDatum_.hasChildren d) == (treeDatum_.x d < pi)
        then 8.0
        else (-8.0))
    , textAnchor (\d ->
        if (treeDatum_.hasChildren d) == (treeDatum_.x d < pi)
        then "start"
        else "end")
    , text treeDatum_.name
    , fill textColor
    , fontSize 11.0
    ]

  pure svg
-- Snippet_End
