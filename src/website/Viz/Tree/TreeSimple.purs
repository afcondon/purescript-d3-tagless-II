module D3.Viz.TreeSimple where

import Prelude

import PSD3.Internal.Attributes.Sugar (AlignAspectRatio_X(..), AlignAspectRatio_Y(..), AspectRatioPreserve(..), AspectRatioSpec(..), preserveAspectRatio, transform, viewBox)
import PSD3.Data.Tree (TreeJson_, TreeLayout(..), TreeType(..))
import PSD3.Internal.Types (D3Selection_, Datum_, Selector)
import D3.Viz.Tree.Draw (draw) as Tree
import D3.Viz.Tree.Draw (treeDatum_)
import PSD3.Internal.FFI (getLayout, hNodeHeight_, hierarchyFromJSON_, runLayoutFn_, treeMinMax_, treeSetNodeSize_, treeSetSeparation_, treeSetSize_)
import PSD3.Internal.Hierarchical (horizontalLink, radialLink, radialSeparation, verticalLink)
import PSD3.Internal.Scales.Scales (d3SchemeCategory10N_)
import PSD3.Capabilities.Selection (class SelectionM)
import Data.Number (pi, abs)
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect, liftEffect)
import Utility (getWindowWidthHeight)

-- | Simple horizontal tree layout (TidyTree, Horizontal)
-- Snippet_Start
-- Name: TreeHorizontalDraw
drawTreeHorizontal :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  TreeJson_ -> Selector D3Selection_ -> m D3Selection_
drawTreeHorizontal json selector = do
  (Tuple width height) <- liftEffect getWindowWidthHeight
  let root = hierarchyFromJSON_ json
      numberOfLevels = (hNodeHeight_ root) + 1.0
      spacing = { interChild: 10.0, interLevel: width / numberOfLevels }
      layout = (getLayout TidyTree) `treeSetNodeSize_` [ spacing.interChild, spacing.interLevel ]
      laidOutRoot = layout `runLayoutFn_` root
      { xMin, xMax, yMin, yMax } = treeMinMax_ laidOutRoot
      xExtent = abs $ xMax - xMin
      pad n = n * 1.2
      htreeYOffset = xMin
      config = {
          layout: Horizontal
        , selector
        , linkPath: horizontalLink
        , spacing
        , viewbox: [ viewBox (-xExtent * 0.1) (pad htreeYOffset) (pad yExtent) (pad xExtent)
                   , preserveAspectRatio $ AspectRatio XMin YMid Meet ]
        , nodeTransform: [ transform [ positionXYreflected ] ]
        , color: d3SchemeCategory10N_ 4.0
        , svg: { width, height }
      }
      yExtent = abs $ yMax - yMin
  Tree.draw config laidOutRoot
-- Snippet_End

-- | Simple vertical tree layout (TidyTree, Vertical)
-- Snippet_Start
-- Name: TreeVerticalDraw
drawTreeVertical :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  TreeJson_ -> Selector D3Selection_ -> m D3Selection_
drawTreeVertical json selector = do
  (Tuple width height) <- liftEffect getWindowWidthHeight
  let root = hierarchyFromJSON_ json
      numberOfLevels = (hNodeHeight_ root) + 1.0
      spacing = { interChild: 10.0, interLevel: height / numberOfLevels }
      layout = (getLayout TidyTree) `treeSetNodeSize_` [ spacing.interChild, spacing.interLevel ]
      laidOutRoot = layout `runLayoutFn_` root
      { xMin, xMax, yMin, yMax } = treeMinMax_ laidOutRoot
      xExtent = abs $ xMax - xMin
      yExtent = abs $ yMax - yMin
      pad n = n * 1.2
      vtreeYOffset = (abs (height - yExtent)) / 2.0
      vtreeXOffset = xMin
      config = {
          layout: Vertical
        , selector
        , linkPath: verticalLink
        , spacing
        , viewbox: [ viewBox vtreeXOffset (-vtreeYOffset) (pad xExtent) (pad yExtent)
                   , preserveAspectRatio $ AspectRatio XMid YMid Meet ]
        , nodeTransform: [ transform [ positionXY ] ]
        , color: d3SchemeCategory10N_ 5.0
        , svg: { width, height }
      }
  Tree.draw config laidOutRoot
-- Snippet_End

-- | Simple radial tree layout (TidyTree, Radial)
-- Snippet_Start
-- Name: TreeRadialDraw
drawTreeRadial :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  TreeJson_ -> Selector D3Selection_ -> m D3Selection_
drawTreeRadial json selector = do
  (Tuple width height) <- liftEffect getWindowWidthHeight
  let root = hierarchyFromJSON_ json
      layout = ((getLayout TidyTree) `treeSetSize_` [ 2.0 * pi, (width / 2.0) - 100.0 ])
                                     `treeSetSeparation_` radialSeparation
      laidOutRoot = layout `runLayoutFn_` root
      { yMax } = treeMinMax_ laidOutRoot
      radialRadius = yMax
      radialExtent = 2.0 * radialRadius
      config = {
          layout: Radial
        , selector
        , linkPath: radialLink treeDatum_.x treeDatum_.y
        , spacing: { interChild: 0.0, interLevel: 0.0 }
        , viewbox: [ viewBox (-radialRadius * 1.2) (-radialRadius * 1.2) (radialExtent * 1.2) (radialExtent * 1.2)
                   , preserveAspectRatio $ AspectRatio XMin YMin Meet ]
        , nodeTransform: [ transform [ radialRotateCommon, radialTranslate, rotateRadialLabels ] ]
        , color: d3SchemeCategory10N_ 6.0
        , svg: { width, height }
      }
  Tree.draw config laidOutRoot
-- Snippet_End

-- Helper functions for transforms
radialRotate :: Number -> String
radialRotate x = show $ (x * 180.0 / pi - 90.0)

radialRotateCommon :: Datum_ -> String
radialRotateCommon d = "rotate(" <> radialRotate (treeDatum_.x d) <> ")"

radialTranslate :: Datum_ -> String
radialTranslate d = "translate(" <> show (treeDatum_.y d) <> ",0)"

rotateRadialLabels :: Datum_ -> String
rotateRadialLabels d =
  "rotate(" <>
    (if (treeDatum_.onRHS Radial d)
    then "180"
    else "0")
    <> ")"

positionXYreflected :: Datum_ -> String
positionXYreflected d = "translate(" <> show (treeDatum_.y d) <> "," <> show (treeDatum_.x d) <> ")"

positionXY :: Datum_ -> String
positionXY d = "translate(" <> show (treeDatum_.x d) <> "," <> show (treeDatum_.y d) <> ")"
