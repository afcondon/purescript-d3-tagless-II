module D3.Viz.Tree.Configure where

import D3.Viz.Tree.Model

import PSD3.Internal.Attributes.Sugar (AlignAspectRatio_X(..), AlignAspectRatio_Y(..), AspectRatioPreserve(..), AspectRatioSpec(..), preserveAspectRatio, transform, viewBox)
import PSD3.Data.Tree (TreeJson_, TreeLayout(..), TreeLayoutFn_, TreeModel, TreeType(..))
import PSD3.Internal.Types (D3Selection_, Datum_, Selector)
import D3.Viz.Tree.Draw (draw) as Tree
import D3.Viz.Tree.Draw (treeDatum_)
import PSD3.Internal.FFI (getLayout, hNodeHeight_, hierarchyFromJSON_, runLayoutFn_, treeMinMax_, treeSetNodeSize_, treeSetSeparation_, treeSetSize_)
import PSD3.Internal.Hierarchical (horizontalClusterLink, horizontalLink, radialLink, radialSeparation, verticalClusterLink, verticalLink)
import PSD3.Internal.Scales.Scales (d3SchemeCategory10N_)
import PSD3.Capabilities.Selection (class SelectionM)
import PSD3.Interpreter.MetaTree (D3GrammarNode, ScriptTree(..), runMetaTree, scriptTreeToJSON)
import PSD3.Interpreter.String (runPrinter)
import PSD3.Interpreter.D3 (runD3M)
import Data.Map (toUnfoldable)
import Data.Number (pi, abs)
import Data.Tuple (Tuple(..), snd)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Prelude (class Bind, Unit, bind, negate, pure, show, unit, ($), (*), (+), (-), (/), (<>), (==))
import Utility (getWindowWidthHeight)

-- TODO move this to a library, it really only needs the params for runPrinter to be completely generic
-- | Evaluate the tree drawing script in the "printer" monad which will render it as a string
-- | rather than drawing SVG or Canvas. In principle this could be basis for compiling to JS D3 script
getPrintTree :: TreeModel -> Aff String
getPrintTree treeModel = liftEffect $ do
  widthHeight   <- getWindowWidthHeight
  printedScript <- runPrinter  (configureAndRunScript widthHeight treeModel "Printer Interpreter Root> ") "Tree Script"
  pure $ snd printedScript


getMetaTreeJSON :: TreeModel -> Aff TreeJson_
getMetaTreeJSON treeModel = liftEffect $ do
  widthHeight <- getWindowWidthHeight
  metaScript <- runMetaTree (configureAndRunScript widthHeight treeModel "MetaTree root> ") -- no need for actual widthHeight in metaTree
  let (ScriptTree _ treeMap links) = snd metaScript
      (_ :: Array (Tuple Int D3GrammarNode)) = toUnfoldable treeMap
      (_ :: Array (Tuple Int Int))          = links
      treeified                             = snd metaScript
  pure $ scriptTreeToJSON treeified

-- | Evaluate the tree drawing script in the "d3" monad which will render it in SVG
-- TODO specialize runD3M so that this function isn't necessary
drawTree :: forall selection. TreeModel -> Selector selection -> Aff Unit
drawTree treeModel selector = liftEffect $ do
  widthHeight <- getWindowWidthHeight
  (_ :: Tuple D3Selection_ Unit) <- runD3M (configureAndRunScript widthHeight treeModel selector)
  pure unit


-- | configure function which enables Tree.script to be run for different layouts - WIP
configureAndRunScript :: forall m selection. 
  Bind m => 
  SelectionM selection m => 
  Tuple Number Number -> TreeModel -> Selector selection -> m selection
configureAndRunScript (Tuple width height ) model selector = 
  Tree.draw { spacing, viewbox, selector, linkPath, nodeTransform, color, layout: model.treeLayout, svg } laidOutRoot_
  where
    svg     = { width, height }

    root    = hierarchyFromJSON_ model.json
    numberOfLevels = (hNodeHeight_ root) + 1.0
    spacing =
      case model.treeType, model.treeLayout of
        Dendrogram, Horizontal -> { interChild: 10.0, interLevel: svg.width / numberOfLevels }
        Dendrogram, Vertical   -> { interChild: 10.0, interLevel: svg.height / numberOfLevels }
        Dendrogram, Radial     -> { interChild: 0.0,  interLevel: 0.0} -- not sure this is used in radial case

        TidyTree, Horizontal   -> { interChild: 10.0, interLevel: svg.width / numberOfLevels }
        TidyTree, Vertical     -> { interChild: 10.0, interLevel: svg.height / numberOfLevels}
        TidyTree, Radial       -> { interChild: 0.0,  interLevel: 0.0} -- not sure this is used in radial case

    layout :: TreeLayoutFn_
    layout = 
      if model.treeLayout == Radial
      then ((getLayout model.treeType)  `treeSetSize_`       [ 2.0 * pi, (svg.width / 2.0) - 100.0 ]) 
                                        `treeSetSeparation_` radialSeparation
      else
        (getLayout model.treeType)   `treeSetNodeSize_`   [ spacing.interChild, spacing.interLevel ]

    laidOutRoot_ :: FlareTreeNode
    laidOutRoot_ = layout `runLayoutFn_` root

    { xMin, xMax, yMin, yMax } = treeMinMax_ laidOutRoot_
    xExtent = abs $ xMax - xMin -- ie if tree spans from -50 to 200, it's extent is 250
    yExtent = abs $ yMax - yMin -- ie if tree spans from -50 to 200, it's extent is 250
    radialRadius = yMax  -- on the radial tree the y is the distance from origin, ie yMax == radius
    radialExtent       = 2.0 * radialRadius
    pad n = n * 1.2
    vtreeYOffset = (abs (height - yExtent)) / 2.0
    vtreeXOffset = xMin -- the left and right sides might be different so (xExtent / 2) would not necessarily be right
    htreeYOffset = xMin

    viewbox =
      case model.treeType, model.treeLayout of
        _, Vertical   -> [ viewBox vtreeXOffset (-vtreeYOffset) (pad xExtent) (pad yExtent) -- 
                         , preserveAspectRatio $ AspectRatio XMid YMid Meet ]
        _, Horizontal -> [ viewBox (-xExtent * 0.1) (pad htreeYOffset) (pad yExtent) (pad xExtent)
                         , preserveAspectRatio $ AspectRatio XMin YMid Meet ] -- x and y are reversed in horizontal layouts
        _, Radial     -> [ viewBox (-radialRadius * 1.2) (-radialRadius * 1.2)  (radialExtent * 1.2)    (radialExtent * 1.2)
                         , preserveAspectRatio $ AspectRatio XMin YMin Meet ]
      
    linkPath =
      case model.treeType, model.treeLayout of
        Dendrogram, Horizontal -> horizontalClusterLink spacing.interLevel
        Dendrogram, Vertical   -> verticalClusterLink   spacing.interLevel 
        Dendrogram, Radial     -> radialLink treeDatum_.x treeDatum_.y

        TidyTree, Horizontal   -> horizontalLink
        TidyTree, Vertical     -> verticalLink
        TidyTree, Radial       -> radialLink treeDatum_.x treeDatum_.y

    nodeTransform =
      case model.treeType, model.treeLayout of
        Dendrogram, Horizontal -> [ transform [ positionXYreflected ] ]
        Dendrogram, Vertical   -> [ transform [ positionXY ] ]
        Dendrogram, Radial     -> [ transform [ radialRotateCommon, radialTranslate, rotateRadialLabels ] ]

        TidyTree, Horizontal   -> [ transform [ positionXYreflected ] ]
        TidyTree, Vertical     -> [ transform [ positionXY ] ]
        TidyTree, Radial       -> [ transform [ radialRotateCommon, radialTranslate, rotateRadialLabels ] ]

    color = d3SchemeCategory10N_ $
      case model.treeType, model.treeLayout of
        Dendrogram, Horizontal -> 1.0
        Dendrogram, Vertical   -> 2.0
        Dendrogram, Radial     -> 3.0

        TidyTree, Horizontal   -> 4.0
        TidyTree, Vertical     -> 5.0
        TidyTree, Radial       -> 6.0

radialRotate :: Number -> String
radialRotate x = show $ (x * 180.0 / pi - 90.0)

radialRotateCommon :: Datum_ -> String
radialRotateCommon d = "rotate(" <> radialRotate (treeDatum_.x d) <> ")"

radialTranslate :: Datum_ -> String
radialTranslate d = "translate(" <> show (treeDatum_.y d) <> ",0)"

rotateRadialLabels :: Datum_ -> String
rotateRadialLabels d = -- TODO replace with nodeIsOnRHS 
  "rotate(" <> 
    (if (treeDatum_.onRHS Radial d) 
    then "180"
    else "0")
    <> ")"

positionXYreflected :: Datum_ -> String  
positionXYreflected d = "translate("  <> show (treeDatum_.y d) <> "," <> show (treeDatum_.x d) <>")"

positionXY :: Datum_ -> String  
positionXY d = "translate(" <> show (treeDatum_.x d) <> "," <> show (treeDatum_.y d) <>")"