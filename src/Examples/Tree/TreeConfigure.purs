module D3.Examples.Tree.Configure where

import D3.Layouts.Hierarchical

import D3.Attributes.Instances (Datum)
import D3.Attributes.Sugar (transform, viewBox)
import D3.Examples.Tree.Script (treeScript)
import D3.Examples.Tree.Types (datumIsTreeNode)
import D3.Interpreter (class D3InterpreterM)
import D3.Interpreter.D3 (runD3M)
import D3.Interpreter.MetaTree (MetaTreeNode, ScriptTree(..), runMetaTree, scriptTreeToJSON)
import D3.Interpreter.String (runPrinter)
import D3.Layouts.Hierarchical as H
import D3.Layouts.Hierarchical.Types (TreeLayout(..), TreeType(..))
import D3.Scales (d3SchemeCategory10_)
import D3.Selection (D3Selection_)
import Data.Map (toUnfoldable)
import Data.Tuple (Tuple(..), fst, snd)
import Debug (spy, trace)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Math (pi)
import Prelude (class Bind, Unit, bind, discard, negate, pure, show, unit, (>=), (==), ($), (+), (<), (-), (*), (<>), (/))

-- TODO move this to a library, it really only needs the params for runPrinter to be completely generic
-- | Evaluate the tree drawing script in the "printer" monad which will render it as a string
-- | rather than drawing SVG or Canvas. In principle this could be basis for compiling to JS D3 script
printTree :: forall v. Model String v -> Aff Unit
printTree treeModel = liftEffect $ do
  log "Tree example"
  widthHeight <- getWindowWidthHeight
  printedScript <- runPrinter  (configureAndRunScript widthHeight treeModel) "Tree Script"
  log $ snd printedScript
  log $ fst printedScript
  pure unit


getMetaTreeJSON :: forall v. Model String v -> Aff TreeJson_
getMetaTreeJSON treeModel = liftEffect $ do
  log "Getting meta-tree for radial tree example"
  widthHeight <- getWindowWidthHeight
  metaScript <- runMetaTree (configureAndRunScript widthHeight treeModel) -- no need for actual widthHeight in metaTree
  let (ScriptTree _ treeMap links) = snd metaScript
      (_ :: Array (Tuple Int MetaTreeNode)) = spy "script map" $ toUnfoldable treeMap
      (_ :: Array (Tuple Int Int))          = spy "link map" $ links
      treeified                             = spy "script tree" $ snd metaScript
  pure $ scriptTreeToJSON treeified

-- | Evaluate the tree drawing script in the "d3" monad which will render it in SVG
-- | TODO specialize runD3M so that this function isn't necessary
drawTree :: forall v. Model String v -> Aff Unit
drawTree treeModel = liftEffect $ do
  widthHeight <- getWindowWidthHeight
  (_ :: Tuple D3Selection_ Unit) <- runD3M (configureAndRunScript widthHeight treeModel)
  pure unit


-- | configure function which enables treeScript to be run for different layouts - WIP
configureAndRunScript :: forall m v selection. 
  Bind m => 
  D3InterpreterM selection m => 
  Tuple Number Number -> H.Model String v -> m selection
configureAndRunScript (Tuple width height ) model = 
  treeScript { spacing, selector, viewbox, tree: laidOutRoot_, linkPath, nodeTransform, color, textDirection } model
  where
    columns                    = 3.0  -- 3 columns, set in the grid CSS in index.html
    gap                        = 10.0 -- 10px set in the grid CSS in index.html
    svgWidth                   = spy "svgWidth" $ ((width - ((columns - 1.0) * gap)) / columns)
    svgHeight                  = spy "svgHeight" $ height / 2.0 -- 2 rows

    numberOfLevels             = spy "number of Levels in tree: " $ (hNodeHeight_ model.root_) + 1.0
    spacing = spy "spacing: " $ 
      case model.treeType, model.treeLayout of
        Dendrogram, Horizontal -> { interChild: 10.0, interLevel: svgWidth / numberOfLevels }
        Dendrogram, Vertical   -> { interChild: 10.0, interLevel: svgHeight / numberOfLevels }
        Dendrogram, Radial     -> { interChild: 0.0,  interLevel: 0.0} -- not sure this is used in radial case

        TidyTree, Horizontal   -> { interChild: 10.0, interLevel: svgWidth / numberOfLevels }
        TidyTree, Vertical     -> { interChild: 10.0, interLevel: svgHeight / numberOfLevels}
        TidyTree, Radial       -> { interChild: 0.0,  interLevel: 0.0} -- not sure this is used in radial case

    layout = 
      case model.treeType, model.treeLayout of
        Dendrogram, Horizontal -> (initCluster_ unit)   `treeSetNodeSize_` [ spacing.interChild, spacing.interLevel ]
        Dendrogram, Vertical   -> (initCluster_ unit)   `treeSetNodeSize_` [ spacing.interChild, spacing.interLevel ]
        Dendrogram, Radial     -> ((initCluster_ unit)  `treeSetSize_`     [ 2.0 * pi, (svgWidth / 2.0) - 100.0 ]) -- note that in radial case doesn't seem to be initialized by d3.cluster
                                                        `treeSetSeparation_` radialSeparation
        TidyTree  , Horizontal -> (initTree_ unit)      `treeSetNodeSize_` [ spacing.interChild, spacing.interLevel ]
        TidyTree  , Vertical   -> (initTree_ unit)      `treeSetNodeSize_` [ spacing.interChild, spacing.interLevel ]
        TidyTree  , Radial     -> ((initTree_ unit)     `treeSetSize_`     [ 2.0 * pi, (svgHeight / 2.0) - 50.0 ])
                                                        `treeSetSeparation_` radialSeparation

    laidOutRoot_ =
      layout `treeSetRoot_` model.root_

    { xMin, xMax, yMin, yMax } = trace { treeType: model.treeType, layout: model.treeLayout } \_ -> spy "MinMax" $ treeMinMax_ laidOutRoot_
    xExtent = xMax - xMin -- ie if tree spans from -50 to 200, it's extent is 250
    yExtent = yMax - yMin -- ie if tree spans from -50 to 200, it's extent is 250

    viewbox =
      case model.treeType, model.treeLayout of
        Dendrogram, Horizontal -> [ viewBox (-10.0) (svgHeight - xExtent / 2.0) yExtent xExtent ] -- x and y are reversed in horizontal layouts
        Dendrogram, Vertical   -> [ viewBox xMin 0.0 xExtent yExtent ]
        Dendrogram, Radial     -> [ viewBox (-svgWidth/2.0) (-svgHeight/2.0) svgWidth svgHeight ]

        TidyTree  , Horizontal -> [ viewBox (-10.0) (svgHeight - xExtent / 2.0) yExtent xExtent ] -- x and y are reversed in horizontal layouts
        TidyTree  , Vertical   -> [ viewBox xMin 0.0 xExtent yExtent ]
        TidyTree  , Radial     -> [ viewBox (-svgWidth/2.0) (-svgHeight/2.0) svgWidth svgHeight ]

      
    linkPath =
      case model.treeType, model.treeLayout of
        Dendrogram, Horizontal -> horizontalClusterLink spacing.interLevel
        Dendrogram, Vertical   -> verticalClusterLink   spacing.interLevel 
        Dendrogram, Radial     -> radialLink _.x _.y

        TidyTree, Horizontal   -> horizontalLink
        TidyTree, Vertical     -> verticalLink
        TidyTree, Radial       -> radialLink _.x _.y

    selector =
      case model.treeType, model.treeLayout of
        Dendrogram, Horizontal -> "div#hdendro"
        Dendrogram, Vertical   -> "div#vdendro"
        Dendrogram, Radial     -> "div#rdendro"

        TidyTree, Horizontal   -> "div#htree"
        TidyTree, Vertical     -> "div#vtree"
        TidyTree, Radial       -> "div#rtree"

    nodeTransform =
      case model.treeType, model.treeLayout of
        Dendrogram, Horizontal -> [ transform [ positionXYreflected ] ]
        Dendrogram, Vertical   -> [ transform [ positionXY ] ]
        Dendrogram, Radial     -> [ transform [ radialRotateCommon, radialTranslate, rotateRadialLabels ] ]

        TidyTree, Horizontal   -> [ transform [ positionXYreflected ] ]
        TidyTree, Vertical     -> [ transform [ positionXY ] ]
        TidyTree, Radial       -> [ transform [ radialRotateCommon, radialTranslate, rotateRadialLabels ] ]

    color = d3SchemeCategory10_ $
      case model.treeType, model.treeLayout of
        Dendrogram, Horizontal -> 1.0
        Dendrogram, Vertical   -> 2.0
        Dendrogram, Radial     -> 3.0

        TidyTree, Horizontal   -> 4.0
        TidyTree, Vertical     -> 5.0
        TidyTree, Radial       -> 6.0

    textDirection = 
      if model.treeLayout == Radial
      then \d -> hasChildren_ d == nodeIsOnRHS d
      else hasChildren_

-- | some small functions that are used to parameterize the differing tree layouts
-- | these are passed in to the Script as part of the configuration
radialRotate :: Number -> String
radialRotate x = show $ (x * 180.0 / pi - 90.0)

radialRotateCommon :: forall d v. D3HierarchicalNode d v -> String
radialRotateCommon (D3HierarchicalNode d) = "rotate(" <> radialRotate d.x <> ")"

radialTranslate :: forall d v. D3HierarchicalNode d v -> String
radialTranslate (D3HierarchicalNode d) = "translate(" <> show d.y <> ",0)"

rotateRadialLabels :: forall d v. D3HierarchicalNode d v -> String
rotateRadialLabels (D3HierarchicalNode d) = -- TODO replace with nodeIsOnRHS 
  "rotate(" <> if d.x >= pi 
  then "180" <> ")" 
  else "0" <> ")"

nodeIsOnRHS :: Datum -> Boolean
nodeIsOnRHS d = node.x < pi
  where (D3HierarchicalNode node) = datumIsTreeNode d
