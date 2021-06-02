module D3.Examples.Tree.Configure where

import Utility

import D3.Attributes.Sugar (transform, viewBox)
import D3.Data.Tree (TreeJson_, TreeLayout(..), TreeModel, TreeType(..))
import D3.Data.Types (D3Selection_, Datum_)
import D3.Examples.Spago.Model (tree_datum_)
import D3.Examples.Tree.Script (FlareTreeNode)
import D3.Examples.Tree.Script (script) as Tree
import D3.FFI (getLayout, hNodeHeight_, hierarchyFromJSON_, runLayoutFn_, treeMinMax_, treeSetNodeSize_, treeSetSeparation_, treeSetSize_)
import D3.Interpreter (class D3InterpreterM)
import D3.Interpreter.D3 (runD3M)
import D3.Interpreter.MetaTree (D3GrammarNode, ScriptTree(..), runMetaTree, scriptTreeToJSON)
import D3.Interpreter.String (runPrinter)
import D3.Layouts.Hierarchical (horizontalClusterLink, horizontalLink, radialLink, radialSeparation, verticalClusterLink, verticalLink)
import D3.Scales (d3SchemeCategory10N_)
import Data.Map (toUnfoldable)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Math (pi)
import Prelude (class Bind, Unit, bind, discard, negate, pure, show, unit, ($), (*), (+), (-), (/), (<>), (==))

-- TODO move this to a library, it really only needs the params for runPrinter to be completely generic
-- | Evaluate the tree drawing script in the "printer" monad which will render it as a string
-- | rather than drawing SVG or Canvas. In principle this could be basis for compiling to JS D3 script
printTree :: TreeModel -> Aff Unit
printTree treeModel = liftEffect $ do
  log "Tree example"
  widthHeight   <- getWindowWidthHeight
  printedScript <- runPrinter  (configureAndRunScript widthHeight treeModel) "Tree Script"
  log $ snd printedScript
  log $ fst printedScript
  pure unit


getMetaTreeJSON :: TreeModel -> Aff TreeJson_
getMetaTreeJSON treeModel = liftEffect $ do
  log "Getting meta-tree for radial tree example"
  widthHeight <- getWindowWidthHeight
  metaScript <- runMetaTree (configureAndRunScript widthHeight treeModel) -- no need for actual widthHeight in metaTree
  let (ScriptTree _ treeMap links) = snd metaScript
      (_ :: Array (Tuple Int D3GrammarNode)) = toUnfoldable treeMap
      (_ :: Array (Tuple Int Int))          = links
      treeified                             = snd metaScript
  pure $ scriptTreeToJSON treeified

-- | Evaluate the tree drawing script in the "d3" monad which will render it in SVG
-- | TODO specialize runD3M so that this function isn't necessary
drawTree :: TreeModel -> Aff Unit
drawTree treeModel = liftEffect $ do
  widthHeight <- getWindowWidthHeight
  (_ :: Tuple D3Selection_ Unit) <- runD3M (configureAndRunScript widthHeight treeModel)
  pure unit


-- | configure function which enables Tree.script to be run for different layouts - WIP
configureAndRunScript :: forall m selection. 
  Bind m => 
  D3InterpreterM selection m => 
  Tuple Number Number -> TreeModel -> m selection
configureAndRunScript (Tuple width height ) model = 
  Tree.script { spacing, selector, viewbox, linkPath, nodeTransform, color, layout: model.treeLayout, svg } laidOutRoot_
  where
    columns = 3.0  -- 3 columns, set in the grid CSS in index.html
    gap     = 10.0 -- 10px set in the grid CSS in index.html
    svg     = { width : ((width - ((columns - 1.0) * gap)) / columns)
              , height: height / 2.0 } -- 2 rows

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

    layout = 
      if model.treeLayout == Radial
      then ((getLayout model.treeType)  `treeSetSize_`       [ 2.0 * pi, (svg.width / 2.0) - 100.0 ]) 
                                        `treeSetSeparation_` radialSeparation
      else
        (getLayout model.treeType)   `treeSetNodeSize_`   [ spacing.interChild, spacing.interLevel ]

    laidOutRoot_ :: FlareTreeNode
    laidOutRoot_ = layout `runLayoutFn_` root

    { xMin, xMax, yMin, yMax } = treeMinMax_ laidOutRoot_
    xExtent = xMax - xMin -- ie if tree spans from -50 to 200, it's extent is 250
    yExtent = yMax - yMin -- ie if tree spans from -50 to 200, it's extent is 250

    viewbox =
      case model.treeType, model.treeLayout of
        Dendrogram, Horizontal -> [ viewBox (-10.0) (svg.height - xExtent / 2.0) yExtent xExtent ] -- x and y are reversed in horizontal layouts
        Dendrogram, Vertical   -> [ viewBox xMin 0.0 xExtent yExtent ]
        Dendrogram, Radial     -> [ viewBox (-svg.width/2.0) (-svg.height/2.0) svg.width svg.height ]

        TidyTree  , Horizontal -> [ viewBox (-10.0) (svg.height - xExtent / 2.0) yExtent xExtent ] -- x and y are reversed in horizontal layouts
        TidyTree  , Vertical   -> [ viewBox xMin 0.0 xExtent yExtent ]
        TidyTree  , Radial     -> [ viewBox (-svg.width/2.0) (-svg.height/2.0) svg.width svg.height ]

      
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
radialRotateCommon d = "rotate(" <> radialRotate (tree_datum_.x d) <> ")"

radialTranslate :: Datum_ -> String
radialTranslate d = "translate(" <> show (tree_datum_.y d) <> ",0)"

rotateRadialLabels :: Datum_ -> String
rotateRadialLabels d = -- TODO replace with nodeIsOnRHS 
  "rotate(" <> 
    (if (tree_datum_.onRHS Radial d) 
    then "180"
    else "0")
    <> ")"

positionXYreflected :: Datum_ -> String  
positionXYreflected d = "translate(" <> show (tree_datum_.y d) <> "," <> show (tree_datum_.x d) <>")"

positionXY :: Datum_ -> String  
positionXY d = "translate(" <> show (tree_datum_.x d) <> "," <> show (tree_datum_.y d) <>")"