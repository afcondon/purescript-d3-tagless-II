module D3.Examples.Spago.Tree where

import D3.Attributes.Sugar (classed, dy, fill, fontFamily, fontSize, radius, strokeColor, strokeOpacity, strokeWidth, text, textAnchor, transform, viewBox, x)
import D3.Examples.Spago.Model 
import D3.Data.Types (Datum_, Element(..))
import D3.Data.Tree (TreeType(..))
import D3.Examples.Tree.Configure (datumIsTreeNode)
import D3.FFI (descendants_, getLayout, hNodeHeight_, hasChildren_, links_, runLayoutFn_, treeMinMax_, treeSetSeparation_, treeSetSize_)
import D3.Interpreter (class D3InterpreterM, append, attach, (<+>))
import D3.Layouts.Hierarchical (radialLink, radialSeparation)
import D3.Node (D3_SimulationNode(..), D3_TreeNode(..), D3_XY, NodeID)
import D3.Scales (d3SchemeCategory10N_)
import D3.Selection (Join(..), Keys(..), node)
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Math (pi)
import Math (sqrt) as Math
import Prelude (class Bind, bind, negate, pure, show, ($), (*), (+), (-), (/), (<), (<>), (==), (>=))
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)


-- | **************************************************************************************************************
-- | draw the spago graph - only the tree part - as a radial tree
-- | **************************************************************************************************************

-- TODO forall d should be explicit, this script requires certain data structures, fix sig to specify
treeScript :: forall m selection. Bind m => D3InterpreterM selection m => 
  Tuple Number Number -> SpagoModel -> m selection
treeScript _ model@{ tree: Nothing } = do
  attach "div#spagotree"            -- FIXME this is bogus but saves messing about with the Maybe tree in the drawGraph script for now            

treeScript (Tuple width height) model@{ tree: Just (Tuple _ theTree)} = do
  let 
    -- configure dimensions
    columns                    = 2.0  -- 3 columns, set in the grid CSS in index.html
    rows                       = 1.0
    gap                        = 10.0 -- 10px set in the grid CSS in index.html
    svgWH                      = { width : ((width - ((columns - 1.0) * gap)) / columns)
                                 , height: height / rows }
    numberOfLevels             = (hNodeHeight_ theTree) + 1.0
    spacing                    = { interChild: 120.0, interLevel: height / numberOfLevels}
    layoutFn                   = ((getLayout TidyTree) `treeSetSize_`       [ 2.0 * pi, width ]) 
                                                       `treeSetSeparation_` radialSeparation
    laidOutRoot_               = layoutFn `runLayoutFn_` theTree
    { xMin, xMax, yMin, yMax } = treeMinMax_ laidOutRoot_
    xExtent                    = xMax - xMin -- ie if tree spans from -50 to 200, it's extent is 250
    yExtent                    = yMax - yMin -- ie if tree spans from -50 to 200, it's extent is 250


  -- "script"
  root       <- attach "div#spagotree"                           
  svg        <- root `append` (node Svg  [ viewBox (-width / 2.0) (-height / 2.0) width height ] )          
  container  <- svg  `append` (node Group [ fontFamily      "sans-serif"
                                          , fontSize        18.0
                                          ])
  links      <- container `append` (node Group [ classed "links"])
  nodes      <- container `append` (node Group [ classed "nodes"])

  theLinks_  <- links <+> Join {
      element   : Path
    , key       : UseDatumAsKey
    , "data"    : links_ theTree
    , behaviour : [ strokeWidth   1.5
                  , strokeColor   "black"
                  , strokeOpacity 0.4
                  , fill          "none"
                  , radialLink _.x _.y
                  ]
  }

  nodeJoin_  <- nodes <+> Join {
      element   : Group
    , key       : UseDatumAsKey
    , "data"    : descendants_ theTree
    -- there could be other stylistic stuff here but the transform is key structuring component
    , behaviour : [ transform [ radialRotateCommon, radialTreeTranslate, rotateRadialLabels ] ]
  }

  theNodes <- nodeJoin_ `append` 
                (node Circle  [ fill         (colorByGroupTree model.id2PackageIDMap)
                              , radius       (chooseRadiusTree model.path2LOCMap)
                              , strokeColor "white"
                              ])

  theLabels <- nodeJoin_ `append`
                (node Text  [ dy         0.31
                            , x          (\datum -> if textDirection datum then 6.0 else (-6.0))
                            , textAnchor (\datum -> if textDirection datum then "start" else "end")
                            , text       labelName
                            , fill       "#555"
                            ])
                            
  pure svg

radialRotate :: Number -> String
radialRotate x = show $ (x * 180.0 / pi - 90.0)

radialRotateCommon :: forall r. D3_TreeNode (D3_XY + r) -> String
radialRotateCommon (D3TreeNode d) = "rotate(" <> radialRotate d.x <> ")"

radialTreeTranslate :: forall r. D3_TreeNode (D3_XY + r) -> String
radialTreeTranslate (D3TreeNode d) = "translate(" <> show d.y <> ",0)"

rotateRadialLabels :: forall r. D3_TreeNode (D3_XY + r) -> String
rotateRadialLabels (D3TreeNode d) = -- TODO replace with nodeIsOnRHS 
  "rotate(" <> if d.x >= pi 
  then "180" <> ")" 
  else "0" <> ")"

nodeIsOnRHS :: Datum_ -> Boolean
nodeIsOnRHS d = node.x < pi
  where (D3TreeNode node) = datumIsTreeNode d

textDirection :: Datum_ -> Boolean
textDirection = \d -> hasChildren_ d == nodeIsOnRHS d

labelName :: Datum_ -> String
labelName d = node."data".name
  where node = unsafeCoerce d

colorByGroupTree :: M.Map NodeID NodeID -> Datum_ -> String
colorByGroupTree packageMap datum = d3SchemeCategory10N_ (toNumber $ fromMaybe 0 packageID)
  where
    (D3SimNode d) = unsafeCoerce datum
    packageID     = M.lookup d.data.id packageMap

chooseRadiusTree :: Map String Number -> Datum_ -> Number
chooseRadiusTree locMap datum = do
  let (D3SimNode d) = unsafeCoerce datum -- TODO unsafe because despite all the row types this still cashes out to a simnode not a treenode here which must be fixed
  case d.data.nodetype of
    IsModule   -> Math.sqrt (fromMaybe 10.0 $ M.lookup d.data.path locMap)
    IsPackage -> packageRadius

