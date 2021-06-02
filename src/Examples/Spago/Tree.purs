module D3.Examples.Spago.Tree where

import D3.Examples.Spago.Attributes
import Prelude

import D3.Attributes.Sugar (classed, dy, fill, fontFamily, fontSize, radius, strokeColor, strokeOpacity, strokeWidth, text, textAnchor, transform, viewBox, x)
import D3.Data.Tree (TreeLayout(..), TreeType(..))
import D3.Data.Types (Datum_, Element(..))
import D3.Examples.Spago.Model (SpagoModel, tree_datum_)
import D3.FFI (descendants_, getLayout, hNodeHeight_, links_, runLayoutFn_, treeMinMax_, treeSetSeparation_, treeSetSize_)
import D3.Interpreter (class D3InterpreterM, append, attach, (<+>))
import D3.Layouts.Hierarchical (radialLink, radialSeparation)
import D3.Selection (Join(..), Keys(..), node)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Math (pi, sqrt)

-- | **************************************************************************************************************
-- | draw the spago graph - only the tree part - as a radial tree
-- | **************************************************************************************************************

-- TODO forall d should be explicit, this script requires certain data structures, fix sig to specify
script :: forall m selection. Bind m => D3InterpreterM selection m => 
  Tuple Number Number -> SpagoModel -> m selection
script _ model@{ tree: Nothing } = do
  attach "div#spagotree"            -- FIXME this is bogus but saves messing about with the Maybe tree in the drawGraph script for now            

script (Tuple width height) model@{ tree: Just (Tuple _ theTree)} = do
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

  theNodes  <- nodes <+> Join {
      element   : Group
    , key       : UseDatumAsKey
    , "data"    : descendants_ theTree
    -- there could be other stylistic stuff here but the transform is key structuring component
    , behaviour : [ transform [ radialRotateCommon, radialTreeTranslate, rotateRadialLabels ] ]
  }

  _ <- theNodes `append` (node Circle [ fill         tree_datum_.colorByGroup 
                                      , radius       (sqrt <<< tree_datum_.loc)
                                      , strokeColor "white"
                                      ])

  _ <- theNodes `append` (node Text [ dy         0.31
                                    , x          (tree_datum_.textX Radial)
                                    , textAnchor (tree_datum_.textAnchor Radial)
                                    , text       tree_datum_.name
                                    , fill       "#555"
                                    ])
                                      
  pure svg



radialRotate :: Number -> String
radialRotate x = show $ (x * 180.0 / pi - 90.0)

radialRotateCommon :: Datum_ -> String
radialRotateCommon d = "rotate(" <> radialRotate (tree_datum_.x d) <> ")"

radialTreeTranslate :: Datum_ -> String
radialTreeTranslate d = "translate(" <> show (tree_datum_.y d) <> ",0)"

rotateRadialLabels :: Datum_ -> String
rotateRadialLabels d = -- TODO replace with nodeIsOnRHS 
  "rotate(" <> 
    (if (tree_datum_.onRHS Radial d) 
    then "180"
    else "0")
    <> ")"
