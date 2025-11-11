module D3.Viz.MetaTree where

import PSD3.Internal.Attributes.Sugar
import Prelude

import PSD3.Attributes (DatumFn(..), DatumFnI(..), unwrapDatumFn)
import PSD3.Data.Tree (TreeModel, TreeType(..))
import PSD3.Internal.Types (D3Selection_, Datum_, Element(..))
import D3.Viz.MetaTree.Model (MetaTreeNode)
import D3.Viz.MetaTree.Unsafe (coerceToTreeNode, unboxD3TreeNode)
import PSD3.Internal.FFI (descendants_, getLayout, hNodeHeight_, hierarchyFromJSON_, keyIsID_, links_, runLayoutFn_, treeMinMax_, treeSetNodeSize_)
import PSD3.Internal.Hierarchical (verticalLink)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach, setAttributes, simpleJoin)
import PSD3.Interpreter.D3 (runD3M)
import Data.Number (abs)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Utility (getWindowWidthHeight)

datum_ :: { 
  param1     :: Datum_ -> String
, positionXY :: Datum_ -> String
, symbol     :: Datum_ -> String
, x          :: Datum_ -> Number
, y          :: Datum_ -> Number
}
datum_ = {
    x     : _.x <<< unboxD3TreeNode <<< coerceToTreeNode
  , y     : _.y <<< unboxD3TreeNode <<< coerceToTreeNode
-- now the fields which are in the original data object, embedded in this tree object
  , symbol: _.data.symbol <<< unboxD3TreeNode <<< coerceToTreeNode
  , param1: _.data.param1 <<< unboxD3TreeNode <<< coerceToTreeNode
  , positionXY: \d -> "translate(" <> show (datum_.x d) <> "," <> show (datum_.y d) <>")"
}

-- | Evaluate the tree drawing script in the "d3" monad which will render it in SVG
-- | TODO specialize runD3M so that this function isn't necessary
drawTree :: TreeModel -> Aff Unit
drawTree treeModel = liftEffect $ do
  widthHeight <- getWindowWidthHeight
  let tree = hierarchyFromJSON_ treeModel.json
  (_ :: Tuple D3Selection_ Unit) <- runD3M (draw widthHeight tree)
  pure unit

-- Snippet_Start
-- Name: MetaTreeDraw
-- | "script" to produce the documentation-ready rendering of another script's structure
-- | (could also be the basis for graphical editor of scripts / trees)
draw :: forall m selection d. Bind m => SelectionM selection m =>
  Tuple Number Number -> MetaTreeNode -> m (selection d)
draw (Tuple w h) tree = do
  let 
    -- configure dimensions
    numberOfLevels             = (hNodeHeight_ tree) + 1.0
    spacing                    = { interChild: (w/5.0), interLevel: h / numberOfLevels}
    layoutFn                   = (getLayout TidyTree) `treeSetNodeSize_` [ spacing.interChild, spacing.interLevel ]
    laidOutRoot_               = layoutFn `runLayoutFn_` tree
    { xMin, xMax, yMin, yMax } = treeMinMax_ laidOutRoot_
    xExtent = abs $ xMax - xMin -- ie if tree spans from -50 to 200, it's extent is 250
    yExtent = abs $ yMax - yMin -- ie if tree spans from -50 to 200, it's extent is 250
    vtreeYOffset = (abs (h - yExtent)) / 2.0
    vtreeXOffset = pad xMin -- the left and right sides might be different so (xExtent / 2) would not necessarily be right
    pad n = n * 1.2


  -- "script"
  root       <- attach ".svg-container"                           
  svg        <- appendTo root Svg [ viewBox vtreeXOffset (-vtreeYOffset) (pad xExtent) (pad yExtent)
                                  , preserveAspectRatio $ AspectRatio XMin YMid Meet 
                                  , width w
                                  , height h
                                  , classed "metatree" ]
  container  <- appendTo svg  Group [ fontFamily      "sans-serif"
                                    , fontSize        18.0
                                    ]
  links      <- appendTo container Group [ classed "links"]
  nodes      <- appendTo container Group [ classed "nodes"]

  theLinks_  <- simpleJoin links Path (links_ tree) keyIsID_
  setAttributes theLinks_ 
    [ strokeWidth 1.5, strokeColor "black", strokeOpacity 0.4, fill "none", verticalLink]

  nodeJoin_  <- simpleJoin nodes Group (descendants_ tree) keyIsID_
  setAttributes nodeJoin_ [ transform [ unwrapDatumFn (DatumFn datum_.positionXY) ] ]


  theNodes <- appendTo nodeJoin_
                Circle  [ fill         "blue"
                        , radius       20.0
                        , strokeColor "white"
                        , strokeWidth 3.0
                        ]

  labelsWhite <- appendTo nodeJoin_
                Text  [ x          0.0
                      , y          3.0
                      , textAnchor "middle"
                      , text       (DatumFn datum_.symbol)
                      , fill       "white"
                      ]

  labelsGray <- appendTo nodeJoin_
                Text  [ x          22.0
                      , y          3.0
                      , textAnchor "start"
                      , text       (DatumFn datum_.param1)
                      , fill       "gray"
                      ]
                            
  pure svg
-- Snippet_End