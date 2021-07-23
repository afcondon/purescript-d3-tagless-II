module D3.Examples.MetaTree where

import D3.Attributes.Sugar 
import D3.Data.Tree (TreeModel, TreeType(..))
import D3.Data.Types (D3Selection_, Datum_, Element(..))
import D3.Examples.MetaTree.Model (MetaTreeNode)
import D3.Examples.MetaTree.Unsafe (unboxD3TreeNode)
import D3.FFI (descendants_, getLayout, hNodeHeight_, hierarchyFromJSON_, links_, runLayoutFn_, treeMinMax_, treeSetNodeSize_)
import D3.Layouts.Hierarchical (verticalLink)
import D3.Selection (Join(..), node)
import D3Tagless.Capabilities (class SelectionM, appendElement, attach, (<+>))
import D3Tagless.Instance.Selection (runD3M)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Prelude (class Bind, Unit, bind, negate, pure, show, unit, ($), (*), (+), (-), (/), (<>))
import Utility (getWindowWidthHeight)
import Math

datum_ :: { 
  param1     :: Datum_ -> String
, positionXY :: Datum_ -> String
, symbol     :: Datum_ -> String
, x          :: Datum_ -> Number
, y          :: Datum_ -> Number
}
datum_ = {
    x     : (\d -> (unboxD3TreeNode d).x)
  , y     : (\d -> (unboxD3TreeNode d).y)
-- now the fields which are in the original data object, embedded in this tree object
  , symbol: (\d -> (unboxD3TreeNode d).data.symbol)
  , param1: (\d -> (unboxD3TreeNode d).data.param1)
  , positionXY: (\d -> "translate(" <> show (datum_.x d) <> "," <> show (datum_.y d) <>")")
}

-- | Evaluate the tree drawing script in the "d3" monad which will render it in SVG
-- | TODO specialize runD3M so that this function isn't necessary
drawTree :: TreeModel -> Aff Unit
drawTree treeModel = liftEffect $ do
  widthHeight <- getWindowWidthHeight
  let tree = hierarchyFromJSON_ treeModel.json
  (_ :: Tuple D3Selection_ Unit) <- runD3M (treeScript widthHeight tree)
  pure unit

-- | "script" to produce the documentation-ready rendering of another script's structure
-- | (could also be the basis for graphical editor of scripts / trees)
treeScript :: forall m selection. Bind m => SelectionM selection m => 
  Tuple Number Number -> MetaTreeNode -> m selection
treeScript (Tuple w h) tree = do
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
  svg        <- root `appendElement` (node Svg  [ viewBox vtreeXOffset (-vtreeYOffset) (pad xExtent) (pad yExtent)
                                                , preserveAspectRatio $ AspectRatio XMin YMid Meet 
                                                , width w
                                                , height h
                                                , classed "metatree" ] )
  container  <- svg  `appendElement` (node Group [ fontFamily      "sans-serif"
                                          , fontSize        18.0
                                          ])
  links      <- container `appendElement` (node Group [ classed "links"])
  nodes      <- container `appendElement` (node Group [ classed "nodes"])

  theLinks_  <- links <+> Join Path (links_ tree) [ strokeWidth   1.5
                                                  , strokeColor   "black"
                                                  , strokeOpacity 0.4
                                                  , fill          "none"
                                                  , verticalLink
                                                  ]

  nodeJoin_  <- nodes <+> Join Group (descendants_ tree) [ transform [ datum_.positionXY ] ]
  

  theNodes <- nodeJoin_ `appendElement` 
                (node Circle  [ fill         "blue"
                              , radius       20.0
                              , strokeColor "white"
                              , strokeWidth 3.0
                              ])

  labelsWhite <- nodeJoin_ `appendElement`
                (node Text  [ x          0.0
                            , y          3.0
                            , textAnchor "middle"
                            , text       datum_.symbol
                            , fill       "white"
                            ])
                            
  labelsGray <- nodeJoin_ `appendElement`
                (node Text  [ x          22.0
                            , y          3.0
                            , textAnchor "start"
                            , text       datum_.param1
                            , fill       "gray"
                            ])
                            
  pure svg

