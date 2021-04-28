module D3.Examples.Tree where

import D3.Layouts.Hierarchical

import D3.Attributes.Instances (Datum)
import D3.Attributes.Sugar (classed, dy, fill, fontFamily, fontSize, radius, strokeColor, strokeOpacity, strokeWidth, text, textAnchor, transform, viewBox, x)
import D3.Interpreter (class D3InterpreterM, append, attach, attachZoom, (<+>))
import D3.Interpreter.D3 (runD3M)
import D3.Interpreter.String (runPrinter)
import D3.Layouts.Hierarchical as H
import D3.Layouts.Hierarchical.Types (TreeLayout(..), TreeType(..))
import D3.Scales (d3SchemeCategory10_)
import D3.Selection (Chainable, D3Selection_, Element(..), Join(..), Keys(..), ScaleExtent(..), ZoomExtent(..), Selector, node)
import Data.Tuple (Tuple(..), fst, snd)
import Debug (spy)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Prelude (class Bind, Unit, bind, discard, negate, pure, show, unit, ($), (+), (<>), (/))
import Unsafe.Coerce (unsafeCoerce)

-- | this is the eDSL script that renders tree layouts
-- | it's parameterized rather heavily using the ScriptConfig record so that it can draw
-- | six variations (TODO) of [Radial, Horizontal, Vertical] * [Dendrogram, TidyTree] 
treeScript :: forall m v selection. Bind m => D3InterpreterM selection m => 
  ScriptConfig -> H.Model String v -> m selection
treeScript config model = do
  root      <- attach config.selector
  svg       <- root      `append` (node Svg config.viewbox)
  container <- svg       `append` (node Group [ fontFamily "sans-serif"
                                              , fontSize   10.0
                                              ])
  links     <- container `append` (node Group [ classed "links"])
  nodes     <- container `append` (node Group [ classed "nodes"])

  theLinks_ <- links <+> Join {
      element   : Path
    , key       : UseDatumAsKey
    , "data"    : H.links_ model.root_
    , behaviour : [ strokeWidth   1.5
                  , strokeColor   config.color
                  , strokeOpacity 0.4
                  , fill          "none"
                  , config.linkPath
                  ]
  }

  nodeJoin_ <- nodes <+> Join {
      element   : Group
    , key       : UseDatumAsKey
    , "data"    : H.descendants_ model.root_
    -- there could be other stylistic stuff here but the transform is key structuring component
    , behaviour : config.nodeTransform -- <- the key positioning calculation for the tree!!!
  }

  theNodes <- nodeJoin_ `append` 
                (node Circle  [ fill (\d -> if hasChildren_ d then "#999" else "#555")
                              , radius 2.5
                              , strokeColor "white"
                              ])

  theLabels <- nodeJoin_ `append`
                (node Text  [ dy         0.31
                            , x          (\d -> if hasChildren_ d then 6.0 else (-6.0))
                            , textAnchor (\d -> if hasChildren_ d then "start" else "end")
                            , text       labelName
                            ])

  svgZ <- attachZoom container  
                    { extent    : ZoomExtent { top: 0.0, left: 0.0 , bottom: model.svgConfig.height, right: model.svgConfig.width }
                    , scale     : ScaleExtent 1 8 -- wonder if ScaleExtent ctor could be range operator `..`
                    , qualifier : "tree"
                    }

  pure svg


-- TODO move this to a library, it really only needs the params for runPrinter to be completely generic
-- | Evaluate the tree drawing script in the "printer" monad which will render it as a string
-- | rather than drawing SVG or Canvas. In principle this could be basis for compiling to JS D3 script
printTree :: forall v. Model String v -> Aff Unit
printTree treeModel = liftEffect $ do
  log "Horizontal tree example"
  widthHeight <- getWindowWidthHeight
  printedScript <- runPrinter  (configureAndRunScript widthHeight treeModel) "Horizontal Tree Script"
  log $ snd printedScript
  log $ fst printedScript
  pure unit

-- | Evaluate the tree drawing script in the "d3" monad which will render it in SVG
-- | TODO specialize runD3M so that this function isn't necessary
drawTree :: forall v. Model String v -> Aff Unit
drawTree treeModel = liftEffect $ do
  widthHeight <- getWindowWidthHeight
  (_ :: Tuple D3Selection_ Unit) <- runD3M (configureAndRunScript widthHeight treeModel)
  pure unit

-- | Coercion function to recover the structure that was given to D3, it's an unsafeCoerce but the types
-- | give some protection
datumIsTreeNode :: forall d v. Datum -> D3HierarchicalNode d v
datumIsTreeNode = unsafeCoerce

-- | Coercion function to recover the "extra" data that lives within the generic structure that was given to D3, 
-- | it's an unsafeCoerce but the types give some protection
labelName :: Datum -> String
labelName d = node."data".name
  where (D3HierarchicalNode node) = datumIsTreeNode d

-- this is the extra data that is part of a Datum beyond the D3HierarchicalNode_ minimum
type TreeNodeExtra = { name :: String }

type ScriptConfig = { 
    linkPath      :: Chainable
  , selector      :: Selector
  , offset        :: { x :: Number, y :: Number }
  , tree          :: D3HierarchicalNode_
  , viewbox       :: Array Chainable
  , nodeTransform :: Array Chainable
  , color         :: String
}
-- | configure function which enables treeScript to be run for different layouts
-- NB radial, vertical not yet working AND cluster not doing links 
configureAndRunScript :: forall m v selection. 
  Bind m => 
  D3InterpreterM selection m => 
  Tuple Number Number -> H.Model String v -> m selection
configureAndRunScript (Tuple width height ) model = 
  treeScript { offset, selector, viewbox, tree, linkPath, nodeTransform, color } model
  where
    offset = 
      case model.treeType, model.treeLayout of
        Dendrogram, Horizontal -> { x: 10.0, y: width / ((hNodeHeight_ model.root_) + 1.0)}
        Dendrogram, Vertical   -> { x: width / 2.0, y: 10.0 }
        Dendrogram, Radial     -> { x: 0.0, y: 0.0}

        TidyTree, Horizontal   -> { x: 0.0, y: 0.0}
        TidyTree, Vertical     -> { x: 0.0, y: 0.0}
        TidyTree, Radial       -> { x: 0.0, y: 0.0}


    layout = 
      case model.treeType of
        Dendrogram -> initCluster_ unit
        TidyTree   -> initTree_ unit

    layout' = 
      case model.treeLayout of
        Horizontal -> layout `treeSetNodeSize_` [ offset.x, offset.y ]
        Vertical   -> layout `treeSetNodeSize_` [ offset.x, offset.y ]
        Radial     -> (layout `treeSetSize_`    [width,   height]) `treeSetSeparation_` radialSeparation

    tree =
      layout' `treeSetRoot_` model.root_

    viewbox =
      case model.treeType, model.treeLayout of
        Dendrogram, Horizontal -> [ viewBox 0.0 0.0 width offset.y ]
        Dendrogram, Vertical   -> [ viewBox 0.0 0.0 width height ]
        Dendrogram, Radial     -> [ viewBox (-width/2.0) (-height/2.0) width height ]

        TidyTree, Horizontal   -> [ viewBox 0.0 0.0 width height ]
        TidyTree, Vertical     -> [ viewBox 0.0 0.0 width height ]
        TidyTree, Radial       -> [ viewBox (-width/2.0) (-height/2.0) width height ]

      
    linkPath =
      case model.treeType, model.treeLayout of
        Dendrogram, Horizontal -> horizontalClusterLink offset.y
        Dendrogram, Vertical   -> verticalClusterLink   offset.x 
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
        Dendrogram, Radial     -> []

        TidyTree, Horizontal   -> [ transform [ positionXYreflected ] ]
        TidyTree, Vertical     -> [ transform [ positionXY ] ]  -- no transform required for vertical case
        TidyTree, Radial       -> []

    color =
      case model.treeType, model.treeLayout of
        Dendrogram, Horizontal -> d3SchemeCategory10_ 1.0
        Dendrogram, Vertical   -> d3SchemeCategory10_ 2.0
        Dendrogram, Radial     -> d3SchemeCategory10_ 3.0

        TidyTree, Horizontal   -> d3SchemeCategory10_ 4.0
        TidyTree, Vertical     -> d3SchemeCategory10_ 5.0
        TidyTree, Radial       -> d3SchemeCategory10_ 6.0





