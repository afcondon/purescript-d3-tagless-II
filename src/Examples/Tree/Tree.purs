module D3.Examples.Tree where

import D3.Layouts.Hierarchical

import D3.Attributes.Instances (Datum)
import D3.Attributes.Sugar (classed, dy, fill, fontFamily, fontSize, radius, strokeColor, strokeOpacity, strokeWidth, text, textAnchor, transform, viewBox, x)
import D3.Interpreter (class D3InterpreterM, append, attach, attachZoom, (<+>))
import D3.Interpreter.D3 (runD3M)
import D3.Interpreter.String (runPrinter)
import D3.Layouts.Hierarchical as H
import D3.Layouts.Hierarchical.Types (TreeLayout(..), TreeType(..))
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
  ScriptConfig String v -> H.Model String v -> m selection
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
                  , strokeColor   "#555"
                  , strokeOpacity 0.4
                  , fill          "none"
                  , config.linkPath
                  ]
  }

  nodeJoin_ <- nodes <+> Join {
      element   : Group
    , key       : UseDatumAsKey
    , "data"    : H.descendants_ model.root_
    , behaviour : [ transform config.transformations ] -- TODO this is still hardwired for Horizontal tree
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

-- translation for the <g> containing the label (Text) and node (Circle)
-- TODO move to Tree library
reflectXY :: forall d v. D3HierarchicalNode d v -> String
reflectXY (D3HierarchicalNode d) = "translate(" <> show d.y <> "," <> show d.x <>")"

-- transformations :: forall d v. Array (D3HierarchicalNode d v -> String)
-- transformations = [ reflectXY ]

-- this is the extra data that is part of a Datum beyond the D3HierarchicalNode_ minimum
type TreeNodeExtra = { name :: String }

type ScriptConfig d v = { 
    linkPath  :: Chainable
  , selector  :: Selector
  , offset    :: { x :: Number, y :: Number }
  , tree      :: D3HierarchicalNode_
  , viewbox   :: Array Chainable
  , transformations :: Array (D3HierarchicalNode d v -> String)
}
-- | configure function which enables treeScript to be run for different layouts
-- NB radial, vertical not yet working AND cluster not doing links 
configureAndRunScript :: forall m v selection. 
  Bind m => 
  D3InterpreterM selection m => 
  Tuple Number Number -> H.Model String v -> m selection
configureAndRunScript (Tuple width height ) model = 
  treeScript { offset, selector, viewbox, tree, linkPath, transformations } model
  where
    offset = { x: 10.0, y: width / ((hNodeHeight_ model.root_) + 1.0)}

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
      [ viewBox 0.0 0.0 width offset.y ]
      
    linkPath =
      case model.treeType, model.treeLayout of
        Dendrogram, Horizontal -> horizontalClusterLink offset.y
        Dendrogram, Vertical   -> horizontalClusterLink offset.y -- TODO obviously wrong
        Dendrogram, Radial     -> radialLink _.x _.y

        TidyTree, Horizontal  -> horizontalLink
        TidyTree, Vertical    -> horizontalLink -- TODO obviously wrong
        TidyTree, Radial      -> radialLink _.x _.y

    selector =
      case model.treeType, model.treeLayout of
        Dendrogram, Horizontal -> "div#hdendro"
        Dendrogram, Vertical   -> "div#vdendro"
        Dendrogram, Radial     -> "div#rdendro"

        TidyTree, Horizontal  -> "div#htree"
        TidyTree, Vertical    -> "div#vtree"
        TidyTree, Radial      -> "div#rtree"

    transformations = 
      case model.treeType, model.treeLayout of
        Dendrogram, Horizontal -> [ reflectXY ]
        Dendrogram, Vertical   -> []
        Dendrogram, Radial     -> []

        TidyTree, Horizontal  -> [ reflectXY ]
        TidyTree, Vertical    -> []
        TidyTree, Radial      -> []




