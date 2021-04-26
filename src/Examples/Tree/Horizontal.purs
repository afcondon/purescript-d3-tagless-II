module D3.Examples.Tree.Horizontal where

import D3.Layouts.Hierarchical

import D3.Attributes.Instances (Datum)
import D3.Attributes.Sugar (classed, dy, fill, fontFamily, fontSize, radius, strokeColor, strokeOpacity, strokeWidth, text, textAnchor, transform, viewBox, x)
import D3.Interpreter (class D3InterpreterM, append, attach, attachZoom, (<+>))
import D3.Interpreter.D3 (runD3M)
import D3.Interpreter.String (runPrinter)
import D3.Layouts.Hierarchical as H
import D3.Selection (Chainable, D3Selection_, Element(..), Join(..), Keys(..), ScaleExtent(..), ZoomExtent(..), node)
import Data.Tuple (Tuple(..), fst, snd)
import Debug (spy)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Prelude (class Bind, Unit, bind, discard, negate, pure, show, unit, ($), (*), (+), (-), (/), (<>))
import Unsafe.Coerce (unsafeCoerce)

-- TODO this can be made generic if parameterized with the "enter" function which is the D3 script
printTree :: forall v. Model String v -> Aff Unit
printTree treeModel = liftEffect $ do
  log "Horizontal tree example"
  widthHeight <- getWindowWidthHeight
  printedScript <- runPrinter  (enter widthHeight treeModel) "Horizontal Tree Script"
  log $ snd printedScript
  log $ fst printedScript
  pure unit

drawTree :: forall v. Model String v -> Aff Unit
drawTree treeModel = liftEffect $ do
  widthHeight <- getWindowWidthHeight
  (_ :: Tuple D3Selection_ Unit) <- runD3M (enter widthHeight treeModel)
  pure unit


datumIsTreeNode :: forall d v. Datum -> D3HierarchicalNode d v
datumIsTreeNode = unsafeCoerce

labelName :: Datum -> String
labelName d = node."data".name
  where (D3HierarchicalNode node) = datumIsTreeNode d

getY :: Datum -> Number
getY d = node.y
  where (D3HierarchicalNode node) = datumIsTreeNode d

getX :: Datum -> Number
getX d = node.x
  where (D3HierarchicalNode node) = datumIsTreeNode d

-- | Script components, attributes, transformations etc
svgHeight :: Number -> Number -> Number -> Number
svgHeight xMin xMax xOffset = xMax - xMin + xOffset * 2.0

svgAttributes :: Number -> Number -> Array Chainable
svgAttributes width heightSVG = [ viewBox 0.0 0.0 width heightSVG ]

translateContainer :: Number -> Number -> Number -> (Datum -> String)
translateContainer xOffset yOffset xMin = \d -> 
  "translate(" <> show (yOffset / 3.0) <> "," <> show (xOffset - xMin) <> ")"

-- translation for <g> containing the label (Text) and node (Circle)
translateNode :: forall d v. D3HierarchicalNode d v -> String
translateNode (D3HierarchicalNode d) = "translate(" <> show d.y <> "," <> show d.x <>")"

transformations :: forall d v. Array (D3HierarchicalNode d v -> String)
transformations = [ translateNode ]

-- this is the extra data that is part of a Datum beyond the D3HierarchicalNode_ minimum
type TreeNodeExtra = { name :: String }

-- | recipe for a horizontal tree
enter :: forall m v selection. Bind m => D3InterpreterM selection m => 
  Tuple Number Number -> H.Model String v -> m selection
enter (Tuple width height) model = do
  let xOffset = 10.0
      yOffset = width / ((hNodeHeight_ model.root_) + 1.0)
      { xMin, xMax, yMin, yMax } = treeMinMax_ model.root_
      viewbox = svgAttributes model.svgConfig.width (svgHeight xMin xMax xOffset)
  root      <- attach "div#htree"
  svg       <- root      `append` (node Svg viewbox)
  container <- svg       `append` (node Group [ fontFamily "sans-serif"
                                              , fontSize   10.0
                                              , transform [ translateContainer xOffset yOffset xMin ]
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
                  , horizontalLink
                  ]
  }

  nodeJoin_ <- nodes <+> Join {
      element   : Group
    , key       : UseDatumAsKey
    , "data"    : H.descendants_ model.root_
    , behaviour : [ transform transformations ]
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


