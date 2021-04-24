module D3.Examples.Tree.Horizontal where

import D3.Layouts.Hierarchical

import Affjax (printError)
import Affjax as AJAX
import Affjax.ResponseFormat as ResponseFormat
import D3.Attributes.Instances (Datum)
import D3.Attributes.Sugar (classed, dy, fill, fontFamily, fontSize, radius, strokeColor, strokeOpacity, strokeWidth, text, textAnchor, transform, viewBox, x, y)
import D3.Interpreter (class D3InterpreterM, append, attach, attachZoom, (<+>))
import D3.Interpreter.D3 (runD3M)
import D3.Interpreter.String (runPrinter)
import D3.Layouts.Hierarchical as H
import D3.Selection (Chainable, D3Selection_, Element(..), Join(..), Keys(..), ScaleExtent(..), ZoomExtent(..), node)
import Data.Either (Either(..))
import Data.Tuple (Tuple, fst, snd)
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
  printedScript <- runPrinter  (enter treeModel) "Horizontal Tree Script"
  log $ snd printedScript
  log $ fst printedScript
  pure unit

drawTree :: forall v. Model String v -> Aff Unit
drawTree treeModel = liftEffect $ do
  widthHeight <- getWindowWidthHeight
  (_ :: Tuple D3Selection_ Unit) <- runD3M (enter treeModel)
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
svgHeight :: HorizontalTreeConfig -> Number
svgHeight config = config.x1 - config.x0 + (config.rootDx * 2.0)

svgAttributes :: Number -> Number -> Array Chainable
svgAttributes width heightSVG = [ viewBox 0.0 0.0 width heightSVG ]

-- translateContainer :: forall d v. x0 :: Number -> H.D3HierarchicalNode d v -> String
translateContainer :: HorizontalTreeConfig -> (Datum -> String)
translateContainer config = \d -> 
  "translate(" <> show (config.rootDy / 3.0) <> "," <> show (config.rootDx - config.x0) <> ")"

containerAttributes :: HorizontalTreeConfig -> Array Chainable
containerAttributes config = [
    fontFamily "sans-serif"
  , fontSize   10.0
  , transform [ translateContainer config ]
]

-- | instructions for entering the links of the radial tree
enterLinks :: Array Chainable
enterLinks = [  strokeWidth   1.5
              , strokeColor   "#555"
              , strokeOpacity 0.4
              , fill          "none"
              , horizontalLink
              , x getY -- swap x and y for horizontal tree
              , y getX -- swap y and x for horizontal tree
              ] 

-- | instructions for entering the nodes of the radial tree
enterNodes :: Array Chainable
enterNodes =  [ fill (\d -> if hasChildren_ d then "#555" else "#999")
              , radius 2.5
              , x getY -- swap x and y for horizontal tree
              , y getX -- swap y and x for horizontal tree
              ]

-- | instructions for entering the labels of the radial tree
enterLabels :: Array Chainable
enterLabels = [ dy         0.31
              , x          (\d -> if hasChildren_ d then (-6.0) else 6.0)
              , textAnchor (\d -> if hasChildren_ d then "end" else "start")
              , text       labelName
              ]

-- this is the extra data that is part of a Datum beyond the D3HierarchicalNode_ minimum
type TreeNodeExtra = { name :: String }

-- | recipe for a horizontal tree
enter :: forall m v selection. Bind m => D3InterpreterM selection m => H.Model String v -> m selection
enter model = do
  -- TODO inherently gross to case, fix model and or enter function
  let config = case model.treeConfig of
                  (HorizontalTree c) -> c
                  _ -> { rootDx: 0.0, rootDy: 0.0, x0: 0.0, x1: 0.0 }
      viewbox = svgAttributes model.svgConfig.width (svgHeight config)
  root      <- attach "div#htree"
  svg       <- root      `append` (node Svg viewbox)
  container <- svg       `append` (node Group (containerAttributes config))
  links     <- container `append` (node Group [ classed "links"])
  nodes     <- container `append` (node Group [ classed "nodes"])
  labels    <- container `append` (node Group [ classed "labels"])

  linkJoinSelection_ <- links <+> Join {
      element   : Path
    , key       : UseDatumAsKey
    , "data"    : H.links_ model.root_
    , behaviour : enterLinks
  }

  nodeJoinSelection_ <- nodes <+> Join {
      element   : Circle
    , key       : UseDatumAsKey
    , "data"    : H.descendants_ model.root_
    , behaviour : enterNodes
  }

  labelJoinSelection_ <- labels <+> Join {
      element   : Text
    , key       : UseDatumAsKey
    , "data"    : H.descendants_ model.root_
    , behaviour : enterLabels
  }

  svgZ <- attachZoom container  
                    { extent    : ZoomExtent { top: 0.0, left: 0.0 , bottom: model.svgConfig.height, right: model.svgConfig.width }
                    , scale     : ScaleExtent 1 8 -- wonder if ScaleExtent ctor could be range operator `..`
                    , qualifier : "tree"
                    }

  pure svg


