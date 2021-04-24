module D3.Examples.Tree.Radial where

import D3.Attributes.Instances (Datum)
import D3.Attributes.Sugar (classed, dy, fill, height, radius, strokeColor, strokeOpacity, strokeWidth, text, textAnchor, transform, viewBox, width, x)
import D3.Interpreter (class D3InterpreterM, append, attach, attachZoom, (<+>))
import D3.Interpreter.D3 (runD3M)
import D3.Interpreter.MetaTree (MetaTreeNode, ScriptTree(..), runMetaTree, scriptTreeToJSON)
import D3.Interpreter.String (runPrinter)
import D3.Layouts.Hierarchical (D3HierarchicalNode(..), Model, TreeJson_, getWindowWidthHeight, hasChildren_, initRadialTree, makeModel, radialLink)
import D3.Layouts.Hierarchical as H
import D3.Layouts.Hierarchical.Types (nullModel_)
import D3.Selection (Chainable, D3Selection_, Element(..), Join(..), Keys(..), node, zoomExtent, zoomRange)
import Data.Map (toUnfoldable)
import Data.Tuple (Tuple(..), fst, snd)
import Debug (spy)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Math (pi)
import Prelude (class Bind, Unit, bind, discard, negate, pure, show, unit, ($), (*), (-), (/), (<), (<>), (==), (>=))
import Unsafe.Coerce (unsafeCoerce)

getMetaTreeJSON :: forall v. Aff TreeJson_
getMetaTreeJSON = do
  log "Getting meta-tree for radial tree example"
  let -- these values are just placeholders - no data is used in producing metaTree
      model       = nullModel_ "dummy" 0
      widthHeight = Tuple 0.0 0.0
  metaScript <- liftEffect $ runMetaTree (enter widthHeight model) -- no need for actual data in metaTree
  let (ScriptTree _ treeMap links) = snd metaScript
      (_ :: Array (Tuple Int MetaTreeNode)) = spy "script map" $ toUnfoldable treeMap
      (_ :: Array (Tuple Int Int))          = spy "link map" $ links
      treeified                             = spy "script tree" $ snd metaScript
  pure $ scriptTreeToJSON treeified

-- TODO this can be made generic if parameterized with the "enter" function which is the D3 script
printTree :: forall v. Model String v -> Aff Unit
printTree treeModel = liftEffect $ do
  log "Radial tree example"
  widthHeight <- getWindowWidthHeight
  printedScript <- runPrinter  (enter widthHeight treeModel) "Radial Tree Script"
  log $ snd printedScript
  log $ fst printedScript
  pure unit

drawTree :: forall v. Model String v -> Aff Unit
drawTree treeModel = liftEffect $ do
  widthHeight <- getWindowWidthHeight
  (_ :: Tuple D3Selection_ Unit) <- runD3M (enter widthHeight treeModel)
  pure unit



-- three little transform functions to build up the transforms on nodes and labels
rotate :: Number -> String
rotate x       = show $ (x * 180.0 / pi - 90.0)
rotateCommon :: forall d v. D3HierarchicalNode d v -> String
rotateCommon (D3HierarchicalNode d) = "rotate(" <> rotate d.x <> ")"
rotateText2 :: forall d v. D3HierarchicalNode d v -> String
rotateText2 (D3HierarchicalNode d) = -- TODO replace with nodeIsOnRHS 
  "rotate(" <> if d.x >= pi 
  then "180" <> ")" 
  else "0" <> ")"
-- same translation for both text and node
translate :: forall d v. D3HierarchicalNode d v -> String
translate (D3HierarchicalNode d) = "translate(" <> show d.y <> ",0)"

transformations :: forall d v. Array (D3HierarchicalNode d v -> String)
transformations = [ rotateCommon, translate ]

labelTransformations :: forall d v. Array (D3HierarchicalNode d v -> String)
labelTransformations = [ rotateCommon, translate, rotateText2 ]

datumIsTreeNode :: forall d v. Datum -> D3HierarchicalNode d v
datumIsTreeNode = unsafeCoerce

nodeIsOnRHS :: Datum -> Boolean
nodeIsOnRHS d = node.x < pi
  where (D3HierarchicalNode node) = datumIsTreeNode d

labelName :: Datum -> String
labelName d = node."data".name
  where (D3HierarchicalNode node) = datumIsTreeNode d

-- | Script components, attributes, transformations etc
svgAttributes :: Array Chainable
svgAttributes = [
    width   1000.0
  , height  1000.0
  , viewBox (-500.0) (-500.0) 2000.0 2000.0
]

-- this is the extra row info that is part of a Datum beyond the D3Tree minimum
type TreeNodeExtra = { name :: String }
type TreeNode v = D3HierarchicalNode TreeNodeExtra v -- v is the value calculated in the tree, ie for sum, count etc

-- | recipe for a radial tree
enter :: forall m v selection. 
  Bind m => 
  D3InterpreterM selection m =>
  Tuple Number Number -> (Model String v) -> m selection
enter (Tuple width height) model = do
  root      <- attach "div#rtree"
  svg       <- root      `append` (node Svg   svgAttributes)
  container <- svg       `append` (node Group [ classed "container" ])
  links     <- container `append` (node Group [ classed "links"])
  nodes     <- container `append` (node Group [ classed "nodes"])
  labels    <- container `append` (node Group [ classed "labels"])

  linkJoinSelection_ <- links <+> Join { 
      element   : Path
    , key       : UseDatumAsKey
    , "data"    : H.links_ model.root_
    , behaviour : [ strokeWidth   1.5
                  , strokeColor   "#555"
                  , strokeOpacity 0.4
                  , fill          "none"
                  , radialLink    _.x _.y
                  ] 
  }
  nodeJoinSelection_ <- nodes <+> Join {
      element   : Circle
    , key       : UseDatumAsKey
    , "data"    : H.descendants_ model.root_
    , behaviour : [ transform transformations
                  , fill (\d -> if hasChildren_ d then "#555" else "#999")
                  , radius 2.5
                  ]
  }
  labelJoinSelection_ <- labels <+> Join {
      element   : Text
    , key       : UseDatumAsKey
    , "data"    : H.descendants_ model.root_
    , behaviour : [ transform  labelTransformations
                  , dy         0.31
                  , x          (\d -> if (hasChildren_ d == nodeIsOnRHS d) then 6.0 else (-6.0))
                  , textAnchor (\d -> if (hasChildren_ d == nodeIsOnRHS d) then "start" else "end")
                  , text       labelName
                  ]
  }

  svgZ <- attachZoom container  
                    { extent    : zoomExtent { top: 0.0, left: 0.0 , bottom: height, right: width }
                    , scale     : 1 `zoomRange` 8 
                    , qualifier : "tree"
                    }

  pure svgZ

