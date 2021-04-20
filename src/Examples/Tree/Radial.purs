module D3.Examples.Tree.Radial where

import D3.Attributes.Sugar (classed, dy, fill, height, radius, strokeColor, strokeOpacity, strokeWidth, text, textAnchor, transform, viewBox, width, x)

import Affjax (Error, printError)
import Affjax as AJAX
import Affjax.ResponseFormat as ResponseFormat
import D3.Attributes.Instances (Datum)
import D3.Interpreter (class D3Tagless, append, attach, attachZoom, (<+>))
import D3.Interpreter.D3 (runD3M)
import D3.Layouts.Hierarchical (D3HierarchicalNode(..), Model, TreeJson_, hasChildren_, hierarchy_, initRadialTree, radialLink, readJSON_)
import D3.Layouts.Hierarchical as H
import D3.Selection (Chainable, D3Selection_, Element(..), Join(..), Keys(..), node, zoomExtent, zoomRange)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Math (pi)
import Prelude (class Bind, Unit, bind, discard, negate, pure, show, unit, ($), (*), (*>), (-), (/), (<), (<>), (==), (>=))
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML (window)
import Web.HTML.Window (innerHeight, innerWidth)


getWindowWidthHeight :: Effect (Tuple Number Number)
getWindowWidthHeight = do
  win <- window
  width <- innerWidth win
  height <- innerHeight win
  pure $ Tuple (toNumber width) (toNumber height)

readTreeFromFileContents :: forall r d v. Tuple Number Number -> Either Error { body ∷ String | r } -> Either Error (Model d v)
readTreeFromFileContents widthHeight (Right { body } ) = Right $ makeModel widthHeight (readJSON_ body)
readTreeFromFileContents _           (Left error)      = Left error

drawTree :: Aff Unit
drawTree = do
  log "Radial tree example"
  widthHeight   <- liftEffect getWindowWidthHeight
  treeJSON      <- AJAX.get ResponseFormat.string "http://localhost:1234/flare-2.json"

  case readTreeFromFileContents widthHeight treeJSON of
    (Left error)      -> liftEffect $ log $ printError error
    (Right treeModel) -> liftEffect $ runD3M (enter widthHeight treeModel) *> pure unit



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

makeModel :: forall d v. Tuple Number Number -> TreeJson_ -> Model d v
makeModel (Tuple width height) json = { json, root, root_, treeConfig, svgConfig }
  where
    root_      = hierarchy_ json
    treeConfig = initRadialTree width root_
    svgConfig  = { width, height }
    root       = D3HierarchicalNode (unsafeCoerce root_)

-- | recipe for a radial tree
enter :: forall m v. Bind m => D3Tagless D3Selection_ m =>
  Tuple Number Number -> (Model String v) -> m D3Selection_
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

