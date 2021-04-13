module D3.Examples.Tree.Radial where

import Affjax (Error, printError)
import Affjax as AJAX
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.State (class MonadState, get)
import D3.Attributes.Instances (Datum)
import D3.Attributes.Sugar (classed, dy, fill, height, radius, strokeColor, strokeOpacity, strokeWidth, text, textAnchor, transform, viewBox, width, x)
import D3.Interpreter.Tagless (class D3Tagless, appendTo, hook, join, runD3M)
import D3.Layouts.Hierarchical (D3HierarchicalNode(..), Model, TreeJson_, d3InitTree, hasChildren_, hierarchy_, radialLink, radialTreeConfig, readJSON_)
import D3.Layouts.Hierarchical as H
import D3.Selection (Chainable, D3Selection_, D3State(..), Element(..), EnterUpdateExit, Join(..), Keys(..), ScaleExtent(..), SelectionName(..), ZoomExtent(..), attachZoom, enterOnly, makeD3State', makeProjection, node, zoomExtent, zoomRange)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Math (pi)
import Prelude (class Bind, Unit, bind, discard, negate, pure, show, unit, (&&), ($), (*), (*>), (-), (/), (<), (<>), (==), (>=))
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML (window)
import Web.HTML.Window (innerHeight, innerWidth)


getWindowWidthHeight :: Effect (Tuple Number Number)
getWindowWidthHeight = do
  win <- window
  width <- innerWidth win
  height <- innerHeight win
  pure $ Tuple (toNumber width) (toNumber height)

readTreeFromFileContents :: forall r v. Tuple Number Number -> Either Error { body ∷ String | r } -> Either Error (Model String v)
readTreeFromFileContents (Tuple width _) (Right { body } ) = Right $ makeModel width (readJSON_ body)
readTreeFromFileContents _               (Left error)      = Left error

drawTree :: Aff Unit
drawTree = do
  log "Radial tree example"
  widthHeight   <- liftEffect getWindowWidthHeight
  treeJSON      <- AJAX.get ResponseFormat.string "http://localhost:1234/flare-2.json"

  case readTreeFromFileContents widthHeight treeJSON of
    (Left error)      -> liftEffect $ log $ printError error
    (Right treeModel) -> liftEffect $ runD3M (enter widthHeight treeModel) makeD3State' *> pure unit



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

-- | instructions for entering the links of the radial tree
enterLinks :: Array Chainable
enterLinks = [  strokeWidth   1.5
              , strokeColor   "#555"
              , strokeOpacity 0.4
              , fill          "none"
              , radialLink    _.x _.y
              ] 

-- | instructions for entering the nodes of the radial tree
enterNodes :: Array Chainable
enterNodes =  [ transform transformations
              , fill (\d -> if hasChildren_ d then "#555" else "#999")
              , radius 2.5
              ]

-- | instructions for entering the labels of the radial tree
enterLabels :: Array Chainable
enterLabels = [ transform  labelTransformations
              , dy         0.31
              , x          (\d -> if (hasChildren_ d == nodeIsOnRHS d) then 6.0 else (-6.0))
              , textAnchor (\d -> if (hasChildren_ d == nodeIsOnRHS d) then "start" else "end")
              , text       labelName
              ]

-- this is the extra row info that is part of a Datum beyond the D3Tree minimum
type TreeNodeExtra = { name :: String }
type TreeNode v = D3HierarchicalNode TreeNodeExtra v -- v is the value calculated in the tree, ie for sum, count etc

makeModel :: forall v. Number -> TreeJson_ -> Model TreeNodeExtra v
makeModel width json = { json, root, config }
  where
    config = radialTreeConfig width
    root' = hierarchy_ json
    root  = unsafeCoerce $ d3InitTree config root'

-- | recipe for a radial tree
enter :: forall m v. Bind m => D3Tagless m => MonadState (D3State (Model String v)) m => 
  Tuple Number Number -> Model String v -> m D3Selection_
enter (Tuple width height) model = do
  root      <- hook "div#tree"
  svg       <- root      `appendTo` (node Svg svgAttributes)
  container <- svg       `appendTo` (node Group [ classed "container" ])
  links     <- container `appendTo` (node Group [ classed "links"])
  nodes     <- container `appendTo` (node Group [ classed "nodes"])
  labels    <- container `appendTo` (node Group [ classed "labels"])

  linkJoinSelection_ <- join model $ Join {
      element   : Path
    , key       : DatumIsUnique
    , hook      : links
    , projection: makeProjection (\model -> H.links_ model.root)
    , behaviour : enterLinks
  }
-- TODO this separation of labels and circles comes from original radial tree example
-- however, other trees have the label and circle grouped, which seems better to me anyway
-- and it means only one join to do it, but then two appends after the join
-- this is a different pattern and it's worth exploring both
-- now that we no longer have Maybe Selection from the join, should be easy to do this
  nodeJoinSelection_ <- join model $ Join {
      element   : Circle
    , key       : DatumIsUnique
    , hook      : nodes
    , projection: makeProjection (\model -> H.descendants_ model.root)
    , behaviour : enterNodes
  }

  labelJoinSelection_ <- join model $ Join {
      element   : Text
    , key       : DatumIsUnique
    , hook      : labels
    , projection: makeProjection (\model -> H.descendants_ model.root)
    , behaviour : enterLabels
  }

  let _ = attachZoom container  
                    { extent    : zoomExtent { top: 0.0, left: 0.0 , bottom: height, right: width }
                    , scale     : 1 `zoomRange` 8 
                    , qualifier : "tree"
                    }

  pure svg

