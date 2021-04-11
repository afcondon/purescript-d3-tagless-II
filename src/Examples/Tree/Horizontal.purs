module D3.Examples.Tree.Horizontal where

import Affjax (Error, printError)
import Affjax as AJAX
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.State (class MonadState, get)
import D3.Attributes.Instances (Datum)
import D3.Attributes.Sugar (classed, dy, fill, height, radius, strokeColor, strokeOpacity, strokeWidth, text, textAnchor, transform, viewBox, width, x)
import D3.Interpreter.Tagless (class D3Tagless, appendTo, hook, join, runD3M)
import D3.Layouts.Tree (D3TreeNode, Model, TreeJson, d3HierarchyDescendants_, d3HierarchyLinks_, d3Hierarchy_, d3InitTree, d3InitTree_, hasChildren_, horizontalTreeConfig, radialLink, radialTreeConfig, readJSONJS_)
import D3.Selection (Chainable, D3Selection_, D3State(..), Element(..), EnterUpdateExit, Join(..), Keys(..), ScaleExtent(..), SelectionName(..), ZoomExtent(..), attachZoom, enterOnly, makeD3State', makeProjection, node)
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

readTreeFromFileContents :: forall r. Tuple Number Number -> Either Error { body ∷ String | r } -> Either Error (Model String)
readTreeFromFileContents (Tuple width _) (Right { body } ) = Right $ makeModel width (readJSONJS_ body)
readTreeFromFileContents _               (Left error)      = Left error

drawTree :: Aff Unit
drawTree = do
  log "Radial tree example"
  widthHeight   <- liftEffect getWindowWidthHeight
  treeJSON      <- AJAX.get ResponseFormat.string "http://localhost:1234/flare-2.json"

  case readTreeFromFileContents widthHeight treeJSON of
    (Left error)      -> liftEffect $ log $ printError error
    (Right treeModel) -> liftEffect $ runD3M (enter widthHeight treeModel) makeD3State' *> pure unit

datumIsTreeNode :: Datum -> TreeNode
datumIsTreeNode = unsafeCoerce

labelName :: Datum -> String
labelName d = node."data".name
  where node = datumIsTreeNode d

-- | Script components, attributes, transformations etc
svgAttributes :: Array Chainable
svgAttributes = [
    width 1000.0
  , height 1000.0
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
enterNodes =  [ fill (\d -> if hasChildren_ d then "#555" else "#999")
              , radius 2.5
              ]

-- | instructions for entering the labels of the radial tree
enterLabels :: Array Chainable
enterLabels = [ dy         0.31
              , x          (\d -> if hasChildren_ d then (-6.0) else 6.0)
              , textAnchor (\d -> if hasChildren_ d then "end" else "start")
              , text       labelName
              ]

-- this is the extra row info that is part of a Datum beyond the D3Tree minimum
type TreeNodeExtra = { name :: String }
type TreeNode = D3TreeNode TreeNodeExtra 

makeModel :: Number -> TreeJson -> Model TreeNodeExtra
makeModel width json = { json, d3Tree, config }
  where
    config           = horizontalTreeConfig width
    hierarchicalData = d3Hierarchy_ json
    d3Tree           = d3InitTree config hierarchicalData

-- | recipe for a radial tree
enter :: forall m. Bind m => D3Tagless m => MonadState (D3State (Model String)) m => 
  Tuple Number Number -> Model String -> m D3Selection_
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
    , projection: makeProjection (\model -> d3HierarchyLinks_ model.d3Tree)
    , behaviour : enterLinks
  }

  nodeJoinSelection_ <- join model $ Join {
      element   : Circle
    , key       : DatumIsUnique
    , hook      : nodes
    , projection: makeProjection (\model -> d3HierarchyDescendants_ model.d3Tree)
    , behaviour : enterNodes
  }

  labelJoinSelection_ <- join model $ Join {
      element   : Text
    , key       : DatumIsUnique
    , hook      : labels
    , projection: makeProjection (\model -> d3HierarchyDescendants_ model.d3Tree)
    , behaviour : enterLabels
  }

  let _ = attachZoom container  
                    { extent    : ZoomExtent { top: 0.0, left: 0.0 , bottom: height, right: width }
                    , scale     : ScaleExtent 1 8 -- wonder if ScaleExtent ctor could be range operator `..`
                    , qualifier : "tree"
                    }

  pure svg


