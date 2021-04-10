module D3.Examples.Tree where

import Affjax (Error, printError)
import Affjax as AJAX
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.State (class MonadState, get)
import D3.Attributes.Instances (Datum)
import D3.Attributes.Sugar (classed, dy, fill, height, radius, strokeColor, strokeOpacity, strokeWidth, text, textAnchor, transform, viewBox, width, x)
import D3.Interpreter.Tagless (class D3Tagless, appendTo, hook, join, runD3M)
import D3.Layouts.Tree (D3TreeNode, Model, TreeJson, d3HierarchyDescendants_, d3HierarchyLinks_, d3Hierarchy_, d3InitTree_, hasChildren_, radialLink, radialTreeConfig, readJSONJS_)
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
    (Right treeModel) -> liftEffect $ runD3M enter (makeD3State' treeModel) *> pure unit



-- three little transform functions to build up the transforms on nodes and labels
rotate :: Number -> String
rotate x       = show $ (x * 180.0 / pi - 90.0)
rotateCommon :: forall a. D3TreeNode a -> String
rotateCommon d = "rotate(" <> rotate d.x <> ")"
rotateText2 :: forall a. D3TreeNode a -> String
rotateText2 d  = "rotate(" <> if d.x >= pi 
                              then "180" <> ")" 
                              else "0" <> ")"
-- same translation for both text and node
translate :: forall a. D3TreeNode a -> String
translate d = "translate(" <> show d.y <> ",0)"

transformations :: forall a. Array (D3TreeNode a -> String)
transformations = [ rotateCommon, translate ]

labelTransformations :: forall a. Array (D3TreeNode a -> String)
labelTransformations = [ rotateCommon, translate, rotateText2 ]

datumIsTreeNode :: Datum -> TreeNode
datumIsTreeNode = unsafeCoerce

unwrapInnerDatum :: Datum -> TreeNodeExtra
unwrapInnerDatum d = (unsafeCoerce d)."data"

labelOffset :: Datum -> Number
labelOffset d = do
  let node = datumIsTreeNode d
  if (node.x < pi) == hasChildren_ d
  then 6.0
  else (-6.0)

labelName :: Datum -> String
labelName d = do
  let node = unwrapInnerDatum d
  node.name

textOffset :: Datum -> String
textOffset d = do
  let node = datumIsTreeNode d
  if (node.x < pi) == hasChildren_ d
  then "start"
  else "end"

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
enterNodes =  [ transform transformations
              , fill (\d -> if hasChildren_ d then "#555" else "#999")
              , radius 2.5
              ]

-- | instructions for entering the labels of the radial tree
enterLabels :: Array Chainable
enterLabels = [ transform  labelTransformations
              , dy         0.31
              , x          labelOffset
              , textAnchor textOffset
              , text       labelName
              ]

-- this is the extra row info that is part of a Datum beyond the D3Tree minimum
type TreeNodeExtra = { name :: String }
type TreeNode = D3TreeNode TreeNodeExtra 

makeModel :: Number -> TreeJson -> Model TreeNodeExtra
makeModel width json = { json, d3Tree, config }
  where
    config           = radialTreeConfig width
    hierarchicalData = d3Hierarchy_ json
    d3Tree           = d3InitTree_ config hierarchicalData

-- | recipe for a radial tree
enter :: forall m. Bind m => D3Tagless m => MonadState (D3State (Model String)) m => m D3Selection_
enter = do
  root   <- hook "div#tree"
  svg    <- appendTo root "svg-tree"    (node Svg svgAttributes)
  container <- appendTo svg "container" (node Group [ classed "container" ])
  links  <- appendTo container "links-group"  (node Group [ classed "links"])
  nodes  <- appendTo container "nodes-group"  (node Group [ classed "nodes"])
  labels <- appendTo container "labels-group" (node Group [ classed "labels"])

  (D3State state) <- get

  linkJoinSelection_ <- join state.model $ Join {
      element   : Path
    , key       : DatumIsUnique
    , hook      : SelectionName "links-group"
    , projection: makeProjection (\model -> d3HierarchyLinks_ model.d3Tree)
    , behaviour : enterLinks
  }

  nodeJoinSelection_ <- join state.model $ Join {
      element   : Circle
    , key       : DatumIsUnique
    , hook      : SelectionName "nodes-group"
    , projection: makeProjection (\model -> d3HierarchyDescendants_ model.d3Tree)
    , behaviour : enterNodes
  }

  labelJoinSelection_ <- join state.model $ Join {
      element   : Text
    , key       : DatumIsUnique
    , hook      : SelectionName "labels-group"
    , projection: makeProjection (\model -> d3HierarchyDescendants_ model.d3Tree)
    , behaviour : enterLabels
  }

  let width = 2000.0
  let height = 1000.0 -- TODO pass these in or look up correct values

      _ = attachZoom container  
                    { extent     : ZoomExtent { top: 0.0, left: 0.0 , bottom: height, right: width }
                    , scaleExtent: ScaleExtent 1 8 -- wonder if ScaleExtent ctor could be range operator `..`
                    , qualifier  : "tree"
                    }

  pure svg


  -- svg.call(d3.zoom()
  --     .extent([[0, 0], [width, height]])
  --     .scaleExtent([1, 8])
  --     .on("zoom", zoomed));

  -- function zoomed({transform}) {
  --   g.attr("transform", transform);
  -- }



