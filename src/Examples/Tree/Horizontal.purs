module D3.Examples.Tree.Horizontal where

import D3.Attributes.Sugar

import Affjax (Error, printError)
import Affjax as AJAX
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.State (class MonadState)
import D3.Attributes.Instances (Datum)
import D3.Interpreter.Tagless (class D3Tagless, appendTo, hook, join, runD3M)
import D3.Layouts.Hierarchical 
import D3.Layouts.Hierarchical as H
import D3.Selection (Chainable, D3Selection_, D3State, Element(..), Join(..), Keys(..), ScaleExtent(..), ZoomExtent(..), attachZoom, makeD3State', makeProjection, node)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Prelude (class Bind, Unit, bind, discard, negate, pure, show, unit, ($), (*), (*>), (+), (-), (/), (<>))
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML (window)
import Web.HTML.Window (innerHeight, innerWidth)


getWindowWidthHeight :: Effect (Tuple Number Number)
getWindowWidthHeight = do
  win <- window
  width <- innerWidth win
  height <- innerHeight win
  pure $ Tuple (toNumber width) (toNumber height)

readTreeFromFileContents :: forall r v. Tuple Number Number -> Either Error { body ∷ String | r } -> Either Error (H.Model String v)
readTreeFromFileContents widthHeight (Right { body } ) = Right $ makeModel widthHeight (readJSON_ body)
readTreeFromFileContents _               (Left error)      = Left error

drawTree :: Aff Unit
drawTree = do
  log "Radial tree example"
  widthHeight   <- liftEffect getWindowWidthHeight
  treeJSON      <- AJAX.get ResponseFormat.string "http://localhost:1234/flare-2.json"

  case readTreeFromFileContents widthHeight treeJSON of
    (Left error)      -> liftEffect $ log $ printError error
    (Right treeModel) -> liftEffect $ runD3M (enter widthHeight treeModel) makeD3State' *> pure unit

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
svgHeight config = (config.x1 - config.x0 + (config.rootDx * 2.0))

svgAttributes :: Number -> Number -> Array Chainable
svgAttributes width heightSVG = [ viewBox 0.0 0.0 width heightSVG ]

-- translateContainer :: forall d v. x0 :: Number -> H.D3HierarchicalNode d v -> String
translateContainer :: HorizontalTreeConfig -> String
translateContainer config = 
  "translate(" <> show (config.rootDy / 3.0) <> "," <> show (config.rootDx - config.x0) <> ")"

containerAttributes :: HorizontalTreeConfig -> Array Chainable
containerAttributes config = [
    fontFamily "sans-serif"
  , fontSize   10.0
  , transform [ translateContainer config.rootDx config.rootDy config.x0 ]
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
makeModel :: forall v. Tuple Number Number -> TreeJson_
   -> { config :: TreeConfig
      , json :: TreeJson_
      , root :: D3HierarchicalNode_
      }
makeModel widthHeight json = { json, root: config.root, config }
  where
    root1  = hierarchy_ json
    config = initHorizontalTree widthHeight root1

-- | recipe for a radial tree
enter :: forall m v. Bind m => D3Tagless m => MonadState (D3State (H.Model String v)) m => 
  HorizontalTreeConfig -> H.Model String v -> m D3Selection_
enter config model = do
  root      <- hook "div#tree"
  svg       <- root      `appendTo` (node Svg (svgAttributes width (svgHeight config)))
  container <- svg       `appendTo` (node Group (containerAttributes config))
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
                    { extent    : ZoomExtent { top: 0.0, left: 0.0 , bottom: height, right: width }
                    , scale     : ScaleExtent 1 8 -- wonder if ScaleExtent ctor could be range operator `..`
                    , qualifier : "tree"
                    }

  pure svg


