module D3.Examples.Tree where

import Affjax (Error)
import Control.Monad.State (class MonadState, get)
import D3.Attributes.Instances (Attribute(..), Datum, toAttr)
import D3.Attributes.Sugar (classed, fill, height, radius, strokeColor, strokeOpacity, strokeWidth, transform, viewBox, width)
import D3.Interpreter.Tagless (class D3Tagless, appendTo, hook, join)
import D3.Selection (Chainable(..), D3Data_, D3Selection_, D3State(..), Element(..), EnterUpdateExit, Join(..), Keys(..), SelectionName(..), makeProjection, node)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Math (pi)
import Prelude (class Bind, bind, negate, pure, show, ($), (*), (-), (/), (<>), (>=))
import Unsafe.Coerce (unsafeCoerce)

-- three little transform functions to build up the transforms on nodes and labels
rotate :: Number -> String
rotate x       = show $ (x * 180.0 / pi - 90.0)
rotateCommon :: forall a. D3TreeNode a -> String
rotateCommon d = "rotate(" <> rotate d.x <> ")"
rotateText2 d  = "rotate(" <> if d.x >= pi 
                              then "180" <> ")" 
                              else "0" <> ")"
-- same translation for both text and node
translate :: forall a. D3TreeNode a -> String
translate d = "translate(" <> show d.y <> ",0)"

transformations :: forall a. Array (D3TreeNode a -> String)
transformations = [ rotateCommon, translate ]

-- | Script components, attributes, transformations etc
svgAttributes :: Array Chainable
svgAttributes = [
    width 1000.0
  , height 1000.0
  , viewBox (-500.0) (-500.0) 1000.0 1000.0
]

-- | instructions for entering the links in the radial tree
enterLinks :: EnterUpdateExit
enterLinks =
  { enter:  
    [ strokeWidth 1.5
    , strokeColor "#555"
    , strokeOpacity 0.4
    , fill "none"
    , radialLink (\d -> d.x) (\d -> d.y)
    ] 

  , update: [] 
  , exit:   []
  }

-- | instructions for entering the nodes in the radial tree
enterNodes :: EnterUpdateExit
enterNodes =
  { enter:
    [ transform transformations
    , fill (\d -> if hasChildren_ d then "#555" else "#999")
    , radius 2.5
    ]
  
  , update: []
  , exit: []
  }

-- this is the extra row info that is part of a Datum beyond the D3Tree minimum
type TreeNodeExtra = (name :: String) 
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
  links  <- appendTo svg "links-group"  (node Group [ classed "links"])
  nodes  <- appendTo svg "nodes-group"  (node Group [ classed "nodes"])
  labels <- appendTo svg "labels-group" (node Group [ classed "labels"])

  (D3State state) <- get

  linkJoinSelection_ <- join state.model $ Join {
      element   : Path
    , key       : DatumIsKey
    , selection : SelectionName "links-group"
    , projection: unsafeCoerce $ makeProjection (\model -> d3HierarchyLinks_ model.d3Tree)
    , behaviour : enterLinks
  }

  nodeJoinSelection_ <- join state.model $ Join {
      element   : Circle
    , key       : DatumIsKey
    , selection : SelectionName "nodes-group"
    , projection: unsafeCoerce $ makeProjection (\model -> d3HierarchyDescendants_ model.d3Tree)
    , behaviour : enterNodes
  }

  pure svg


-- | TODO All this stuff below belongs eventually in library, ie D3.Layout.Hierarchical or something
data Tree a = Node a (Array (Tree a))
type TreeConfig :: forall k. k -> Type
type TreeConfig a = {
    size       :: Array Number
  , separation :: Datum -> Datum -> Int
}

radialTreeConfig :: forall a. Number -> TreeConfig a
radialTreeConfig width = 
  { size      : [2.0 * pi, width / 2.0]
  , separation: radialSeparationJS_
  }

type Model a = {
      json   :: TreeJson
    , d3Tree :: D3Tree
    , config :: TreeConfig a
}

type D3TreeNode r = {
    x        :: Number
  , y        :: Number
  , value    :: String
  , depth    :: Number
  , height   :: Number
-- these next too are guaranteed coercible to the same type, ie D3TreeNode
-- BUT ONLY IF the D3Tree is a successful conversion using d3Hierarchy
-- TODO code out exceptions
  , parent   :: RecursiveD3TreeNode       -- this won't be present in the root node
  , children :: Array RecursiveD3TreeNode -- this won't be present in leaf nodes
  | r -- whatever other fields we fed in to D3.hierarchy will still be present, but they're not generic, ie need coercion
}

-- helpers for Radial tree
radialLink :: forall a b. (a -> Number) -> (b -> Number) -> Chainable
radialLink angleFn radius_Fn = do
  let radialFn = d3LinkRadial_ (unsafeCoerce angleFn) (unsafeCoerce radius_Fn)
  AttrT $ Attribute "d" $ toAttr radialFn


-- do the decode on the Purescript side unless files are ginormous, this is just for prototyping
-- this is an opaque type behind which hides the data type of the Purescript tree that was converted
foreign import data RecursiveD3TreeNode :: Type
-- this is the Purescript Tree after processing in JS to remove empty child fields from leaves etc
-- need to ensure that this structure is encapsulated in libraries (ie by moving this code)
foreign import data D3Tree              :: Type
foreign import data D3Hierarchical      :: Type
foreign import data TreeJson            :: Type
foreign import radialSeparationJS_ :: Datum -> Datum -> Int
foreign import readJSONJS_         :: String -> TreeJson -- TODO no error handling at all here RN
foreign import d3Hierarchy_        :: TreeJson -> D3Hierarchical
foreign import d3InitTree_         :: forall a. TreeConfig a -> D3Hierarchical -> D3Tree 
foreign import hasChildren_        :: Datum -> Boolean
foreign import d3LinkRadial_       :: (Datum -> Number) -> (Datum -> Number) -> (Datum -> String)

foreign import d3HierarchyLinks_       :: D3Tree -> D3Data_
foreign import d3HierarchyDescendants_ :: D3Tree -> D3Data_
