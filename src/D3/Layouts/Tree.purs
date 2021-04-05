module D3.Layouts.Tree where


import D3.Attributes.Instances (Attribute(..), Datum, toAttr)
import D3.Selection (Chainable(..), D3Data_)
import Math (pi)
import Prelude (($), (*), (/))
import Unsafe.Coerce (unsafeCoerce)

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

type Model :: forall k. k -> Type
type Model a = {
      json   :: TreeJson
    , d3Tree :: D3Tree
    , config :: TreeConfig a
}

type D3TreeNode d = {
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
  , "data"   :: d -- whatever other fields we fed in to D3.hierarchy will still be present, but they're not generic, ie need coercion
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
