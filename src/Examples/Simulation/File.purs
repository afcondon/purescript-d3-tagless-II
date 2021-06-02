module D3.Examples.LesMiserables.File where

import Affjax (Error)
import D3.Data.Types (Datum_)
import D3.Node (D3SimulationRow, D3TreeRow, D3_ID, D3_Indexed, D3_Leaf, D3_Link(..), D3_SimulationNode(..), D3_TreeNode(..), D3_TreeRow, D3_VxyFxy, D3_XY, EmbeddedData, NodeID)
import D3.Scales (d3SchemeCategory10N_)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Nullable (Nullable)
import Prelude (($))
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

-- TODO no error handling at all here RN (OTOH - performant!!)
foreign import readJSONJS :: String -> LesMisRawModel 

readGraphFromFileContents :: forall r. Either Error { body ∷ String | r } -> LesMisRawModel
readGraphFromFileContents (Right { body } ) = readJSONJS body
-- TODO exceptions dodged using empty Model, fix with Maybe
readGraphFromFileContents (Left err)        = { links: [], nodes: [] } 

-- | ==========================================================================================
-- |                  Model data types specialized with inital data
-- | ==========================================================================================

-- we make the model like so, but D3 then swizzles it to the "cooked" model below
type LesMisRawModel = { links :: Array (D3_Link NodeID LesMisLinkData) 
                      , nodes :: Array LesMisSimNode 
                      }
-- this is what the model looks like after the NodeID references have been swizzled in the Simulation code
type LesMisCookedModel = { links :: Array LesMisGraphLinkObj
                         , nodes :: Array LesMisSimNode
                         }

-- the "extra / model-specific" data above and beyond what any D3 Tree Node is going to have:
type LesMisNodeRow row = ( group :: Int | row ) 
-- this extra data inside a D3SimNode as used in PureScript:
type LesMisSimNode     = D3SimulationRow ( LesMisNodeRow  + ()) 
-- same as above but as a bare record, this is the "datum" that D3 sees and which it returns to you for attr setting:
type LesMisSimRecord   = Record (D3_Indexed + D3_XY + D3_VxyFxy + LesMisNodeRow  + ()) 
-- now a definition for that same row if it is embedded instead in a D3 Hierarchical structure, in which case
-- our extra data is available in the "datum" as an embedded object at the field "data"
type LesMisTreeNode    = D3TreeRow (EmbeddedData { | LesMisNodeRow () } + ())
-- type LesMisTreeRecord  = Record    (D3_ID + D3_TreeRow + D3_XY   + D3_Leaf + EmbeddedData { | LesMisNodeRow () } + ())

-- first the "extra / model-specific" data in the links
type LesMisLinkData = ( value :: Number )
type LesMisGraphLinkObj =  { source :: LesMisSimRecord, target :: LesMisSimRecord | LesMisLinkData }


unboxD3SimNode :: Datum_ -> LesMisSimRecord
unboxD3SimNode datum = do
  let (D3SimNode d) = unsafeCoerce datum
  d

unboxD3SimLink :: Datum_ -> LesMisGraphLinkObj
unboxD3SimLink datum = do
  let (D3_Link l) = unsafeCoerce datum
  l

unboxD3TreeNode :: Datum_
  -> { children :: Array
                     (D3_TreeNode
                        ( data :: { group :: Int
                                  }
                        , depth :: Int
                        , height :: Int
                        , id :: Int
                        , isLeaf :: Boolean
                        , value :: Nullable Number
                        , x :: Number
                        , y :: Number
                        )
                     )
     , data :: { group :: Int
               }
     , depth :: Int
     , height :: Int
     , id :: Int
     , isLeaf :: Boolean
     , parent :: Nullable
                   (D3_TreeNode
                      ( data :: { group :: Int
                                }
                      , depth :: Int
                      , height :: Int
                      , id :: Int
                      , isLeaf :: Boolean
                      , value :: Nullable Number
                      , x :: Number
                      , y :: Number
                      )
                   )
     , value :: Nullable Number
     , x :: Number
     , y :: Number
     }
unboxD3TreeNode datum = do
  let (t' :: D3_TreeNode (D3_ID + D3_TreeRow + D3_XY   + D3_Leaf + (EmbeddedData { | LesMisNodeRow () }) + () ) )  = unsafeCoerce datum
      (D3TreeNode t) = t'
  t

datum_tree_ :: { depth :: Datum_ -> Int
, height :: Datum_ -> Int
, id :: Datum_ -> Int
, isLeaf :: Datum_ -> Boolean
, value :: Datum_ -> Nullable Number
, x :: Datum_ -> Number
, y :: Datum_ -> Number
}
datum_tree_ = {
    depth : (\d -> (unboxD3TreeNode d).depth)
  , height: (\d -> (unboxD3TreeNode d).height)
  , id    : (\d -> (unboxD3TreeNode d).id)
  , isLeaf: (\d -> (unboxD3TreeNode d).isLeaf)
  , value : (\d -> (unboxD3TreeNode d).value)
  , x     : (\d -> (unboxD3TreeNode d).x)
  , y     : (\d -> (unboxD3TreeNode d).y)
}

link_ :: { source :: Datum_
            -> { fx :: Nullable Number
               , fy :: Nullable Number
               , group :: Int
               , index :: Int
               , vx :: Number
               , vy :: Number
               , x :: Number
               , y :: Number
               }
, target :: Datum_
            -> { fx :: Nullable Number
               , fy :: Nullable Number
               , group :: Int
               , index :: Int
               , vx :: Number
               , vy :: Number
               , x :: Number
               , y :: Number
               }
, value :: Datum_ -> Number
}
link_ = {
    source: (\d -> (unboxD3SimLink d).source)
  , target: (\d -> (unboxD3SimLink d).target)
  , value:  (\d -> (unboxD3SimLink d).value)
}

datum_ :: { colorByGroup :: Datum_ -> String
, group :: Datum_ -> Int
, index :: Datum_ -> Int
, x :: Datum_ -> Number
, y :: Datum_ -> Number
}
datum_ = {
-- direct accessors to fields of the datum (BOILERPLATE)
    index : (\d -> (unboxD3SimNode d).index)
  , x     : (\d -> (unboxD3SimNode d).x)
  , y     : (\d -> (unboxD3SimNode d).y)
  , group : (\d -> (unboxD3SimNode d).group)

  , colorByGroup:
      (\d -> d3SchemeCategory10N_ (toNumber $ datum_.group d))
}
