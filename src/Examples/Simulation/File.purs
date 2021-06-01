module D3.Examples.LesMiserables.File where

import D3.Node
import Prelude

import Affjax (Error)
import D3.Data.Types (Datum_)
import D3.Scales (d3SchemeCategory10N_)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Nullable (Nullable, null, notNull)
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
type LesMisTreeRecord  = Record    (D3_ID + D3_TreeRow + D3_XY   + D3_Leaf + EmbeddedData { | LesMisNodeRow () } + ())

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

unboxD3TreeNode :: Datum_ -> LesMisTreeRecord
unboxD3TreeNode datum = do
  let (D3TreeNode t) = unsafeCoerce datum
  t

link = {
    source: (\d -> (unboxD3SimLink d).source)
  , target: (\d -> (unboxD3SimLink d).target)
  , value:  (\d -> (unboxD3SimLink d).value)
}

datum = {
-- direct accessors to fields of the datum (BOILERPLATE)
    index : (\d -> (unboxD3SimNode d).index)
  , x     : (\d -> (unboxD3SimNode d).x)
  , y     : (\d -> (unboxD3SimNode d).y)
  , group : (\d -> (unboxD3SimNode d).group)

  , colorByGroup:
      (\d -> d3SchemeCategory10N_ (toNumber $ datum.group d))
}

lesMisTreeNode :: LesMisTreeNode
lesMisTreeNode = D3TreeNode { 
    parent  : null
  , children: []
  , isLeaf  : true
  , id      : 0
  , depth   : 0
  , height  : 0
  , value   : notNull 10.0
  , x       : 0.0
  , y       : 0.0
  , "data"  : { group: 2 }
}

lesMisSimNode :: LesMisSimNode
lesMisSimNode = D3SimNode { 
    index : 0
  , x     : 0.0
  , y     : 0.0
  , vx    : 0.0
  , vy    : 0.0
  , fx    : (null :: Nullable Number)
  , fy    : (null :: Nullable Number)
  , group : 2
}