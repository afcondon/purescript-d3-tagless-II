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
foreign import readJSONJS :: String -> LesMisModel 

readGraphFromFileContents :: forall r. Either Error { body ∷ String | r } -> LesMisModel
readGraphFromFileContents (Right { body } ) = readJSONJS body
-- TODO exceptions dodged using empty Model, fix with Maybe
readGraphFromFileContents (Left err)        = { links: [], nodes: [] } 


-- Model data types specialized with inital data
type LesMisNodeRow row = ( group :: Int | row )
type LesMisNodeData    = { | LesMisNodeRow () }

type LesMisTreeNode    = D3TreeRow       (EmbeddedData LesMisNodeData + ())
type LesMisSimNode     = D3SimulationRow (             LesMisNodeRow  + ())

type LesMisLinkData = ( value :: Number )
type LesMisModel    = { links :: Array (D3_Link NodeID LesMisLinkData)
                      , nodes :: Array LesMisSimNode }

type LesMisDataRecord = Record (D3_Indexed + D3_XY + D3_VxyFxy + LesMisNodeRow  + ())
type LesMisGraphLinkObj =  { source :: LesMisDataRecord, target :: LesMisDataRecord | LesMisLinkData }

unboxD3SimNode :: Datum_ -> LesMisDataRecord
unboxD3SimNode datum = do
  let (D3SimNode d) = unsafeCoerce datum
  d

unboxD3SimLink :: Datum_ -> LesMisGraphLinkObj
unboxD3SimLink datum = do
  let (D3_Link l) = unsafeCoerce datum
  l

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
  , group: 2
}