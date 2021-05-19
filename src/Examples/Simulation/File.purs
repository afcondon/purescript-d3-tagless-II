module D3.Examples.LesMiserables.File where

import D3.Node

import Affjax (Error)
import Data.Either (Either(..))
import Data.Nullable (Nullable, null, notNull)
import Type.Row (type (+))

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
                      , nodes :: Array LesMisNodeData }

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