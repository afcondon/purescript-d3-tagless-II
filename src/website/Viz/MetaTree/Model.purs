module D3.Viz.MetaTree.Model where

import D3.Node (D3TreeRow, D3Link, EmbeddedData, NodeID)

import Type.Row (type (+))

-- Model data types specialized with inital data
type MetaTreeNodeRow row = ( 
    name   :: String
  , symbol :: String
  , param1 :: String
  , param2 :: String 
  | row )
type MetaTreeNodeData = { | MetaTreeNodeRow () }

type MetaTreeNode     = D3TreeRow (EmbeddedData MetaTreeNodeData + ())

type MetaTreeLinkData  = ( example :: Number )
type MetaTreeSimRecord = Record (MetaTreeNodeRow  + ()) 
type MetaTreeLinkObj   =  { source :: MetaTreeSimRecord, target :: MetaTreeSimRecord | MetaTreeLinkData }

type MetaTreeRawModel = { 
    links :: Array (D3Link NodeID MetaTreeLinkData)
  , nodes :: Array MetaTreeNodeData
}

type MetaTreeCookedModel = { 
    links :: Array (D3Link NodeID MetaTreeLinkData)
  , nodes :: Array MetaTreeNodeData
}
