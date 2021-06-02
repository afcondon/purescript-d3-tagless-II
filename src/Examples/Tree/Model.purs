module D3.Examples.Tree.Model where


import D3.Node (D3SimulationRow, D3TreeRow, D3_Link, EmbeddedData, NodeID)
import Type.Row (type (+))


-- Model data types specialized with inital data
type FlareNodeRow row = ( name :: String | row )
type FlareNodeData    = { | FlareNodeRow () }

type FlareTreeNode    = D3TreeRow       (EmbeddedData FlareNodeData + ())
type FlareSimNode     = D3SimulationRow (             FlareNodeRow  + ())

type FlareLinkData  = ( value :: Number )
type FlareSimRecord = Record (FlareNodeRow  + ()) 
type FlareLinkObj   =  { source :: FlareSimRecord, target :: FlareSimRecord | FlareLinkData }

type FlareRawModel = { 
    links :: Array (D3_Link NodeID FlareLinkData)
  , nodes :: Array FlareNodeData
}

type FlareCookedModel = { 
    links :: Array (D3_Link NodeID FlareLinkData)
  , nodes :: Array FlareNodeData
}
