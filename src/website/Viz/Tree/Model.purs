module D3.Viz.Tree.Model where


import PSD3.Data.Node (D3TreeRow, D3Link, D3_SimulationNode, D3_VxyFxy, D3_XY, EmbeddedData, NodeID)
import Type.Row (type (+))


-- Model data types specialized with inital data
type FlareNodeRow row = ( name :: String | row )
type FlareNodeData    = { | FlareNodeRow () }

type FlareTreeNode    = D3TreeRow       (EmbeddedData FlareNodeData + ())
-- type FlareSimNode     = D3SimulationRow (             FlareNodeRow  + ())
type FlareSimNode     = D3_SimulationNode (FlareNodeRow + D3_XY + D3_VxyFxy + ())

type FlareLinkData  = ( value :: Number )
type FlareSimRecord = Record (FlareNodeRow  + ()) 
type FlareLinkObj   =  { source :: FlareSimRecord, target :: FlareSimRecord | FlareLinkData }

type FlareRawModel = { 
    links :: Array (D3Link NodeID FlareLinkData)
  , nodes :: Array FlareNodeData
}

type FlareCookedModel = { 
    links :: Array (D3Link NodeID FlareLinkData)
  , nodes :: Array FlareNodeData
}
