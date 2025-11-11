module D3.Viz.LesMiserables.Model where

import PSD3.Data.Node (D3Link_Unswizzled, D3_SimulationNode, D3_VxyFxy, D3_XY)
import Type.Row (type (+))

-- | ==========================================================================================
-- |                  Model data types specialized with inital data
-- | ==========================================================================================

-- the "extra / model-specific" data above and beyond what any D3 Tree Node is going to have:
type LesMisNodeData row = ( id :: String, group :: Int | row ) 
-- this extra data inside a D3SimNode as used in PureScript:
type LesMisSimNode     = D3_SimulationNode ( LesMisNodeData  + D3_XY + D3_VxyFxy + ()) 

-- first the "extra / model-specific" data in the links
type LesMisLinkData     = ( value :: Number )
type LesMisGraphLinkObj = { source :: LesMisSimRecord, target :: LesMisSimRecord | LesMisLinkData }


-- we make the model like so, but D3 then swizzles it to the "cooked" model below
-- the source and target in the links are given as "String" to match id in the node data (UNSWIZZLED)
type LesMisRawModel    = { links :: Array D3Link_Unswizzled, nodes :: Array LesMisSimNode  }

-- same as above but as a bare record, this is the "datum" that D3 sees and which it returns to you for attr setting:
type LesMisSimRecord   = Record (D3_XY + D3_VxyFxy + LesMisNodeData  + ())



