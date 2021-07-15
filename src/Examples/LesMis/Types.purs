module D3.Examples.LesMiserables.Types where

import D3.Node (D3SimulationRow, D3TreeRow, D3_Indexed, D3_Link, D3_VxyFxy, D3_XY, EmbeddedData, NodeID)
import Type.Row (type (+))

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
type LesMisNodeRow row = ( id :: String, group :: Int | row ) 
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


