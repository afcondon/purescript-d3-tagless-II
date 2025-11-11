module D3.Viz.LesMis.Unsafe where

import Prelude
import PSD3.Internal.Types (Datum_)
import D3.Viz.LesMiserables.Model (LesMisSimNode)
import PSD3.Data.Node (SimulationNode(..))
import Unsafe.Coerce (unsafeCoerce)

-- | Node type after unboxing from Datum_
-- | D3 flattens the SimulationNode structure when processing
-- | So at runtime, id and group are at the top level (not nested under data_)
type LesMisNode =
  { id :: String
  , group :: Int
  , x :: Number
  , y :: Number
  , vx :: Number
  , vy :: Number
  , fx :: Number
  , fy :: Number
  }

-- | Convert Datum_ to flattened node record
-- | D3 flattens the structure, so we get a flat object at runtime
unboxD3SimNode :: Datum_ -> LesMisNode
unboxD3SimNode datum = unsafeCoerce datum

-- | Convert Datum_ to swizzled link
-- | After D3 swizzles links, source/target are node object references
type SwizzledLesMisLink =
  { source :: LesMisNode
  , target :: LesMisNode
  , value :: Number
  }

unboxD3SimLink :: Datum_ -> SwizzledLesMisLink
unboxD3SimLink datum = unsafeCoerce datum

-- Accessor helpers for flattened node structure
-- D3 flattens the nodes, so we access fields at the top level

getNodeX :: LesMisNode -> Number
getNodeX n = n.x

getNodeY :: LesMisNode -> Number
getNodeY n = n.y

getNodeId :: LesMisNode -> String
getNodeId n = n.id

getNodeGroup :: LesMisNode -> Int
getNodeGroup n = n.group

-- Link accessors
getLinkValue :: SwizzledLesMisLink -> Number
getLinkValue l = l.value

getLinkSourceX :: SwizzledLesMisLink -> Number
getLinkSourceX l = l.source.x

getLinkSourceY :: SwizzledLesMisLink -> Number
getLinkSourceY l = l.source.y

getLinkTargetX :: SwizzledLesMisLink -> Number
getLinkTargetX l = l.target.x

getLinkTargetY :: SwizzledLesMisLink -> Number
getLinkTargetY l = l.target.y

getLinkTargetGroup :: SwizzledLesMisLink -> Int
getLinkTargetGroup l = l.target.group
