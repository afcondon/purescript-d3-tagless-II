-- | Initialize node positions from auxiliary properties
-- | This bridges the gap between node initializers (which set gridXY, treeXY, etc.)
-- | and D3 force simulation (which needs x, y properties)
module Component.CodeExplorerV2.InitPositions where

import Effect (Effect)

-- | Initialize x/y from gridXY for nodes that don't have positions
-- | Mutates the node array
foreign import initPositionsFromGridXY_ :: forall node. Array node -> Effect (Array node)
