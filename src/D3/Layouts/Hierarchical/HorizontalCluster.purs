module D3.Layouts.Hierarchical.HorizontalCluster (
    initHorizontalCluster
  , horizontalClusterLink
 ) where

import D3.Attributes.Instances (Attribute(..), Datum, toAttr)
import D3.Layouts.Hierarchical.Types (D3HierarchicalNode_, TreeConfig(..), hNodeHeight_)
import D3.Selection (Chainable(..))
import Data.Tuple (Tuple(..))
import Debug (spy)
import Prelude (($), (+), (/))

-- this function is given an initialized hierarchy root and some config info
initHorizontalCluster :: Tuple Number Number -> D3HierarchicalNode_ -> TreeConfig
initHorizontalCluster (Tuple width height) root = do
  let rootDx = 10.0
      rootHeight = spy "horizontal tree root height: " $ hNodeHeight_ root
      rootDy = spy "root.dy: " $ width / (rootHeight + 1.0)
      tree   = initHorizontalCluster_ root [rootDx, rootDy]
  HorizontalCluster { rootDx, rootDy }

horizontalClusterLink :: Chainable
horizontalClusterLink = AttrT $ Attribute "d" $ toAttr linkHorizontalCluster_



foreign import linkHorizontalCluster_ :: (Datum -> String) 
foreign import initHorizontalCluster_ :: D3HierarchicalNode_ -> Array Number -> D3HierarchicalNode_ -- effectful function on hierarchy