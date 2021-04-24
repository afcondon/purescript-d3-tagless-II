module D3.Layouts.Hierarchical.HorizontalTree (
    initHorizontalTree
  , horizontalLink
 ) where

import D3.Attributes.Instances (Attribute(..), Datum, toAttr)
import D3.Layouts.Hierarchical.Types (D3HierarchicalNode_, TreeConfig(..), hNodeHeight_)
import D3.Selection (Chainable(..))
import Data.Tuple (Tuple(..))
import Debug (spy)
import Prelude (($), (+), (/))

-- this function is given an initialized hierarchy root and some config info
initHorizontalTree :: Tuple Number Number -> D3HierarchicalNode_ -> TreeConfig
initHorizontalTree (Tuple width height) root = do
  let rootDx = 10.0
      rootHeight = spy "horizontal tree root height: " $ hNodeHeight_ root
      rootDy = spy "root.dy: " $ width / (rootHeight + 1.0)
      tree   = initHorizontalTree_ root [rootDx, rootDy]
      { x0, x1 }  = horizontalTreeX0X1_ tree 
  HorizontalTree { rootDx, rootDy, x0, x1 }

horizontalLink :: Chainable
horizontalLink = AttrT $ Attribute "d" $ toAttr linkHorizontal_

foreign import initHorizontalTree_ :: D3HierarchicalNode_ -> Array Number -> D3HierarchicalNode_ -- effectful function on hierarchy
foreign import linkHorizontal_     :: (Datum -> String) 
foreign import horizontalTreeX0X1_ :: D3HierarchicalNode_ -> { x0 :: Number, x1 :: Number }
  