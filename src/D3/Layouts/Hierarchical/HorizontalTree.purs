module D3.Layouts.Hierarchical.HorizontalTree where

import D3.Attributes.Instances (Attribute(..), Datum, toAttr)
import D3.Layouts.Hierarchical.Types (D3HierarchicalNode_, TreeConfig(..), hNodeHeight_)
import D3.Selection (Chainable(..))
import Data.Array ((!!))
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import Prelude (($), (+), (/))

-- this function is given an initialized hierarchy root and some config info
initHorizontalTree :: Tuple Number Number -> D3HierarchicalNode_ -> TreeConfig
initHorizontalTree (Tuple width height) root = do
  let rootDx = 10.0
      rootHeight = hNodeHeight_ root
      rootDy = width / (height + 1.0)
      tree   = initHorizontalTree_ root [rootDx, rootDy]
      { x0, x1 }  = horizontalTreeX0X1 tree 
  HorizontalTree { rootDx, rootDy, x0, x1 }

horizontalLink :: Chainable
horizontalLink = AttrT $ Attribute "d" $ toAttr linkHorizontal_

horizontalTreeX0X1 :: D3HierarchicalNode_ -> { x0 :: Number, x1 :: Number }
horizontalTreeX0X1 d = do
  let result = horizontalTreeX0X1_ d
      x0     = fromMaybe 0.0 $ result !! 0
      x1     = fromMaybe 0.0 $ result !! 1
  { x0, x1 }

foreign import initHorizontalTree_ :: D3HierarchicalNode_ -> Array Number -> D3HierarchicalNode_ -- effectful function on hierarchy
foreign import linkHorizontal_     :: (Datum -> String) 
foreign import horizontalTreeX0X1_ :: D3HierarchicalNode_ -> Array Number
  