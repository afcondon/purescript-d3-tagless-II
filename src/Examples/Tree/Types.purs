module D3.Examples.Tree.Types where

import D3.Data.Types (Datum_, Selector)
import D3.Node (D3_Hierarchy_Node_XY)
import D3.Selection (Chainable)

-- a record that packages up all the customizations that are needed to render the 6 variations on Tree
type ScriptConfig d = { 
    linkPath      :: Chainable
  , selector      :: Selector
  , spacing       :: { interChild :: Number, interLevel :: Number }
  , tree          :: D3_Hierarchy_Node_XY d
  , viewbox       :: Array Chainable
  , nodeTransform :: Array Chainable
  , color         :: String
  , textDirection :: Datum_ -> Boolean
  , svg           :: { width :: Number, height :: Number }
}
