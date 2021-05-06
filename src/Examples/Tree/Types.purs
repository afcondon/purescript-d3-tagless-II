module D3.Examples.Tree.Types where

import D3.Data.Types (D3HierarchicalNode_, D3HierarchicalNode(..), Datum_, Selector)
import D3.Selection (Chainable)
import Unsafe.Coerce (unsafeCoerce)

-- this is the extra data that is part of a Datum_ in addition to the fields from D3HierarchicalNode_
type TreeNodeExtra = { name :: String }

-- a record that packages up all the customizations that are needed to render the 6 variations on Tree
type ScriptConfig = { 
    linkPath      :: Chainable
  , selector      :: Selector
  , spacing       :: { interChild :: Number, interLevel :: Number }
  , tree          :: D3HierarchicalNode_
  , viewbox       :: Array Chainable
  , nodeTransform :: Array Chainable
  , color         :: String
  , textDirection :: Datum_ -> Boolean
  , svg           :: { width :: Number, height :: Number }
}

-- | Coercion function to recover the structure that was given to D3, it's an unsafeCoerce but the types
-- | give some protection
datumIsTreeNode :: forall d v. Datum_ -> D3HierarchicalNode d v
datumIsTreeNode = unsafeCoerce

-- | Coercion function to recover the "extra" data that lives within the generic structure that was given to D3, 
-- | it's an unsafeCoerce but the types give some protection
labelName :: Datum_ -> String
labelName d = node."data".name
  where (D3HierarchicalNode node) = datumIsTreeNode d
