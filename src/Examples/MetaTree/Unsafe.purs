module D3.Examples.MetaTree.Unsafe where

import D3.Data.Types (Datum_)
import D3.Examples.MetaTree.Model (MetaTreeNodeRow)
import D3.Node (D3_ID, D3_TreeNode(..), D3_TreeRow, D3_XY, EmbeddedData)
import Data.Nullable (Nullable)
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

unboxD3TreeNode :: forall t5.
  Datum_
  -> { depth :: Int
     , height :: Int
     , id :: Int
     , value :: Nullable Number
     | t5
     }
unboxD3TreeNode datum = do
  let (D3TreeNode t)  = unsafeCoerce datum
  t
