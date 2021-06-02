module D3.Examples.MetaTree.Unsafe where

import D3.Examples.MetaTree.Model (MetaTreeNodeRow)
import D3.Node (D3_ID, D3_TreeNode(..), D3_TreeRow, D3_XY, EmbeddedData)
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

unboxD3TreeNode datum = do
  let (t' :: D3_TreeNode (D3_ID + D3_TreeRow + D3_XY + (EmbeddedData { | MetaTreeNodeRow () }) + () ) )  = unsafeCoerce datum
      (D3TreeNode t) = t'
  t
