module D3.Viz.Tree.Unsafe where


import D3.Data.Types (Datum_)
import D3.Viz.Tree.Model (FlareLinkObj, FlareNodeRow)
import D3.Node (D3_ID, D3LinkSwizzled(..), D3_TreeNode(..), D3_TreeRow, D3_XY, EmbeddedData)
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)


unboxD3SimLink :: Datum_ -> FlareLinkObj
unboxD3SimLink datum = l
  where (D3LinkObj l) = unsafeCoerce datum

unboxD3TreeNode datum = do
  let (t' :: D3_TreeNode (D3_ID + D3_TreeRow + D3_XY + (EmbeddedData { | FlareNodeRow () }) + () ) )  = unsafeCoerce datum
      (D3TreeNode t) = t'
  t