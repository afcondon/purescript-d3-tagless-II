module D3.Viz.MetaTree.Unsafe where

import PSD3.Internal.Types (Datum_)
import D3.Viz.MetaTree.Model (MetaTreeNodeRow)
import PSD3.Data.Node (D3_ID, D3_TreeNode(..), D3_TreeRow, D3_XY, EmbeddedData)
import Data.Nullable (Nullable)
import Halogen.Svg.Attributes (r)
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

unboxD3TreeNode :: forall row.
  D3_TreeNode row
  -> { depth :: Int
     , height :: Int
     , id :: Int
     , value :: Nullable Number
     | row
     }
unboxD3TreeNode (D3TreeNode t) = t

coerceToTreeNode :: forall r. Datum_ -> D3_TreeNode r
coerceToTreeNode = unsafeCoerce
