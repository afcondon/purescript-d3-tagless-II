module D3.Examples.Spago.Attributes where

import D3.Data.Types (Datum_)
import D3.Examples.Spago.Files (NodeType(..))
import D3.Examples.Spago.Model (packageRadius)
import D3.Examples.Tree.Configure (datumIsTreeNode)
import D3.FFI (hasChildren_)
import D3.Node (D3_SimulationNode(..), D3_TreeNode(..), D3_XY, NodeID)
import D3.Scales (d3SchemeCategory10N_)
import Data.Int (toNumber)
import Data.Map as M
import Data.Maybe (fromMaybe)
import Math (pi)
import Math (sqrt) as Math
import Prelude (show, ($), (*), (-), (/), (<), (<>), (==), (>=))
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

-- these ones came from the TreeScript

radialRotate :: Number -> String
radialRotate x = show $ (x * 180.0 / pi - 90.0)

radialRotateCommon :: forall r. D3_TreeNode (D3_XY + r) -> String
radialRotateCommon (D3TreeNode d) = "rotate(" <> radialRotate d.x <> ")"

radialTreeTranslate :: forall r. D3_TreeNode (D3_XY + r) -> String
radialTreeTranslate (D3TreeNode d) = "translate(" <> show d.y <> ",0)"

rotateRadialLabels :: forall r. D3_TreeNode (D3_XY + r) -> String
rotateRadialLabels (D3TreeNode d) = -- TODO replace with nodeIsOnRHS 
  "rotate(" <> if d.x >= pi 
  then "180" <> ")" 
  else "0" <> ")"

nodeIsOnRHS :: Datum_ -> Boolean
nodeIsOnRHS d = node.x < pi
  where (D3TreeNode node) = datumIsTreeNode d

textDirection :: Datum_ -> Boolean
textDirection = \d -> hasChildren_ d == nodeIsOnRHS d

labelName :: Datum_ -> String
labelName d = node."data".name
  where node = unsafeCoerce d

colorByGroupTree :: M.Map NodeID NodeID -> Datum_ -> String
colorByGroupTree packageMap datum = d3SchemeCategory10N_ (toNumber $ fromMaybe 0 packageID)
  where
    (D3SimNode d) = unsafeCoerce datum
    packageID     = M.lookup d.data.id packageMap

chooseRadiusTree :: Datum_ -> Number
chooseRadiusTree datum = do
  let (D3SimNode d) = unsafeCoerce datum -- TODO unsafe because despite all the row types this still cashes out to a simnode not a treenode here which must be fixed
  case d.data.nodetype of
    (IsModule _)  -> Math.sqrt d.data.loc
    (IsPackage _) -> packageRadius
