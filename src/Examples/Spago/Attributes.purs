module D3.Examples.Spago.Attributes where

import D3.Data.Types (Datum_)
import D3.Examples.Spago.Files (NodeType(..))
import D3.Examples.Spago.Model (datumIsGraphNode, datumIsSpagoLink, datumIsSpagoSimNode, packageRadius)
import D3.Examples.Tree.Configure (datumIsTreeNode)
import D3.FFI (hasChildren_)
import D3.Node (D3_Link(..), D3_SimulationNode(..), D3_TreeNode(..), D3_XY, NodeID, getNodeX, getNodeY, getSourceX, getSourceY, getTargetX, getTargetY)
import D3.Scales (d3SchemeCategory10N_)
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (fromMaybe)
import Math (pi)
import Math (sqrt) as Math
import Prelude (negate, show, ($), (*), (-), (/), (<), (<>), (==), (>=))
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

positionLabel :: Datum_ -> Number
positionLabel datum = do
  let (D3SimNode d) = datumIsSpagoSimNode datum
  case d.nodetype of
    (IsModule _)  -> negate d.loc
    (IsPackage _) -> 0.0

nodeClass :: Datum_ -> String
nodeClass datum = do
  let (D3SimNode d) = datumIsSpagoSimNode datum
  show d.nodetype <> " " <> d.packageName <> " " <> d.name

linkClass :: Datum_ -> String
linkClass datum = do
  let (D3_Link d) = datumIsSpagoLink datum
  show d.linktype

translateNode :: Datum_ -> String
translateNode datum = "translate(" <> show x <> "," <> show y <> ")"
  where 
    d = datumIsGraphNode datum
    (x :: Number) = (unsafeCoerce datum).x
    (y :: Number) = (unsafeCoerce datum).y

colorByGroup :: Datum_ -> String
colorByGroup datum = d3SchemeCategory10N_ d.packageID
  where
    (D3SimNode d) = unsafeCoerce datum

datumDotRadius :: Datum_ -> Number
datumDotRadius datum = d.r
  where
    (D3SimNode d) = unsafeCoerce datum


setX1 :: Datum_ -> Number
setX1 = getSourceX
setY1 :: Datum_ -> Number
setY1 = getSourceY
setX2 :: Datum_ -> Number
setX2 = getTargetX
setY2 :: Datum_ -> Number
setY2 = getTargetY
setCx :: Datum_ -> Number
setCx = getNodeX
setCy :: Datum_ -> Number
setCy = getNodeY

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
