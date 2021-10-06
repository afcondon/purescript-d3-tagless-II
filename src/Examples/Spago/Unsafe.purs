module D3.Examples.Spago.Unsafe where

import Prelude

import D3.Data.Types (Datum_, Index_)
import D3.Examples.Spago.Files (NodeType, SpagoDataRecord, SpagoLinkData, SpagoNodeRow, SpagoTreeObj)
import D3.Node (D3Link(..), D3LinkSwizzled, D3_FocusXY, D3_ID, D3_Radius, D3_TreeNode(..), D3_TreeRow, D3_VxyFxy, D3_XY, EmbeddedData, NodeID)
import D3.Node (D3LinkDatum, D3LinkSwizzled(..), D3_SimulationNode(..), D3_TreeNode(..))
import Data.Nullable (Nullable)
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

-- | why a single unsafeCoerce and not, say, some kind of lens for each field ? PERFORMANCE
unboxD3SimNode :: Datum_ -> SpagoDataRecord
unboxD3SimNode datum = d
  where (D3SimNode d) = unsafeCoerce datum
  
unboxD3SimLink :: Datum_ -> D3LinkDatum SpagoDataRecord SpagoLinkData
unboxD3SimLink datum = l
  where
    (D3LinkObj l) = unsafeCoerce datum

coerceToIndex_ :: forall a. (Ord a) => a -> Index_
coerceToIndex_ = unsafeCoerce

spagoNodeKeyFunction :: Datum_ -> Index_
spagoNodeKeyFunction d = index
  where
    index = unsafeCoerce $ (unboxD3SimNode d).id

-- ======================================================================
-- | Tree object contains SpagoNodeData embedded in it in a "data" field 
-- ======================================================================
unwrapD3TreeNode :: forall t3.
  D3_TreeNode t3 
  -> { depth :: Int
     , height :: Int
     , id :: Int
     , value :: Nullable Number
     | t3
     }
unwrapD3TreeNode (D3TreeNode t) = t

recoverSpagoTreeObj :: Datum_ -> SpagoTreeObj
recoverSpagoTreeObj datum = t'
  where
    (t' :: SpagoTreeObj )  = unsafeCoerce datum


-- unboxTreeDatum :: Datum_ -> SpagoTreeObj
unboxD3TreeNode :: Datum_
  -> { data :: { connected :: Boolean
              , containerID :: Int
              , containerName :: String
              , containsMany :: Boolean
              , gridXY :: Nullable
                            { x :: Number
                            , y :: Number
                            }
              , id :: Int
              , inSim :: Boolean
              , links :: { contains :: Array Int
                          , inPackage :: Array Int
                          , outPackage :: Array Int
                          , sources :: Array Int
                          , targets :: Array Int
                          , treeChildren :: Array Int
                          }
              , loc :: Number
              , name :: String
              , nodetype :: NodeType
              , showChildren :: Boolean
              , treeDepth :: Nullable Int
              , treeXY :: Nullable
                            { x :: Number
                            , y :: Number
                            }
              }
    , depth :: Int
    , height :: Int
    , id :: Int
    , value :: Nullable Number
    , x :: Number
    , y :: Number
    }
unboxD3TreeNode = unwrapD3TreeNode <<< recoverSpagoTreeObj
