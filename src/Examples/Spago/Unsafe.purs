module D3.Examples.Spago.Unsafe where

import D3.Data.Types (Datum_)
import D3.Examples.Spago.Files (NodeType, Pinned, SpagoDataRecord, SpagoGraphLinkObj, SpagoTreeObj)
import D3.Node (D3_Link(..), D3_SimulationNode(..), D3_TreeNode(..))
import Data.Nullable (Nullable)
import Unsafe.Coerce (unsafeCoerce)

unboxD3SimNode :: Datum_ -> SpagoDataRecord
unboxD3SimNode datum = d
  where (D3SimNode d) = unsafeCoerce datum
  
unboxD3SimLink :: Datum_ -> SpagoGraphLinkObj
unboxD3SimLink datum = l
  where (D3_Link l) = unsafeCoerce datum

-- morally, this is just the following short signature, but because of 
-- the need for the newtype (because of the recursion in the type) we have 
-- to accept the compiler generated monstrosity above, which is also 
-- brittle unfortunately
-- ======================================================================
-- unboxD3TreeNode :: Datum_ -> SpagoDataRecord -- + Children / Parent
-- ======================================================================
unboxD3TreeNode :: forall t2.
  t2
  -> { children :: Array
                     (D3_TreeNode
                        ( data :: { connected :: Boolean
                                  , containerID :: Int
                                  , containerName :: String
                                  , containsMany :: Boolean
                                  , id :: Int
                                  , inSim :: Boolean
                                  , links :: { contains :: Array Int
                                             , inPackage :: Array Int
                                             , outPackage :: Array Int
                                             , sources :: Array Int
                                             , targets :: Array Int
                                             , tree :: Array Int
                                             }
                                  , loc :: Number
                                  , name :: String
                                  , nodetype :: NodeType
                                  , pinned :: Pinned
                                  , treeX :: Nullable Number
                                  , treeY :: Nullable Number
                                  }
                        , depth :: Int
                        , height :: Int
                        , id :: Int
                        , isLeaf :: Boolean
                        , r :: Number
                        , value :: Nullable Number
                        , x :: Number
                        , y :: Number
                        )
                     )
     , data :: { connected :: Boolean
               , containerID :: Int
               , containerName :: String
               , containsMany :: Boolean
               , id :: Int
               , inSim :: Boolean
               , links :: { contains :: Array Int
                          , inPackage :: Array Int
                          , outPackage :: Array Int
                          , sources :: Array Int
                          , targets :: Array Int
                          , tree :: Array Int
                          }
               , loc :: Number
               , name :: String
               , nodetype :: NodeType
               , pinned :: Pinned
               , treeX :: Nullable Number
               , treeY :: Nullable Number
               }
     , depth :: Int
     , height :: Int
     , id :: Int
     , isLeaf :: Boolean
     , parent :: Nullable
                   (D3_TreeNode
                      ( data :: { connected :: Boolean
                                , containerID :: Int
                                , containerName :: String
                                , containsMany :: Boolean
                                , id :: Int
                                , inSim :: Boolean
                                , links :: { contains :: Array Int
                                           , inPackage :: Array Int
                                           , outPackage :: Array Int
                                           , sources :: Array Int
                                           , targets :: Array Int
                                           , tree :: Array Int
                                           }
                                , loc :: Number
                                , name :: String
                                , nodetype :: NodeType
                                , pinned :: Pinned
                                , treeX :: Nullable Number
                                , treeY :: Nullable Number
                                }
                      , depth :: Int
                      , height :: Int
                      , id :: Int
                      , isLeaf :: Boolean
                      , r :: Number
                      , value :: Nullable Number
                      , x :: Number
                      , y :: Number
                      )
                   )
     , r :: Number
     , value :: Nullable Number
     , x :: Number
     , y :: Number
     }
unboxD3TreeNode datum =
  let 
    (t' :: SpagoTreeObj )  = unsafeCoerce datum
    (D3TreeNode t) = t'
  in
    t
