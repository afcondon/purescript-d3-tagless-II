module D3.Examples.Tree.Unsafe where


import D3.Data.Types (Datum_)
import D3.Examples.Tree.Model (FlareLinkObj, FlareNodeRow)
import D3.Node (D3_ID, D3_Link(..), D3_TreeNode(..), D3_TreeRow, D3_XY, EmbeddedData)
import Data.Nullable (Nullable)
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)


unboxD3SimLink :: Datum_ -> FlareLinkObj
unboxD3SimLink datum = l
  where (D3_Link l) = unsafeCoerce datum

unboxD3TreeNode :: Datum_ -> { children :: Array
                     (D3_TreeNode
                        ( data :: { name :: String
                                  }
                        , depth :: Int
                        , height :: Int
                        , id :: Int
                        , value :: Nullable Number
                        , x :: Number
                        , y :: Number
                        )
                     )
     , data :: { name :: String
               }
     , depth :: Int
     , height :: Int
     , id :: Int
     , parent :: Nullable
                   (D3_TreeNode
                      ( data :: { name :: String
                                }
                      , depth :: Int
                      , height :: Int
                      , id :: Int
                      , value :: Nullable Number
                      , x :: Number
                      , y :: Number
                      )
                   )
     , value :: Nullable Number
     , x :: Number
     , y :: Number
     }
unboxD3TreeNode datum = do
  let (t' :: D3_TreeNode (D3_ID + D3_TreeRow + D3_XY + (EmbeddedData { | FlareNodeRow () }) + () ) )  = unsafeCoerce datum
      (D3TreeNode t) = t'
  t