{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia   #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeFamilies  #-}

module ReingoldTilford.Types
  ( BinTree(..)
  , XCoord, YCoord, Offset, Distance
  , Contour, Contours(..), ScanResult(..), emptyContours
  , example1, example2Left, example2Right, example3, example3Tagged, example4, completeBinTree, raggedContourTree
  )
where

import Data.Fix ( Nu )
import Data.Functor.Classes ( Show1 )
import Data.Functor.Classes.Generic ( FunctorClassesDefault(..) )
import Data.Functor.Foldable ( Base, Recursive, cata, ana )
import Data.Int ( Int64 )
import Data.Word ( Word64 )
import GHC.Generics ( Generic1 )

data BinTree a = Leaf | Node (BinTree a) a (BinTree a)
  deriving (Show, Eq, Functor, Generic1)
  deriving Show1 via FunctorClassesDefault BinTree

data BinTreeF a f = LeafF | NodeF f a f
  deriving (Show, Functor, Generic1)
  deriving Show1 via FunctorClassesDefault (BinTreeF a)

toBinTree
  :: Recursive (t (BinTreeF a))
  => Base (t (BinTreeF a)) ~ BinTreeF a
  => t (BinTreeF a)
  -> BinTree a
toBinTree =
  cata $ \case
    LeafF -> Leaf
    NodeF l x r -> Node l x r

type XCoord = Int64
type YCoord = Int64
type Offset = Int64
type Distance = Word64

type Contour = [Offset]

data ScanResult = ScanResult
  { rootOffset :: !Offset
  , lloffsum :: !Offset
  , lroffsum :: !Offset
  , rloffsum :: !Offset
  , rroffsum :: !Offset
  }
  deriving Show

data Contours = Contours
  { left :: !Contour
  , right :: !Contour
  }
  deriving Show

emptyContours :: Contours
emptyContours = Contours
  { left = []
  , right = []
  }

{- example1 is the very first example Reingold and Tilford give of a tree being -}
{- displayed poorly by algorithm WS, where the right wing is skewed in Fig. 1 -}

example1 :: BinTree ()
example1 =
  Node (Node (Node (Node (Node (Node Leaf () Leaf) () (Node (Node Leaf () Leaf) () (Node (Node Leaf () Leaf) () (Node (Node Leaf () Leaf) () (Node Leaf () Leaf))))) () (Node Leaf () Leaf)) () (Node Leaf () Leaf)) () (Node Leaf () Leaf)) () (Node (Node Leaf () Leaf) () (Node (Node Leaf () Leaf) () (Node (Node Leaf () Leaf) () (Node (Node (Node (Node (Node Leaf () Leaf) () (Node Leaf () Leaf)) () (Node Leaf () Leaf)) () (Node Leaf () Leaf)) () (Node Leaf () Leaf)))))

{- example2 corresponds to Fig. 4, where a tree and its mirror image are -}
{- positioned differently -}

example2Left :: BinTree ()
example2Left =
  Node (Node Leaf () (Node Leaf () (Node Leaf () (Node Leaf () (Node Leaf () Leaf))))) () (Node Leaf () Leaf)

example2Right :: BinTree ()
example2Right =
  Node (Node Leaf () Leaf) () (Node (Node (Node (Node (Node Leaf () Leaf) () Leaf) () Leaf) () Leaf) () Leaf)

{- example3 corresponds to Fig. 7, although we may not need to show the threads -}

example3 :: BinTree ()
example3 =
  Node (Node (Node (Node Leaf () (Node Leaf () Leaf)) () Leaf) () (Node (Node Leaf () Leaf) () Leaf)) () (Node (Node (Node Leaf () Leaf) () (Node Leaf () Leaf)) () (Node Leaf () Leaf))

example3Tagged :: BinTree Word64
example3Tagged =
  Node (Node (Node (Node Leaf 7 (Node Leaf 11 Leaf)) 3 Leaf) 1 (Node (Node Leaf 8 Leaf) 4 Leaf)) 0 (Node (Node (Node Leaf 9 Leaf) 5 (Node Leaf 10 Leaf)) 2 (Node Leaf 6 Leaf))

example4 :: BinTree ()
example4 = Node (Node Leaf () (Node Leaf () Leaf)) () (Node (Node Leaf () Leaf) () Leaf)

completeBinTreeF :: Nu (BinTreeF ())
completeBinTreeF =
  flip ana 0 $ \x ->
    if x >= 4 then LeafF else NodeF (x+1) () (x+1)

completeBinTree :: BinTree ()
completeBinTree =
  toBinTree completeBinTreeF

raggedContourTree :: BinTree ()
raggedContourTree =
  Node
    (Node
      (Node (Node Leaf () Leaf) () (Node Leaf () (Node Leaf () (Node Leaf () Leaf))))
      ()
      (Node Leaf () Leaf))
    ()
    (Node
      (Node Leaf () Leaf)
      ()
      (Node (Node (Node (Node Leaf () Leaf) () Leaf) () Leaf) () (Node Leaf () Leaf)))