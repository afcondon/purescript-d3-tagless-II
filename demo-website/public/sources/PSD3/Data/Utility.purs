module Utility where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NA
import Data.Int (toNumber)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Web.HTML (window)
import Web.HTML.Window (innerHeight, innerWidth)

getWindowWidthHeight :: Effect (Tuple Number Number)
getWindowWidthHeight = do
  win <- window
  w <- innerWidth win
  h <- innerHeight win
  pure $ Tuple (toNumber w) (toNumber h)


-- | move to utility file
compareFst :: forall a b x. Ord a => Tuple a b -> Tuple a x -> Ordering
compareFst a b = compare (fst a) (fst b)
compareSnd :: forall a b x. Ord b => Tuple a b -> Tuple x b -> Ordering
compareSnd a b = compare (snd a) (snd b)
equalFst :: forall a b x. Eq a => Tuple a b -> Tuple a x -> Boolean
equalFst   a b = eq (fst a) (fst b)
equalSnd :: forall a b x. Eq b => Tuple a b -> Tuple x b -> Boolean
equalSnd   a b = eq (snd a) (snd b)

-- | chunk is a utility function that's easier to show than tell:
-- | example input  [ [(m1,p1), (m4,p1)], [(m2,p2), (m3,p2)], [(m5,p3)] ] 
-- | example ouput  [ (p1, [m1,m4]), (p2, [m2,m3]), (p3, [m5]) ]
chunk :: forall a b. NonEmptyArray (Tuple a b) -> Tuple b (Array a)
chunk tuples = do
  let
    package  = snd $ NA.head tuples
    contains = NA.toArray $ fst <$> tuples
  Tuple package contains
