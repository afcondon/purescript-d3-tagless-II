module Data.Tree
  ( Tree(..)
  , leaf
  , node
  ) where

import Prelude

import Data.Foldable (class Foldable)
import Data.List (List(..), foldMap, foldl, foldr, fromFoldable)
import Data.Traversable (class Traversable, traverse)

data Tree a = Node a (List (Tree a))

-- ===========================================================================
-- || Instances for Tree type
-- ===========================================================================
instance showTree :: (Show a) => Show (Tree a) where
  show (Node a cs) = show a <> " " <> show cs

instance eqTree :: (Eq a) => Eq (Tree a) where
  eq (Node a cs) (Node a' cs') = a == a' && cs == cs'

instance functorTree :: Functor Tree where
  map f (Node a cs) = Node (f a) (map f <$> cs)

instance foldableTree :: Foldable Tree where
  foldr f b (Node a cs) = f a (foldr (flip $ foldr f) b cs)
  foldl f b (Node a cs) = f (foldl (foldl f) b cs) a
  foldMap f (Node a cs) = f a <> foldMap (foldMap f) cs

instance traversableTree :: Traversable Tree where
  traverse f (Node a cs) = Node <$> f a <*> traverse (traverse f) cs
  sequence = traverse identity

-- || Simple functions to enable us to write test cases more easily
leaf :: forall a. a -> Tree a
leaf a = Node a Nil

node :: forall a. a -> Array (Tree a) -> Tree a
node a childArray = Node a (fromFoldable childArray)
