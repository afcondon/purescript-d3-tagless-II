module Data.Tree.Functions
  ( treeMapDeep
  , treeMapOverChildren
  , hasChildren
  , subTree
  , filterTransformToList
  , filterTransformToListRecursive
  ) where

import Prelude

import Control.Comonad.Cofree (head, tail)
import Data.Foldable (foldl)
import Data.List (List(..), catMaybes, fromFoldable)
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Tree (Tree, mkTree)

-- | Map over the tree which updates tree's own data incorporating data from updated children
treeMapDeep :: forall a b. (a -> List b -> b) -> Tree a -> Tree b
treeMapDeep f tree = mkTree (f (head tree) newChildData) newChildren
  where
    children = tail tree
    newChildren = children <#> treeMapDeep f
    newChildData = head <$> newChildren

-- | Map over the child arrays in the tree without touching tree data
treeMapOverChildren :: forall a. (List (Tree a) -> List (Tree a)) -> Tree a -> Tree a
treeMapOverChildren f tree = mkTree (head tree) (f newChildren)
  where
    children = tail tree
    newChildren = children <#> treeMapOverChildren f

-- | Check if a tree node has any children
hasChildren :: forall a. Tree a -> Boolean
hasChildren tree = case tail tree of
  Nil -> false
  _   -> true

-- | Find a subtree matching a predicate
subTree :: forall a. Tree a -> (a -> Boolean) -> Maybe (Tree a)
subTree tree pred =
  if pred (head tree)
  then Just tree
  else L.head $ catMaybes $ (tail tree) <#> \subtree -> subTree subtree pred

-- | Filter and transform tree nodes to a list
-- | You provide a predicate on the type in the tree and a transform on that data type
-- | and you get back a list of the ones that matched transformed by the second function
-- | eg filterTransformToList pred identity tree  == List.fromFoldable
filterTransformToList :: forall a b. (a -> Boolean) -> (a -> b) -> Tree a -> List b
filterTransformToList nodePredicate dataTransform tree = foldl fn Nil tree
  where
    fn acc d = if nodePredicate d
               then Cons (dataTransform d) acc
               else acc

-- | Filter and transform tree nodes recursively (operates on subtrees)
-- | This is a potentially quite costly operation but it is very powerful as it takes a
-- | predicate that works on entire (sub)trees
filterTransformToListRecursive :: forall a b. (Tree a -> Boolean) -> (a -> b) -> Tree a -> List b
filterTransformToListRecursive treePredicate dataTransform tree
  = catMaybes $ fromFoldable $ go tree
  where
    go :: Tree a -> Tree (Maybe b)
    go subtree =
      if treePredicate subtree
        then mkTree (Just (dataTransform (head subtree))) (go <$> tail subtree)
        else mkTree Nothing (go <$> tail subtree)
