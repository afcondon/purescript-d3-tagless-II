module Data.Tree.Functions
  ( treeMapDeep
  , treeMapOverChildren
  , hasChildren
  , subTree
  , filterTransformToList
  , filterTransformToListRecursive
  ) where

import Prelude

import Data.Foldable (foldl)
import Data.List (List(..), catMaybes, fromFoldable, head)
import Data.Maybe (Maybe(..))
import Data.Tree (Tree(..))

-- | Map over the tree which updates tree's own data incorporating data from updated children
treeMapDeep :: ∀ a b. (a -> List b -> b) -> Tree a -> Tree b
treeMapDeep f (Node node' children) = Node (f node' newChildData) newChildren
  where
    newChildren        = children <#> treeMapDeep f
    runData (Node d _) = d
    newChildData       = runData <$> newChildren

-- | Map over the child arrays in the tree without touching tree data
treeMapOverChildren :: ∀ a. (List (Tree a) -> List (Tree a)) -> Tree a -> Tree a
treeMapOverChildren f (Node nd cs) = Node nd (f newCs)
  where
    newCs = cs <#> treeMapOverChildren f

-- | Check if a tree node has any children
hasChildren :: ∀ a. Tree a -> Boolean
hasChildren (Node _ Nil) = false
hasChildren (Node _ _)   = true

-- | Find a subtree matching a predicate
subTree :: ∀ a. Tree a -> (a -> Boolean) -> Maybe (Tree a)
subTree n@(Node d trees) pred =
  if pred d
  then Just n
  else head $ catMaybes $ trees <#> \tree -> subTree tree pred

-- | Filter and transform tree nodes to a list
-- | You provide a predicate on the type in the tree and a transform on that data type
-- | and you get back a list of the ones that matched transformed by the second function
-- | eg filterTransformToList pred identity tree  == List.fromFoldable
filterTransformToList :: ∀ a b. (a -> Boolean) -> (a -> b) -> Tree a -> List b
filterTransformToList nodePredicate dataTransform tree = foldl fn Nil tree
  where
    fn acc d = if nodePredicate d
               then Cons (dataTransform d) acc
               else acc

-- | Filter and transform tree nodes recursively (operates on subtrees)
-- | This is a potentially quite costly operation but it is very powerful as it takes a
-- | predicate that works on entire (sub)trees
filterTransformToListRecursive :: ∀ a b. (Tree a -> Boolean) -> (a -> b) -> Tree a -> List b
filterTransformToListRecursive treePredicate dataTransform tree
  = catMaybes $ fromFoldable $ go tree
  where
    go :: Tree a -> Tree (Maybe b)
    go subtree@(Node a cs)
      = if treePredicate subtree
        then Node (Just (dataTransform a)) (go <$> cs)
        else Node Nothing (go <$> cs)
