module Data.Tree where

import Prelude

import Data.Foldable (class Foldable)
import Data.List (List(..), catMaybes, foldMap, foldl, foldr, fromFoldable, head)
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable, traverse)

data Tree a = Node a (List (Tree a))

-- ===========================================================================
-- || Maps and folds for Tree type
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

-- | map over the tree which updates tree's own data incorporating data from updated children
treeMapDeep ::  ∀ a b. (a -> List b -> b) -> Tree a -> Tree b
treeMapDeep f (Node node' children) = Node (f node' newChildData) newChildren
  where newChildren        = children <#> treeMapDeep f
        runData (Node d _) = d
        newChildData       = runData <$> newChildren

-- | map over the child arrays in the tree without touching tree data
treeMapOverChildren ::  ∀ a. (List (Tree a) -> List (Tree a)) -> Tree a -> Tree a
treeMapOverChildren f (Node nd cs) = Node nd (f newCs)
  where
    newCs = cs <#> treeMapOverChildren f

hasChildren ::  ∀ a. Tree a -> Boolean
hasChildren (Node _ Nil) = false
hasChildren (Node _ _)   = true

subTree ::  ∀ a. Tree a -> (a -> Boolean) -> Maybe (Tree a)
subTree n@(Node d trees) pred =
  if pred d
  then Just n
  else head $ catMaybes $ trees <#> \tree -> subTree tree pred

-- || you provide a predicate on the type in the tree and a transform on that data type
-- || and you get back a list of the ones that matched transformed by the second function
-- || eg filterTransformToList pred identity tree  == List.fromFoldable
filterTransformToList ::  ∀ a b. (a -> Boolean) -> (a -> b) -> Tree a -> List b
filterTransformToList nodePredicate dataTransform tree = foldl fn Nil tree
  where 
    fn acc d = if nodePredicate d 
               then Cons (dataTransform d) acc 
               else acc

-- || this is a potentially quite costly operation but it is very powerful as it takes a
-- || predicate that works on entire (sub)trees 
filterTransformToListRecursive ::  ∀ a b. (Tree a -> Boolean) -> (a -> b) -> Tree a -> List b
filterTransformToListRecursive treePredicate dataTransform tree
  = catMaybes $ fromFoldable $ go tree
  where 
    go :: Tree a -> Tree (Maybe b)
    go subtree@(Node a cs) 
      = if treePredicate subtree
        then Node (Just (dataTransform a)) (go <$> cs)
        else Node Nothing (go <$> cs)
    
-- || simple functions to enable us to write test cases more easily
leaf :: forall a. a -> Tree a
leaf a = Node a Nil

node :: forall a. a -> Array (Tree a) -> Tree a
node a childArray = Node a (fromFoldable childArray)