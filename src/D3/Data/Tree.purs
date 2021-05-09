module D3.Data.Tree where

import Prelude

import D3.Data.Foreign (Datum_)
import Data.Foldable (class Foldable)
import Data.List (List(..), catMaybes, foldMap, foldl, foldr, fromFoldable, head)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Traversable (class Traversable, traverse)
import Unsafe.Coerce (unsafeCoerce)

-- these definitions have to be here to avoid cycle (and probably all type defs should in fact be here)
foreign import data TreeJson_           :: Type
foreign import data D3HierarchicalNode_ :: Type

-- TODO pull out the stuff that isn't generic Tree to D3.Model.Tree or something like that 
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

data TreeType   = TidyTree | Dendrogram
derive instance eqTreeType :: Eq TreeType

data TreeLayout = Radial | Horizontal | Vertical
derive instance eqTreeLayout :: Eq TreeLayout

-- TODO put in proxy fields here to carry the type allowing safe coerce of root etc
-- TODO need to define a model here that works for all hierarchic layouts, this has its origins in Radial tree only

-- d is the type of the datum and v is the type of computed value, ie for summing etc
-- type Model :: forall d v. d -> v -> Type
type TreeModel d v = {
      json       :: TreeJson_
    , root       :: D3HierarchicalNode d v
    , root_      :: D3HierarchicalNode_
    , treeType   :: TreeType
    , treeLayout :: TreeLayout
    , svgConfig  :: { width :: Number, height :: Number }
}

-- the PureScript rep of opaque type D3HierarchicalNode_
-- we can safely cast any D3HierarchicalNode_ to this if we know the types d and v
-- there might be some way, passing proxies around, to enforce that constraint?
newtype D3HierarchicalNode d v = D3HierarchicalNode { -- (newtype to avoid cycles in types)
    "data"   :: d -- the data that is passed in to the tree
  , depth    :: Int
  , height   :: Int
  , parent   :: Nullable (D3HierarchicalNode d v)
  , children :: Array (D3HierarchicalNode d v)
  , value    :: v -- set by some function passed to node.value() or by node.count()
  , x        :: Number
  , y        :: Number
}

-- | Coercion function to recover the structure that was given to D3, it's an unsafeCoerce but the types
-- | give some protection
datumIsTreeNode :: forall d v. Datum_ -> D3HierarchicalNode d v
datumIsTreeNode = unsafeCoerce

-- | Coercion function to recover the "extra" data that lives within the generic structure that was given to D3, 
-- | it's an unsafeCoerce but the types give some protection
labelName :: Datum_ -> String
labelName d = node."data".name
  where (D3HierarchicalNode node) = datumIsTreeNode d


-- | additional, might-be-useful-someday, stuff for Trees and Treeish Graphs

-- | map over the tree which updates tree's own data incorporating data from updated children
treeMapDeep ::  ∀ a b. (a -> List b -> b) -> Tree a -> Tree b
treeMapDeep f (Node node children) = Node (f node newChildData) newChildren
  where newChildren     = (treeMapDeep f) <$> children
        runData (Node d _) = d
        newChildData    = runData <$> newChildren

-- | map over the child arrays in the tree without touching tree data
treeMapOverChildren ::  ∀ a. (List (Tree a) -> List (Tree a)) -> Tree a -> Tree a
treeMapOverChildren f (Node nd cs) = Node nd (f newCs)
  where
    newCs = (treeMapOverChildren f) <$> cs

hasChildren ::  ∀ a. Tree a -> Boolean
hasChildren (Node _ Nil) = false
hasChildren (Node _ _)   = true

subTree ::  ∀ a. Tree a -> (a -> Boolean) -> Maybe (Tree a)
subTree n@(Node d trees) pred =
  if pred d
  then Just n
  else head $ catMaybes $ (\tree -> subTree tree pred) <$> trees

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