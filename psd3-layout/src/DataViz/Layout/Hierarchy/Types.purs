-- | DataViz.Layout.Hierarchy.Types
-- |
-- | Pure PureScript implementation of D3 hierarchy types.
-- | Follows D3's conceptual model but uses separate types for each processing phase.
module DataViz.Layout.Hierarchy.Types
  ( HierarchyNode(..)
  , ValuedNode(..)
  , getData
  , getDepth
  , getHeight
  , getParent
  , getChildren
  , getValuedData
  , getValue
  , getValuedDepth
  , getValuedHeight
  , getValuedChildren
  , HierarchyConfig
  , Hierarchy
  , ValuedHierarchy
  ) where

import Prelude

import Data.Maybe (Maybe)

-- | Basic hierarchy node after construction
-- | Analogous to D3's Node structure but without layout-specific fields
-- | Type parameter `a` represents the user's data type
-- |
-- | Note: This is a data type (not newtype or type synonym) to allow:
-- | 1. Recursive definition without cycles
-- | 2. Pattern matching
-- | 3. Direct field access via record syntax
data HierarchyNode a = HNode
  { data_ :: a -- Original user data (embedded like D3)
  , depth :: Int -- Distance from root (root = 0)
  , height :: Int -- Distance to deepest descendant (leaf = 0)
  , parent :: Maybe (HierarchyNode a) -- Reference to parent (Nothing for root)
  , children :: Array (HierarchyNode a) -- Child nodes (empty for leaves)
  }

derive instance eqHierarchyNode :: Eq a => Eq (HierarchyNode a)

instance showHierarchyNode :: Show a => Show (HierarchyNode a) where
  show (HNode n) = "HNode { data_: " <> show n.data_ <> ", depth: " <> show n.depth <> ", height: " <> show n.height <> " }"

-- | Node with computed value (after calling .sum() or .count())
-- | This is a separate type to ensure type safety - can't use value before computing it
data ValuedNode a = VNode
  { data_ :: a
  , depth :: Int
  , height :: Int
  , parent :: Maybe (ValuedNode a)
  , children :: Array (ValuedNode a)
  , value :: Number -- NEW: Computed aggregate value
  }

derive instance eqValuedNode :: Eq a => Eq (ValuedNode a)

instance showValuedNode :: Show a => Show (ValuedNode a) where
  show (VNode n) = "VNode { data_: " <> show n.data_ <> ", value: " <> show n.value <> ", depth: " <> show n.depth <> " }"

-- | Helper functions to access node fields
getData :: forall a. HierarchyNode a -> a
getData (HNode n) = n.data_

getDepth :: forall a. HierarchyNode a -> Int
getDepth (HNode n) = n.depth

getHeight :: forall a. HierarchyNode a -> Int
getHeight (HNode n) = n.height

getParent :: forall a. HierarchyNode a -> Maybe (HierarchyNode a)
getParent (HNode n) = n.parent

getChildren :: forall a. HierarchyNode a -> Array (HierarchyNode a)
getChildren (HNode n) = n.children

-- | Helper functions for ValuedNode
getValuedData :: forall a. ValuedNode a -> a
getValuedData (VNode n) = n.data_

getValue :: forall a. ValuedNode a -> Number
getValue (VNode n) = n.value

getValuedDepth :: forall a. ValuedNode a -> Int
getValuedDepth (VNode n) = n.depth

getValuedHeight :: forall a. ValuedNode a -> Int
getValuedHeight (VNode n) = n.height

getValuedChildren :: forall a. ValuedNode a -> Array (ValuedNode a)
getValuedChildren (VNode n) = n.children

-- | Configuration for hierarchy construction
type HierarchyConfig a =
  { children :: a -> Maybe (Array a) -- Accessor function for children
  }

-- | Result of hierarchy construction
-- | Just a type alias for clarity
type Hierarchy a = HierarchyNode a

-- | Result of sum/count operations
-- | Just a type alias for clarity
type ValuedHierarchy a = ValuedNode a
