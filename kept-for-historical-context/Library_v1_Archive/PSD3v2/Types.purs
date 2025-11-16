module PSD3v2.VizTree.Types where

import Prelude

import PSD3v2.Attribute.Types (Attribute)
import PSD3v2.Selection.Types (ElementType)
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)
import Prim.Row as Row

-- | A declarative tree structure for visualizations
-- | This represents the DOM structure before rendering
-- |
-- | Type parameters:
-- | - fields: Row type of named selections that will be returned
-- | - datum: The data type bound to this part of the tree
data VizTree (fields :: Row Type) datum

-- | A leaf node - single element with no children
-- | Returns a single selection field
data Leaf (name :: Symbol) datum = Leaf ElementType (Array (Attribute datum))

-- | A branch node - element with children
-- | Returns parent selection plus all child selections
data Branch (name :: Symbol) parentDatum childFields =
  Branch ElementType (Array (Attribute parentDatum)) (VizTree childFields parentDatum)

-- | A sibling sequence - multiple trees side by side
-- | Returns merged selections from all siblings
data Siblings leftFields rightFields datum =
  Siblings (VizTree leftFields datum) (VizTree rightFields datum)

-- | A data join point - creates N copies of a subtree
-- | Returns selections for all elements created from the join
data DataJoin (name :: Symbol) parentDatum childDatum childFields =
  DataJoin
    { key :: String                    -- Join key for enter/update/exit
    , data :: Array childDatum         -- Data to join
    , template :: VizTree childFields childDatum  -- Tree template to repeat
    }

-- | Smart constructor for leaf nodes
-- |
-- | Usage: leaf (Proxy :: _ "myCircle") Circle [radius 5, fill "blue"]
leaf :: forall name datum.
  IsSymbol name =>
  Proxy name ->
  ElementType ->
  Array (Attribute datum) ->
  Leaf name datum
leaf _ elemType attrs = Leaf elemType attrs

-- | Smart constructor for branch nodes
-- |
-- | Usage: branch (Proxy :: _ "myGroup") Group [class_ "nodes"] children
branch :: forall name parentDatum childFields.
  IsSymbol name =>
  Proxy name ->
  ElementType ->
  Array (Attribute parentDatum) ->
  VizTree childFields parentDatum ->
  Branch name parentDatum childFields
branch _ elemType attrs children = Branch elemType attrs children

-- | Smart constructor for data joins
-- |
-- | Usage: dataJoin (Proxy :: _ "nodes") "circle" nodeData nodeTemplate
dataJoin :: forall name parentDatum childDatum childFields.
  IsSymbol name =>
  Proxy name ->
  String ->
  Array childDatum ->
  VizTree childFields childDatum ->
  DataJoin name parentDatum childDatum childFields
dataJoin _ key childData template = DataJoin { key, data: childData, template }

-- | Child operator: parent >: child
-- | Creates a branch where the right side is a child of the left side
-- |
-- | Usage: Group >: (Circle +: Text)
-- |
-- | Note: For now, these are just type class definitions without implementations.
-- | Users should use the smart constructors (branch, leaf, etc.) directly.
-- | We'll add proper operators once we figure out the type-level machinery.
-- infixl 6 childOf as >:

-- class ChildOf parent child result | parent child -> result where
--   childOf :: parent -> child -> result

-- | Sibling operator: left +: right
-- | Creates a sequence of siblings at the same level
-- |
-- | Usage: Circle +: Text +: Rect
-- infixl 5 siblingOf as +:

-- class SiblingOf left right result | left right -> result where
--   siblingOf :: left -> right -> result

-- | Instance: Leaf +: Leaf = Siblings
-- instance SiblingOf (Leaf name1 datum) (Leaf name2 datum) (Siblings (name1 :: sel1) (name2 :: sel2) datum) where
--   siblingOf = Siblings

-- | Instance: Branch +: Branch = Siblings
-- instance SiblingOf (Branch name1 datum fields1) (Branch name2 datum fields2) (Siblings ...) where
--   siblingOf = Siblings

-- | Instance: Leaf >: child = Branch
-- instance ChildOf (Leaf parentName parentDatum) (VizTree childFields childDatum) (Branch parentName parentDatum childFields) where
--   childOf (Leaf elemType attrs) children = Branch elemType attrs children

-- Note: Type class instances are tricky here because we need to track field names
-- May need to use a different approach - perhaps type-level lists or HLists
-- For now, let's keep the data constructors and use them directly
--
-- UPDATE: We're using the simpler Tree.purs approach with named/elem/withChild/withChildren
-- This Types.purs file remains as a sketch of the type-safe version for future exploration
