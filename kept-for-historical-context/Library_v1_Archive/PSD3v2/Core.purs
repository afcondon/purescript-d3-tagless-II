module PSD3v2.VizTree.Core where

import Prelude

import PSD3v2.Attribute.Types (Attribute)
import PSD3v2.Selection.Types (ElementType(..))

-- | Core idea: A VizTree is a declarative specification of DOM structure
-- |
-- | Instead of trying to encode the result type in the tree itself,
-- | let's make the tree structure simple and put the complexity in the render function
-- |
-- | The key insight: we can use labeled trees (rose trees with labels)
-- | and extract named selections during rendering

-- | A labeled tree node
-- | - name: Optional name for this node (if we want to get a selection back)
-- | - elemType: What kind of element (SVG, Group, Circle, etc.)
-- | - attrs: Attributes to apply
-- | - children: Child nodes
type TreeNode datum =
  { name :: String  -- Empty string means "don't return a selection for this"
  , elemType :: ElementType
  , attrs :: Array (Attribute datum)
  , children :: Array (Tree datum)
  }

-- | A tree is either a single node or a data join
data Tree datum
  = Node (TreeNode datum)
  | DataJoin
      { name :: String
      , key :: String
      , data :: forall childDatum. Array childDatum
      , template :: forall childDatum. Tree childDatum
      }

-- | Smart constructors for building trees
-- | These give us the Emmet-style syntax

-- | Create a named leaf node (no children)
named :: forall datum. String -> ElementType -> Array (Attribute datum) -> Tree datum
named name elemType attrs = Node { name, elemType, attrs, children: [] }

-- | Create an anonymous leaf node
elem :: forall datum. ElementType -> Array (Attribute datum) -> Tree datum
elem elemType attrs = Node { name: "", elemType, attrs, children: [] }

-- | Add a single child to a node
withChild :: forall datum. Tree datum -> Tree datum -> Tree datum
withChild parent child = case parent of
  Node node -> Node node { children = node.children <> [child] }
  DataJoin dj -> DataJoin dj  -- Can't add children to a data join

-- | Add multiple children to a node
withChildren :: forall datum. Tree datum -> Array (Tree datum) -> Tree datum
withChildren parent children = case parent of
  Node node -> Node node { children = node.children <> children }
  DataJoin dj -> DataJoin dj

-- | Operators for Emmet-style syntax

infixl 6 withChild as >:
infixl 5 withSibling as +:

-- | Helper to collect siblings
withSibling :: forall datum. Tree datum -> Tree datum -> Array (Tree datum)
withSibling left right = [left, right]

-- | Example usage:
-- |
-- | ```purescript
-- | myTree =
-- |   named "svg" SVG [width 800] >:
-- |     (named "zoom" Group [class_ "zoom"] >:
-- |       [ named "links" Group []
-- |       , named "nodes" Group [] >:
-- |           (elem Circle [radius 5] +: elem Text [content "hi"])
-- |       ])
-- | ```
