module PSD3v2.VizTree.Tree where

import Prelude

import PSD3v2.Attribute.Types (Attribute)
import PSD3v2.Selection.Types (ElementType)
import Data.Maybe (Maybe(..))

-- | A simple tree structure for declaratively building DOM
-- |
-- | Philosophy: Keep the runtime representation simple.
-- | Type safety will be added later through builder types.

-- | A node in the visualization tree
-- | - name: Optional name to retrieve this selection later (Nothing = anonymous)
-- | - elemType: What kind of element (SVG, Group, Circle, etc.)
-- | - attrs: Attributes to set on this element
-- | - children: Child nodes
type TreeNode datum =
  { name :: Maybe String
  , elemType :: ElementType
  , attrs :: Array (Attribute datum)
  , children :: Array (Tree datum)
  }

-- | A tree is either a regular node or a data join point
data Tree datum
  = Node (TreeNode datum)
  -- | DataJoin creates N copies of a template tree, one per data item
  -- | This is how we handle enter/update/exit with the declarative API
  | Join
      { name :: Maybe String       -- Name for the parent container
      , key :: String               -- Join key (e.g., "circle", "g")
      , joinData :: Array datum     -- Data to join (note: this changes datum type!)
      , template :: Tree datum      -- Template tree to repeat for each datum
      }

-- | Smart constructors

-- | Create a named element
-- |
-- | Usage: named "svg" SVG [width 800, height 600]
named :: forall datum. String -> ElementType -> Array (Attribute datum) -> Tree datum
named name elemType attrs =
  Node { name: Just name, elemType, attrs, children: [] }

-- | Create an anonymous element (won't be in the returned selections)
-- |
-- | Usage: elem Group [class_ "container"]
elem :: forall datum. ElementType -> Array (Attribute datum) -> Tree datum
elem elemType attrs =
  Node { name: Nothing, elemType, attrs, children: [] }

-- | Add a single child to a tree node
-- |
-- | Usage: parent `withChild` child
withChild :: forall datum. Tree datum -> Tree datum -> Tree datum
withChild parent child = case parent of
  Node node -> Node node { children = node.children <> [child] }
  Join j -> Join j  -- Joins can't have additional children (template already defined)

-- | Add multiple children to a tree node
-- |
-- | Usage: parent `withChildren` [child1, child2, child3]
withChildren :: forall datum. Tree datum -> Array (Tree datum) -> Tree datum
withChildren parent newChildren = case parent of
  Node node -> Node node { children = node.children <> newChildren }
  Join j -> Join j

-- | Create a data join
-- |
-- | Usage: join "nodes" "g" nodeData (named "nodeGroup" Group [class_ "node"])
-- |
-- | This will create one copy of the template for each item in nodeData
join :: forall datum. String -> String -> Array datum -> Tree datum -> Tree datum
join name key data' template =
  Join { name: Just name, key, joinData: data', template }

-- | Anonymous data join (parent container won't be in selections)
join' :: forall datum. String -> Array datum -> Tree datum -> Tree datum
join' key data' template =
  Join { name: Nothing, key, joinData: data', template }

-- | Operators for Emmet-style syntax

infixl 6 withChild as >:
infixl 5 beside as +:

-- | Combine two trees as siblings
-- | Returns an array, meant to be used with `withChildren`
-- |
-- | Usage: group `withChildren` (circle +: text)
beside :: forall datum. Tree datum -> Tree datum -> Array (Tree datum)
beside left right = [left, right]

-- | Helper to combine multiple siblings
-- |
-- | Usage: siblings [child1, child2, child3]
siblings :: forall datum. Array (Tree datum) -> Array (Tree datum)
siblings = identity

-- | Example usage:
-- |
-- | Simple tree:
-- | ```purescript
-- | tree = named "svg" SVG [width 800] >: named "circle" Circle [radius 5]
-- | ```
-- |
-- | Tree with siblings:
-- | ```purescript
-- | tree =
-- |   named "svg" SVG [width 800] `withChildren`
-- |     [ named "circle" Circle [radius 5]
-- |     , named "text" Text [content "Hello"]
-- |     ]
-- | ```
-- |
-- | Using operators:
-- | ```purescript
-- | tree =
-- |   named "svg" SVG [width 800] `withChildren`
-- |     (named "circle" Circle [radius 5] +: named "text" Text [content "Hello"])
-- | ```
