-- | TreeBuilder3 Type Propagation
-- |
-- | Computes datum types for each node in the tree based on structure.
-- | Types flow from parents to children, with joins introducing new types.
module TreeBuilder3.TypePropagation
  ( -- * Type Propagation
    propagateTypes
  , propagateTypesWithContext
    -- * Built-in Types
  , builtinTypes
  , pointType
  , nodeType
  , linkType
  , countryType
    -- * Array Types (for nested joins)
  , cellType
  , rowType
  , boardType
  ) where

import Prelude

import Control.Comonad.Cofree (head, tail)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Data.Tree (Tree, mkTree)

import TreeBuilder3.Types (TreeNode, DslNodeType(..), DatumType(..), PrimType(..))

-- =============================================================================
-- Built-in Example Types
-- =============================================================================

-- | Point type: { x :: Number, y :: Number }
pointType :: DatumType
pointType = TypeRecord "Point"
  [ { name: "x", typ: TNumber }
  , { name: "y", typ: TNumber }
  ]

-- | Node type for force graphs: { id :: String, x :: Number, y :: Number, group :: Int }
nodeType :: DatumType
nodeType = TypeRecord "Node"
  [ { name: "id", typ: TString }
  , { name: "x", typ: TNumber }
  , { name: "y", typ: TNumber }
  , { name: "group", typ: TInt }
  ]

-- | Link type for force graphs: { source :: String, target :: String, value :: Number }
linkType :: DatumType
linkType = TypeRecord "Link"
  [ { name: "source", typ: TString }
  , { name: "target", typ: TString }
  , { name: "value", typ: TNumber }
  ]

-- | Country type for wealth/health: { name :: String, population :: Number, gdp :: Number, lifeExpectancy :: Number }
countryType :: DatumType
countryType = TypeRecord "Country"
  [ { name: "name", typ: TString }
  , { name: "population", typ: TNumber }
  , { name: "gdp", typ: TNumber }
  , { name: "lifeExpectancy", typ: TNumber }
  ]

-- =============================================================================
-- Array Types (for nested joins - chess/sudoku/go boards)
-- =============================================================================

-- | Cell type for board games: { row :: Int, col :: Int, value :: String }
cellType :: DatumType
cellType = TypeRecord "Cell"
  [ { name: "row", typ: TInt }
  , { name: "col", typ: TInt }
  , { name: "value", typ: TString }
  ]

-- | Row type: Array of Cells (for nested join outer type)
rowType :: DatumType
rowType = TypeArray cellType

-- | Board type: Array of Rows (for nested join on 2D boards)
boardType :: DatumType
boardType = TypeArray rowType

-- | All built-in types as a map from name to type
builtinTypes :: Map String DatumType
builtinTypes = Map.fromFoldable
  [ Tuple "Point" pointType
  , Tuple "Node" nodeType
  , Tuple "Link" linkType
  , Tuple "Country" countryType
  ]

-- =============================================================================
-- Type Propagation
-- =============================================================================

-- | Propagate types through a tree starting with Unit at root
-- | This is the simple version that doesn't know about join types yet
propagateTypes :: Tree TreeNode -> Tree TreeNode
propagateTypes = propagateTypesWithContext TypeUnit

-- | Propagate types starting with a given parent type
-- | The parent type flows down to children, except:
-- | - Join nodes: children get the join's declared datumType (or TypeUnknown if not set)
-- | - NestedJoin: children get the decomposed type
propagateTypesWithContext :: DatumType -> Tree TreeNode -> Tree TreeNode
propagateTypesWithContext parentType tree =
  let
    node = head tree
    children = tail tree

    -- Determine this node's datum type based on its role
    thisType = computeNodeType node parentType

    -- Update the node with its computed type
    updatedNode = node { datumType = thisType }

    -- Determine what type to pass to children
    childType = computeChildType node thisType

    -- Recursively propagate to children
    updatedChildren = map (propagateTypesWithContext childType) children
  in
    mkTree updatedNode updatedChildren

-- | Compute what datum type a node should have
computeNodeType :: TreeNode -> DatumType -> DatumType
computeNodeType node parentType = case node.nodeType of
  -- Joins: use their existing type if not Unit, otherwise mark as unknown
  -- (In Phase 2, users will be able to assign types to joins)
  NodeJoin -> case node.datumType of
    TypeUnit -> TypeUnknown
    t -> t
  NodeNestedJoin -> case node.datumType of
    TypeUnit -> TypeUnknown
    t -> t
  NodeUpdateJoin -> case node.datumType of
    TypeUnit -> TypeUnknown
    t -> t
  NodeUpdateNestedJoin -> case node.datumType of
    TypeUnit -> TypeUnknown
    t -> t

  -- GUP phases inherit from parent (the join's type)
  NodeEnter -> parentType
  NodeUpdate -> parentType
  NodeExit -> parentType

  -- Elements inherit from parent
  NodeElem _ -> parentType

  -- Attrs and behaviors don't really have a datum type (they use parent's)
  NodeAttr _ -> parentType
  NodeBehavior _ -> parentType

  -- Pending nodes inherit parent type
  PendingElement -> parentType
  PendingAttr -> parentType
  PendingAttrValue _ -> parentType
  PendingBehavior -> parentType

-- | Compute what datum type to pass to children
computeChildType :: TreeNode -> DatumType -> DatumType
computeChildType node thisType = case node.nodeType of
  -- Regular Join children get the join's type
  NodeJoin -> thisType
  NodeUpdateJoin -> thisType

  -- NestedJoin children get the UNWRAPPED type (decompose array)
  -- If the join has TypeArray inner, children get inner
  NodeNestedJoin -> unwrapArray thisType
  NodeUpdateNestedJoin -> unwrapArray thisType

  -- GUP phases pass their type to attr children
  NodeEnter -> thisType
  NodeUpdate -> thisType
  NodeExit -> thisType

  -- Elements pass their type to children
  NodeElem _ -> thisType

  -- Others pass through
  _ -> thisType

-- | Unwrap an array type, returning its inner type
-- | TypeArray inner -> inner
-- | other -> other (unchanged)
unwrapArray :: DatumType -> DatumType
unwrapArray (TypeArray inner) = inner
unwrapArray other = other
