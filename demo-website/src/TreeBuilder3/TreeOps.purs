-- | TreeBuilder3 Tree Operations
-- |
-- | Pure functions for manipulating the Tree TreeNode structure.
-- | No Halogen dependencies - all functions are pure.
module TreeBuilder3.TreeOps
  ( -- * Tree Queries
    findNodeById
  , findParentId
  , getChildrenIds
  , findMaxId
  , isJoinChild
  , isUpdateJoinChild
    -- * Node Type Predicates
  , isBadgeNodeType
  , isJoinType
  , isUpdateJoinType
    -- * Tree Transformations
  , addChildToNode
  , removeNodeById
  , updateNodeType
  , filterStructuralTree
    -- * Path-based Updates (for form integration)
  , updateNameAtPath
  , updateKeyAtPath
    -- * Badge Handling
  , getBadgeChildren
  , positionBadges
    -- * Layout Helpers
  , flattenTree
  , makeLinks
  , applyLayout
    -- * Types
  , LinkData
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Comonad.Cofree (head, tail)
import Data.Array as Array
import Data.Int (toNumber)
import Data.Int as Int
import Data.List (List(..), (:))
import Data.List as Data.List
import Data.Maybe (Maybe(..))
import Data.Tree (Tree, mkTree)
import DataViz.Layout.Hierarchy.Tree (defaultTreeConfig, tree)

import TreeBuilder3.Types (TreeNode, DslNodeType(..))

-- =============================================================================
-- Types
-- =============================================================================

-- | Link data for rendering tree edges
type LinkData =
  { id :: String
  , sourceX :: Number
  , sourceY :: Number
  , targetX :: Number
  , targetY :: Number
  }

-- =============================================================================
-- Node Type Predicates
-- =============================================================================

-- | Check if a node type should render as a badge (attr, behavior, pending variants)
isBadgeNodeType :: DslNodeType -> Boolean
isBadgeNodeType (NodeAttr _) = true
isBadgeNodeType (NodeBehavior _) = true
isBadgeNodeType PendingAttr = true
isBadgeNodeType (PendingAttrValue _) = true
isBadgeNodeType PendingBehavior = true
isBadgeNodeType _ = false

-- | Check if a node type is a Join variant
isJoinType :: DslNodeType -> Boolean
isJoinType NodeJoin = true
isJoinType NodeNestedJoin = true
isJoinType NodeUpdateJoin = true
isJoinType NodeUpdateNestedJoin = true
isJoinType _ = false

-- | Check if a node type is a Scene Join (GUP) variant
isUpdateJoinType :: DslNodeType -> Boolean
isUpdateJoinType NodeUpdateJoin = true
isUpdateJoinType NodeUpdateNestedJoin = true
isUpdateJoinType _ = false

-- =============================================================================
-- Tree Queries
-- =============================================================================

-- | Find a node by id in the tree
findNodeById :: Int -> Tree TreeNode -> Maybe TreeNode
findNodeById targetId t =
  let
    val = head t
    children = tail t
  in
    if val.id == targetId then Just val
    else Array.foldl (\acc child -> acc <|> findNodeById targetId child) Nothing (Array.fromFoldable children)

-- | Find the parent ID of a node
findParentId :: Int -> Tree TreeNode -> Maybe Int
findParentId targetId t =
  let
    val = head t
    children = tail t
    childIds = map (\c -> (head c).id) children
  in
    if Array.elem targetId (Array.fromFoldable childIds) then Just val.id
    else Array.foldl
      ( \acc c -> case acc of
          Just pid -> Just pid
          Nothing -> findParentId targetId c
      )
      Nothing
      (Array.fromFoldable children)

-- | Get the IDs of all children of a node
getChildrenIds :: Int -> Tree TreeNode -> Array Int
getChildrenIds targetId t =
  let
    val = head t
    children = tail t
  in
    if val.id == targetId then map (\c -> (head c).id) (Array.fromFoldable children)
    else Array.foldl (\acc c -> if Array.null acc then getChildrenIds targetId c else acc)
      []
      (Array.fromFoldable children)

-- | Find the maximum ID in a tree (for setting nextId after import)
findMaxId :: Tree TreeNode -> Int
findMaxId t =
  let
    val = head t
    children = tail t
    childMaxes = map findMaxId (Array.fromFoldable children)
  in
    Array.foldl max val.id childMaxes

-- | Check if a node's parent is a Join type (making it a "template" node)
isJoinChild :: Int -> Tree TreeNode -> Boolean
isJoinChild nodeId t = case findParentId nodeId t of
  Nothing -> false
  Just parentId -> case findNodeById parentId t of
    Nothing -> false
    Just parent -> isJoinType parent.nodeType

-- | Check if a node's parent is a Scene Join type (GUP - has enter/update/exit)
isUpdateJoinChild :: Int -> Tree TreeNode -> Boolean
isUpdateJoinChild nodeId t = case findParentId nodeId t of
  Nothing -> false
  Just parentId -> case findNodeById parentId t of
    Nothing -> false
    Just parent -> isUpdateJoinType parent.nodeType

-- =============================================================================
-- Tree Transformations
-- =============================================================================

-- | Add a child node to a target node
addChildToNode :: Int -> TreeNode -> Tree TreeNode -> Tree TreeNode
addChildToNode targetId newChild t =
  let
    val = head t
    children = tail t
  in
    if val.id == targetId then mkTree val (children <> (mkTree newChild Nil : Nil))
    else mkTree val (map (addChildToNode targetId newChild) children)

-- | Remove a node by id from the tree
removeNodeById :: Int -> Tree TreeNode -> Tree TreeNode
removeNodeById targetId t =
  let
    val = head t
    children = tail t
    filteredChildren = Data.List.filter (\child -> (head child).id /= targetId) children
  in
    mkTree val (map (removeNodeById targetId) filteredChildren)

-- | Update a node's type in the tree
updateNodeType :: Int -> DslNodeType -> Tree TreeNode -> Tree TreeNode
updateNodeType targetId newType t =
  let
    val = head t
    children = tail t
    newVal = if val.id == targetId then val { nodeType = newType } else val
  in
    mkTree newVal (map (updateNodeType targetId newType) children)

-- | Filter tree to only structural nodes (for layout calculation)
-- | Badge nodes are removed; their positions will be computed relative to parent
filterStructuralTree :: Tree TreeNode -> Tree TreeNode
filterStructuralTree t =
  let
    val = head t
    children = tail t
    -- Filter out badge children, recurse on structural children
    structuralChildren = Data.List.filter (\c -> not (isBadgeNodeType (head c).nodeType)) children
  in
    mkTree val (map filterStructuralTree structuralChildren)

-- =============================================================================
-- Path-based Updates (for form integration)
-- =============================================================================

-- | Update name at a FormPath
-- | Path structure: ["name"] for root, ["children", "0", "name"] for first child, etc.
-- | "template" navigates to the first child of a join
updateNameAtPath :: Array String -> String -> Tree TreeNode -> Tree TreeNode
updateNameAtPath path newName t = case Array.uncons path of
  Nothing -> t -- Empty path, no change
  Just { head: segment, tail: rest } ->
    case segment of
      "name" ->
        -- Update this node's name
        let
          val = head t
          newVal = val { name = Just newName }
        in
          mkTree newVal (tail t)
      "children" ->
        -- Navigate to children, next segment is index
        case Array.uncons rest of
          Just { head: idxStr, tail: restAfterIdx } ->
            case Int.fromString idxStr of
              Just idx ->
                let
                  val = head t
                  children = tail t
                  -- Only count structural children (skip attrs/behaviors)
                  structuralChildren = Data.List.filter (\c -> not (isBadgeNodeType (head c).nodeType)) children
                  structArray = Array.fromFoldable structuralChildren
                in
                  case Array.index structArray idx of
                    Just targetChild ->
                      let
                        updatedChild = updateNameAtPath restAfterIdx newName targetChild
                        -- Replace the child in the original list
                        newChildren = map (\c -> if (head c).id == (head targetChild).id then updatedChild else c) children
                      in
                        mkTree val newChildren
                    Nothing -> t -- Index out of bounds
              Nothing -> t -- Invalid index
          Nothing -> t -- Missing index
      "template" ->
        -- Template refers to first structural child (for joins)
        let
          val = head t
          children = tail t
          structuralChildren = Data.List.filter (\c -> not (isBadgeNodeType (head c).nodeType)) children
        in
          case Data.List.head structuralChildren of
            Just firstChild ->
              let
                updatedChild = updateNameAtPath rest newName firstChild
                newChildren = map (\c -> if (head c).id == (head firstChild).id then updatedChild else c) children
              in
                mkTree val newChildren
            Nothing -> t
      _ -> t -- Unknown segment

-- | Update key at a FormPath (similar to updateNameAtPath)
updateKeyAtPath :: Array String -> String -> Tree TreeNode -> Tree TreeNode
updateKeyAtPath path newKey t = case Array.uncons path of
  Nothing -> t
  Just { head: segment, tail: rest } ->
    case segment of
      "key" ->
        let
          val = head t
          newVal = val { key = Just newKey }
        in
          mkTree newVal (tail t)
      "children" ->
        case Array.uncons rest of
          Just { head: idxStr, tail: restAfterIdx } ->
            case Int.fromString idxStr of
              Just idx ->
                let
                  val = head t
                  children = tail t
                  structuralChildren = Data.List.filter (\c -> not (isBadgeNodeType (head c).nodeType)) children
                  structArray = Array.fromFoldable structuralChildren
                in
                  case Array.index structArray idx of
                    Just targetChild ->
                      let
                        updatedChild = updateKeyAtPath restAfterIdx newKey targetChild
                        newChildren = map (\c -> if (head c).id == (head targetChild).id then updatedChild else c) children
                      in
                        mkTree val newChildren
                    Nothing -> t
              Nothing -> t
          Nothing -> t
      "template" ->
        let
          val = head t
          children = tail t
          structuralChildren = Data.List.filter (\c -> not (isBadgeNodeType (head c).nodeType)) children
        in
          case Data.List.head structuralChildren of
            Just firstChild ->
              let
                updatedChild = updateKeyAtPath rest newKey firstChild
                newChildren = map (\c -> if (head c).id == (head firstChild).id then updatedChild else c) children
              in
                mkTree val newChildren
            Nothing -> t
      _ -> t

-- =============================================================================
-- Badge Handling
-- =============================================================================

-- | Get badge children (attrs, behaviors) for a node
getBadgeChildren :: Int -> Tree TreeNode -> Array TreeNode
getBadgeChildren targetId t =
  let
    val = head t
    children = tail t
  in
    if val.id == targetId then Array.filter (\n -> isBadgeNodeType n.nodeType) (Array.fromFoldable (map head children))
    else Array.foldl (\acc c -> if Array.null acc then getBadgeChildren targetId c else acc)
      []
      (Array.fromFoldable children)

-- | Position badge nodes relative to their parent
-- | Returns array of (TreeNode with updated position, badge index)
positionBadges :: TreeNode -> Array TreeNode -> Array { node :: TreeNode, index :: Int }
positionBadges parent badges =
  Array.mapWithIndex
    ( \i badge ->
        let
          -- Badges appear in a column to the right of the parent
          -- Each badge is stacked vertically with small gap
          badgeX = parent.x + 80.0 -- 70px to the right (past the 40px half-width + 30px gap)
          badgeY = parent.y + toNumber i * 25.0 -- Stack vertically, 25px apart
        in
          { node: badge { x = badgeX, y = badgeY }, index: i }
    )
    badges

-- =============================================================================
-- Layout Helpers
-- =============================================================================

-- | Flatten tree to array of nodes
flattenTree :: Tree TreeNode -> Array TreeNode
flattenTree = Array.fromFoldable

-- | Create link data from tree structure
makeLinks :: Tree TreeNode -> Array LinkData
makeLinks t =
  let
    val = head t
    children = tail t
    -- Only create links to structural children (not badges)
    structuralChildren = Data.List.filter (\c -> not (isBadgeNodeType (head c).nodeType)) children
    childLinks = Array.fromFoldable structuralChildren >>= \child ->
      let
        childVal = head child
      in
        [ { id: show val.id <> "->" <> show childVal.id
          , sourceX: val.x
          , sourceY: val.y
          , targetX: childVal.x
          , targetY: childVal.y
          }
        ]
    grandchildLinks = Array.fromFoldable structuralChildren >>= makeLinks
  in
    childLinks <> grandchildLinks

-- | Apply tree layout algorithm
applyLayout :: Tree TreeNode -> Tree TreeNode
applyLayout t =
  let
    config = defaultTreeConfig
      { size = { width: 700.0, height: 600.0 }
      , minSeparation = 2.0
      , layerSeparation = Just 60.0 -- Fixed 60px between layers (2.5x node height)
      }
  in
    tree config t
