-- | DataViz.Layout.Hierarchy.Core
-- |
-- | Pure PureScript implementation of D3 hierarchy core functions.
-- | Matches D3's algorithms exactly but using pure functional style.
module DataViz.Layout.Hierarchy.Core
  ( hierarchy
  , getHeight
  , sum
  , count
  , eachBefore
  , eachAfter
  , descendants
  , leaves
  , sortHierarchy
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl, foldr, maximum)
import Data.Maybe (Maybe(..), fromMaybe)
import DataViz.Layout.Hierarchy.Types (HierarchyNode(..), ValuedNode(..), getChildren, getValue)

-- | Construct a hierarchy from user data
-- | Matches D3's hierarchy() function
-- |
-- | Simplified implementation using recursion instead of D3's iterative approach
-- | This is more idiomatic for PureScript while producing identical results
hierarchy :: forall a. a -> (a -> Maybe (Array a)) -> HierarchyNode a
hierarchy rootData childrenAccessor = computeHeights $ buildNode Nothing 0 rootData
  where
  -- Recursively build node and its descendants
  buildNode :: Maybe (HierarchyNode a) -> Int -> a -> HierarchyNode a
  buildNode parentRef currentDepth nodeData =
    case childrenAccessor nodeData of
      Nothing ->
        -- Leaf node
        HNode
          { data_: nodeData
          , depth: currentDepth
          , height: 0 -- Will be computed later
          , parent: parentRef
          , children: []
          }

      Just childrenData ->
        -- Internal node
        let
          -- Create this node first (without children set yet)
          thisNode = HNode
            { data_: nodeData
            , depth: currentDepth
            , height: 0
            , parent: parentRef
            , children: [] -- Placeholder
            }

          -- Recursively build children, passing this node as parent
          builtChildren = map (buildNode (Just thisNode) (currentDepth + 1)) childrenData

          -- Update this node with actual children
          nodeWithChildren = HNode
            { data_: nodeData
            , depth: currentDepth
            , height: 0
            , parent: parentRef
            , children: builtChildren
            }
        in
          nodeWithChildren

  -- Compute heights bottom-up (matches D3's computeHeight)
  computeHeights :: HierarchyNode a -> HierarchyNode a
  computeHeights (HNode n) =
    if Array.null n.children then HNode (n { height = 0 }) -- Leaf node
    else
      let
        -- Recursively compute heights for children
        childrenWithHeights = map computeHeights n.children

        -- Extract heights
        childHeights = map getHeight childrenWithHeights

        -- Parent height = max(child heights) + 1
        maxChildHeight = fromMaybe 0 $ maximum childHeights
      in
        HNode (n { children = childrenWithHeights, height = maxChildHeight + 1 })

-- Helper to get height from HierarchyNode
getHeight :: forall a. HierarchyNode a -> Int
getHeight (HNode n) = n.height

-- | Compute aggregate values bottom-up
-- | Matches D3's .sum() method
-- |
-- | Formula: node.value = value(node.data) + Σ(child.value)
sum :: forall a. HierarchyNode a -> (a -> Number) -> ValuedNode a
sum (HNode n) valueAccessor =
  if Array.null n.children then
    -- Leaf node: value comes from data
    VNode
      { data_: n.data_
      , depth: n.depth
      , height: n.height
      , parent: Nothing -- Parent reference omitted to avoid circular structure
      , children: []
      , value: valueAccessor n.data_
      }
  else
    -- Internal node: sum of own value + children values
    let
      -- Recursively compute children values
      childrenValued = map (\child -> sum child valueAccessor) n.children

      -- Sum children values
      childrenSum = foldl (\acc child -> acc + getValue child) 0.0 childrenValued

      -- Own value from data
      ownValue = valueAccessor n.data_

      -- Total value
      totalValue = ownValue + childrenSum
    in
      VNode
        { data_: n.data_
        , depth: n.depth
        , height: n.height
        , parent: Nothing
        , children: childrenValued
        , value: totalValue
        }

-- | Count descendants
-- | Matches D3's .count() method
-- |
-- | Formula:
-- | - Leaf: value = 1
-- | - Internal: value = Σ(child.value)
count :: forall a. HierarchyNode a -> ValuedNode a
count (HNode n) =
  if Array.null n.children then
    -- Leaf node: count = 1
    VNode
      { data_: n.data_
      , depth: n.depth
      , height: n.height
      , parent: Nothing
      , children: []
      , value: 1.0
      }
  else
    -- Internal node: sum of children counts
    let
      childrenCounted = map count n.children
      totalCount = foldl (\acc child -> acc + getValue child) 0.0 childrenCounted
    in
      VNode
        { data_: n.data_
        , depth: n.depth
        , height: n.height
        , parent: Nothing
        , children: childrenCounted
        , value: totalCount
        }

-- | Pre-order traversal (visit parent before children)
-- | Matches D3's .eachBefore() method
-- |
-- | Order: root → A → A1 → A2 → B → B1 → B2
eachBefore :: forall a b. (HierarchyNode a -> b) -> HierarchyNode a -> Array b
eachBefore callback root = go [ root ] []
  where
  go :: Array (HierarchyNode a) -> Array b -> Array b
  go stack acc = case Array.uncons stack of
    Nothing -> acc
    Just { head: node@(HNode n), tail: rest } ->
      let
        result = callback node
        -- Add children to stack in reverse order (to process left-to-right)
        newStack = (Array.reverse n.children) <> rest
      in
        go newStack (Array.snoc acc result)

-- | Post-order traversal (visit children before parent)
-- | Matches D3's .eachAfter() method
-- |
-- | Order: A1 → A2 → A → B1 → B2 → B → root
eachAfter :: forall a b. (HierarchyNode a -> b) -> HierarchyNode a -> Array b
eachAfter callback (HNode n) =
  let
    -- Process all children first (recursively)
    childResults = foldr (\child acc -> (eachAfter callback child) <> acc) [] n.children

    -- Then process this node
    thisResult = callback (HNode n)
  in
    childResults <> [ thisResult ]

-- | Get all descendant nodes (including self)
-- | Matches D3's .descendants() method
descendants :: forall a. HierarchyNode a -> Array (HierarchyNode a)
descendants root = eachBefore identity root

-- | Get all leaf nodes (nodes with no children)
-- | Matches D3's .leaves() method
leaves :: forall a. HierarchyNode a -> Array (HierarchyNode a)
leaves root = Array.filter (\node -> Array.null (getChildren node)) (descendants root)

-- | Sort children by comparator
-- | Matches D3's .sort() method
-- |
-- | Note: This returns a new hierarchy (pure function)
sortHierarchy :: forall a. (HierarchyNode a -> HierarchyNode a -> Ordering) -> HierarchyNode a -> HierarchyNode a
sortHierarchy comparator (HNode n) =
  if Array.null n.children then HNode n
  else
    let
      -- Recursively sort children's children first
      sortedGrandchildren = map (sortHierarchy comparator) n.children

      -- Sort direct children
      sortedChildren = Array.sortBy comparator sortedGrandchildren
    in
      HNode (n { children = sortedChildren })
