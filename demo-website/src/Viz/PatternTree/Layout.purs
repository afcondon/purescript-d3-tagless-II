module D3.Viz.PatternTree.Layout
  ( patternTreeToTree
  , patternForestToTree
  , makeLinks
  , makeForestLinks
  , makeForestNodes
  , linkPath
  , nodeColor
  ) where

import Prelude

import Component.PatternTree (PatternTree(..))
import Control.Comonad.Cofree (head, tail)
import Data.Array as Array
import Data.Int as Int
import Data.List (List(..))
import Data.Tree (Tree, mkTree)
import D3.Viz.PatternTree.Types (PatternNode, LinkDatum)

-- | Convert PatternTree to Data.Tree for layout
patternTreeToTree :: PatternTree -> Tree PatternNode
patternTreeToTree = go []
  where
  go :: Array Int -> PatternTree -> Tree PatternNode
  go currentPath = case _ of
    Sound s ->
      mkTree { label: s, nodeType: "sound", x: 0.0, y: 0.0, depth: 0, path: currentPath } Nil

    Rest ->
      mkTree { label: "~", nodeType: "rest", x: 0.0, y: 0.0, depth: 0, path: currentPath } Nil

    Sequence children ->
      mkTree
        { label: "seq", nodeType: "sequence", x: 0.0, y: 0.0, depth: 0, path: currentPath }
        (Array.toUnfoldable $ Array.mapWithIndex (\i c -> go (currentPath <> [i]) c) children)

    Parallel children ->
      mkTree
        { label: "par", nodeType: "parallel", x: 0.0, y: 0.0, depth: 0, path: currentPath }
        (Array.toUnfoldable $ Array.mapWithIndex (\i c -> go (currentPath <> [i]) c) children)

    Choice children ->
      mkTree
        { label: "choice", nodeType: "choice", x: 0.0, y: 0.0, depth: 0, path: currentPath }
        (Array.toUnfoldable $ Array.mapWithIndex (\i c -> go (currentPath <> [i]) c) children)

    -- New extended constructors - wrap child with modifier label
    Fast n child ->
      mkTree
        { label: "*" <> show (Int.round n), nodeType: "fast", x: 0.0, y: 0.0, depth: 0, path: currentPath }
        (Cons (go (currentPath <> [0]) child) Nil)

    Slow n child ->
      mkTree
        { label: "/" <> show (Int.round n), nodeType: "slow", x: 0.0, y: 0.0, depth: 0, path: currentPath }
        (Cons (go (currentPath <> [0]) child) Nil)

    Euclidean n k child ->
      mkTree
        { label: "(" <> show n <> "," <> show k <> ")", nodeType: "euclidean", x: 0.0, y: 0.0, depth: 0, path: currentPath }
        (Cons (go (currentPath <> [0]) child) Nil)

    Degrade prob child ->
      mkTree
        { label: "?" <> show (Int.round (prob * 100.0)) <> "%", nodeType: "degrade", x: 0.0, y: 0.0, depth: 0, path: currentPath }
        (Cons (go (currentPath <> [0]) child) Nil)

    Repeat n child ->
      mkTree
        { label: "!" <> show n, nodeType: "repeat", x: 0.0, y: 0.0, depth: 0, path: currentPath }
        (Cons (go (currentPath <> [0]) child) Nil)

    Elongate n child ->
      mkTree
        { label: "@" <> show (Int.round n), nodeType: "elongate", x: 0.0, y: 0.0, depth: 0, path: currentPath }
        (Cons (go (currentPath <> [0]) child) Nil)

-- | Forest layout: "fake giant tree" approach
-- | Takes multiple pattern trees and lays them out as a forest
patternForestToTree :: Array PatternTree -> Tree PatternNode
patternForestToTree patterns =
  let
    -- Convert each pattern to a tree
    trees = map patternTreeToTree patterns
    -- Create fake root connecting all trees (path is empty - root is not clickable)
    fakeRoot = mkTree
      { label: "forest", nodeType: "forest", x: 0.0, y: 0.0, depth: 0, path: [] }
      (Array.toUnfoldable trees)
  in
    fakeRoot

-- | Create links from parent to children
makeLinks :: Tree PatternNode -> Array LinkDatum
makeLinks tree' = Array.fromFoldable $ makeLinksList tree'
  where
  makeLinksList :: Tree PatternNode -> List LinkDatum
  makeLinksList t =
    let
      val = head t
      children = tail t
      childLinks = children >>= \child ->
        let childVal = head child
        in Cons { source: { x: val.x, y: val.y }, target: { x: childVal.x, y: childVal.y } } Nil
      grandchildLinks = children >>= makeLinksList
    in
      childLinks <> grandchildLinks

-- | Create links for forest, filtering out fake root connections
makeForestLinks :: Tree PatternNode -> Array LinkDatum
makeForestLinks tree' =
  let
    val = head tree'
    children = tail tree'
  in
    if val.nodeType == "forest"
      then Array.fromFoldable $ children >>= (\child -> makeLinksList child)
      else makeLinks tree'
  where
  makeLinksList :: Tree PatternNode -> List LinkDatum
  makeLinksList t =
    let
      val = head t
      children = tail t
      childLinks = children >>= \child ->
        let childVal = head child
        in Cons { source: { x: val.x, y: val.y }, target: { x: childVal.x, y: childVal.y } } Nil
      grandchildLinks = children >>= makeLinksList
    in
      childLinks <> grandchildLinks

-- | Flatten tree to nodes, filtering out fake root
makeForestNodes :: Tree PatternNode -> Array PatternNode
makeForestNodes tree' =
  let
    allNodes = Array.fromFoldable tree'
  in
    Array.filter (\node -> node.nodeType /= "forest") allNodes

-- | Link path generator (curved links)
linkPath :: Number -> Number -> Number -> Number -> String
linkPath x1' y1' x2' y2' =
  "M" <> show x1' <> "," <> show y1'
    <> "C"
    <> show x1'
    <> ","
    <> show ((y1' + y2') / 2.0)
    <> " "
    <> show x2'
    <> ","
    <> show ((y1' + y2') / 2.0)
    <> " "
    <> show x2'
    <> ","
    <> show y2'

-- | Get color for node type
nodeColor :: String -> String
nodeColor = case _ of
  "sound" -> "#4CAF50"      -- Green for sounds
  "rest" -> "#999999"       -- Gray for rests
  "sequence" -> "#2196F3"   -- Blue for sequences
  "parallel" -> "#FF9800"   -- Orange for parallel
  "choice" -> "#9C27B0"     -- Purple for choice
  "fast" -> "#E91E63"       -- Pink for fast
  "slow" -> "#00BCD4"       -- Cyan for slow
  "euclidean" -> "#FFEB3B"  -- Yellow for euclidean
  "degrade" -> "#795548"    -- Brown for degrade/probability
  "repeat" -> "#673AB7"     -- Deep purple for repeat
  "elongate" -> "#009688"   -- Teal for elongate
  _ -> "#000000"
