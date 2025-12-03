-- | DataViz.Layout.Hierarchy.EdgeBundle
-- |
-- | Hierarchical edge bundling layout for visualizing dependencies in hierarchical data.
-- |
-- | Based on Danny Holten's algorithm:
-- | "Hierarchical Edge Bundles: Visualization of Adjacency Relations in Hierarchical Data"
-- |
-- | This module provides:
-- | - Hierarchy construction from flat dot-notation names
-- | - Bidirectional link creation (bilink)
-- | - Radial cluster layout for node positioning
-- | - Bundle curve rendering with adjustable tension (beta)
-- |
-- | ## Example Usage:
-- |
-- | ```purescript
-- | import DataViz.Layout.Hierarchy.EdgeBundle as EdgeBundle
-- |
-- | -- Given flat data with imports
-- | nodes :: Array { name :: String, size :: Number, imports :: Array String }
-- |
-- | -- Build the visualization
-- | result = EdgeBundle.edgeBundle
-- |   { getName: _.name
-- |   , getImports: _.imports
-- |   , beta: 0.85
-- |   , radius: 400.0
-- |   }
-- |   nodes
-- |
-- | -- result contains:
-- | --   nodes: positioned nodes for rendering
-- | --   links: paths between connected nodes
-- | ```
module DataViz.Layout.Hierarchy.EdgeBundle
  ( -- * Main API
    edgeBundle
  , EdgeBundleConfig
  , EdgeBundleResult
  , PositionedNode
  , BundledLink
  -- * Re-exports for advanced usage
  , module Hierarchy
  , module Bilink
  , module RadialCluster
  , module BundleCurve
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (cos, sin, pi)
import DataViz.Layout.Hierarchy.EdgeBundle.Hierarchy as Hierarchy
import DataViz.Layout.Hierarchy.EdgeBundle.Hierarchy (TreeNode(..), buildHierarchy, pathBetween, leaves, findNode, getFullName)
import DataViz.Layout.Hierarchy.EdgeBundle.Bilink as Bilink
import DataViz.Layout.Hierarchy.EdgeBundle.Bilink (BilinkedNode(..), Link(..), bilink, getOutgoing, getBilinkedFullName, allBilinkedNodes)
import DataViz.Layout.Hierarchy.EdgeBundle.RadialCluster as RadialCluster
import DataViz.Layout.Hierarchy.EdgeBundle.RadialCluster (RadialNode(..), radialCluster, toCartesian, RadialLayoutConfig)
import DataViz.Layout.Hierarchy.EdgeBundle.BundleCurve as BundleCurve
import DataViz.Layout.Hierarchy.EdgeBundle.BundleCurve (bundlePathRadial, BundlePoint)

-- | Configuration for edge bundle layout
type EdgeBundleConfig a =
  { getName :: a -> String -- Extract full name from data
  , getImports :: a -> Array String -- Extract imports from data
  , beta :: Number -- Bundle tension (0-1, default 0.85)
  , innerRadius :: Number -- Radius for root (default 100)
  , outerRadius :: Number -- Radius for leaves (default 400)
  }

-- | A node positioned for rendering
type PositionedNode a =
  { fullName :: String
  , shortName :: String
  , x :: Number -- Angle in radians
  , y :: Number -- Radius from center
  , cartX :: Number -- Cartesian x coordinate
  , cartY :: Number -- Cartesian y coordinate
  , data_ :: Maybe a -- Original data (for leaves)
  , isLeaf :: Boolean
  , outgoingCount :: Int
  , incomingCount :: Int
  }

-- | A bundled link with SVG path
type BundledLink =
  { source :: String
  , target :: String
  , path :: String -- SVG path data
  }

-- | Result of edge bundle layout
type EdgeBundleResult a =
  { nodes :: Array (PositionedNode a)
  , links :: Array BundledLink
  }

-- | Main entry point: compute edge bundle layout
-- |
-- | Takes configuration and array of nodes with imports,
-- | returns positioned nodes and bundled link paths.
edgeBundle
  :: forall a
   . EdgeBundleConfig a
  -> Array a
  -> EdgeBundleResult a
edgeBundle config inputNodes =
  let
    -- Step 1: Build hierarchy from flat nodes
    tree = buildHierarchy { getName: config.getName } inputNodes

    -- Step 2: Create bidirectional links
    bilinked = bilink config.getImports tree

    -- Step 3: Apply radial cluster layout to the original tree
    radialConfig =
      { innerRadius: config.innerRadius
      , outerRadius: config.outerRadius
      , startAngle: 0.0
      , endAngle: 2.0 * pi
      }
    radialTree = radialCluster radialConfig tree

    -- Step 4: Extract positioned nodes with link counts from bilinked tree
    positionedNodes = extractPositionedNodes bilinked radialTree

    -- Step 5: Generate bundled link paths
    bundledLinks = generateBundledLinks config.beta tree radialTree bilinked
  in
    { nodes: positionedNodes
    , links: bundledLinks
    }

-- | Extract positioned nodes by combining bilink info with radial layout
extractPositionedNodes
  :: forall a
   . BilinkedNode a
  -> RadialNode a
  -> Array (PositionedNode a)
extractPositionedNodes bilinked radial =
  let
    -- Get all bilinked nodes with link counts
    allBilinked = getAllBilinkedNodes bilinked

    -- Get all radial nodes with positions
    allRadial = getAllRadialNodes radial

    -- Merge by fullName
    merged = Array.mapMaybe
      ( \rNode ->
          let
            name = getRadialFullName rNode
            maybeBilinked = Array.find (\b -> getBilinkedFullName b == name) allBilinked
            cart = toCartesian rNode
          in
            Just
              { fullName: name
              , shortName: getRadialShortName rNode
              , x: getRadialAngle rNode
              , y: getRadialRadius rNode
              , cartX: cart.x
              , cartY: cart.y
              , data_: getRadialData rNode
              , isLeaf: Array.null (getRadialChildren rNode)
              , outgoingCount: case maybeBilinked of
                  Just b -> Array.length (getOutgoing b)
                  Nothing -> 0
              , incomingCount: case maybeBilinked of
                  Just b -> Array.length (Bilink.getIncoming b)
                  Nothing -> 0
              }
      )
      allRadial
  in
    merged

-- | Generate bundled links by finding paths and rendering curves
generateBundledLinks
  :: forall a
   . Number
  -> -- Beta tension
  TreeNode a
  -> -- Original tree (for path finding)
  RadialNode a
  -> -- Radial tree (for positions)
  BilinkedNode a
  -> -- Bilinked tree (for link info)
  Array BundledLink
generateBundledLinks tension tree radialTree bilinked =
  let
    -- Get all links from bilinked tree
    allLinks = Bilink.getLinks bilinked

    -- Get all radial nodes for position lookup
    radialNodes = getAllRadialNodes radialTree

    -- For each link, compute the path and render
    bundled = Array.mapMaybe
      ( \(Link link) ->
          -- Find source and target in trees
          case findNode link.source tree, findNode link.target tree of
            Just sourceTree, Just targetTree ->
              -- Get path through tree
              let
                pathNodes = pathBetween tree sourceTree targetTree

                -- Convert path to radial coordinates
                radialPath = Array.mapMaybe
                  ( \pathNode ->
                      let
                        name = getFullName pathNode
                      in
                        Array.find (\r -> getRadialFullName r == name) radialNodes
                  )
                  pathNodes

                -- Convert to angle/radius pairs
                radialPoints = map
                  ( \r ->
                      { angle: getRadialAngle r
                      , radius: getRadialRadius r
                      }
                  )
                  radialPath

                -- Generate SVG path
                svgPath = bundlePathRadial tension radialPoints
              in
                Just { source: link.source, target: link.target, path: svgPath }
            _, _ -> Nothing
      )
      allLinks
  in
    bundled

-- | Helper: get all nodes from a bilinked tree
getAllBilinkedNodes :: forall a. BilinkedNode a -> Array (BilinkedNode a)
getAllBilinkedNodes node@(BilinkedNode n) =
  [ node ] <> Array.concatMap getAllBilinkedNodes n.children

-- | Helper: get all nodes from a radial tree
getAllRadialNodes :: forall a. RadialNode a -> Array (RadialNode a)
getAllRadialNodes node@(RadialNode n) =
  [ node ] <> Array.concatMap getAllRadialNodes n.children

-- | Helper accessors for RadialNode
getRadialFullName :: forall a. RadialNode a -> String
getRadialFullName (RadialNode n) = n.fullName

getRadialShortName :: forall a. RadialNode a -> String
getRadialShortName (RadialNode n) = n.name

getRadialAngle :: forall a. RadialNode a -> Number
getRadialAngle (RadialNode n) = n.x

getRadialRadius :: forall a. RadialNode a -> Number
getRadialRadius (RadialNode n) = n.y

getRadialData :: forall a. RadialNode a -> Maybe a
getRadialData (RadialNode n) = n.data_

getRadialChildren :: forall a. RadialNode a -> Array (RadialNode a)
getRadialChildren (RadialNode n) = n.children
