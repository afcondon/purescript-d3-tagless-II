-- | DataViz.Layout.Hierarchy.EdgeBundle.RadialCluster
-- |
-- | Radial cluster layout for edge bundling.
-- | Positions leaf nodes around a circle and internal nodes at the center.
-- |
-- | The layout uses (angle, radius) coordinates:
-- | - angle: position around the circle (0 to 2π radians)
-- | - radius: distance from center (internal nodes closer to center, leaves at edge)
module DataViz.Layout.Hierarchy.EdgeBundle.RadialCluster
  ( RadialLayoutConfig
  , defaultRadialConfig
  , RadialNode(..)
  , radialCluster
  , toCartesian
  , radialClusterFromTree
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (cos, sin, pi)
import DataViz.Layout.Hierarchy.EdgeBundle.Hierarchy (TreeNode(..), getFullName, getTreeNodeChildren, isLeaf, leaves)

-- | Configuration for radial cluster layout
type RadialLayoutConfig =
  { innerRadius :: Number -- Radius for internal nodes
  , outerRadius :: Number -- Radius for leaf nodes
  , startAngle :: Number -- Starting angle (default: 0)
  , endAngle :: Number -- Ending angle (default: 2π)
  }

-- | Default configuration
defaultRadialConfig :: RadialLayoutConfig
defaultRadialConfig =
  { innerRadius: 100.0
  , outerRadius: 400.0
  , startAngle: 0.0
  , endAngle: 2.0 * pi
  }

-- | A node with radial coordinates
data RadialNode a = RadialNode
  { name :: String
  , fullName :: String
  , children :: Array (RadialNode a)
  , data_ :: Maybe a
  , depth :: Int
  , height :: Int
  , x :: Number -- Angle in radians
  , y :: Number -- Radius from center
  }

instance showRadialNode :: Show a => Show (RadialNode a) where
  show (RadialNode n) = "RadialNode { fullName: " <> n.fullName
    <> ", x: "
    <> show n.x
    <> ", y: "
    <> show n.y
    <> " }"

-- | Convert radial coordinates to Cartesian for SVG rendering
toCartesian :: forall a. RadialNode a -> { x :: Number, y :: Number }
toCartesian (RadialNode n) =
  { x: n.y * cos n.x
  , y: n.y * sin n.x
  }

-- | Apply radial cluster layout to a tree
-- |
-- | Algorithm:
-- | 1. Assign sequential indices to leaves (angular positions)
-- | 2. Position leaves evenly around the circle at outerRadius
-- | 3. Position internal nodes at mean angle of children, at appropriate radius
radialCluster :: forall a. RadialLayoutConfig -> TreeNode a -> RadialNode a
radialCluster config tree =
  let
    -- Count total leaves for spacing calculation
    leafNodes = leaves tree
    numLeaves = Array.length leafNodes
    angleStep = (config.endAngle - config.startAngle) / toNumber numLeaves

    -- Get max height for radius calculation
    maxHeight = getMaxHeight tree

    -- Layout with state (current leaf index)
    result = layoutNode config maxHeight angleStep { currentIndex: 0 } tree
  in
    result.node

-- | Layout a single node, threading through the current leaf index
layoutNode
  :: forall a
   . RadialLayoutConfig
  -> Int
  -> -- Max tree height
  Number
  -> -- Angle step per leaf
  { currentIndex :: Int }
  -> -- State: current leaf index
  TreeNode a
  -> { node :: RadialNode a, state :: { currentIndex :: Int } }
layoutNode config maxHeight angleStep state (TreeNode n) =
  if Array.null n.children then
    -- Leaf node: assign next available angle
    let
      angle = config.startAngle + (toNumber state.currentIndex + 0.5) * angleStep
      radius = config.outerRadius
      newState = { currentIndex: state.currentIndex + 1 }
    in
      { node: RadialNode
          { name: n.name
          , fullName: n.fullName
          , children: []
          , data_: n.data_
          , depth: n.depth
          , height: n.height
          , x: angle
          , y: radius
          }
      , state: newState
      }
  else
    -- Internal node: layout children first, then position at mean angle
    let
      -- Process all children, threading state
      childResults = foldl
        ( \acc child ->
            let
              result = layoutNode config maxHeight angleStep acc.state child
            in
              { children: Array.snoc acc.children result.node, state: result.state }
        )
        { children: [], state: state }
        n.children

      -- Calculate mean angle of children
      childAngles = map (\(RadialNode c) -> c.x) childResults.children
      meanAngle = case childAngles of
        [] -> 0.0
        angles -> foldl (+) 0.0 angles / toNumber (Array.length angles)

      -- Calculate radius based on height (closer to center for higher nodes)
      -- height = 0 is leaf (at outerRadius), height = maxHeight is root (at innerRadius)
      heightRatio = toNumber n.height / toNumber maxHeight
      radius = config.innerRadius + (config.outerRadius - config.innerRadius) * (1.0 - heightRatio)
    in
      { node: RadialNode
          { name: n.name
          , fullName: n.fullName
          , children: childResults.children
          , data_: n.data_
          , depth: n.depth
          , height: n.height
          , x: meanAngle
          , y: radius
          }
      , state: childResults.state
      }

-- | Get maximum height in tree
getMaxHeight :: forall a. TreeNode a -> Int
getMaxHeight (TreeNode n) = n.height

-- | Convenience function: layout from a TreeNode using default config
radialClusterFromTree :: forall a. TreeNode a -> RadialNode a
radialClusterFromTree = radialCluster defaultRadialConfig
