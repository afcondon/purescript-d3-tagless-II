-- | Tree Layout for Code Explorer
-- |
-- | Calculates radial tree positions for packages and their modules.
-- | Packages are arranged on a ring, modules cluster around their package.
module Viz.SpagoGridTest.TreeLayout
  ( calculateTreePositions
  ) where

import Prelude

import Data.Array as Array
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Number (cos, sin)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Types (SimNode, NodeType(..))

-- =============================================================================
-- Tree Layout
-- =============================================================================

-- | Calculate radial tree positions for all nodes
-- | Packages are placed on a ring, modules offset from their package
calculateTreePositions :: Array SimNode -> Object { x :: Number, y :: Number }
calculateTreePositions nodes =
  let
    packages = Array.filter (\n -> n.nodeType == PackageNode) nodes
    packageCount = Array.length packages

    ringRadius = 800.0
    packagePositions = Array.mapWithIndex (\i pkg ->
      let
        angle = 2.0 * pi * toNumber i / toNumber packageCount
        x = cos angle * ringRadius
        y = sin angle * ringRadius
      in Tuple (show pkg.id) { x, y }
    ) packages

    packagePosMap = Object.fromFoldable packagePositions

    modulePositions = Array.mapMaybe (\n ->
      if n.nodeType == ModuleNode
        then case Object.lookup (show n.cluster) packagePosMap of
          Just { x: px, y: py } ->
            let
              offsetAngle = toNumber n.id * 0.3
              offsetDist = 50.0 + toNumber (n.id `mod` 100) * 0.5
              mx = px + cos offsetAngle * offsetDist
              my = py + sin offsetAngle * offsetDist
            in Just (Tuple (show n.id) { x: mx, y: my })
          Nothing -> Nothing
        else Nothing
    ) nodes
  in
    Object.fromFoldable (packagePositions <> modulePositions)
  where
  pi = 3.14159265358979
