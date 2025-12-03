-- | Orbit Layout for Code Explorer
-- |
-- | Packages arranged on a radial ring around the center.
-- | Modules cluster near their parent package.
-- | This is an intermediate view between Grid and Tree.
module Viz.SpagoGridTest.OrbitLayout
  ( calculateOrbitPositions
  , orbitRadius
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
-- Constants
-- =============================================================================

-- | Radius of the package orbit ring
-- | Should be larger than treeRadius (700) so tree fits inside
orbitRadius :: Number
orbitRadius = 900.0

-- | Distance modules are offset from their package
moduleOffsetBase :: Number
moduleOffsetBase = 40.0

-- =============================================================================
-- Orbit Layout
-- =============================================================================

-- | Calculate orbit positions for all nodes
-- | Packages are placed on a ring at orbitRadius
-- | Modules are clustered near their parent package
calculateOrbitPositions :: Array SimNode -> Object { x :: Number, y :: Number }
calculateOrbitPositions nodes =
  let
    packages = Array.filter (\n -> n.nodeType == PackageNode) nodes

    -- Calculate package positions on the orbit ring
    packagePositions = map (\pkg ->
      let
        -- Use pre-calculated orbitAngle from node (assigned in Loader)
        angle = pkg.orbitAngle
        x = cos angle * orbitRadius
        y = sin angle * orbitRadius
      in Tuple (show pkg.id) { x, y }
    ) packages

    packagePosMap = Object.fromFoldable packagePositions

    -- Calculate module positions - clustered near their package
    modulePositions = Array.mapMaybe (\n ->
      if n.nodeType == ModuleNode
        then case Object.lookup (show n.cluster) packagePosMap of
          Just { x: px, y: py } ->
            let
              -- Spread modules in a small cluster around package
              -- Use node id to get deterministic but varied positions
              -- Modules form a small arc "behind" the package (away from center)
              pkgAngle = getPackageAngle n.cluster packages

              -- Module offset: spread along the arc
              arcSpread = 0.4 -- radians of arc to spread modules across
              moduleIndex = n.id `mod` 100
              moduleOffset = (toNumber moduleIndex / 100.0 - 0.5) * arcSpread

              -- Distance from package (varies per module)
              dist = moduleOffsetBase + toNumber (n.id `mod` 50) * 0.5

              -- Position along the arc extending outward from package
              angle = pkgAngle + moduleOffset

              -- Offset from package position (away from center)
              offsetX = cos angle * dist
              offsetY = sin angle * dist

              mx = px + offsetX
              my = py + offsetY
            in Just (Tuple (show n.id) { x: mx, y: my })
          Nothing -> Nothing
        else Nothing
    ) nodes
  in
    Object.fromFoldable (packagePositions <> modulePositions)

-- | Get the orbit angle for a package by its ID
getPackageAngle :: Int -> Array SimNode -> Number
getPackageAngle pkgId packages =
  case Array.find (\p -> p.id == pkgId) packages of
    Just pkg -> pkg.orbitAngle
    Nothing -> 0.0
