-- | Forces.purs - Centralized force definitions for CodeExplorerV2
-- |
-- | All forces are defined as top-level expressions.
-- | Scenes reference forces directly (no string lookups).
module Component.CodeExplorerV2.Forces where

import Prelude

import D3.Viz.Spago.Model (SpagoSimNode, isModule, isPackage)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (toMaybe)
import Data.Number (infinity)
import PSD3.Internal.Simulation.Config as F
import PSD3.Internal.Simulation.Forces (createForce, createLinkForce)
import PSD3.Internal.Simulation.Types (Force, ForceFilter(..), ForceType(..), RegularForceType(..), allNodes)
import Unsafe.Coerce (unsafeCoerce)

-- Force filters
packagesOnly :: forall d. Maybe (ForceFilter d)
packagesOnly = Just $ ForceFilter "packages" (isPackage <<< unsafeCoerce)

modulesOnly :: forall d. Maybe (ForceFilter d)
modulesOnly = Just $ ForceFilter "modules" (isModule <<< unsafeCoerce)

-- | Filter for tree parent nodes (nodes with children) - spreads tree out
treeParentsOnly :: forall d. Maybe (ForceFilter d)
treeParentsOnly = Just $ ForceFilter "tree parents" (isTreeParent <<< unsafeCoerce)
  where
    isTreeParent :: SpagoSimNode -> Boolean
    isTreeParent d = case d.links.treeChildren of
      [] -> false
      _ -> true

-- Helper functions for cluster forces
gridPointX :: SpagoSimNode -> Number
gridPointX d = fromMaybe d.x $ map _.x $ toMaybe d.gridXY

gridPointY :: SpagoSimNode -> Number
gridPointY d = fromMaybe d.y $ map _.y $ toMaybe d.gridXY

-- Helper for collision radius - use node's actual radius with padding
collideRadius :: SpagoSimNode -> Number
collideRadius d = d.r + 2.0

-- Helper for collision radius with extra space
-- Tuned to +19 for force graph layout
collideRadiusBig :: SpagoSimNode -> Number
collideRadiusBig d = d.r + 19.0

-- Helper for bubble pack collision radius (larger padding)
collideRadiusPack :: SpagoSimNode -> Number
collideRadiusPack d = d.r + 5.0

-- | Charge force - nodes repel each other
charge :: Force SpagoSimNode
charge = createForce "charge" (RegularForce ForceManyBody) allNodes [ F.strengthVal 300.0 ]

-- | Collision force - prevents overlap, uses each node's actual radius
collision :: Force SpagoSimNode
collision = createForce "collision" (RegularForce ForceCollide) allNodes [ F.radiusFn (\d _ -> collideRadius (unsafeCoerce d)) ]

-- | Center force - pulls toward origin
center :: Force SpagoSimNode
center = createForce "center" (RegularForce ForceCenter) allNodes [ F.xVal 0.0, F.yVal 0.0, F.strengthVal 0.1 ]

-- | Stronger center force for tree layout
centerStrong :: Force SpagoSimNode
centerStrong = createForce "centerStrong" (RegularForce ForceCenter) allNodes [ F.xVal 0.0, F.yVal 0.0, F.strengthVal 0.5 ]

-- | Link force - maintains link distances
-- | Tuned to 40 for force graph layout
links :: Force SpagoSimNode
links = createLinkForce allNodes [ F.distanceVal 40.0 ]

-- | Collision force with larger radius for tree layout
-- | Tuned for force graph: radius +19, strength 0.6
collide2 :: Force SpagoSimNode
collide2 = createForce "collide2" (RegularForce ForceCollide) allNodes
  [ F.strengthVal 0.6, F.radiusFn (\d _ -> collideRadiusBig (unsafeCoerce d)) ]

-- | Charge force for tree - repels all nodes with distance limit
charge2 :: Force SpagoSimNode
charge2 = createForce "charge2" (RegularForce ForceManyBody) allNodes
  [ F.strengthVal (-100.0), F.thetaVal 0.9, F.distanceMinVal 1.0, F.distanceMaxVal 400.0 ]

-- | Collision force for bubble packs - uses actual node radius
collidePack :: Force SpagoSimNode
collidePack = createForce "collidePack" (RegularForce ForceCollide) allNodes
  [ F.strengthVal 1.0, F.radiusFn (\d _ -> collideRadiusPack (unsafeCoerce d)), F.iterationsVal 3.0 ]

-- | Stronger charge force for bubble packs
chargePack :: Force SpagoSimNode
chargePack = createForce "chargePack" (RegularForce ForceManyBody) allNodes
  [ F.strengthVal (-200.0), F.thetaVal 0.9, F.distanceMinVal 1.0, F.distanceMaxVal 600.0 ]

-- | Charge force only on tree parent nodes - spreads tree structure
-- | Tuned for balanced force graph layout: strength -290, theta 0.8, distanceMin 9, distanceMax 300
chargeTree :: Force SpagoSimNode
chargeTree = createForce "chargetree" (RegularForce ForceManyBody) treeParentsOnly
  [ F.strengthVal (-290.0), F.thetaVal 0.8, F.distanceMinVal 9.0, F.distanceMaxVal 300.0 ]

-- | Package orbit - radial force for packages
packageOrbit :: Force SpagoSimNode
packageOrbit = createForce "packageOrbit" (RegularForce ForceRadial) packagesOnly
  [ F.strengthVal 0.7, F.xVal 0.0, F.yVal 0.0, F.radiusVal 900.0 ]

-- | Module orbit - radial force for modules
moduleOrbit :: Force SpagoSimNode
moduleOrbit = createForce "moduleOrbit" (RegularForce ForceRadial) modulesOnly
  [ F.strengthVal 0.8, F.xVal 0.0, F.yVal 0.0, F.radiusVal 900.0 ]

-- | Cluster X - pulls modules toward package X position
clusterX :: Force SpagoSimNode
clusterX = createForce "clusterX_M" (RegularForce ForceX) modulesOnly
  [ F.strengthVal 0.2, F.xFn (\d _ -> gridPointX (unsafeCoerce d)) ]

-- | Cluster Y - pulls modules toward package Y position
clusterY :: Force SpagoSimNode
clusterY = createForce "clusterY_M" (RegularForce ForceY) modulesOnly
  [ F.strengthVal 0.2, F.yFn (\d _ -> gridPointY (unsafeCoerce d)) ]

-- | All forces for initialization
allForces :: Array (Force SpagoSimNode)
allForces =
  [ charge
  , collision
  , center
  , centerStrong
  , links
  , collide2
  , charge2
  , chargeTree
  , packageOrbit
  , moduleOrbit
  , clusterX
  , clusterY
  , collidePack
  , chargePack
  ]
