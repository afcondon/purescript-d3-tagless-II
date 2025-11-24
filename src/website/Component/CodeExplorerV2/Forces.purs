-- | Forces.purs - Centralized force definitions for CodeExplorerV2
-- |
-- | All forces are defined as top-level expressions.
-- | Scenes reference forces directly (no string lookups).
module Component.CodeExplorerV2.Forces where

import Prelude

import D3.Viz.Spago.Model (SpagoSimNode, isModule, isPackage)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (toMaybe)
import PSD3.Internal.Simulation.Config as F
import PSD3.Internal.Simulation.Forces (createForce, createLinkForce)
import PSD3.Internal.Simulation.Types (Force, ForceFilter(..), ForceType(..), RegularForceType(..), allNodes)
import Unsafe.Coerce (unsafeCoerce)

-- Force filters
packagesOnly :: forall d. Maybe (ForceFilter d)
packagesOnly = Just $ ForceFilter "packages" (isPackage <<< unsafeCoerce)

modulesOnly :: forall d. Maybe (ForceFilter d)
modulesOnly = Just $ ForceFilter "modules" (isModule <<< unsafeCoerce)

-- Helper functions for cluster forces
gridPointX :: SpagoSimNode -> Number
gridPointX d = fromMaybe d.x $ map _.x $ toMaybe d.gridXY

gridPointY :: SpagoSimNode -> Number
gridPointY d = fromMaybe d.y $ map _.y $ toMaybe d.gridXY

-- | Charge force - nodes repel each other
charge :: Force SpagoSimNode
charge = createForce "charge" (RegularForce ForceManyBody) allNodes [ F.strengthVal 300.0 ]

-- | Collision force - prevents overlap
collision :: Force SpagoSimNode
collision = createForce "collision" (RegularForce ForceCollide) allNodes [ F.radiusVal 20.0 ]

-- | Center force - pulls toward origin
center :: Force SpagoSimNode
center = createForce "center" (RegularForce ForceCenter) allNodes [ F.xVal 0.0, F.yVal 0.0, F.strengthVal 0.1 ]

-- | Link force - maintains link distances
links :: Force SpagoSimNode
links = createLinkForce allNodes [ F.distanceVal 50.0 ]

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
  , links
  , packageOrbit
  , moduleOrbit
  , clusterX
  , clusterY
  ]
