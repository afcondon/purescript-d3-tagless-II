-- | Force Configurations for CodeExplorerV2
-- |
-- | Pure, immutable force configurations.
-- | These replace the mutable force handles in Forces.purs
module Component.CodeExplorerV2.ForceConfigs where

import Prelude

import D3.Viz.Spago.Model (SpagoSimNode, isModule, isPackage)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (toMaybe)
import Data.Number (infinity)
import PSD3.Config.Force (AttrValue(..), ForceConfig(..), ForceFilter(..), ForceParams(..), ForceType(..), centerForce, collideForce, manyBodyForce, withRadius, withStrength)
import Unsafe.Coerce (unsafeCoerce)

-- =============================================================================
-- Force Filters
-- =============================================================================

packagesOnly :: Maybe ForceFilter
packagesOnly = Just $ ForceFilter {
    description: "packages"
  , predicate: isPackage <<< unsafeCoerce
}

modulesOnly :: Maybe ForceFilter
modulesOnly = Just $ ForceFilter {
    description: "modules"
  , predicate: isModule <<< unsafeCoerce
}

treeParentsOnly :: Maybe ForceFilter
treeParentsOnly = Just $ ForceFilter {
    description: "tree parents"
  , predicate: isTreeParent <<< unsafeCoerce
}
  where
    isTreeParent :: SpagoSimNode -> Boolean
    isTreeParent d = case d.links.treeChildren of
      [] -> false
      _ -> true

-- =============================================================================
-- Helper Functions
-- =============================================================================

gridPointX :: SpagoSimNode -> Number
gridPointX d = fromMaybe d.x $ map _.x $ toMaybe d.gridXY

gridPointY :: SpagoSimNode -> Number
gridPointY d = fromMaybe d.y $ map _.y $ toMaybe d.gridXY

collideRadius :: SpagoSimNode -> Number
collideRadius d = d.r + 2.0

collideRadiusBig :: SpagoSimNode -> Number
collideRadiusBig d = d.r + 19.0

collideRadiusPack :: SpagoSimNode -> Number
collideRadiusPack d = d.r + 5.0

-- =============================================================================
-- Force Configurations (matching Forces.purs)
-- =============================================================================

-- | Charge force - nodes repel each other
charge :: ForceConfig
charge = manyBodyForce "charge"
  # withStrength 300.0

-- | Collision force - prevents overlap, uses each node's actual radius
collision :: ForceConfig
collision = ForceConfig {
    name: "collision"
  , forceType: ForceCollide
  , params: CollideParams {
      radius: DynamicIndexedValue (\d _ -> collideRadius (unsafeCoerce d))
    , strength: StaticValue 1.0
    , iterations: StaticValue 1.0
    }
  , filter: Nothing
}

-- | Center force - pulls toward origin
center :: ForceConfig
center = ForceConfig {
    name: "center"
  , forceType: ForceCenter
  , params: CenterParams {
      x: StaticValue 0.0
    , y: StaticValue 0.0
    , strength: StaticValue 0.1
    }
  , filter: Nothing
}

-- | Stronger center force for tree layout
centerStrong :: ForceConfig
centerStrong = ForceConfig {
    name: "centerStrong"
  , forceType: ForceCenter
  , params: CenterParams {
      x: StaticValue 0.0
    , y: StaticValue 0.0
    , strength: StaticValue 0.5
    }
  , filter: Nothing
}

-- | Link force - maintains link distances
-- | Tuned to 40 for force graph layout
links :: ForceConfig
links = ForceConfig {
    name: "links"
  , forceType: ForceLink
  , params: LinkParams {
      distance: StaticValue 40.0
    , strength: StaticValue 1.0
    , iterations: StaticValue 1.0
    }
  , filter: Nothing
}

-- | Collision force with larger radius for tree layout
-- | Tuned for force graph: radius +19, strength 0.6
collide2 :: ForceConfig
collide2 = ForceConfig {
    name: "collide2"
  , forceType: ForceCollide
  , params: CollideParams {
      radius: DynamicIndexedValue (\d _ -> collideRadiusBig (unsafeCoerce d))
    , strength: StaticValue 0.6
    , iterations: StaticValue 1.0
    }
  , filter: Nothing
}

-- | Charge force for tree - repels all nodes with distance limit
charge2 :: ForceConfig
charge2 = ForceConfig {
    name: "charge2"
  , forceType: ForceManyBody
  , params: ManyBodyParams {
      strength: StaticValue (-100.0)
    , theta: StaticValue 0.9
    , distanceMin: StaticValue 1.0
    , distanceMax: StaticValue 400.0
    }
  , filter: Nothing
}

-- | Collision force for bubble packs - uses actual node radius
collidePack :: ForceConfig
collidePack = ForceConfig {
    name: "collidePack"
  , forceType: ForceCollide
  , params: CollideParams {
      radius: DynamicIndexedValue (\d _ -> collideRadiusPack (unsafeCoerce d))
    , strength: StaticValue 1.0
    , iterations: StaticValue 3.0
    }
  , filter: Nothing
}

-- | Stronger charge force for bubble packs
chargePack :: ForceConfig
chargePack = ForceConfig {
    name: "chargePack"
  , forceType: ForceManyBody
  , params: ManyBodyParams {
      strength: StaticValue (-200.0)
    , theta: StaticValue 0.9
    , distanceMin: StaticValue 1.0
    , distanceMax: StaticValue 600.0
    }
  , filter: Nothing
}

-- | Charge force only on tree parent nodes - spreads tree structure
-- | Tuned for balanced force graph layout: strength -290, theta 0.8, distanceMin 9, distanceMax 300
chargeTree :: ForceConfig
chargeTree = ForceConfig {
    name: "chargetree"
  , forceType: ForceManyBody
  , params: ManyBodyParams {
      strength: StaticValue (-290.0)
    , theta: StaticValue 0.8
    , distanceMin: StaticValue 9.0
    , distanceMax: StaticValue 300.0
    }
  , filter: treeParentsOnly
}

-- | Package orbit - radial force for packages
packageOrbit :: ForceConfig
packageOrbit = ForceConfig {
    name: "packageOrbit"
  , forceType: ForceRadial
  , params: RadialParams {
      radius: StaticValue 900.0
    , strength: StaticValue 0.7
    , x: StaticValue 0.0
    , y: StaticValue 0.0
    }
  , filter: packagesOnly
}

-- | Module orbit - radial force for modules
moduleOrbit :: ForceConfig
moduleOrbit = ForceConfig {
    name: "moduleOrbit"
  , forceType: ForceRadial
  , params: RadialParams {
      radius: StaticValue 900.0
    , strength: StaticValue 0.8
    , x: StaticValue 0.0
    , y: StaticValue 0.0
    }
  , filter: modulesOnly
}

-- | Cluster X - pulls modules toward package X position
clusterX :: ForceConfig
clusterX = ForceConfig {
    name: "clusterX_M"
  , forceType: ForceX
  , params: ForceXParams {
      x: DynamicIndexedValue (\d _ -> gridPointX (unsafeCoerce d))
    , strength: StaticValue 0.2
    }
  , filter: modulesOnly
}

-- | Cluster Y - pulls modules toward package Y position
clusterY :: ForceConfig
clusterY = ForceConfig {
    name: "clusterY_M"
  , forceType: ForceY
  , params: ForceYParams {
      y: DynamicIndexedValue (\d _ -> gridPointY (unsafeCoerce d))
    , strength: StaticValue 0.2
    }
  , filter: modulesOnly
}

-- | All force configurations
allForceConfigs :: Array ForceConfig
allForceConfigs =
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
