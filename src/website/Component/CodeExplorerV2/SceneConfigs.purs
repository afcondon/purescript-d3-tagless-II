-- | Scene Configurations for CodeExplorerV2
-- |
-- | Each scene defines its forces using the SimulationManager's force creation functions.
-- | These are pure data - no D3 handles created until actually needed.
module Component.CodeExplorerV2.SceneConfigs
  ( SceneConfig
  , orbitScene
  , treeScene
  , forceGraphScene
  , bubblePackScene
  , applyScene
  ) where

import Prelude

import Component.CodeExplorerV2.SimulationManager as Sim
import D3.Viz.Spago.Model (SpagoSimNode, isModule, isPackage)
import Data.Array as Array
import Data.Maybe (Maybe(..), isJust)
import Data.Nullable (toMaybe)
import Effect (Effect)
import Effect.Ref (Ref)

-- =============================================================================
-- Scene Configuration Type
-- =============================================================================

-- | A scene configuration - describes what forces to apply
type SceneConfig =
  { name :: String
  , forces :: Array { name :: String, create :: Effect Sim.ForceHandle }
  , alpha :: Number
  , velocityDecay :: Number
  }

-- =============================================================================
-- Predicates for Filters
-- =============================================================================

-- | Is the node a tree parent (has tree children)?
isTreeParent :: SpagoSimNode -> Boolean
isTreeParent d = not $ Array.null d.links.treeChildren

-- =============================================================================
-- Orbit Scene
-- =============================================================================

-- | Orbit scene - packages on outer ring, modules clustered toward packages
orbitScene :: SceneConfig
orbitScene =
  { name: "Orbit"
  , forces:
      [ { name: "collision"
        , create: pure $ Sim.createCollision
            { padding: 2.0
            , strength: 1.0
            , iterations: 1.0
            }
        }
      , { name: "clusterX"
        , create: pure $ Sim.createClusterX 0.2 isModule
        }
      , { name: "clusterY"
        , create: pure $ Sim.createClusterY 0.2 isModule
        }
      ]
  , alpha: 0.3
  , velocityDecay: 0.4
  }

-- =============================================================================
-- Tree Scene
-- =============================================================================

-- | Tree scene - nodes pinned in radial tree positions
-- | Uses minimal forces since nodes are pinned
treeScene :: SceneConfig
treeScene =
  { name: "Tree"
  , forces:
      [ { name: "collision"
        , create: pure $ Sim.createCollision
            { padding: 2.0
            , strength: 0.3
            , iterations: 1.0
            }
        }
      ]
  , alpha: 0.1  -- Low energy since nodes are pinned
  , velocityDecay: 0.4
  }

-- =============================================================================
-- Force Graph Scene
-- =============================================================================

-- | Force graph scene - tree links with force-directed layout
forceGraphScene :: SceneConfig
forceGraphScene =
  { name: "ForceGraph"
  , forces:
      [ { name: "links"
        , create: pure $ Sim.createLink
            { distance: 125.0
            , strength: 1.0
            , iterations: 6.0
            }
        }
      , { name: "chargeTree"
        , create: pure $ Sim.createChargeFiltered
            { strength: -270.0
            , theta: 0.7
            , distanceMin: 6.0
            , distanceMax: 900.0
            }
            isTreeParent
        }
      , { name: "collision"
        , create: pure $ Sim.createCollision
            { padding: 53.0
            , strength: 0.1
            , iterations: 1.0
            }
        }
      , { name: "center"
        , create: pure $ Sim.createCenter
            { x: 0.0
            , y: 0.0
            , strength: 0.85
            }
        }
      ]
  , alpha: 1.0
  , velocityDecay: 0.4
  }

-- =============================================================================
-- Bubble Pack Scene
-- =============================================================================

-- | Bubble pack scene - packed circles for my-project modules
bubblePackScene :: SceneConfig
bubblePackScene =
  { name: "BubblePack"
  , forces:
      [ { name: "links"
        , create: pure $ Sim.createLink
            { distance: 50.0
            , strength: 0.5
            , iterations: 2.0
            }
        }
      , { name: "chargePack"
        , create: pure $ Sim.createCharge
            { strength: -200.0
            , theta: 0.9
            , distanceMin: 1.0
            , distanceMax: 600.0
            }
        }
      , { name: "collidePack"
        , create: pure $ Sim.createCollision
            { padding: 5.0
            , strength: 1.0
            , iterations: 3.0
            }
        }
      , { name: "center"
        , create: pure $ Sim.createCenter
            { x: 0.0
            , y: 0.0
            , strength: 0.1
            }
        }
      ]
  , alpha: 1.0
  , velocityDecay: 0.4
  }

-- =============================================================================
-- Scene Application
-- =============================================================================

-- | Apply a scene configuration to the simulation
applyScene :: SceneConfig -> Ref Sim.SimState -> Effect Unit
applyScene config simRef = do
  -- Clear existing forces
  Sim.clearForces simRef

  -- Create and add each force
  for_ config.forces \forceConfig -> do
    handle <- forceConfig.create
    Sim.addForce forceConfig.name handle simRef

  -- Set alpha (this will be applied on next start/reheat)
  -- Note: We'd need to add setAlpha to SimulationManager for this
  pure unit

  where
  for_ :: forall a. Array a -> (a -> Effect Unit) -> Effect Unit
  for_ arr f = void $ traverse_ f arr

  traverse_ :: forall a. (a -> Effect Unit) -> Array a -> Effect Unit
  traverse_ f arr = case Array.uncons arr of
    Nothing -> pure unit
    Just { head, tail } -> do
      f head
      traverse_ f tail
