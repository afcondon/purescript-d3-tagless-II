-- | Pure Force Engine Types
-- |
-- | These types model the data structures needed for force-directed layouts.
-- | The force calculations are pure functions over these structures.
-- |
-- | Key insight: D3's force functions just mutate vx/vy on nodes.
-- | We can call them directly without the simulation wrapper.
module PSD3.ForceEngine.Types
  ( -- * Node Types
    SimNode
    -- * Link Types
  , SimLink
  , RawLink
    -- * Simulation State
  , SimulationState
  , defaultSimParams
    -- * Force Configurations
  , ManyBodyConfig
  , defaultManyBody
  , CollideConfig
  , defaultCollide
  , LinkConfig
  , defaultLink
  , CenterConfig
  , defaultCenter
  , ForceXConfig
  , ForceYConfig
  , RadialConfig
    -- * Filtered/Dynamic Configurations
  , ManyBodyFilteredConfig
  , RadialFilteredConfig
  , CollideDynamicConfig
  , ForceXDynamicConfig
  , ForceYDynamicConfig
  , LinkDynamicConfig
    -- * Force Specification
  , ForceSpec(..)
  , forceName
  ) where

import Prelude


-- =============================================================================
-- Core Node Type
-- =============================================================================

-- | A simulation node with position and velocity.
-- | This is the minimal structure that D3 force functions expect.
-- |
-- | The `extra` row allows extending with application-specific data.
-- | Note: vx, vy, index are added by the FFI initialization function.
type SimNode extra =
  { x :: Number      -- Current x position
  , y :: Number      -- Current y position
  , vx :: Number     -- X velocity (modified by forces)
  , vy :: Number     -- Y velocity (modified by forces)
  , index :: Int     -- Node index in array (set by initialization)
  | extra
  }

-- =============================================================================
-- Link Type
-- =============================================================================

-- | A link between nodes.
-- | Before simulation: source/target are indices or IDs
-- | After swizzling: source/target are node references
type SimLink nodeRow linkData =
  { source :: { x :: Number, y :: Number | nodeRow }
  , target :: { x :: Number, y :: Number | nodeRow }
  , index :: Int
  | linkData
  }

-- | Raw link with source/target as indices (pre-swizzle)
type RawLink linkData =
  { source :: Int
  , target :: Int
  | linkData
  }

-- =============================================================================
-- Simulation State
-- =============================================================================

-- | The complete state of a force simulation.
-- | We manage this ourselves instead of letting D3 do it.
type SimulationState nodeRow linkData =
  { nodes :: Array (SimNode nodeRow)
  , links :: Array (RawLink linkData)
  , alpha :: Number           -- Current "temperature" (1.0 = hot, 0.0 = frozen)
  , alphaMin :: Number        -- Stop when alpha falls below this
  , alphaDecay :: Number      -- How fast alpha decreases (multiplier per tick)
  , alphaTarget :: Number     -- Alpha moves toward this value
  , velocityDecay :: Number   -- Friction (0.0 = no friction, 1.0 = stop immediately)
  , running :: Boolean        -- Is the simulation active?
  }

-- | Default simulation parameters
defaultSimParams ::
  { alpha :: Number
  , alphaMin :: Number
  , alphaDecay :: Number
  , alphaTarget :: Number
  , velocityDecay :: Number
  }
defaultSimParams =
  { alpha: 1.0
  , alphaMin: 0.001
  , alphaDecay: 0.01  -- Slower cooling than D3 default (0.0228) for better settling
  , alphaTarget: 0.0
  , velocityDecay: 0.4  -- D3's default
  }

-- =============================================================================
-- Force Configuration (Pure Data)
-- =============================================================================

-- | Configuration for a many-body (charge) force
type ManyBodyConfig =
  { strength :: Number       -- Positive = attract, negative = repel
  , theta :: Number          -- Barnes-Hut approximation parameter (0.9 default)
  , distanceMin :: Number    -- Minimum distance (avoids infinite forces)
  , distanceMax :: Number    -- Maximum distance (limits range)
  }

defaultManyBody :: ManyBodyConfig
defaultManyBody =
  { strength: -30.0
  , theta: 0.9
  , distanceMin: 1.0
  , distanceMax: infinity
  }
  where
  infinity = 1.0e10  -- Practical infinity

-- | Configuration for a collision force
type CollideConfig =
  { radius :: Number         -- Collision radius (or function of node)
  , strength :: Number       -- How strongly to resolve collisions (0-1)
  , iterations :: Int        -- Number of iterations per tick
  }

defaultCollide :: CollideConfig
defaultCollide =
  { radius: 1.0
  , strength: 1.0
  , iterations: 1
  }

-- | Configuration for a link force
type LinkConfig =
  { distance :: Number       -- Desired link length
  , strength :: Number       -- Spring strength (0-1)
  , iterations :: Int        -- Number of iterations per tick
  }

defaultLink :: LinkConfig
defaultLink =
  { distance: 30.0
  , strength: 1.0
  , iterations: 1
  }

-- | Configuration for centering force
type CenterConfig =
  { x :: Number              -- Center x
  , y :: Number              -- Center y
  , strength :: Number       -- How strongly to center (0-1)
  }

defaultCenter :: CenterConfig
defaultCenter =
  { x: 0.0
  , y: 0.0
  , strength: 1.0
  }

-- | Configuration for X positioning force
type ForceXConfig =
  { x :: Number              -- Target x position
  , strength :: Number       -- How strongly to pull (0-1)
  }

-- | Configuration for Y positioning force
type ForceYConfig =
  { y :: Number              -- Target y position
  , strength :: Number       -- How strongly to pull (0-1)
  }

-- | Configuration for radial force
type RadialConfig =
  { radius :: Number         -- Target radius from center
  , x :: Number              -- Center x
  , y :: Number              -- Center y
  , strength :: Number       -- How strongly to pull (0-1)
  }

-- =============================================================================
-- Filtered/Dynamic Force Configurations
-- =============================================================================
-- These allow forces to be applied conditionally or with dynamic parameters.
-- The accessor functions are passed to JS which calls them per-node.

-- | Configuration for filtered many-body force
-- | Applies charge only to nodes matching the predicate
type ManyBodyFilteredConfig node =
  { strength :: Number
  , theta :: Number
  , distanceMin :: Number
  , distanceMax :: Number
  , filter :: node -> Boolean  -- Only apply to matching nodes
  }

-- | Configuration for filtered radial force
type RadialFilteredConfig node =
  { radius :: Number
  , x :: Number
  , y :: Number
  , strength :: Number
  , filter :: node -> Boolean  -- Only apply to matching nodes
  }

-- | Configuration for collision force with dynamic radius
-- | Radius is computed per-node via accessor function
type CollideDynamicConfig node =
  { radiusAccessor :: node -> Number  -- Get radius from node (e.g., d.r + padding)
  , strength :: Number
  , iterations :: Int
  }

-- | Configuration for X positioning force with dynamic target
-- | Target X is computed per-node via accessor function
type ForceXDynamicConfig node =
  { xAccessor :: node -> Number  -- Get target X from node
  , strength :: Number
  }

-- | Configuration for Y positioning force with dynamic target
type ForceYDynamicConfig node =
  { yAccessor :: node -> Number  -- Get target Y from node
  , strength :: Number
  }

-- | Configuration for link force with dynamic strength per-link
-- | Strength is computed per-link via accessor function
type LinkDynamicConfig link =
  { distance :: Number              -- Desired link length
  , strengthAccessor :: link -> Number  -- Get strength from link (e.g., link.weight)
  , iterations :: Int
  }

-- =============================================================================
-- Force Sum Type
-- =============================================================================

-- | A force configuration - pure data describing a force
data ForceSpec
  = ManyBody String ManyBodyConfig      -- name, config
  | Collide String CollideConfig
  | Link String LinkConfig
  | Center String CenterConfig
  | PositionX String ForceXConfig
  | PositionY String ForceYConfig
  | Radial String RadialConfig

-- | Get the name of a force
forceName :: ForceSpec -> String
forceName = case _ of
  ManyBody name _ -> name
  Collide name _ -> name
  Link name _ -> name
  Center name _ -> name
  PositionX name _ -> name
  PositionY name _ -> name
  Radial name _ -> name

