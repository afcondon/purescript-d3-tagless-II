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
  , swizzleLinks
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
    -- * Force Specification
  , ForceSpec(..)
  , forceName
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafeCrashWith)


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
  , alphaDecay: 0.0228  -- D3's default: 1 - pow(0.001, 1/300)
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

-- =============================================================================
-- Link Swizzling
-- =============================================================================

-- | Convert raw links (integer indices) to swizzled links (node references)
-- |
-- | This is needed because:
-- | 1. D3's forceLink mutates links by replacing indices with node references
-- | 2. We need node references for rendering (to read x,y from source/target)
-- | 3. We want to keep the original links intact for the simulation
-- |
-- | The transform function allows you to copy extra fields from the raw link.
-- |
-- | Example:
-- | ```purescript
-- | let swizzled = swizzleLinks nodes rawLinks \src tgt i link ->
-- |       { source: src, target: tgt, index: i, value: link.value }
-- | ```
swizzleLinks
  :: forall node rawLink swizzled
   . Array node
  -> Array { source :: Int, target :: Int | rawLink }
  -> (node -> node -> Int -> { source :: Int, target :: Int | rawLink } -> swizzled)
  -> Array swizzled
swizzleLinks nodes links transform =
  Array.mapWithIndex swizzle links
  where
  swizzle i link =
    let src = unsafeArrayIndex nodes link.source
        tgt = unsafeArrayIndex nodes link.target
    in transform src tgt i link

-- | Safe-ish array index (crashes with helpful message if out of bounds)
unsafeArrayIndex :: forall a. Array a -> Int -> a
unsafeArrayIndex arr i = case Array.index arr i of
  Just x -> x
  Nothing -> unsafeCrashWith ("swizzleLinks: Array index out of bounds: " <> show i)
