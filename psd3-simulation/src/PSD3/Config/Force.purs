-- | Force Configuration Module
-- |
-- | This module defines immutable force configurations that are separate from
-- | the mutable d3-force JavaScript handles. This separation allows for:
-- | - Predictable scene transitions
-- | - Easy parameter resets
-- | - Better type safety
-- | - Improved debuggability
-- |
-- | Key types:
-- | - ForceConfig: Complete force specification (type, name, params, filter)
-- | - ForceParams: Type-specific parameters (ManyBody, Collide, etc.)
-- | - AttrValue: Parameter values (static or function-based)
module PSD3.Config.Force where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over)
import Data.Number (infinity)
import PSD3.Internal.Types (Datum_, Index_)

-- Label is just String
type Label = String

-- | An attribute value can be:
-- | - Static: a constant value for all nodes
-- | - Dynamic: computed per node
-- | - DynamicIndexed: computed per node with index
data AttrValue a
  = StaticValue a
  | DynamicValue (Datum_ -> a)
  | DynamicIndexedValue (Datum_ -> Index_ -> a)

derive instance Functor AttrValue

instance Show a => Show (AttrValue a) where
  show (StaticValue a) = "static(" <> show a <> ")"
  show (DynamicValue _) = "fn(datum)"
  show (DynamicIndexedValue _) = "fn(datum, index)"

-- | Force filter - applies force only to nodes matching predicate
-- | e.g., "tree parents only", "packages only", "modules only"
data ForceFilter = ForceFilter {
    description :: String
  , predicate   :: Datum_ -> Boolean
}

instance Show ForceFilter where
  show (ForceFilter f) = "filter(" <> f.description <> ")"

-- | All possible force types in d3-force
data ForceType
  = ForceManyBody
  | ForceCenter
  | ForceCollide
  | ForceX
  | ForceY
  | ForceRadial
  | ForceLink

derive instance Eq ForceType

instance Show ForceType where
  show ForceManyBody = "many-body"
  show ForceCenter = "center"
  show ForceCollide = "collide"
  show ForceX = "x-position"
  show ForceY = "y-position"
  show ForceRadial = "radial"
  show ForceLink = "link"

-- | Type-specific force parameters
-- | Each force type has its own set of valid parameters
data ForceParams
  = ManyBodyParams {
      strength    :: AttrValue Number
    , theta       :: AttrValue Number
    , distanceMin :: AttrValue Number
    , distanceMax :: AttrValue Number
    }
  | CenterParams {
      x        :: AttrValue Number
    , y        :: AttrValue Number
    , strength :: AttrValue Number
    }
  | CollideParams {
      radius     :: AttrValue Number
    , strength   :: AttrValue Number
    , iterations :: AttrValue Number
    }
  | ForceXParams {
      x        :: AttrValue Number
    , strength :: AttrValue Number
    }
  | ForceYParams {
      y        :: AttrValue Number
    , strength :: AttrValue Number
    }
  | RadialParams {
      radius   :: AttrValue Number
    , strength :: AttrValue Number
    , x        :: AttrValue Number
    , y        :: AttrValue Number
    }
  | LinkParams {
      distance   :: AttrValue Number
    , strength   :: AttrValue Number
    , iterations :: AttrValue Number
    }

instance Show ForceParams where
  show (ManyBodyParams p) =
    "ManyBody { strength: " <> show p.strength <> ", theta: " <> show p.theta <> " }"
  show (CenterParams p) =
    "Center { x: " <> show p.x <> ", y: " <> show p.y <> " }"
  show (CollideParams p) =
    "Collide { radius: " <> show p.radius <> ", strength: " <> show p.strength <> " }"
  show (ForceXParams p) =
    "ForceX { x: " <> show p.x <> ", strength: " <> show p.strength <> " }"
  show (ForceYParams p) =
    "ForceY { y: " <> show p.y <> ", strength: " <> show p.strength <> " }"
  show (RadialParams p) =
    "Radial { radius: " <> show p.radius <> ", strength: " <> show p.strength <> " }"
  show (LinkParams p) =
    "Link { distance: " <> show p.distance <> ", strength: " <> show p.strength <> " }"

-- | Complete force configuration
-- | This is pure data with no JavaScript references
newtype ForceConfig = ForceConfig {
    name       :: Label            -- Arbitrary name (can have multiple forces of same type)
  , forceType  :: ForceType        -- Determines which parameters are valid
  , params     :: ForceParams      -- Type-specific parameters
  , filter     :: Maybe ForceFilter -- Optional filter (applies force only to matching nodes)
}

derive instance Newtype ForceConfig _ 

instance Show ForceConfig where
  show (ForceConfig config) =
    config.name <> " [" <> show config.forceType <> "] " <> show config.params

-- =============================================================================
-- Smart Constructors with Defaults
-- =============================================================================

-- | Create a many-body force with default parameters
-- | Default: repulsive force (negative strength)
manyBodyForce :: Label -> ForceConfig
manyBodyForce name = ForceConfig {
    name
  , forceType: ForceManyBody
  , params: ManyBodyParams {
      strength: StaticValue (-30.0)
    , theta: StaticValue 0.9
    , distanceMin: StaticValue 1.0
    , distanceMax: StaticValue infinity
    }
  , filter: Nothing
}

-- | Create a centering force with default parameters
centerForce :: Label -> ForceConfig
centerForce name = ForceConfig {
    name
  , forceType: ForceCenter
  , params: CenterParams {
      x: StaticValue 0.0
    , y: StaticValue 0.0
    , strength: StaticValue 1.0
    }
  , filter: Nothing
}

-- | Create a collision force with default parameters
-- | Note: typically you want to provide a custom radius function
collideForce :: Label -> AttrValue Number -> ForceConfig
collideForce name radius = ForceConfig {
    name
  , forceType: ForceCollide
  , params: CollideParams {
      radius
    , strength: StaticValue 1.0
    , iterations: StaticValue 1.0
    }
  , filter: Nothing
}

-- | Create an X-positioning force with default parameters
forceX :: Label -> AttrValue Number -> ForceConfig
forceX name x = ForceConfig {
    name
  , forceType: ForceX
  , params: ForceXParams {
      x
    , strength: StaticValue 0.1
    }
  , filter: Nothing
}

-- | Create a Y-positioning force with default parameters
forceY :: Label -> AttrValue Number -> ForceConfig
forceY name y = ForceConfig {
    name
  , forceType: ForceY
  , params: ForceYParams {
      y
    , strength: StaticValue 0.1
    }
  , filter: Nothing
}

-- | Create a radial force with default parameters
radialForce :: Label -> Number -> ForceConfig
radialForce name radius = ForceConfig {
    name
  , forceType: ForceRadial
  , params: RadialParams {
      radius: StaticValue radius
    , strength: StaticValue 0.1
    , x: StaticValue 0.0
    , y: StaticValue 0.0
    }
  , filter: Nothing
}

-- | Create a link force with default parameters
linkForce :: Label -> ForceConfig
linkForce name = ForceConfig {
    name
  , forceType: ForceLink
  , params: LinkParams {
      distance: StaticValue 30.0
    , strength: StaticValue 1.0
    , iterations: StaticValue 1.0
    }
  , filter: Nothing
}

-- =============================================================================
-- Parameter Update Functions
-- =============================================================================

-- | Update a force's parameters
-- | Provides a type-safe way to modify parameters
updateParams :: ForceConfig -> ForceParams -> ForceConfig
updateParams config params = over ForceConfig (_ { params = params }) config

-- | Add or update a filter on a force
withFilter :: ForceConfig -> ForceFilter -> ForceConfig
withFilter config filter = over ForceConfig (_ { filter = Just filter }) config

-- | Remove filter from a force
withoutFilter :: ForceConfig -> ForceConfig
withoutFilter =  over ForceConfig $ _ { filter = Nothing }

-- =============================================================================
-- Parameter Helpers for Common Updates
-- =============================================================================

-- | Update strength parameter (works for forces that have strength)
withStrength :: Number -> ForceConfig -> ForceConfig
withStrength val (ForceConfig config) = ForceConfig $ case config.params of
  ManyBodyParams p -> config { params = ManyBodyParams $ p { strength = StaticValue val } }
  CenterParams p   -> config { params = CenterParams $ p { strength = StaticValue val } }
  CollideParams p  -> config { params = CollideParams $ p { strength = StaticValue val } }
  ForceXParams p   -> config { params = ForceXParams $ p { strength = StaticValue val } }
  ForceYParams p   -> config { params = ForceYParams $ p { strength = StaticValue val } }
  RadialParams p   -> config { params = RadialParams $ p { strength = StaticValue val } }
  LinkParams p     -> config { params = LinkParams $ p { strength = StaticValue val } }

-- | Update radius parameter (works for Collide and Radial forces)
withRadius :: AttrValue Number -> ForceConfig -> ForceConfig
withRadius val (ForceConfig config) = ForceConfig $ case config.params of
  CollideParams p -> config { params = CollideParams $ p { radius = val } }
  RadialParams p  -> config { params = RadialParams $ p { radius = val } }
  _               -> config  -- No-op for forces without radius

-- | Update distance parameter (Link force only)
withDistance :: Number -> ForceConfig -> ForceConfig
withDistance val (ForceConfig config) = ForceConfig $ case config.params of
  LinkParams p -> config { params = LinkParams $ p { distance = StaticValue val } }
  _            -> config
