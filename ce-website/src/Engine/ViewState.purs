-- | ViewState - Semantic view descriptions for TangleJS-style panel
-- |
-- | User-facing layer that hides scene/force mechanics.
-- | Each ViewState generates natural language with interactive controls.
module Engine.ViewState
  ( ViewState(..)
  , ScopeFilter(..)
  , LayoutStyle(..)
  , Control
  , describe
  , availableTransitions
  ) where

import Prelude

-- =============================================================================
-- Core ADTs
-- =============================================================================

-- | What's being shown and how
data ViewState
  = ModuleTreemap ScopeFilter  -- Static treemap (initial view)
  | PackageGrid ScopeFilter
  | ModuleOrbit ScopeFilter
  | DependencyTree ScopeFilter
  | Neighborhood String  -- module name
  | FunctionCalls String -- module name

-- | Scope of what to include
data ScopeFilter
  = ProjectOnly
  | ProjectAndLibraries

-- | Layout style (for future use)
data LayoutStyle
  = Grid
  | Radial
  | Tree
  | Force

derive instance eqViewState :: Eq ViewState
derive instance eqScopeFilter :: Eq ScopeFilter
derive instance eqLayoutStyle :: Eq LayoutStyle

-- =============================================================================
-- Description Generator
-- =============================================================================

-- | Generate TangleJS-style description
-- | Returns { text, controls } where controls are the interactive bits
describe :: ViewState -> { text :: String, controls :: Array Control }
describe vs = case vs of
  ModuleTreemap scope ->
    { text: "[" <> layoutName <> "] of [" <> scopeName scope <> "] modules, sized by lines of code"
    , controls:
        [ { id: "layout", current: "grid", options: ["grid", "orbit"] }
        , { id: "scope", current: showScope scope, options: ["project", "all"] }
        ]
    }
    where layoutName = "Grid"

  PackageGrid scope ->
    { text: "[" <> layoutName <> "] of [" <> scopeName scope <> "] packages"
    , controls:
        [ { id: "layout", current: "grid", options: ["grid", "orbit"] }
        , { id: "scope", current: showScope scope, options: ["project", "all"] }
        ]
    }
    where layoutName = "Grid"

  ModuleOrbit scope ->
    { text: "[" <> layoutName <> "] of [" <> scopeName scope <> "] modules, grouped by package"
    , controls:
        [ { id: "layout", current: "orbit", options: ["grid", "orbit", "tree"] }
        , { id: "scope", current: showScope scope, options: ["project", "all"] }
        ]
    }
    where layoutName = "Orbit"

  DependencyTree scope ->
    { text: "[" <> layoutName <> "] of [" <> scopeName scope <> "] module dependencies"
    , controls:
        [ { id: "layout", current: "tree", options: ["grid", "orbit", "tree"] }
        , { id: "scope", current: showScope scope, options: ["project", "all"] }
        ]
    }
    where layoutName = "Dependency tree"

  Neighborhood modName ->
    { text: "Neighborhood of [" <> modName <> "] — imports and dependents"
    , controls:
        [ { id: "module", current: modName, options: [] } -- options populated dynamically
        ]
    }

  FunctionCalls modName ->
    { text: "Function calls in [" <> modName <> "]"
    , controls:
        [ { id: "module", current: modName, options: [] }
        ]
    }

-- | Control descriptor for TangleJS rendering
type Control =
  { id :: String
  , current :: String
  , options :: Array String
  }

-- =============================================================================
-- Helpers
-- =============================================================================

scopeName :: ScopeFilter -> String
scopeName ProjectOnly = "project"
scopeName ProjectAndLibraries = "all"

showScope :: ScopeFilter -> String
showScope ProjectOnly = "project"
showScope ProjectAndLibraries = "all"

-- =============================================================================
-- State Machine - Available Transitions
-- =============================================================================

-- | What views can we reach from here?
availableTransitions :: ViewState -> Array ViewState
availableTransitions vs = case vs of
  ModuleTreemap scope ->
    [ PackageGrid scope
    , ModuleOrbit scope
    ]

  PackageGrid scope ->
    [ ModuleTreemap scope
    , ModuleOrbit scope
    , DependencyTree scope
    ]

  ModuleOrbit scope ->
    [ ModuleTreemap scope
    , PackageGrid scope
    , DependencyTree scope
    ]

  DependencyTree scope ->
    [ ModuleTreemap scope
    , PackageGrid scope
    , ModuleOrbit scope
    ]

  Neighborhood _ ->
    -- From neighborhood, can go back to full views
    [ ModuleTreemap ProjectAndLibraries
    , PackageGrid ProjectAndLibraries
    , DependencyTree ProjectAndLibraries
    ]

  FunctionCalls modName ->
    -- From function calls, go back to neighborhood
    [ Neighborhood modName
    ]
