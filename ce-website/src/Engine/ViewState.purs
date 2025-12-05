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
  = Treemap ScopeFilter  -- Treemap of packages and modules (initial view)
  | TreeLayout ScopeFilter String  -- Tree layout with scope and root module
  | ForceLayout ScopeFilter String  -- Radial force layout with scope and root module
  | Neighborhood String  -- module name
  | FunctionCalls String -- module name

-- | Scope of what to include
data ScopeFilter
  = UsedOnly  -- Only modules that are actually used
  | ProjectOnly  -- All modules in the project (used or not)
  | ProjectAndLibraries  -- Everything including dependencies

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
  Treemap scope ->
    { text: "[Treemap] showing [" <> scopeName scope <> "] packages and [" <> scopeName scope <> "] modules"
    , controls:
        [ { id: "layout", current: "treemap", options: ["treemap", "tree", "force"] }
        , { id: "scope", current: showScope scope, options: ["used", "project", "all"] }
        ]
    }

  TreeLayout scope rootModule ->
    { text: "[Tree] layout showing [" <> scopeName scope <> "] modules, with [" <> rootModule <> "] as the root"
    , controls:
        [ { id: "layout", current: "tree", options: ["treemap", "tree", "force"] }
        , { id: "scope", current: showScope scope, options: ["used", "project", "all"] }
        , { id: "root", current: rootModule, options: [] } -- options populated dynamically
        ]
    }

  ForceLayout scope rootModule ->
    { text: "[Force layout] showing [" <> scopeName scope <> "] modules, with [" <> rootModule <> "] as the root"
    , controls:
        [ { id: "layout", current: "force", options: ["treemap", "tree", "force"] }
        , { id: "scope", current: showScope scope, options: ["used", "project", "all"] }
        , { id: "root", current: rootModule, options: [] } -- options populated dynamically
        ]
    }

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
scopeName UsedOnly = "used"
scopeName ProjectOnly = "project"
scopeName ProjectAndLibraries = "all"

showScope :: ScopeFilter -> String
showScope UsedOnly = "used"
showScope ProjectOnly = "project"
showScope ProjectAndLibraries = "all"

-- =============================================================================
-- State Machine - Available Transitions
-- =============================================================================

-- | What views can we reach from here?
availableTransitions :: ViewState -> Array ViewState
availableTransitions vs = case vs of
  Treemap scope ->
    -- From treemap, can switch to tree or force layout
    [ TreeLayout scope "PSD3.Main"
    , ForceLayout scope "PSD3.Main"
    ]

  TreeLayout scope rootModule ->
    -- From tree, can switch back to treemap or to force
    [ Treemap scope
    , ForceLayout scope rootModule
    ]

  ForceLayout scope rootModule ->
    -- From force, can switch back to treemap or to tree
    [ Treemap scope
    , TreeLayout scope rootModule
    ]

  Neighborhood _ ->
    -- From neighborhood, can go back to treemap
    [ Treemap ProjectAndLibraries
    ]

  FunctionCalls modName ->
    -- From function calls, go back to neighborhood
    [ Neighborhood modName
    ]
