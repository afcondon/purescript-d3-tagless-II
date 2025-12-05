-- | ViewState - Semantic view descriptions for TangleJS-style panel
-- |
-- | User-facing layer that hides scene/force mechanics.
-- | Each ViewState generates natural language with interactive controls.
-- |
-- | Implements the Tangle typeclass for structured document generation.
module Engine.ViewState
  ( ViewState(..)
  , ScopeFilter(..)
  , LayoutStyle(..)
  , Control
  , describe
  , describeDoc
  , applyViewControl
  , availableTransitions
  ) where

import Prelude

import Tangle.Core as Tangle
import Tangle.Core (TangleDoc, (<+>))

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

-- =============================================================================
-- Tangle Interface
-- =============================================================================

-- | Generate a structured TangleDoc for this ViewState
-- | This is the preferred interface - controls are embedded in the document structure
describeDoc :: ViewState -> TangleDoc
describeDoc vs = case vs of
  Treemap scope ->
    Tangle.cycle "layout" "Treemap" ["Treemap", "Tree", "Force"]
    <+> Tangle.text " showing "
    <+> Tangle.cycle "scope" (scopeLabel scope) ["project", "all"]
    <+> Tangle.text " packages and modules"

  TreeLayout scope rootModule ->
    Tangle.cycle "layout" "Tree" ["Treemap", "Tree", "Force"]
    <+> Tangle.text " layout showing "
    <+> Tangle.cycle "scope" (scopeLabel scope) ["project", "all"]
    <+> Tangle.text " modules, rooted at "
    <+> Tangle.display "root" rootModule

  ForceLayout scope rootModule ->
    Tangle.cycle "layout" "Force" ["Treemap", "Tree", "Force"]
    <+> Tangle.text " layout showing "
    <+> Tangle.cycle "scope" (scopeLabel scope) ["project", "all"]
    <+> Tangle.text " modules, rooted at "
    <+> Tangle.display "root" rootModule

  Neighborhood modName ->
    -- "Neighborhood" is an action control - clicking it navigates back to overview
    Tangle.action "navigate" "Neighborhood" "overview"
    <+> Tangle.text " of "
    <+> Tangle.display "module" modName
    <+> Tangle.text " — imports and dependents"

  FunctionCalls modName ->
    -- "Function calls" navigates back to neighborhood
    Tangle.action "navigate" "Function calls" "neighborhood"
    <+> Tangle.text " in "
    <+> Tangle.display "module" modName

-- | Apply a control change to a ViewState
-- | Returns the new ViewState after applying the change
applyViewControl :: String -> String -> ViewState -> ViewState
applyViewControl controlId newValue vs = case controlId of
  "layout" -> applyLayoutChange newValue vs
  "scope" -> applyScopeChange newValue vs
  "navigate" -> applyNavigate newValue vs
  _ -> vs  -- Unknown control, no change

-- | Apply layout change
applyLayoutChange :: String -> ViewState -> ViewState
applyLayoutChange newLayout vs =
  let scope = extractScope vs
      root = extractRoot vs
  in case newLayout of
    "Treemap" -> Treemap scope
    "Tree" -> TreeLayout scope root
    "Force" -> ForceLayout scope root
    _ -> vs

-- | Apply scope change
applyScopeChange :: String -> ViewState -> ViewState
applyScopeChange newScope vs =
  let scope = parseScope newScope
  in case vs of
    Treemap _ -> Treemap scope
    TreeLayout _ root -> TreeLayout scope root
    ForceLayout _ root -> ForceLayout scope root
    other -> other

-- | Apply navigation action (one-way escape hatches)
applyNavigate :: String -> ViewState -> ViewState
applyNavigate target vs = case target of
  "overview" -> Treemap ProjectAndLibraries  -- Always go back to default treemap
  "neighborhood" -> case vs of
    FunctionCalls modName -> Neighborhood modName  -- Go back to neighborhood of same module
    _ -> vs
  _ -> vs

-- | Extract scope from ViewState
extractScope :: ViewState -> ScopeFilter
extractScope (Treemap s) = s
extractScope (TreeLayout s _) = s
extractScope (ForceLayout s _) = s
extractScope _ = ProjectAndLibraries

-- | Extract root module from ViewState (with default)
extractRoot :: ViewState -> String
extractRoot (TreeLayout _ root) = root
extractRoot (ForceLayout _ root) = root
extractRoot _ = "PSD3.Main"

-- | Parse scope string to ScopeFilter
parseScope :: String -> ScopeFilter
parseScope "project" = ProjectOnly
parseScope "all" = ProjectAndLibraries
parseScope "used" = UsedOnly
parseScope _ = ProjectAndLibraries

-- | User-friendly scope label for display
scopeLabel :: ScopeFilter -> String
scopeLabel UsedOnly = "used"
scopeLabel ProjectOnly = "project"
scopeLabel ProjectAndLibraries = "all"
