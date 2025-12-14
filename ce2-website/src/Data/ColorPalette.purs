-- | ColorPalette - Single source of truth for color palettes
-- |
-- | This module defines all color palettes used in visualizations and UI panels.
-- | It provides a unified interface for palette selection and color mapping.
-- |
-- | The module provides two main APIs:
-- | 1. Declaration type colors - for coloring by PureScript declaration kind
-- | 2. Node colors - view-aware coloring for simulation nodes (packages/modules)
module Data.ColorPalette
  ( -- * Palette Types
    PaletteType(..)
  , PaletteConfig
  , LegendItem
  , ColorMapping
  -- * Palette API
  , getPalette
  , getCategoryColor
  , getPaletteName
  , allPaletteTypes
  -- * Node Coloring (View-Aware)
  , getNodeStroke
  , getNodeFill
  , getClusterColor
  -- * Color Utilities
  , addOpacity
  ) where

import Prelude

import Data.Array as Array
import Data.Int (hexadecimal, fromStringAs)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import ViewState (ViewState(..), OverviewView(..))
import PSD3.Scale (schemeTableau10At)
import Types (SimNode, NodeType(..))

-- =============================================================================
-- Types
-- =============================================================================

-- | Available palette types for different data dimensions
data PaletteType
  = DeclarationTypes -- Color by PureScript declaration kind
  | GitActivity -- Color by git commit activity (future)
  | IssueInvolvement -- Color by issue involvement (future)
  | AuthorActivity -- Color by author/contributor (future)

derive instance eqPaletteType :: Eq PaletteType

-- | A single legend item
type LegendItem =
  { label :: String
  , category :: String
  , color :: String
  , description :: Maybe String
  }

-- | Configuration for a complete color palette
type PaletteConfig =
  { name :: String
  , description :: String
  , legendItems :: Array LegendItem
  , colorMapping :: ColorMapping
  }

-- | Function to map a category string to a color
type ColorMapping = String -> String

-- =============================================================================
-- Palette Definitions
-- =============================================================================

-- | Get all available palette types (for UI selection)
allPaletteTypes :: Array PaletteType
allPaletteTypes = [ DeclarationTypes ] -- Will add more as implemented

-- | Get the display name of a palette
getPaletteName :: PaletteType -> String
getPaletteName DeclarationTypes = "Declaration Types"
getPaletteName GitActivity = "Git Activity"
getPaletteName IssueInvolvement = "Issue Involvement"
getPaletteName AuthorActivity = "Author Activity"

-- | Get the palette configuration for a given type
getPalette :: PaletteType -> PaletteConfig
getPalette DeclarationTypes = declarationTypesPalette
getPalette GitActivity = gitActivityPalette
getPalette IssueInvolvement = issueInvolvementPalette
getPalette AuthorActivity = authorActivityPalette

-- | Get color for a category using the palette's color mapping
getCategoryColor :: PaletteType -> String -> String
getCategoryColor paletteType category =
  let
    config = getPalette paletteType
  in
    config.colorMapping category

-- =============================================================================
-- Declaration Types Palette (Tableau10-based)
-- =============================================================================

declarationTypesPalette :: PaletteConfig
declarationTypesPalette =
  { name: "Declaration Types"
  , description: "Colors represent different PureScript declaration types"
  , legendItems:
      [ { label: "Type Class"
        , category: "typeClass"
        , color: schemeTableau10At 0
        , description: Just "Type classes define interfaces"
        }
      , { label: "Data Type"
        , category: "data"
        , color: schemeTableau10At 1
        , description: Just "Data types and type constructors"
        }
      , { label: "Type Synonym"
        , category: "typeSynonym"
        , color: schemeTableau10At 2
        , description: Just "Type aliases"
        }
      , { label: "Extern Data"
        , category: "externData"
        , color: schemeTableau10At 3
        , description: Just "Foreign data types"
        }
      , { label: "Alias"
        , category: "alias"
        , color: schemeTableau10At 4
        , description: Just "Value aliases"
        }
      , { label: "Value"
        , category: "value"
        , color: schemeTableau10At 5
        , description: Just "Functions and values"
        }
      , { label: "Type Class Instance"
        , category: "typeClassInstance"
        , color: schemeTableau10At 6
        , description: Just "Instance implementations"
        }
      , { label: "Extern Value"
        , category: "externValue"
        , color: schemeTableau10At 7
        , description: Just "Foreign functions"
        }
      ]
  , colorMapping: declarationTypesColorMapping
  }

declarationTypesColorMapping :: ColorMapping
declarationTypesColorMapping category = case category of
  "typeClass" -> schemeTableau10At 0
  "data" -> schemeTableau10At 1
  "typeSynonym" -> schemeTableau10At 2
  "externData" -> schemeTableau10At 3
  "alias" -> schemeTableau10At 4
  "value" -> schemeTableau10At 5
  "typeClassInstance" -> schemeTableau10At 6
  "externValue" -> schemeTableau10At 7
  _ -> schemeTableau10At 8 -- fallback for unknown types

-- =============================================================================
-- Future Palettes (Placeholder Implementations)
-- =============================================================================

-- | Git Activity Palette (placeholder - to be implemented)
gitActivityPalette :: PaletteConfig
gitActivityPalette =
  { name: "Git Activity"
  , description: "Colors represent commit frequency and recency"
  , legendItems:
      [ { label: "Very Active", category: "veryActive", color: "#2ca02c", description: Nothing }
      , { label: "Active", category: "active", color: "#98df8a", description: Nothing }
      , { label: "Moderate", category: "moderate", color: "#ffbb78", description: Nothing }
      , { label: "Inactive", category: "inactive", color: "#ff7f0e", description: Nothing }
      , { label: "Stale", category: "stale", color: "#d62728", description: Nothing }
      ]
  , colorMapping: \category -> case category of
      "veryActive" -> "#2ca02c"
      "active" -> "#98df8a"
      "moderate" -> "#ffbb78"
      "inactive" -> "#ff7f0e"
      "stale" -> "#d62728"
      _ -> "#cccccc"
  }

-- | Issue Involvement Palette (placeholder - to be implemented)
issueInvolvementPalette :: PaletteConfig
issueInvolvementPalette =
  { name: "Issue Involvement"
  , description: "Colors represent involvement in issue discussions"
  , legendItems:
      [ { label: "High", category: "high", color: "#d62728", description: Nothing }
      , { label: "Medium", category: "medium", color: "#ff7f0e", description: Nothing }
      , { label: "Low", category: "low", color: "#1f77b4", description: Nothing }
      , { label: "None", category: "none", color: "#cccccc", description: Nothing }
      ]
  , colorMapping: \category -> case category of
      "high" -> "#d62728"
      "medium" -> "#ff7f0e"
      "low" -> "#1f77b4"
      "none" -> "#cccccc"
      _ -> "#cccccc"
  }

-- | Author Activity Palette (placeholder - to be implemented)
authorActivityPalette :: PaletteConfig
authorActivityPalette =
  { name: "Author Activity"
  , description: "Colors represent different authors/contributors"
  , legendItems:
      [ { label: "Author 1", category: "author1", color: schemeTableau10At 0, description: Nothing }
      , { label: "Author 2", category: "author2", color: schemeTableau10At 1, description: Nothing }
      , { label: "Author 3", category: "author3", color: schemeTableau10At 2, description: Nothing }
      , { label: "Author 4", category: "author4", color: schemeTableau10At 3, description: Nothing }
      , { label: "Others", category: "others", color: "#cccccc", description: Nothing }
      ]
  , colorMapping: \category -> case category of
      "author1" -> schemeTableau10At 0
      "author2" -> schemeTableau10At 1
      "author3" -> schemeTableau10At 2
      "author4" -> schemeTableau10At 3
      "others" -> "#cccccc"
      _ -> "#cccccc"
  }

-- =============================================================================
-- View-Aware Node Coloring
-- =============================================================================
-- |
-- | These functions provide view-aware coloring for simulation nodes.
-- | With the new four-view architecture, nodes not in a view are removed from DOM,
-- | so coloring is simpler - we just need to style the nodes that ARE present.
-- |
-- | **Treemap view:**
-- | - Packages: solid cluster color
-- | - Modules: white fill, white stroke
-- |
-- | **Tree/Force views:**
-- | - Modules only: cluster-colored stroke, semi-transparent fill
-- |
-- | **Topo view:**
-- | - Packages only: solid cluster color

-- | Get the base color for a cluster index.
-- | Uses Tableau10 color scheme.
getClusterColor :: Int -> String
getClusterColor = schemeTableau10At

-- | Get stroke color for a node in a specific view.
getNodeStroke :: ViewState -> SimNode -> String
getNodeStroke viewState n = case n.nodeType of
  PackageNode -> schemeTableau10At n.cluster
  ModuleNode -> case viewState of
    Overview TreemapView -> "rgba(255, 255, 255, 0.9)"
    _ -> schemeTableau10At n.cluster

-- | Get fill color for a node in a specific view.
getNodeFill :: ViewState -> SimNode -> String
getNodeFill viewState n = case n.nodeType of
  PackageNode -> schemeTableau10At n.cluster -- Always solid for packages
  ModuleNode -> case viewState of
    Overview TreemapView ->
      if Array.null n.sources then "none"
      else "rgba(255, 255, 255, 0.9)"
    _ -> addOpacity (schemeTableau10At n.cluster) 0.5 -- Semi-transparent for tree/force

-- =============================================================================
-- Color Utilities
-- =============================================================================

-- | Add opacity to a hex color string.
-- | Converts #RRGGBB to rgba(r,g,b,a).
addOpacity :: String -> Number -> String
addOpacity hexColor opacity =
  case String.stripPrefix (String.Pattern "#") hexColor of
    Just hex ->
      let
        r = fromMaybe 0 $ hexToInt $ String.take 2 hex
        g = fromMaybe 0 $ hexToInt $ String.take 2 $ String.drop 2 hex
        b = fromMaybe 0 $ hexToInt $ String.take 2 $ String.drop 4 hex
      in
        "rgba(" <> show r <> "," <> show g <> "," <> show b <> "," <> show opacity <> ")"
    Nothing -> hexColor

-- | Convert hex string to integer
hexToInt :: String -> Maybe Int
hexToInt hex = fromStringAs hexadecimal hex
