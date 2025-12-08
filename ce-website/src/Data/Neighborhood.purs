-- | Shared utilities for neighborhood views (Chord, Matrix, etc.)
-- |
-- | Provides common functions for building adjacency matrices and
-- | extracting neighborhood information from SimNode arrays.
module Data.Neighborhood
  ( idsToNames
  , setMatrixValue
  , getNeighborhoodInfo
  , NeighborhoodInfo
  , shortenModuleName
  , IdMapping
  , buildIdMapping
  ) where

import Prelude

import Data.Array ((!!))
import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))
import Types (SimNode)

-- | Type alias for ID-to-name mapping
type IdMapping = Map.Map Int String

-- | Information about a neighborhood centered on a module
type NeighborhoodInfo =
  { importIds :: Array Int      -- IDs of modules this module imports
  , dependentIds :: Array Int   -- IDs of modules that import this module
  , importNames :: Array String -- Names of import modules
  , dependentNames :: Array String -- Names of dependent modules
  }

-- | Build a mapping from node IDs to names
buildIdMapping :: Array SimNode -> IdMapping
buildIdMapping nodes = Map.fromFoldable $ map (\n -> Tuple n.id n.name) nodes

-- | Convert an array of IDs to names using a node list
idsToNames :: Array Int -> Array SimNode -> Array String
idsToNames ids nodes =
  let idToName = buildIdMapping nodes
  in Array.mapMaybe (\id -> Map.lookup id idToName) ids

-- | Extract neighborhood info for a central module
-- | Returns the import and dependent IDs and names
getNeighborhoodInfo :: String -> Array SimNode -> NeighborhoodInfo
getNeighborhoodInfo centralName nodes =
  case Array.find (\n -> n.name == centralName) nodes of
    Just cn ->
      { importIds: cn.targets
      , dependentIds: cn.sources
      , importNames: idsToNames cn.targets nodes
      , dependentNames: idsToNames cn.sources nodes
      }
    Nothing ->
      { importIds: []
      , dependentIds: []
      , importNames: []
      , dependentNames: []
      }

-- | Helper to set a value in a 2D matrix
-- | Safely updates matrix[row][col] = val
setMatrixValue :: Array (Array Number) -> Int -> Int -> Number -> Array (Array Number)
setMatrixValue m row col val =
  case m !! row of
    Just rowArr ->
      let newRow = fromMaybe rowArr $ Array.updateAt col val rowArr
      in fromMaybe m $ Array.updateAt row newRow m
    Nothing -> m

-- | Shorten module name for display by taking the last component
-- | "Data.Array.Extra" becomes "Extra"
shortenModuleName :: String -> String
shortenModuleName name =
  let parts = String.split (Pattern ".") name
  in fromMaybe name $ Array.last parts
