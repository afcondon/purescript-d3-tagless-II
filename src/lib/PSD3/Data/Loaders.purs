module PSD3.Data.Loaders where

-- | Data loading utilities for CSV, JSON, and other formats
-- | These wrap D3's data loading functions (d3.csv, d3.json, etc.)

import Prelude

import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Foreign (Foreign)

-- | Load a CSV file and return an array of objects
-- | Each row becomes an object with keys from the CSV header
-- | Uses D3's d3.csv() under the hood
loadCSV :: String -> Aff (Array Foreign)
loadCSV url = fromEffectFnAff $ loadCSVImpl url

-- | Load a JSON file
-- | Uses D3's d3.json() under the hood
loadJSON :: String -> Aff Foreign
loadJSON url = fromEffectFnAff $ loadJSONImpl url

foreign import loadCSVImpl :: String -> EffectFnAff (Array Foreign)
foreign import loadJSONImpl :: String -> EffectFnAff Foreign
