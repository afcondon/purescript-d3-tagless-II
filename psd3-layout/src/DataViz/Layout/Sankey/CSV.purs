-- | DataViz.Layout.Sankey.CSV
-- |
-- | CSV parsing utilities for Sankey diagram data.
-- | Parses CSV files with source,target,value format.
module DataViz.Layout.Sankey.CSV
  ( parseSankeyCSV
  , parseLine
  ) where

import Prelude

import Data.Array (drop, filter, mapMaybe)
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.String (Pattern(..), split, trim)
import Data.String.Common (null) as String
import DataViz.Layout.Sankey.Types (LinkCSVRow)

-- | Parse CSV string into array of LinkCSVRow
-- | Expected format: "source,target,value" with header row
-- | Example:
-- |   source,target,value
-- |   A,B,10
-- |   B,C,5
parseSankeyCSV :: String -> Array LinkCSVRow
parseSankeyCSV csvString =
  let
    lines = split (Pattern "\n") csvString
    dataLines = filter (not <<< isEmpty) $ drop 1 lines -- Skip header
  in
    mapMaybe parseLine dataLines
  where
  isEmpty :: String -> Boolean
  isEmpty s = String.null (trim s)

-- | Parse a single CSV line into LinkCSVRow
parseLine :: String -> Maybe LinkCSVRow
parseLine line =
  let
    parts = map trim $ split (Pattern ",") line
  in
    case parts of
      [ s, t, valueStr ] ->
        case Number.fromString valueStr of
          Just v ->
            Just { s, t, v }
          Nothing -> Nothing
      _ -> Nothing
