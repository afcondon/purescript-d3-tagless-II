-- | SPLOM Data Loading
-- |
-- | Load and parse the Palmer Penguins dataset.
module D3.Viz.SPLOM.Data
  ( loadPenguins
  ) where

import Prelude

import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web as AJAX
import Data.Array as Array
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.String (split, trim, Pattern(..))
import Effect.Aff (Aff)
import D3.Viz.SPLOM.Types (Penguin)

-- | Load and parse penguins from CSV
loadPenguins :: Aff (Either String (Array Penguin))
loadPenguins = do
  result <- AJAX.get ResponseFormat.string "./data/penguins.csv"
  pure $ case result of
    Left err -> Left (AJAX.printError err)
    Right response -> parseCSV response.body

-- | Parse CSV text into penguin records
parseCSV :: String -> Either String (Array Penguin)
parseCSV text =
  let lines = split (Pattern "\n") text
      -- Skip header, filter empty lines
      dataLines = Array.filter (\l -> trim l /= "") (Array.drop 1 lines)
      parsed = Array.mapMaybe parseLine dataLines
  in if Array.length parsed > 0
     then Right parsed
     else Left "No valid penguin records found"

-- | Parse a single CSV line into a Penguin
parseLine :: String -> Maybe Penguin
parseLine line =
  let fields = split (Pattern ",") line
  in case fields of
    [species, island, billLen, billDep, flipper, mass, sex, year] ->
      Just
        { species: trim species
        , island: trim island
        , billLength: parseNumber billLen
        , billDepth: parseNumber billDep
        , flipperLength: parseNumber flipper
        , bodyMass: parseNumber mass
        , sex: parseSex sex
        , year: parseYear year
        }
    _ -> Nothing

-- | Parse a numeric field (handles "NA" as Nothing)
parseNumber :: String -> Maybe Number
parseNumber s =
  let trimmed = trim s
  in if trimmed == "NA" || trimmed == ""
     then Nothing
     else Number.fromString trimmed

-- | Parse sex field
parseSex :: String -> Maybe String
parseSex s =
  let trimmed = trim s
  in if trimmed == "NA" || trimmed == ""
     then Nothing
     else Just trimmed

-- | Parse year field
parseYear :: String -> Int
parseYear s = case Int.fromString (trim s) of
  Just y -> y
  Nothing -> 2007  -- Default
