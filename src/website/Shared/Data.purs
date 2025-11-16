module PSD3.Shared.Data where

import Prelude

import Affjax.Web as AJAX
import Affjax.ResponseFormat as ResponseFormat
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Effect.Class.Console (log)

-- | Available data files with type-safe identifiers
data DataFile
  = EnergyCSV                    -- Sankey diagram data
  | FlareJSON                    -- Hierarchical data for tree/pack/partition layouts
  | MiserablesJSON               -- Character network graph
  | NationsJSON                  -- Wealth & Health of Nations
  | MetroUnemploymentCSV         -- BLS metro unemployment data
  | USPopulationStateAgeCSV      -- US population by state and age (for grouped bar chart)

derive instance Eq DataFile

-- | Get the file path for a data file
dataFilePath :: DataFile -> String
dataFilePath EnergyCSV = "./data/energy.csv"
dataFilePath FlareJSON = "./data/flare-2.json"
dataFilePath MiserablesJSON = "./data/miserables.json"
dataFilePath NationsJSON = "./data/nations.json"
dataFilePath MetroUnemploymentCSV = "./data/bls-metro-unemployment.csv"
dataFilePath USPopulationStateAgeCSV = "./data/us-population-state-age.csv"

-- | Human-readable description
dataFileDescription :: DataFile -> String
dataFileDescription EnergyCSV = "Energy flow data for Sankey diagrams"
dataFileDescription FlareJSON = "Flare visualization toolkit hierarchy (252 nodes)"
dataFileDescription MiserablesJSON = "Les Misérables character co-occurrence network (77 nodes, 254 links)"
dataFileDescription NationsJSON = "Wealth & Health of Nations time series data"
dataFileDescription MetroUnemploymentCSV = "BLS metro area unemployment rates"
dataFileDescription USPopulationStateAgeCSV = "US population by state and age group"

-- | Load a data file as a string
-- | Returns Either error string or file contents
loadDataFile :: DataFile -> Aff (Either String String)
loadDataFile dataFile = do
  let path = dataFilePath dataFile
  log $ "Loading data file: " <> path
  response <- AJAX.get ResponseFormat.string path
  case response of
    Left err -> do
      log $ "Failed to load " <> path <> ": " <> AJAX.printError err
      pure $ Left $ "Failed to load data file: " <> AJAX.printError err
    Right { body } -> do
      log $ "Successfully loaded " <> path
      pure $ Right body
