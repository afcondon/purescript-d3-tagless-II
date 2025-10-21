module PSD3.Spago.Data where

import Prelude

import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web as AJAX
import D3.Viz.Spago.Model (SpagoModel, convertFilesToGraphModel)
import D3.Viz.Spago.Tree (treeReduction)
import Data.Either (hush)
import Data.Map as M
import Data.Maybe (Maybe)
import Effect.Aff (Aff)

readModelData :: Aff (Maybe SpagoModel)
readModelData = do
  let datadir = "./data/spago-data/"  -- Relative to v2/ serving directory
  moduleJSON  <- AJAX.get ResponseFormat.string $ datadir <> "modules.json"
  packageJSON <- AJAX.get ResponseFormat.string $ datadir <> "packages.json"
  lsdepJSON   <- AJAX.get ResponseFormat.string $ datadir <> "lsdeps.jsonlines"
  locJSON     <- AJAX.get ResponseFormat.string $ datadir <> "LOC.json"
  let model = hush $ convertFilesToGraphModel <$> moduleJSON <*> packageJSON <*> lsdepJSON <*> locJSON

  pure (addTreeToModel "Main" model)

addTreeToModel :: String -> Maybe SpagoModel -> Maybe SpagoModel
addTreeToModel rootName maybeModel = do
  model  <- maybeModel
  rootID <- M.lookup rootName model.maps.name2ID
  pure $ treeReduction rootID model
