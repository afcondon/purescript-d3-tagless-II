module D3.Viz.ModuleGraph.LoadModuleGraph where

import Prelude

import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web as AJAX
import D3.Viz.ModuleGraph.Model (ModuleGraph, ModuleSimNode, SpagoModuleGraph, moduleGroup)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Foreign.Object as Object
import PSD3.Data.Node (D3Link_Unswizzled)
import Unsafe.Coerce (unsafeCoerce)

-- | Load and process the spago module graph, filtering to only local modules
loadModuleGraph :: Aff (Either String ModuleGraph)
loadModuleGraph = do
  result <- AJAX.get ResponseFormat.json "./data/spago-modules.json"
  case result of
    Left err -> pure $ Left $ "Failed to load module graph: " <> AJAX.printError err
    Right response -> do
      let raw :: SpagoModuleGraph
          raw = unsafeCoerce response.body
      pure $ Right $ processModuleGraph raw

-- | Filter to only local modules (our source code)
isLocalModule :: String -> Boolean
isLocalModule name =
  startsWith "PSD3" name
  || startsWith "Component." name
  || startsWith "D3.Viz" name
  || startsWith "Utility" name

startsWith :: String -> String -> Boolean
startsWith prefix str =
  case String.indexOf (Pattern prefix) str of
    Just 0 -> true
    _ -> false

-- | Process raw spago data into nodes and links
processModuleGraph :: SpagoModuleGraph -> ModuleGraph
processModuleGraph raw =
  let
    -- Get all local module names
    allModules = Object.keys raw
    localModules = Array.filter isLocalModule allModules
    localModulesSet = Set.fromFoldable localModules

    -- Create nodes with indices
    nodes = Array.mapWithIndex makeNode localModules

    -- Create links (only between local modules)
    links = Array.concat $ Array.mapWithIndex (makeLinks raw localModulesSet) localModules
  in
    { nodes, links }

-- | Create a simulation node from a module name
makeNode :: Int -> String -> ModuleSimNode
makeNode index name =
  { id: name
  , name: name
  , group: moduleGroup name
  , index: index
  , x: 0.0
  , y: 0.0
  , vx: 0.0
  , vy: 0.0
  , fx: toNullable Nothing
  , fy: toNullable Nothing
  }

-- | Create links from a module to its local dependencies
makeLinks :: SpagoModuleGraph -> Set.Set String -> Int -> String -> Array D3Link_Unswizzled
makeLinks raw localModulesSet sourceIndex sourceName =
  case Object.lookup sourceName raw of
    Nothing -> []
    Just node ->
      let
        -- Filter dependencies to only local modules
        localDeps = Array.filter (\dep -> Set.member dep localModulesSet) node.depends
        -- Create link records - will be coerced to D3Link_Unswizzled
        makeLink target = unsafeCoerce { source: sourceName, target: target, value: 1.0 }
      in
        map makeLink localDeps
