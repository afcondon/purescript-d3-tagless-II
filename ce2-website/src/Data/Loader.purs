-- | Data loading and transformation
-- |
-- | Loads JSON data from ce-server API and transforms into SimNode/SimLink arrays with
-- | pre-calculated positions for Grid, Orbit, and Tree scenes.
-- |
-- | Supports multi-project/snapshot selection via:
-- | - fetchProjects: get available projects with their snapshots
-- | - loadModelForSnapshot: load model for a specific snapshot ID
-- | - loadModel: (legacy) loads latest snapshot for backward compatibility
module CE2.Data.Loader
  ( loadModel
  , loadModelForSnapshot
  , loadModelForProject
  , fetchProjects
  , fetchProjectWithSnapshots
  , fetchFunctionCalls
  -- Granular on-demand fetchers
  , fetchModuleDeclarations
  , fetchModuleDeclarationsWithSource
  , fetchModuleFunctionCalls
  , fetchModuleMetrics
  , fetchCallGraphData
  , ModuleDeclarationsResponse
  , ModuleFunctionCallsResponse
  -- Batch fetchers (single request for multiple modules)
  , fetchBatchDeclarations
  , fetchBatchFunctionCalls
  -- Types
  , LoadedModel
  , Project
  , Snapshot
  , apiBaseUrl
  , Declaration
  , DeclarationWithSource
  , DeclarationsMap
  , FunctionCallsMap
  , FunctionInfo
  , CallInfo
  , CallGraphData
  , GitMetrics
  ) where

import Prelude

import Affjax.Web as AW
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, printJsonDecodeError)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (pi, cos, sin, sqrt)
import Data.Nullable (null)
import Data.String.CodeUnits as SCU
import Data.String.Common (joinWith)
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Foreign.Object (Object)
import Foreign.Object as Object
import Data.Graph.Algorithms as TopoAlgorithms
import Data.Foldable (maximum) as Foldable
import CE2.Types (SimNode, SimLink, NodeType(..), LinkType(..), Package)

-- | API base URL for ce-server
-- | TODO: Make this configurable via environment or runtime config
apiBaseUrl :: String
apiBaseUrl = "http://localhost:8080"

-- =============================================================================
-- Types
-- =============================================================================

-- | Raw module from JSON
type RawModule =
  { depends :: Array String
  , package :: String
  , path :: String
  }

-- | Raw package from JSON
type RawPackage =
  { depends :: Array String
  }

-- | LOC entry from JSON
type LocEntry =
  { loc :: Int
  , path :: String
  }

-- | LOC file structure
type LocFile =
  { loc :: Array LocEntry
  }

-- | Declaration summary from declarations-summary.json
type Declaration =
  { kind :: String -- "typeClass", "data", "typeSynonym", "externData", "alias", "value"
  , title :: String
  }

-- | Declaration with source code from module-declarations endpoint
type DeclarationWithSource =
  { kind :: String
  , title :: String
  , sourceCode :: Maybe String  -- Only present in per-module API response
  }

-- | Module declarations map: module name -> array of declarations
type DeclarationsMap = Object (Array Declaration)

-- | Call information from function-calls.json
type CallInfo =
  { target :: String
  , targetModule :: String
  , identifier :: String
  , isCrossModule :: Boolean
  }

-- | Function info from function-calls.json
type FunctionInfo =
  { module :: String
  , name :: String
  , calls :: Array CallInfo
  , calledBy :: Array String -- "Module.func" format
  }

-- | Function calls map: "Module.name" -> FunctionInfo
type FunctionCallsMap = Object FunctionInfo

-- | Response wrapper for function-calls.json
type FunctionCallsResponse =
  { functions :: FunctionCallsMap
  }

-- | Snapshot info from API
type Snapshot =
  { id :: Int
  , gitHash :: String
  , gitRef :: String
  , label :: String
  , snapshotAt :: String
  , moduleCount :: Int
  , packageCount :: Int
  , declarationCount :: Int
  }

-- | Project info from API
type Project =
  { id :: Int
  , name :: String
  , repoPath :: String
  , description :: Maybe String
  , snapshotCount :: Int
  , latestSnapshotAt :: Maybe String
  , snapshots :: Array Snapshot  -- Populated when fetching single project
  }

-- | API response for projects list
type ProjectsListResponse =
  { projects :: Array ProjectListItem
  }

-- | Project list item (without snapshots)
type ProjectListItem =
  { id :: Int
  , name :: String
  , repoPath :: String
  , description :: Maybe String
  , snapshotCount :: Int
  , latestSnapshotAt :: Maybe String
  }

-- | API response for single project with snapshots
-- | Note: Single project response has fewer fields than list response
type ProjectWithSnapshotsResponse =
  { project :: ProjectDetail
  , snapshots :: Array Snapshot
  }

-- | Project detail (returned by GET /api/projects/:id)
type ProjectDetail =
  { id :: Int
  , name :: String
  , repoPath :: String
  , description :: Maybe String
  , createdAt :: String
  }

-- | Loaded and transformed model
type LoadedModel =
  { nodes :: Array SimNode
  , links :: Array SimLink
  , packages :: Array Package
  , declarations :: DeclarationsMap -- Module declarations for bubble packs
  , moduleCount :: Int
  , packageCount :: Int
  }

-- =============================================================================
-- Loading
-- =============================================================================

-- | Load all data from ce-server API and transform to model
loadModel :: Aff (Either String LoadedModel)
loadModel = do
  modulesResult <- fetchJson (apiBaseUrl <> "/data/spago-data/modules.json")
  packagesResult <- fetchJson (apiBaseUrl <> "/data/spago-data/packages.json")
  locResult <- fetchJson (apiBaseUrl <> "/data/spago-data/LOC.json")
  declarationsResult <- fetchJson (apiBaseUrl <> "/data/spago-data/declarations-summary.json")

  pure $ do
    modulesJson <- modulesResult
    packagesJson <- packagesResult
    locJson <- locResult
    declarationsJson <- declarationsResult

    -- Decode JSON
    modules :: Object RawModule <- decodeJson modulesJson # mapLeft printJsonDecodeError
    packages :: Object RawPackage <- decodeJson packagesJson # mapLeft printJsonDecodeError
    locFile :: LocFile <- decodeJson locJson # mapLeft printJsonDecodeError
    declarations :: DeclarationsMap <- decodeJson declarationsJson # mapLeft printJsonDecodeError

    -- Build LOC map (path -> loc)
    let locMap = buildLocMap locFile.loc

    -- Transform to model
    Right $ transformToModel modules packages locMap declarations

fetchJson :: String -> Aff (Either String Json)
fetchJson url = do
  result <- AW.get ResponseFormat.json url
  pure $ case result of
    Left err -> Left $ "Fetch error: " <> AW.printError err
    Right response -> Right response.body

mapLeft :: forall a b c. (a -> c) -> Either a b -> Either c b
mapLeft f (Left a) = Left (f a)
mapLeft _ (Right b) = Right b

-- =============================================================================
-- Multi-Project API
-- =============================================================================

-- | Fetch list of all projects with their snapshot counts
fetchProjects :: Aff (Either String (Array Project))
fetchProjects = do
  result <- fetchJson (apiBaseUrl <> "/api/projects")
  pure $ do
    json <- result
    response :: ProjectsListResponse <- decodeJson json # mapLeft printJsonDecodeError
    -- Convert ProjectListItem to Project (with empty snapshots array)
    Right $ map toProject response.projects
  where
  toProject :: ProjectListItem -> Project
  toProject p =
    { id: p.id
    , name: p.name
    , repoPath: p.repoPath
    , description: p.description
    , snapshotCount: p.snapshotCount
    , latestSnapshotAt: p.latestSnapshotAt
    , snapshots: []
    }

-- | Fetch a project with its snapshots
fetchProjectWithSnapshots :: Int -> Aff (Either String Project)
fetchProjectWithSnapshots projectId = do
  result <- fetchJson (apiBaseUrl <> "/api/projects/" <> show projectId)
  pure $ do
    json <- result
    response :: ProjectWithSnapshotsResponse <- decodeJson json # mapLeft printJsonDecodeError
    -- Compute snapshotCount and latestSnapshotAt from snapshots array
    let latestAt = Array.head response.snapshots <#> _.snapshotAt
    Right
      { id: response.project.id
      , name: response.project.name
      , repoPath: response.project.repoPath
      , description: response.project.description
      , snapshotCount: Array.length response.snapshots
      , latestSnapshotAt: latestAt
      , snapshots: response.snapshots
      }

-- | Fetch function calls data
fetchFunctionCalls :: Aff (Either String FunctionCallsMap)
fetchFunctionCalls = do
  result <- fetchJson (apiBaseUrl <> "/data/function-calls.json")
  pure $ do
    json <- result
    response :: FunctionCallsResponse <- decodeJson json # mapLeft printJsonDecodeError
    Right response.functions

-- =============================================================================
-- Granular On-Demand Fetchers
-- =============================================================================

-- | Response type for module declarations endpoint (summary version)
type ModuleDeclarationsResponse =
  { declarations :: Array Declaration
  }

-- | Response type for module declarations endpoint (with source code)
type ModuleDeclarationsWithSourceResponse =
  { declarations :: Array DeclarationWithSource
  }

-- | Response type for module function-calls endpoint
type ModuleFunctionCallsResponse =
  { module :: String
  , functions :: Object FunctionInfo
  }

-- | Fetch declarations for a specific module (without source code)
fetchModuleDeclarations :: String -> Aff (Either String (Array Declaration))
fetchModuleDeclarations moduleName = do
  result <- fetchJson (apiBaseUrl <> "/api/module-declarations/" <> moduleName)
  pure $ do
    json <- result
    response :: ModuleDeclarationsResponse <- decodeJson json # mapLeft printJsonDecodeError
    Right response.declarations

-- | Fetch declarations for a specific module (with source code)
fetchModuleDeclarationsWithSource :: String -> Aff (Either String (Array DeclarationWithSource))
fetchModuleDeclarationsWithSource moduleName = do
  result <- fetchJson (apiBaseUrl <> "/api/module-declarations/" <> moduleName)
  pure $ do
    json <- result
    response :: ModuleDeclarationsWithSourceResponse <- decodeJson json # mapLeft printJsonDecodeError
    Right response.declarations

-- | Fetch function calls for a specific module
fetchModuleFunctionCalls :: String -> Aff (Either String (Object FunctionInfo))
fetchModuleFunctionCalls moduleName = do
  result <- fetchJson (apiBaseUrl <> "/api/module-function-calls/" <> moduleName)
  pure $ do
    json <- result
    response :: ModuleFunctionCallsResponse <- decodeJson json # mapLeft printJsonDecodeError
    Right response.functions

-- =============================================================================
-- Call Graph Data (for popup)
-- =============================================================================

-- | Git metrics for a module
type GitMetrics =
  { commitCount :: Int
  , daysSinceModified :: Int
  , authorCount :: Int
  , authors :: Array String
  }

-- | Combined data for call graph popup
type CallGraphData =
  { moduleName :: String
  , declarationName :: String
  , callers :: Array CallInfo  -- Functions that call this one
  , callees :: Array CallInfo  -- Functions this one calls
  , sourceCode :: Maybe String
  , declarationKind :: Maybe String
  , gitMetrics :: Maybe GitMetrics
  }

-- | Fetch all data needed for call graph popup
-- | Combines module-function-calls, module-declarations, and module-metrics
fetchCallGraphData :: String -> String -> Aff (Either String CallGraphData)
fetchCallGraphData moduleName declarationName = do
  -- Fetch all three endpoints in parallel
  functionCallsResult <- fetchModuleFunctionCalls moduleName
  declarationsResult <- fetchModuleDeclarationsWithSource moduleName
  metricsResult <- fetchModuleMetrics moduleName

  pure $ do
    functionCalls <- functionCallsResult
    declarations <- declarationsResult
    -- Metrics are optional, don't fail if unavailable
    let metrics = case metricsResult of
          Right m -> Just m
          Left _ -> Nothing

    -- Find the specific function in the function calls map
    let functionInfo = Object.lookup declarationName functionCalls

    -- Find the declaration info
    let declaration = Array.find (\d -> d.title == declarationName) declarations

    -- Build callers from calledBy (convert "Module.func" strings to CallInfo)
    let callers = case functionInfo of
          Just fi -> map parseCallerString fi.calledBy
          Nothing -> []

    -- Build callees from calls
    let callees = case functionInfo of
          Just fi -> fi.calls
          Nothing -> []

    -- Get source code and kind from declaration
    let sourceCode = declaration >>= _.sourceCode
    let declarationKind = declaration <#> _.kind

    Right
      { moduleName
      , declarationName
      , callers
      , callees
      , sourceCode
      , declarationKind
      , gitMetrics: metrics
      }
  where
  -- Parse "Module.funcName" string into CallInfo
  parseCallerString :: String -> CallInfo
  parseCallerString str =
    case lastIndexOf "." str of
      Just idx ->
        { target: drop (idx + 1) str
        , targetModule: take idx str
        , identifier: str
        , isCrossModule: true
        }
      Nothing ->
        { target: str
        , targetModule: moduleName -- Same module if no dot
        , identifier: str
        , isCrossModule: false
        }

  lastIndexOf :: String -> String -> Maybe Int
  lastIndexOf needle haystack =
    let chars = SCU.toCharArray haystack
        needleChar = case SCU.toCharArray needle of
          [c] -> Just c
          _ -> Nothing
    in case needleChar of
      Just c -> Array.findLastIndex (\ch -> ch == c) chars
      Nothing -> Nothing

  take :: Int -> String -> String
  take n s = SCU.take n s

  drop :: Int -> String -> String
  drop n s = SCU.drop n s

-- | Fetch git metrics for a module (optional, may fail)
fetchModuleMetrics :: String -> Aff (Either String GitMetrics)
fetchModuleMetrics moduleName = do
  result <- fetchJson (apiBaseUrl <> "/api/module-metrics/" <> moduleName)
  pure $ do
    json <- result
    -- The API returns flat fields, we need to map them
    raw :: { commit_count :: Maybe Int, days_since_modified :: Maybe Int, author_count :: Maybe Int, authors :: Maybe (Array String) }
      <- decodeJson json # mapLeft printJsonDecodeError
    Right
      { commitCount: fromMaybe 0 raw.commit_count
      , daysSinceModified: fromMaybe 0 raw.days_since_modified
      , authorCount: fromMaybe 0 raw.author_count
      , authors: fromMaybe [] raw.authors
      }

-- =============================================================================
-- Batch Fetchers (single request for multiple modules)
-- =============================================================================

-- | Fetch declarations for multiple modules in a single request
-- | Returns DeclarationsMap: { "ModuleName": [{ kind, title }] }
fetchBatchDeclarations :: Array String -> Aff (Either String DeclarationsMap)
fetchBatchDeclarations moduleNames = do
  let modulesParam = joinWith "," moduleNames
  result <- fetchJson (apiBaseUrl <> "/api/batch-declarations/" <> modulesParam)
  pure $ do
    json <- result
    declarations :: DeclarationsMap <- decodeJson json # mapLeft printJsonDecodeError
    Right declarations

-- | Fetch function calls for multiple modules in a single request
-- | Returns FunctionCallsMap: { "Module.func": { module, name, calls, calledBy } }
fetchBatchFunctionCalls :: Array String -> Aff (Either String FunctionCallsMap)
fetchBatchFunctionCalls moduleNames = do
  let modulesParam = joinWith "," moduleNames
  result <- fetchJson (apiBaseUrl <> "/api/batch-function-calls/" <> modulesParam)
  pure $ do
    json <- result
    response :: FunctionCallsResponse <- decodeJson json # mapLeft printJsonDecodeError
    Right response.functions

-- | Load model for a specific snapshot ID
-- | This fetches data scoped to that snapshot from the legacy endpoints
loadModelForSnapshot :: Int -> Aff (Either String LoadedModel)
loadModelForSnapshot _snapshotId = do
  -- For now, we use the legacy endpoints which always return latest snapshot
  -- TODO: Add snapshot-specific endpoints like /api/snapshots/:id/modules
  -- For now, just use the same loadModel (legacy endpoints use latest snapshot)
  -- This will be enhanced when we add snapshot-specific data endpoints
  loadModel

-- | Load model for a specific project ID
-- | Uses project-specific endpoints to load data for the selected project
loadModelForProject :: Int -> Aff (Either String LoadedModel)
loadModelForProject projectId = do
  let projectUrl = apiBaseUrl <> "/api/project-"
  modulesResult <- fetchJson (projectUrl <> "modules/" <> show projectId)
  packagesResult <- fetchJson (projectUrl <> "packages/" <> show projectId)
  locResult <- fetchJson (projectUrl <> "loc/" <> show projectId)
  declarationsResult <- fetchJson (projectUrl <> "declarations-summary/" <> show projectId)

  pure $ do
    modulesJson <- modulesResult
    packagesJson <- packagesResult
    locJson <- locResult
    declarationsJson <- declarationsResult

    -- Decode JSON
    modules :: Object RawModule <- decodeJson modulesJson # mapLeft printJsonDecodeError
    packages :: Object RawPackage <- decodeJson packagesJson # mapLeft printJsonDecodeError
    locFile :: LocFile <- decodeJson locJson # mapLeft printJsonDecodeError
    declarations :: DeclarationsMap <- decodeJson declarationsJson # mapLeft printJsonDecodeError

    -- Build LOC map (path -> loc)
    let locMap = buildLocMap locFile.loc

    -- Transform to model
    Right $ transformToModel modules packages locMap declarations

-- =============================================================================
-- Transformation
-- =============================================================================

transformToModel :: Object RawModule -> Object RawPackage -> Map String Int -> DeclarationsMap -> LoadedModel
transformToModel modulesObj packagesObj locMap declarations =
  let
    -- Get arrays
    moduleNames = Object.keys modulesObj
    packageNames = Object.keys packagesObj

    -- Build package -> modules map
    packageModules = buildPackageModulesMap modulesObj

    -- Build package -> total LOC map (sum of module LOC for each package)
    packageLocMap = buildPackageLocMap modulesObj locMap

    -- Assign IDs: packages first, then modules
    packageCount = Array.length packageNames
    moduleCount = Array.length moduleNames

    -- =========================================================================
    -- Compute topological layers for packages
    -- =========================================================================

    -- Derive package dependencies from module dependencies
    -- For each module, look at its dependencies and find which packages they belong to
    -- Package A depends on Package B if any module in A depends on any module in B
    derivedPackageDeps :: Map String (Set String)
    derivedPackageDeps = foldl addModuleDeps Map.empty (Object.toUnfoldable modulesObj :: Array (Tuple String RawModule))
      where
        addModuleDeps :: Map String (Set String) -> Tuple String RawModule -> Map String (Set String)
        addModuleDeps acc (Tuple moduleName rawMod) =
          let
            thisPackage = rawMod.package
            -- Find packages of all dependencies
            depPackages = Set.fromFoldable $ Array.mapMaybe getPackageOfModule rawMod.depends
            -- Remove self-dependency
            externalDeps = Set.delete thisPackage depPackages
            -- Add to existing deps for this package
            existing = fromMaybe Set.empty (Map.lookup thisPackage acc)
          in
            Map.insert thisPackage (Set.union existing externalDeps) acc

        getPackageOfModule :: String -> Maybe String
        getPackageOfModule modName = Object.lookup modName modulesObj <#> _.package

    -- Convert packages to TaskNodes for topo sort
    packageTaskNodes :: Array (TopoAlgorithms.TaskNode String)
    packageTaskNodes = packageNames <#> \name ->
      { id: name
      , depends: Array.fromFoldable $ fromMaybe Set.empty (Map.lookup name derivedPackageDeps)
      }

    -- Get layered packages (each has id, layer, depends)
    layeredPackages = TopoAlgorithms.addLayers packageTaskNodes

    -- Build a map from package name to layer
    packageLayerMap :: Map String Int
    packageLayerMap = Map.fromFoldable $ layeredPackages <#> \lp -> Tuple lp.id lp.layer

    -- Find max layer for positioning
    maxLayer = fromMaybe 0 $ Foldable.maximum (layeredPackages <#> _.layer)

    -- Count packages per layer for x-positioning within layer
    packagesByLayer :: Map Int (Array String)
    packagesByLayer = foldl addToLayer Map.empty layeredPackages
      where
        addToLayer acc lp =
          let existing = fromMaybe [] $ Map.lookup lp.layer acc
          in Map.insert lp.layer (Array.snoc existing lp.id) acc

    -- Create name -> ID maps (needed for package dependency lookup)
    packageIdMap = Map.fromFoldable $ Array.mapWithIndex (\i n -> Tuple n i) packageNames

    -- Create package nodes (IDs 0 to packageCount-1)
    -- Pass derivedPackageDeps and packageIdMap to populate targets/sources
    packageNodes = Array.mapWithIndex (mkPackageNode packageNames packageCount packageLocMap packageLayerMap packagesByLayer maxLayer derivedPackageDeps packageIdMap) packageNames
    moduleIdMap = Map.fromFoldable $ Array.mapWithIndex (\i n -> Tuple n (i + packageCount)) moduleNames

    -- Build targets map (module ID -> array of dependency IDs)
    targetsMap = buildTargetsMap modulesObj moduleIdMap

    -- Build sources map (module ID -> array of dependents' IDs)
    sourcesMap = buildSourcesMap targetsMap

    -- Create module nodes (IDs packageCount to packageCount+moduleCount-1)
    moduleNodes = Array.mapWithIndex
      (\i name -> mkModuleNode name i modulesObj locMap packageIdMap moduleIdMap packageCount moduleCount packageNodes targetsMap sourcesMap)
      moduleNames

    -- All nodes - tree positions will be computed on demand by Viz layer
    nodes = packageNodes <> moduleNodes

    -- All module links - tree/graph classification done by Viz layer
    links = buildLinks modulesObj moduleIdMap

    -- Create Package records for model
    packages = Array.mapWithIndex
      ( \_ name ->
          { name
          , depends: fromMaybe [] $ Object.lookup name packagesObj <#> _.depends
          , modules: fromMaybe [] $ Map.lookup name packageModules
          }
      )
      packageNames
  in
    { nodes, links, packages, declarations, moduleCount, packageCount }

-- =============================================================================
-- Node Creation
-- =============================================================================

mkPackageNode :: Array String -> Int -> Map String Int -> Map String Int -> Map Int (Array String) -> Int -> Map String (Set String) -> Map String Int -> Int -> String -> SimNode
mkPackageNode _allPackages totalPackages packageLocMap packageLayerMap packagesByLayer maxLayer derivedPackageDeps packageIdMap idx name =
  let
    -- Grid position (arrange in rows of ~8)
    gridCols = 8
    gridSpacing = 120.0
    gridRow = toNumber (idx / gridCols)
    gridCol = toNumber (idx `mod` gridCols)
    gx = (gridCol - toNumber gridCols / 2.0 + 0.5) * gridSpacing
    gy = (gridRow - toNumber (totalPackages / gridCols) / 2.0) * gridSpacing

    -- Orbit angle (distribute evenly around circle)
    angle = 2.0 * pi * toNumber idx / toNumber totalPackages

    -- Package radius based on total LOC (sqrt scale, with minimum)
    totalLoc = fromMaybe 100 (Map.lookup name packageLocMap)
    r = max 8.0 (sqrt (toNumber totalLoc) * 0.5)

    -- Topological position (DAG layout)
    -- y = layer (top to bottom: layer 0 at top, max layer at bottom)
    -- x = position within layer (centered)
    layer = fromMaybe 0 (Map.lookup name packageLayerMap)
    packagesInLayer = fromMaybe [] (Map.lookup layer packagesByLayer)
    indexInLayer = fromMaybe 0 (Array.elemIndex name packagesInLayer)
    countInLayer = Array.length packagesInLayer

    -- Layout constants for topo view (horizontal: right-to-left)
    -- Project code (high layer) on left, base libs (low layer) on right
    topoLayerSpacing = 150.0  -- Horizontal spacing between layers
    topoNodeSpacing = 60.0    -- Vertical spacing within layer (tighter)

    -- X: layer 0 (base libs) at right, higher layers (project code) at left
    tx = (toNumber maxLayer / 2.0 - toNumber layer) * topoLayerSpacing

    -- Y: center nodes within their layer
    ty = (toNumber indexInLayer - toNumber countInLayer / 2.0 + 0.5) * topoNodeSpacing

    -- Package dependencies (targets = packages this depends on)
    depPackageNames = fromMaybe Set.empty (Map.lookup name derivedPackageDeps)
    targets = Array.mapMaybe (\n -> Map.lookup n packageIdMap) (Array.fromFoldable depPackageNames)

    -- Package dependents (sources = packages that depend on this)
    -- Find all packages whose deps include this package
    sources = Array.mapMaybe (\n -> Map.lookup n packageIdMap) $
      Array.filter (\otherName -> Set.member name (fromMaybe Set.empty (Map.lookup otherName derivedPackageDeps))) _allPackages
  in
    { id: idx
    , name
    , nodeType: PackageNode
    , package: name
    , x: gx
    , y: gy
    , vx: 0.0
    , vy: 0.0
    , fx: null
    , fy: null
    , r -- Package sized by total LOC
    , cluster: idx
    , targets
    , sources
    , gridX: gx
    , gridY: gy
    , orbitAngle: angle
    , treeX: 0.0
    , treeY: 0.0
    , radialX: 0.0
    , radialY: 0.0
    , isInTree: false -- Packages are never in the tree
    , topoX: tx
    , topoY: ty
    , topoLayer: layer
    }

mkModuleNode
  :: String
  -> Int
  -> Object RawModule
  -> Map String Int
  -> Map String Int
  -> Map String Int
  -> Int
  -> Int
  -> Array SimNode -- Pass package nodes to get their positions
  -> Map Int (Array Int) -- targetsMap
  -> Map Int (Array Int) -- sourcesMap
  -> SimNode
mkModuleNode name idx modulesObj locMap packageIdMap _moduleIdMap packageCount _moduleCount packageNodes targetsMap sourcesMap =
  let
    nodeId = idx + packageCount
    rawMod = Object.lookup name modulesObj
    pkgName = fromMaybe "unknown" (rawMod <#> _.package)
    path = fromMaybe "" (rawMod <#> _.path)
    loc = fromMaybe 50 (Map.lookup path locMap)

    -- Get cluster from package
    cluster = fromMaybe 0 (Map.lookup pkgName packageIdMap)

    -- Module radius based on LOC (sqrt scale, visible range ~4-25)
    r = 4.0 + sqrt (toNumber loc) * 0.8

    -- Get parent package's grid position
    pkgId = fromMaybe 0 (Map.lookup pkgName packageIdMap)
    pkgNode = Array.index packageNodes pkgId
    pkgGridX = fromMaybe 0.0 (pkgNode <#> _.gridX)
    pkgGridY = fromMaybe 0.0 (pkgNode <#> _.gridY)

    -- Slight random offset from package center for modules
    -- Use a deterministic "random" based on name hash
    nameHash = stringHash name
    offsetAngle = 2.0 * pi * toNumber (nameHash `mod` 360) / 360.0
    offsetDist = 30.0 + toNumber ((nameHash / 360) `mod` 30)
    offsetX = cos offsetAngle * offsetDist
    offsetY = sin offsetAngle * offsetDist

    -- Absolute grid position = package position + offset
    absGridX = pkgGridX + offsetX
    absGridY = pkgGridY + offsetY

    -- Get targets (dependencies) and sources (dependents) from maps
    targets = fromMaybe [] (Map.lookup nodeId targetsMap)
    sources = fromMaybe [] (Map.lookup nodeId sourcesMap)
  in
    { id: nodeId
    , name
    , nodeType: ModuleNode
    , package: pkgName
    , x: absGridX -- Start at grid position
    , y: absGridY
    , vx: 0.0
    , vy: 0.0
    , fx: null
    , fy: null
    , r
    , cluster
    , targets
    , sources
    , gridX: absGridX -- Absolute position for Grid scene
    , gridY: absGridY
    , orbitAngle: 0.0 -- Modules don't have orbit angle
    , treeX: 0.0
    , treeY: 0.0
    , radialX: 0.0
    , radialY: 0.0
    , isInTree: false -- Will be set true by updateNodeWithTreeData if reachable
    , topoX: 0.0
    , topoY: 0.0
    , topoLayer: 0  -- Modules don't use topo positioning
    }

-- =============================================================================
-- Helpers
-- =============================================================================

buildLocMap :: Array LocEntry -> Map String Int
buildLocMap entries =
  Map.fromFoldable $ map (\e -> Tuple e.path e.loc) entries

buildPackageModulesMap :: Object RawModule -> Map String (Array String)
buildPackageModulesMap modulesObj =
  foldl addModule Map.empty (Object.toUnfoldable modulesObj :: Array (Tuple String RawModule))
  where
  addModule acc (Tuple modName rawMod) =
    let
      pkg = rawMod.package
      existing = fromMaybe [] (Map.lookup pkg acc)
    in
      Map.insert pkg (Array.snoc existing modName) acc

-- | Build map from package name to total LOC (sum of all module LOC)
buildPackageLocMap :: Object RawModule -> Map String Int -> Map String Int
buildPackageLocMap modulesObj locMap =
  foldl addModuleLoc Map.empty (Object.toUnfoldable modulesObj :: Array (Tuple String RawModule))
  where
  addModuleLoc acc (Tuple _ rawMod) =
    let
      pkg = rawMod.package
      moduleLoc = fromMaybe 50 (Map.lookup rawMod.path locMap)
      existing = fromMaybe 0 (Map.lookup pkg acc)
    in
      Map.insert pkg (existing + moduleLoc) acc

buildLinks :: Object RawModule -> Map String Int -> Array SimLink
buildLinks modulesObj moduleIdMap =
  Array.concat $ map mkLinks (Object.toUnfoldable modulesObj :: Array (Tuple String RawModule))
  where
  mkLinks (Tuple modName rawMod) =
    case Map.lookup modName moduleIdMap of
      Nothing -> []
      Just sourceId ->
        Array.mapMaybe
          ( \depName ->
              Map.lookup depName moduleIdMap <#> \targetId ->
                { source: sourceId
                , target: targetId
                , linkType: M2M_Graph -- Default to graph; markLinkType will change to M2M_Tree if in spanning tree
                }
          )
          rawMod.depends

-- | Build map from module ID to its dependencies (targets)
buildTargetsMap :: Object RawModule -> Map String Int -> Map Int (Array Int)
buildTargetsMap modulesObj moduleIdMap =
  foldl addTargets Map.empty (Object.toUnfoldable modulesObj :: Array (Tuple String RawModule))
  where
  addTargets acc (Tuple modName rawMod) =
    case Map.lookup modName moduleIdMap of
      Nothing -> acc
      Just sourceId ->
        let
          targets = Array.mapMaybe (\depName -> Map.lookup depName moduleIdMap) rawMod.depends
        in
          Map.insert sourceId targets acc

-- | Build map from module ID to modules that depend on it (sources/dependents)
buildSourcesMap :: Map Int (Array Int) -> Map Int (Array Int)
buildSourcesMap targetsMap =
  foldl addSources Map.empty (Map.toUnfoldable targetsMap :: Array (Tuple Int (Array Int)))
  where
  addSources acc (Tuple sourceId targets) =
    foldl
      ( \acc' targetId ->
          let
            existing = fromMaybe [] (Map.lookup targetId acc')
          in
            Map.insert targetId (Array.snoc existing sourceId) acc'
      )
      acc
      targets

-- Simple string hash function for deterministic positioning
stringHash :: String -> Int
stringHash s =
  let
    chars = SCU.toCharArray s
  in
    foldl (\acc c -> (acc * 31 + charCode c) `mod` 1000000) 0 chars

-- Get char code (simple implementation)
charCode :: Char -> Int
charCode c = case SCU.indexOf (Pattern (SCU.singleton c)) alphabet of
  Just i -> i
  Nothing -> 0
  where
  alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789._-"

