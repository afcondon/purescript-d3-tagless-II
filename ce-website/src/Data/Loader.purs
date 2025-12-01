-- | Data loading and transformation
-- |
-- | Loads JSON data and transforms into SimNode/SimLink arrays with
-- | pre-calculated positions for Grid, Orbit, and Tree scenes.
module Data.Loader
  ( loadModel
  , LoadedModel
  ) where

import Prelude

import Affjax.Web as AW
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, printJsonDecodeError)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (pi, cos, sin, sqrt)
import Data.Nullable (null)
import Data.String.CodeUnits as SCU
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Foreign.Object (Object)
import Foreign.Object as Object
import Types (SimNode, SimLink, NodeType(..), LinkType(..), Package)

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

-- | Loaded and transformed model
type LoadedModel =
  { nodes :: Array SimNode
  , links :: Array SimLink
  , packages :: Array Package
  , moduleCount :: Int
  , packageCount :: Int
  }

-- =============================================================================
-- Loading
-- =============================================================================

-- | Load all data files and transform to model
loadModel :: Aff (Either String LoadedModel)
loadModel = do
  modulesResult <- fetchJson "/data/spago-data/modules.json"
  packagesResult <- fetchJson "/data/spago-data/packages.json"
  locResult <- fetchJson "/data/spago-data/LOC.json"

  pure $ do
    modulesJson <- modulesResult
    packagesJson <- packagesResult
    locJson <- locResult

    -- Decode JSON
    modules :: Object RawModule <- decodeJson modulesJson # mapLeft printJsonDecodeError
    packages :: Object RawPackage <- decodeJson packagesJson # mapLeft printJsonDecodeError
    locFile :: LocFile <- decodeJson locJson # mapLeft printJsonDecodeError

    -- Build LOC map (path -> loc)
    let locMap = buildLocMap locFile.loc

    -- Transform to model
    Right $ transformToModel modules packages locMap

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
-- Transformation
-- =============================================================================

transformToModel :: Object RawModule -> Object RawPackage -> Map String Int -> LoadedModel
transformToModel modulesObj packagesObj locMap =
  let
    -- Get arrays
    moduleNames = Object.keys modulesObj
    packageNames = Object.keys packagesObj

    -- Build package -> modules map
    packageModules = buildPackageModulesMap modulesObj

    -- Assign IDs: packages first, then modules
    packageCount = Array.length packageNames
    moduleCount = Array.length moduleNames

    -- Create package nodes (IDs 0 to packageCount-1)
    packageNodes = Array.mapWithIndex (mkPackageNode packageNames packageCount) packageNames

    -- Create name -> ID maps
    packageIdMap = Map.fromFoldable $ Array.mapWithIndex (\i n -> Tuple n i) packageNames
    moduleIdMap = Map.fromFoldable $ Array.mapWithIndex (\i n -> Tuple n (i + packageCount)) moduleNames

    -- Create module nodes (IDs packageCount to packageCount+moduleCount-1)
    moduleNodes = Array.mapWithIndex
      (\i name -> mkModuleNode name i modulesObj locMap packageIdMap moduleIdMap packageCount moduleCount packageNodes)
      moduleNames

    -- All nodes
    nodes = packageNodes <> moduleNodes

    -- Create links (module -> module dependencies)
    links = buildLinks modulesObj moduleIdMap

    -- Create Package records for model
    packages = Array.mapWithIndex (\i name ->
      { name
      , depends: fromMaybe [] $ Object.lookup name packagesObj <#> _.depends
      , modules: fromMaybe [] $ Map.lookup name packageModules
      }) packageNames
  in
    { nodes, links, packages, moduleCount, packageCount }

-- =============================================================================
-- Node Creation
-- =============================================================================

mkPackageNode :: Array String -> Int -> Int -> String -> SimNode
mkPackageNode allPackages totalPackages idx name =
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
    , r: 20.0  -- Packages are larger
    , cluster: idx
    , targets: []
    , sources: []
    , gridX: gx
    , gridY: gy
    , orbitAngle: angle
    , treeX: 0.0
    , treeY: 0.0
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
  -> Array SimNode  -- Pass package nodes to get their positions
  -> SimNode
mkModuleNode name idx modulesObj locMap packageIdMap moduleIdMap packageCount moduleCount packageNodes =
  let
    nodeId = idx + packageCount
    rawMod = Object.lookup name modulesObj
    pkgName = fromMaybe "unknown" (rawMod <#> _.package)
    path = fromMaybe "" (rawMod <#> _.path)
    loc = fromMaybe 50 (Map.lookup path locMap)

    -- Get cluster from package
    cluster = fromMaybe 0 (Map.lookup pkgName packageIdMap)

    -- Module radius based on LOC (sqrt scale)
    r = 3.0 + sqrt (toNumber loc) * 0.3

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
  in
    { id: nodeId
    , name
    , nodeType: ModuleNode
    , package: pkgName
    , x: absGridX  -- Start at grid position
    , y: absGridY
    , vx: 0.0
    , vy: 0.0
    , fx: null
    , fy: null
    , r
    , cluster
    , targets: []  -- TODO: fill from depends
    , sources: []
    , gridX: absGridX  -- Absolute position for Grid scene
    , gridY: absGridY
    , orbitAngle: 0.0  -- Modules don't have orbit angle
    , treeX: 0.0
    , treeY: 0.0
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
    let pkg = rawMod.package
        existing = fromMaybe [] (Map.lookup pkg acc)
    in Map.insert pkg (Array.snoc existing modName) acc

buildLinks :: Object RawModule -> Map String Int -> Array SimLink
buildLinks modulesObj moduleIdMap =
  Array.concat $ map mkLinks (Object.toUnfoldable modulesObj :: Array (Tuple String RawModule))
  where
  mkLinks (Tuple modName rawMod) =
    case Map.lookup modName moduleIdMap of
      Nothing -> []
      Just sourceId ->
        Array.mapMaybe (\depName ->
          Map.lookup depName moduleIdMap <#> \targetId ->
            { source: sourceId
            , target: targetId
            , linkType: ModuleToModule
            }) rawMod.depends

-- Simple string hash function for deterministic positioning
stringHash :: String -> Int
stringHash s =
  let chars = SCU.toCharArray s
  in foldl (\acc c -> (acc * 31 + charCode c) `mod` 1000000) 0 chars

-- Get char code (simple implementation)
charCode :: Char -> Int
charCode c = case SCU.indexOf (Pattern (SCU.singleton c)) alphabet of
  Just i -> i
  Nothing -> 0
  where
  alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789._-"
