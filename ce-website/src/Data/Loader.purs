-- | Data loading and transformation
-- |
-- | Loads JSON data and transforms into SimNode/SimLink arrays with
-- | pre-calculated positions for Grid, Orbit, and Tree scenes.
module Data.Loader
  ( loadModel
  , LoadedModel
  , Declaration
  , DeclarationsMap
  , getDependencyTree
  ) where

import Prelude

import Affjax.Web as AW
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, printJsonDecodeError)
import Data.Array as Array
import Data.Either (Either(..))
import Data.List as List
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
import Data.String.Pattern (Pattern(..))
import Control.Comonad.Cofree (head, tail)
import Data.Tree (Tree, mkTree)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Foreign.Object (Object)
import Foreign.Object as Object
import PSD3.Data.Graph as Graph
import PSD3.Data.Graph.Algorithms as Algorithms
import DataViz.Layout.Hierarchy.Tree as Tree
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

-- | Declaration summary from declarations-summary.json
type Declaration =
  { kind :: String -- "typeClass", "data", "typeSynonym", "externData", "alias", "value"
  , title :: String
  }

-- | Module declarations map: module name -> array of declarations
type DeclarationsMap = Object (Array Declaration)

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

-- | Load all data files and transform to model
loadModel :: Aff (Either String LoadedModel)
loadModel = do
  modulesResult <- fetchJson "/data/spago-data/modules.json"
  packagesResult <- fetchJson "/data/spago-data/packages.json"
  locResult <- fetchJson "/data/spago-data/LOC.json"
  declarationsResult <- fetchJson "/data/spago-data/declarations-summary.json"

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

    -- Create package nodes (IDs 0 to packageCount-1)
    packageNodes = Array.mapWithIndex (mkPackageNode packageNames packageCount packageLocMap) packageNames

    -- Create name -> ID maps
    packageIdMap = Map.fromFoldable $ Array.mapWithIndex (\i n -> Tuple n i) packageNames
    moduleIdMap = Map.fromFoldable $ Array.mapWithIndex (\i n -> Tuple n (i + packageCount)) moduleNames

    -- Build targets map (module ID -> array of dependency IDs)
    targetsMap = buildTargetsMap modulesObj moduleIdMap

    -- Build sources map (module ID -> array of dependents' IDs)
    sourcesMap = buildSourcesMap targetsMap

    -- Create module nodes (IDs packageCount to packageCount+moduleCount-1)
    moduleNodes = Array.mapWithIndex
      (\i name -> mkModuleNode name i modulesObj locMap packageIdMap moduleIdMap packageCount moduleCount packageNodes targetsMap sourcesMap)
      moduleNames

    -- All nodes (before tree positions and isInTree marking)
    nodesBeforeTree = packageNodes <> moduleNodes

    -- Create all module links initially (will mark as Tree/Graph later)
    allModuleLinks = buildLinks modulesObj moduleIdMap

    -- =========================================================================
    -- Pre-compute tree layout positions and link types
    -- =========================================================================

    -- Build GraphModel for algorithms
    graphConfig :: Graph.GraphConfig SimNode SimLink
    graphConfig =
      { getNodeId: _.id
      , getLinkSource: _.source
      , getLinkTarget: _.target
      }
    graphModel = Graph.buildGraphModel graphConfig nodesBeforeTree allModuleLinks

    -- Find root: module with no incoming dependencies (sources is empty)
    rootId = findRootModule moduleNodes

    -- Run reachability analysis to get spanning tree
    reachability = Algorithms.getReachableNodes graphConfig rootId graphModel

    -- Get set of reachable node IDs for marking isInTree
    -- NOTE: Must include rootId explicitly - algorithm only adds targets of edges
    reachableNodeIds = Set.insert rootId (Set.fromFoldable reachability.nodes)

    -- Convert spanning tree edges to a Set for O(1) lookup
    spanningTreeEdges = Set.fromFoldable reachability.spanningTree

    -- Mark links as M2M_Tree (in spanning tree) or M2M_Graph (not in tree)
    links = map (markLinkType spanningTreeEdges) allModuleLinks

    -- Build tree from spanning tree edges
    idTree = buildTreeFromEdges rootId reachability.spanningTree

    -- Convert ID tree to tree with position fields (including height for sorting)
    dataTree = mapTree
      (\nodeId -> { id: nodeId, x: 0.0, y: 0.0, depth: 0, height: 0 })
      idTree

    -- Run tree layout with RECTANGULAR coordinates (like RadialTreeViz)
    -- Then project to radial in updateNodeWithTreeData
    treeLayoutSize = 1000.0
    treeConfig =
      { size: { width: treeLayoutSize, height: treeLayoutSize }
      , minSeparation: 1.0 -- Default value
      , separation: Nothing
      , layerScale: Nothing
      }
    laidOutTree = Tree.tree treeConfig dataTree

    -- Flatten tree to map of rectangular positions (will be projected to radial)
    treePositionMap = flattenTreeToPositionMap treeLayoutSize laidOutTree

    -- Update nodes with tree positions AND isInTree flag
    nodes = map (updateNodeWithTreeData treePositionMap reachableNodeIds) nodesBeforeTree

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

-- | Find the root module (the project's main entry point)
-- | Looks for PSD3.Main by name, falls back to module with most dependents
findRootModule :: Array SimNode -> Int
findRootModule modules =
  -- First, look for PSD3.Main by name (the project's entry point)
  case Array.find (\m -> m.name == "PSD3.Main") modules of
    Just m -> m.id
    Nothing ->
      -- Fallback: find module with MOST sources (most dependents = likely root)
      -- This is the opposite of before - we want the module others depend on
      case Array.last $ Array.sortWith (\m -> Array.length m.sources) modules of
        Just m -> m.id
        Nothing -> 0 -- Empty modules array

-- | Build tree from spanning tree edges (array of source->target tuples)
buildTreeFromEdges :: Int -> Array (Tuple Int Int) -> Tree Int
buildTreeFromEdges rootId edges = go rootId
  where
  -- Get children for a node from edges
  getChildren :: Int -> Array Int
  getChildren nodeId =
    Array.mapMaybe (\(Tuple src tgt) -> if src == nodeId then Just tgt else Nothing) edges

  -- Recursively build tree
  go :: Int -> Tree Int
  go nodeId =
    let
      children = getChildren nodeId
      childTrees = go <$> children
    in
      mkTree nodeId (List.fromFoldable childTrees)

-- | Map function over tree values
mapTree :: forall a b. (a -> b) -> Tree a -> Tree b
mapTree f t = mkTree (f (head t)) (map (mapTree f) (tail t))

-- | Flatten tree to map of rectangular positions (x, y from Tree)
flattenTreeToPositionMap :: Number -> Tree { id :: Int, x :: Number, y :: Number, depth :: Int, height :: Int } -> Map Int { x :: Number, y :: Number }
flattenTreeToPositionMap _ tree = go tree Map.empty
  where
  go :: Tree { id :: Int, x :: Number, y :: Number, depth :: Int, height :: Int } -> Map Int { x :: Number, y :: Number } -> Map Int { x :: Number, y :: Number }
  go t acc =
    let node = head t
        children = tail t
        acc' = Map.insert node.id { x: node.x, y: node.y } acc
    in
      foldl (\a child -> go child a) acc' children

-- | Update node with tree position AND isInTree flag
-- | Uses RadialTreeViz approach:
-- | - Tree produces rectangular coords (x in [0, width], y in [0, height])
-- | - Project to radial: x → angle, y → radius
updateNodeWithTreeData :: Map Int { x :: Number, y :: Number } -> Set Int -> SimNode -> SimNode
updateNodeWithTreeData treePositions reachableIds node =
  let
    inTree = Set.member node.id reachableIds
    layoutSize = 1000.0 -- Must match treeLayoutSize
    maxRadius = 500.0 -- Maximum tree radius (half of layoutSize * 0.85 like RadialTreeViz)

    withTreePos = case Map.lookup node.id treePositions of
      Just { x: treeX, y: treeY } ->
        -- RadialTreeViz projection formula:
        -- x → angle: map [0, layoutSize] to [0, 2π], offset by -π/2 to start at top
        let
          angle = (treeX / layoutSize) * 2.0 * pi - (pi / 2.0)
          -- y → radius: map [0, layoutSize] to [0, maxRadius]
          radius = (treeY / layoutSize) * maxRadius
          -- Convert polar to Cartesian
          cartX = radius * cos angle
          cartY = radius * sin angle
        in
          node { treeX = cartX, treeY = cartY }
      Nothing -> node -- Node not in tree (e.g., packages)
  in
    withTreePos { isInTree = inTree }

-- | Mark a link as M2M_Tree if it's in the spanning tree, otherwise M2M_Graph
markLinkType :: Set (Tuple Int Int) -> SimLink -> SimLink
markLinkType spanningTreeEdges link =
  let
    edge = Tuple link.source link.target
  in
    if Set.member edge spanningTreeEdges then link { linkType = M2M_Tree }
    else link { linkType = M2M_Graph }

-- =============================================================================
-- Node Creation
-- =============================================================================

mkPackageNode :: Array String -> Int -> Map String Int -> Int -> String -> SimNode
mkPackageNode _allPackages totalPackages packageLocMap idx name =
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
    , targets: []
    , sources: []
    , gridX: gx
    , gridY: gy
    , orbitAngle: angle
    , treeX: 0.0
    , treeY: 0.0
    , isInTree: false -- Packages are never in the tree
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
    , isInTree: false -- Will be set true by updateNodeWithTreeData if reachable
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

-- =============================================================================
-- Export dependency tree for testing
-- =============================================================================

-- | HierNode type compatible with FlareRadialTree
type HierNode = { name :: String, value :: Number, x :: Number, y :: Number, depth :: Int, height :: Int }

-- | Get the dependency tree in HierNode format for comparison with Flare tree
getDependencyTree :: Aff (Either String (Tree HierNode))
getDependencyTree = do
  modulesResult <- fetchJson "/data/spago-data/modules.json"
  packagesResult <- fetchJson "/data/spago-data/packages.json"
  locResult <- fetchJson "/data/spago-data/LOC.json"

  pure $ do
    modulesJson <- modulesResult
    packagesJson <- packagesResult
    locJson <- locResult

    modules :: Object RawModule <- decodeJson modulesJson # mapLeft printJsonDecodeError
    packages :: Object RawPackage <- decodeJson packagesJson # mapLeft printJsonDecodeError
    locFile :: LocFile <- decodeJson locJson # mapLeft printJsonDecodeError

    let locMap = buildLocMap locFile.loc

    -- Build nodes/links exactly like transformToModel
    let
      moduleNames = Object.keys modules
      packageNames = Object.keys packages
      packageCount = Array.length packageNames
      moduleCount = Array.length moduleNames
      packageIdMap = Map.fromFoldable $ Array.mapWithIndex (\i n -> Tuple n i) packageNames
      moduleIdMap = Map.fromFoldable $ Array.mapWithIndex (\i n -> Tuple n (i + packageCount)) moduleNames
      targetsMap = buildTargetsMap modules moduleIdMap
      sourcesMap = buildSourcesMap targetsMap
      packageLocMap = buildPackageLocMap modules locMap
      packageNodes = Array.mapWithIndex (mkPackageNode packageNames packageCount packageLocMap) packageNames
      moduleNodes = Array.mapWithIndex
        (\i name -> mkModuleNode name i modules locMap packageIdMap moduleIdMap packageCount moduleCount packageNodes targetsMap sourcesMap)
        moduleNames
      nodesBeforeTree = packageNodes <> moduleNodes
      allModuleLinks = buildLinks modules moduleIdMap

    -- Build graph for reachability
    let
      graphConfig :: Graph.GraphConfig SimNode SimLink
      graphConfig =
        { getNodeId: _.id
        , getLinkSource: _.source
        , getLinkTarget: _.target
        }
      graphModel = Graph.buildGraphModel graphConfig nodesBeforeTree allModuleLinks
      rootId = findRootModule moduleNodes
      reachability = Algorithms.getReachableNodes graphConfig rootId graphModel

    -- Build tree from spanning tree edges
    let idTree = buildTreeFromEdges rootId reachability.spanningTree

    -- Build name lookup map: id -> module name
    let idToName = Map.fromFoldable $ Array.mapWithIndex (\i n -> Tuple (i + packageCount) n) moduleNames

    -- Convert ID tree to HierNode tree (same format as Flare)
    let
      hierTree = mapTree
        ( \nodeId ->
            let
              name = fromMaybe ("node-" <> show nodeId) (Map.lookup nodeId idToName)
            in
              { name, value: 1.0, x: 0.0, y: 0.0, depth: 0, height: 0 }
        )
        idTree

    Right hierTree
