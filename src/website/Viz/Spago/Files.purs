module D3.Viz.Spago.Files where

import Prelude

import Affjax (URL)
import PSD3.Internal.Types (PointXY)
import PSD3.Data.Node (D3Link_Unswizzled, D3Link_Swizzled, D3_FocusXY, D3_ID, D3_VxyFxy, D3_XY, NodeID)
import PSD3.Data.Graph (buildGraphModel, getLinksTo)
import Unsafe.Coerce (unsafeCoerce)
import Data.Array (catMaybes, foldl, groupBy, length, range, sortBy, zip, (!!), (:))
import Data.Foldable (sum)
import Data.Map as M
import Data.Maybe (fromMaybe, Maybe(..))
import Data.Nullable (Nullable, null)
import Data.String (Pattern(..), split)
import Data.Tuple (Tuple(..))
import Type.Row (type (+))
import Utility (chunk, compareSnd, equalSnd)

-- ======================================================================================================================
-- | Legacy tree row types (removed from PSD3.Data.Node during native tree refactor)
-- | Defined locally here for Spago's tree building needs
-- ======================================================================================================================
type EmbeddedData a row = ( data :: a | row )
type D3TreeRow row = ( x :: Number, y :: Number, depth :: Int, height :: Int, parent :: Maybe String, childIDs :: Array NodeID | row )
newtype D3_TreeNode a = D3TreeNode (Record (D3TreeRow (EmbeddedData a ())))

-- ======================================================================================================================
-- | Types for all the data from the files
-- ======================================================================================================================
type ModuleRow row  = ( key :: String, depends :: Array String, path :: String | row )
type PackageRow row = ( key :: String, depends :: Array String | row )
type RepoRow row    = ( packageName :: String, version :: String, repo :: { tag :: String, contents :: URL } | row )
type LOCRow row     = ( loc :: Number, path :: String | row )

type ModuleJSON     = { loc :: Number                    | ModuleRow () }  -- Now includes LOC from JS
type ModuleJSONP    = { loc :: Number, package :: String | ModuleRow () }
type ModuleJSONPL   = { loc :: Number, package :: String | ModuleRow () }

type PackageJSON    = {                                         | PackageRow () }
type PackageJSONC   = {                contains :: Array String | PackageRow () }
type PackageJSONCL  = { loc :: Number, contains :: Array String | PackageRow () }

type RepoJSON       = { | RepoRow () }
type LOCJSON        = { | LOCRow () }

-- | Raw JSON data from multiple Spago files merged together
-- | Note: No error handling - assumes well-formed JSON (performance over safety)
type Spago_Raw_JSON_ = { 
    packages        :: Array PackageJSON
  , modules         :: Array ModuleJSON
  , lsDeps          :: Array RepoJSON
  , loc             :: Array LOCJSON
}

-- | FFI function to parse and merge JSON from four separate files
-- | Takes raw JSON strings for modules, packages, dependencies, and LOC data
foreign import readSpago_Raw_JSON_ :: String -> String -> String -> String -> Spago_Raw_JSON_

type Spago_Cooked_JSON    = {
    links              :: Array D3Link_Unswizzled
  , nodes              :: Array SpagoNodeData
  , moduleNodes        :: Array SpagoNodeData
  , packageNodes       :: Array SpagoNodeData
  , moduleLinks        :: Array D3Link_Unswizzled
  , packageLinks       :: Array D3Link_Unswizzled
  , modulePackageLinks :: Array D3Link_Unswizzled
  , sourceLinksMap     :: M.Map NodeID (Array NodeID)
  , name2ID            :: M.Map String NodeID
  , id2Name            :: M.Map NodeID String
  -- id2Node removed: use GraphModel.maps.nodeById instead
  , id2Package         :: M.Map NodeID NodeID
  , id2LOC             :: M.Map NodeID Number
}

data LinkType        = M2M_Tree | M2M_Graph | P2P | M2P
derive instance Eq LinkType

data PackageInfo     = PackageInfo { version :: String, repo :: String }

derive instance eqPackageInfo :: Eq PackageInfo
derive instance ordPackageInfo :: Ord PackageInfo
type ModulePath      = String
data NodeType        = IsModule ModulePath | IsPackage PackageInfo

derive instance eqNodeType :: Eq NodeType
derive instance ordNodeType :: Ord NodeType
type Dependencies    = Array NodeID

type SpagoLinkData = ( linktype :: LinkType, inSim :: Boolean ) 

type SpagoTreeObj = D3_TreeNode SpagoNodeData

type SpagoNodeRow row = ( 
    id       :: NodeID
  , links    :: { targets      :: Dependencies -- the nodes on which this nodes depends
                , sources      :: Dependencies -- the nodes which depend on this node
                , treeChildren :: Dependencies 
                , inPackage    :: Dependencies
                , outPackage   :: Dependencies
                , contains     :: Dependencies -- in case of package, modules; in case of modules, codepoints? (not implemented)
                }
  , connected     :: Boolean
  , showChildren  :: Boolean
  , containerID   :: NodeID
  , containerName :: String
  , containsMany  :: Boolean
  , inSim         :: Boolean -- should we put this node in the Sim? allows fine-grained control of what in
  , loc           :: Number
  , name          :: String
  , nodetype      :: NodeType
  , treeXY        :: Nullable PointXY
  , treeDepth     :: Nullable Int
  , gridXY        :: Nullable PointXY
  | row )
type SpagoNodeData    = { | SpagoNodeRow () }
-- Note: r (radius) field is added by upgradeSpagoNodeData in Model.purs
type D3_Radius row = ( r :: Number | row )
type SpagoDataRow   = (D3_XY + D3_VxyFxy + SpagoNodeRow  + D3_FocusXY + D3_Radius + ())
type SpagoDataRecord = Record SpagoDataRow

-- ======================================================================================================================
-- | Synthesize the Model from the data in the files, would be better done with SQL if data were in DB
-- ======================================================================================================================
getGraphJSONData :: Spago_Raw_JSON_ -> Spago_Cooked_JSON
getGraphJSONData { packages, modules, lsDeps, loc } = do
  let
    -- LOC is now already attached to modules from JS, just need to add package info
    addPackageInfo :: ModuleJSON -> ModuleJSONP
    addPackageInfo { key, depends, path, loc: locValue } = { key, depends, path, loc: locValue, package }
      where
        package = fromMaybe "" packageName
        packageName :: Maybe String
        packageName = do
          let pieces = split (Pattern "/") path
          root          <- pieces !! 0
          packageString <- pieces !! 1
          case root of
            ".spago" ->
              -- Handle both .spago/p/pkg/hash and .spago/pkg/version formats
              if packageString == "p"
                then pieces !! 2  -- .spago/p/aff/hash -> aff
                else Just packageString  -- .spago/aff/version -> aff
            "src"    -> Just "my-project"  -- hardcoded for this project's source files
            _ -> Nothing

    modulesPL :: Array ModuleJSONPL
    modulesPL = addPackageInfo <$> modules

    -- the tuples are basis for Map moduleName packageName
    modulePackageTuples = (\m -> Tuple m.key m.package) <$> modulesPL
    mapNamesToModules :: M.Map String ModuleJSONPL
    mapNamesToModules   = M.fromFoldable $ (\m -> Tuple m.key m) <$> modulesPL

    packageContains :: Array (Tuple String (Array String))
    packageContains = chunk <$> (            -- [ (p1, [m1,m4]), (p2, [m2,m3]), (p3, [m5]) ]
                      groupBy equalSnd   $ -- [ [(m1,p1), (m4,p1)], [(m2,p2), (m3,p2)], [(m5,p3)] ]
                      sortBy  compareSnd $ -- [ (m1,p1), (m4,p1), (m2,p2), (m3,p2), (m5,p3) ]
                      modulePackageTuples  -- [ (m1,p1),(m2,p2),(m3,p2),(m4,p1),(m5,p3)]
                      )

    packageLOC :: Array (Tuple String Number)
    packageLOC = (\(Tuple p ms) -> Tuple p (rollUpLOC ms)) <$> packageContains
    rollUpLOC :: Array String -> Number
    rollUpLOC ms = sum $ _.loc <$> (maybeModules ms)
    maybeModules :: Array String -> Array ModuleJSONPL
    maybeModules ms = catMaybes $ (\k -> M.lookup k mapNamesToModules) <$> ms

    packageContainsMap = M.fromFoldable packageContains
    packageLOCMap      = M.fromFoldable packageLOC

    addContains :: PackageJSON -> PackageJSONC
    addContains { key, depends } = { key, depends, contains: (fromMaybe [] $ M.lookup key packageContainsMap) }

    addRollUpLOC :: PackageJSONC -> PackageJSONCL
    addRollUpLOC { key, depends, contains } = { key, depends, contains, loc: (fromMaybe 0.0 $ M.lookup key packageLOCMap) }
       
    packagesCL :: Array PackageJSONCL
    packagesCL = (addRollUpLOC <<< addContains) <$> packages

    -- | Now we have everything needed to make the Model: Modules + package + LOC, Packages + contains + loc
    names = (_.key <$> modules) <> (_.key <$> packages)
    ids   = 0 `range` ((length names) - 1 ) -- we need a zero-based index for the nodes in D3, so our id's start at 0

    -- | NB discards duplicates but should be impossible that there are any
    name2ID :: M.Map String NodeID
    name2ID = M.fromFoldableWith (\v1 v2 -> v1) $ zip names ids

    getId :: String -> NodeID
    getId s = fromMaybe 0 (M.lookup s name2ID)

    makeNodeFromModuleJSONPL :: ModuleJSONPL -> SpagoNodeData
    makeNodeFromModuleJSONPL m = do
      let id = getId m.key
      { id           : id
      , name         : m.key
      , containerID  : getId m.package
      , containerName: m.package
      , loc          : m.loc
      , nodetype     : IsModule m.path
      , inSim        : true
      , links        :  { targets      : (getId <$> m.depends) -- NB these are Module depends
                        , sources      : []  -- these can be filled in later?
                        , treeChildren : []
                        , inPackage    : []
                        , outPackage   : []
                        , contains     : [] -- we're not looking inside packages yet
                        } 
      , connected    : false -- this is shorthand for "connected to Main by a dependency tree", essentially it's treeNodes.no 
      , showChildren : false
      , containsMany : false 
      , treeXY       : null
      , treeDepth    : null
      , gridXY       : null
      } 

    depsMap :: M.Map String { version :: String, repo :: String }
    depsMap = M.fromFoldable $
              lsDeps <#> \d -> Tuple d.packageName { version: d.version, repo: d.repo.contents }

    makeNodeFromPackageJSONCL :: PackageJSONCL -> SpagoNodeData
    makeNodeFromPackageJSONCL p = do
      let id  = getId p.key
          repo = fromMaybe { version: "not found", repo: "not found" } $ M.lookup p.key depsMap

      { id           : id
      , name         : p.key
      , inSim        : true
      , nodetype     : IsPackage (PackageInfo repo)
      , containerID  : id -- package belongs to itself
      , containerName: p.key
      , loc          : p.loc 
      , links        :  { targets: (getId <$> p.depends)  -- NB these are Package depends
                        , sources     : []
                        , treeChildren: []
                        , inPackage   : []
                        , outPackage  : []
                        , contains    : (getId <$> p.contains)
                        } 
      , connected    : true -- unless something has gone very wrong we shouldn't have any unconnected _packages_ (only modules)
      , showChildren : true
      , containsMany : (length p.contains) > 1 -- we won't try to cluster contents of single module packages
      , treeXY       : null
      , treeDepth    : null
      , gridXY       : null
      }

    moduleNodes        = makeNodeFromModuleJSONPL  <$> modulesPL
    packageNodes       = makeNodeFromPackageJSONCL <$> packagesCL

    packLink :: forall r. { source :: NodeID, target :: NodeID | r } -> D3Link_Unswizzled
    packLink = unsafeCoerce

    makeLink :: LinkType -> Tuple NodeID NodeID -> D3Link_Unswizzled
    makeLink linktype (Tuple source target) = packLink { source, target, linktype, inSim: true }

    foldDepends :: forall r. Array (Tuple NodeID NodeID) -> { key :: String, depends :: Array String | r } -> Array (Tuple NodeID NodeID)
    foldDepends b a = (makeTuple <$> a.depends) <> b
      where id = getId a.key
            makeTuple :: String -> Tuple NodeID NodeID
            makeTuple s = Tuple id (getId s)

    moduleLinks  = (makeLink M2M_Graph) <$> (foldl foldDepends [] modules)
    packageLinks = (makeLink P2P)       <$> (foldl foldDepends [] packages)

    makeModuleToPackageLink :: SpagoNodeData -> D3Link_Unswizzled
    makeModuleToPackageLink m = packLink { source: m.id, target: m.containerID, linktype: M2P, inSim: true }

    modulePackageLinks = makeModuleToPackageLink <$> moduleNodes
    
    nodes = moduleNodes <> packageNodes
    links = moduleLinks <> packageLinks <> modulePackageLinks

    -- Build temporary GraphModel for O(1) link lookups during sourceLinksMap construction
    linkGraphConfig = { getNodeId: _.id
                      , getLinkSource: \link -> (unsafeCoerce link).source
                      , getLinkTarget: \link -> (unsafeCoerce link).target
                      }
    linkGraph = buildGraphModel linkGraphConfig nodes links

    -- | Build sourceLinksMap using O(1) lookups from GraphModel
    -- | Old approach: O(n×m) - for each node, scan all links
    -- | New approach: O(n) - for each node, O(1) map lookup
    getSourceLinks :: SpagoNodeData -> Tuple NodeID (Array NodeID)
    getSourceLinks { id } = Tuple id sources
      where
        incomingLinks = getLinksTo id linkGraph  -- O(1) lookup!
        sources = ((unsafeCoerce >>> _.source) <$> incomingLinks)

    sourceLinksMap = M.fromFoldable $ getSourceLinks <$> nodes

  { links
  , nodes
  , moduleNodes
  , packageNodes
  , moduleLinks
  , packageLinks
  , modulePackageLinks
  , sourceLinksMap
  , name2ID
  -- id2Node removed: use GraphModel.maps.nodeById instead (constructed in buildSpagoGraph)
  , id2Name   : M.empty
  , id2Package: M.empty
  , id2LOC    : M.empty
  }

unpackLink' :: D3Link_Unswizzled -> { linktype :: LinkType }
unpackLink' = unsafeCoerce

isP2P_Link :: D3Link_Unswizzled -> Boolean
isP2P_Link link = (unpackLink' link).linktype == P2P
isM2M_Graph_Link :: D3Link_Unswizzled -> Boolean
isM2M_Graph_Link link = (unpackLink' link).linktype == M2M_Graph
isM2P_Link :: D3Link_Unswizzled -> Boolean
isM2P_Link link = (unpackLink' link).linktype == M2P
isM2M_Tree_Link :: D3Link_Unswizzled -> Boolean
isM2M_Tree_Link link = (unpackLink' link).linktype == M2M_Tree


-- | boilerplate
instance showNodeType :: Show NodeType where
  show (IsModule _) = "module"
  show (IsPackage (PackageInfo _)) = "package" 
instance showLinkType :: Show LinkType where
  show M2M_Tree  = "M2M-Tree"
  show M2M_Graph = "M2M-Graph"
  show P2P = "P2P"
  show M2P = "module to package dependency"
