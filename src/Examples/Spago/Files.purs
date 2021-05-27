module D3.Examples.Spago.Files where

import Data.Array
import Prelude

import Affjax (URL)
import D3.Node (D3_Link(..), NodeID)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NA
import Data.Foldable (sum)
import Data.Map as M
import Data.Maybe (fromMaybe, Maybe(..))
import Data.String (Pattern(..), split)
import Data.Tuple (Tuple(..), fst, snd)

-- | move to utility file
compareFst a b = compare (fst a) (fst b)
compareSnd a b = compare (snd a) (snd b)
equalFst   a b = eq (fst a) (fst b)
equalSnd   a b = eq (snd a) (snd b)
-- | chunk is a utility function that's easier to show than tell:
-- | example input  [ [(m1,p1), (m4,p1)], [(m2,p2), (m3,p2)], [(m5,p3)] ] 
-- | example ouput  [ (p1, [m1,m4]), (p2, [m2,m3]), (p3, [m5]) ]
chunk :: forall a b. NonEmptyArray (Tuple a b) -> Tuple b (Array a)
chunk tuples = do
  let
    package  = snd $ NA.head tuples
    contains = NA.toArray $ fst <$> tuples
  Tuple package contains

type ModuleRow row  = ( key :: String, depends :: Array String, path :: String | row )
type PackageRow row = ( key :: String, depends :: Array String | row )
type RepoRow row    = ( packageName :: String, version :: String, repo :: { tag :: String, contents :: URL } | row )
type LOCRow row     = ( loc :: Number, path :: String | row )

type ModuleJSON     = {                                  | ModuleRow () }
type ModuleJSONP    = {                package :: String | ModuleRow () }
type ModuleJSONPL   = { loc :: Number, package :: String | ModuleRow () }

type PackageJSON    = {                                         | PackageRow () }
type PackageJSONC   = {                contains :: Array String | PackageRow () }
type PackageJSONCL  = { loc :: Number, contains :: Array String | PackageRow () }

type RepoJSON       = { | RepoRow () }
type LOCJSON        = { | LOCRow () }

-- TODO no error handling at all here RN (OTOH - performant!!)
type Spago_Raw_JSON_ = { 
    packages        :: Array PackageJSON
  , modules         :: Array ModuleJSON
  , lsDeps          :: Array RepoJSON
  , loc             :: Array LOCJSON
} 

-- TODO use a generic, per-file read for this, and lose the FFI file here
foreign import readSpago_Raw_JSON_ :: String -> String -> String -> String -> Spago_Raw_JSON_

type Spago_Cooked_JSON    = { 
    links      :: Array SpagoGraphLinkID
  , nodes      :: Array SpagoNodeData
  , name2ID    :: M.Map String NodeID
  , id2Name    :: M.Map NodeID String
  , id2Node    :: M.Map NodeID SpagoNodeData
  , id2Package :: M.Map NodeID NodeID
  , id2LOC     :: M.Map NodeID Number
}

data PackageRelation = InPackage | OutPackage
data LinkType        = M2M_Tree | M2M_Graph | P2P | M2P PackageRelation
data PackageInfo     = PackageInfo { version :: String, repo :: String }
type ModulePath      = String
data NodeType        = IsModule ModulePath | IsPackage PackageInfo
data Pinned          = Pinned | Floating -- might be more categories here, "pinned connectd", "pinned unused" whatever...
type Deps            = Array NodeID

type SpagoLinkData     = ( linktype :: LinkType )
type SpagoGraphLinkID  = D3_Link NodeID SpagoLinkData
type SpagoNodeRow row = ( 
    id       :: NodeID
  , depends  :: { full       :: Deps
                , tree       :: Deps
                , inPackage  :: Deps
                , outPackage :: Deps
                , contains   :: Deps -- in case of package, modules; in case of modules, codepoints? (not implemented)
                }
  , nodetype :: NodeType
  , name     :: String
  , pinned   :: Pinned
  , loc      :: Number
  , package  :: NodeID
  | row )
type SpagoNodeData    = { | SpagoNodeRow () }


getGraphJSONData :: Spago_Raw_JSON_ -> Spago_Cooked_JSON
getGraphJSONData { packages, modules, lsDeps, loc } = do
  let
    path2LOC = M.fromFoldable $ (\o -> Tuple o.path o.loc) <$> loc
    addLOCInfo :: ModuleJSONP -> ModuleJSONPL
    addLOCInfo { key, depends, path, package } = { key, depends, path, package, loc: linecount }
      where
        linecount = fromMaybe 0.0 $ M.lookup path path2LOC

    addPackageInfo :: ModuleJSON -> ModuleJSONP
    addPackageInfo { key, depends, path } = { key, depends, path, package }
      where
        package = fromMaybe "" packageName
        packageName :: Maybe String
        packageName = do
          let pieces = split (Pattern "/") path
          root          <- pieces !! 0
          packageString <- pieces !! 1
          if root == ".spago" 
          then Just packageString
          else Nothing

    modulesPL :: Array ModuleJSONPL
    modulesPL = (addLOCInfo <<< addPackageInfo) <$> modules

    -- the tuples are basis for Map moduleName packageName
    modulePackageTuples = (\m -> Tuple m.package m.key) <$> modulesPL
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
    ids   = 1 `range` (length names)

    -- | NB discards duplicates but should be impossible that there are any
    name2ID :: M.Map String NodeID
    name2ID = M.fromFoldableWith (\v1 v2 -> v1) $ zip names ids

    getId :: String -> NodeID
    getId s = fromMaybe 0 (M.lookup s name2ID)

    makeNodeFromModuleJSONPL :: ModuleJSONPL -> SpagoNodeData
    makeNodeFromModuleJSONPL m = do
      let id = getId m.key
      { id       : id
      , name     : m.key
      , package  : getId m.package
      , loc      : m.loc
      , nodetype : IsModule m.path
      , pinned   : Floating
      , depends  :  { full: (getId <$> m.depends) -- NB these are Module depends
                    , tree: []
                    , inPackage: []
                    , outPackage: []
                    , contains: [] -- we're not looking inside packages yet
                    }  
        } 

    depsMap :: M.Map String { version :: String, repo :: String }
    depsMap = M.fromFoldable $ (\d -> Tuple d.packageName { version: d.version, repo: d.repo.contents } ) <$> lsDeps

    makeNodeFromPackageJSONCL :: PackageJSONCL -> SpagoNodeData
    makeNodeFromPackageJSONCL p = do
      let id  = getId p.key
          repo = fromMaybe { version: "not found", repo: "not found" } $ M.lookup p.key depsMap

      { id       : id
      , name     : p.key
      , nodetype : IsPackage (PackageInfo repo)
      , pinned   : Floating
      , package  : id -- package belongs to itself
      , loc      : p.loc 
      , depends  :  { full: (getId <$> p.depends)  -- NB these are Package depends
                    , tree: []
                    , inPackage: []
                    , outPackage: []
                    , contains: (getId <$> p.contains)
                    } 
      }

    moduleNodes        = makeNodeFromModuleJSONPL  <$> modulesPL
    packageNodes       = makeNodeFromPackageJSONCL <$> packagesCL

    makeLink :: LinkType -> Tuple NodeID NodeID -> SpagoGraphLinkID
    makeLink linktype (Tuple source target) = D3_Link { source, target, linktype }

    foldDepends :: forall r. Array (Tuple NodeID NodeID) -> { key :: String, depends :: Array String | r } -> Array (Tuple NodeID NodeID)
    foldDepends b a = (makeTuple <$> a.depends) <> b    
      where id = getId a.key 
            makeTuple :: String -> Tuple NodeID NodeID
            makeTuple s = Tuple id (getId s)  

    moduleLinks  = (makeLink M2M_Graph) <$> (foldl foldDepends [] modules)             
    packageLinks = (makeLink P2P)       <$> (foldl foldDepends [] packages)

    makeModuleToPackageLink :: SpagoNodeData -> SpagoGraphLinkID
    makeModuleToPackageLink m = D3_Link { source: m.id, target: m.package, linktype: M2P InPackage }

    modulePackageLinks = makeModuleToPackageLink <$> moduleNodes
    
    nodes = moduleNodes <> packageNodes
    links = moduleLinks <> packageLinks <> modulePackageLinks

  { links
  , nodes
  , name2ID   : M.empty
  , id2Name   : M.empty
  , id2Node   : M.empty
  , id2Package: M.empty
  , id2LOC    : M.empty
  }


-- | boilerplate
instance showNodeType :: Show NodeType where
  show (IsModule _) = "module"
  show (IsPackage (PackageInfo _)) = "package" 
instance showLinkType :: Show LinkType where
  show M2M_Tree  = "M2M-Tree"
  show M2M_Graph = "M2M-Graph"
  show P2P = "P2P"
  show (M2P InPackage) = "in-package dependency"
  show (M2P OutPackage) = "out-package dependency"

derive instance eqPinned :: Eq Pinned