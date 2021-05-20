module D3.Examples.Spago.File where

import D3.Node

import Affjax (URL)
import D3.Data.Types (Datum_, PointXY)
import Data.Array (catMaybes, filter, foldl, head, length, null, range, uncons, zip, (!!), (:))
import Data.Array as A
import Data.Graph (Graph, fromMap)
import Data.Graph as G
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable, null) as N
import Data.Nullable (notNull)
import Data.Set as S
import Data.String (Pattern(..), split)
import Data.Tree (Tree)
import Data.Tuple (Tuple(..))
import Debug (spy)
import Prelude (class Show, bind, not, pure, ($), (<$>), (<>), (==))
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

moduleRadius = 5.0 :: Number 
packageRadius = 50.0 :: Number
packageForceRadius = 50.0 :: Number


-- TODO This is all specific to the Spago output, so break out into file handling modules later
type SpagoModuleJSON  = { key :: String, depends :: Array String, path :: String }
type SpagoPackageJSON = { key :: String, depends :: Array String }
type SpagoLsDepJSON   = { packageName :: String, version :: String, repo :: { tag :: String, contents :: URL } }
type SpagoLOCJSON     = { loc :: Number, path :: String }

-- TODO no error handling at all here RN (OTOH - performant!!)
type Spago_Raw_JSON_ = { 
    packages        :: Array SpagoPackageJSON
  , modules         :: Array SpagoModuleJSON
  , lsDeps          :: Array SpagoLsDepJSON
  , loc             :: Array SpagoLOCJSON } 

type Spago_Cooked_JSON    = { 
    links           :: Array SpagoGraphLinkID
  , nodes           :: Array SpagoNodeData
  , name2IdMap      :: M.Map String NodeID
  , id2NameMap      :: M.Map NodeID String
  , id2NodeMap      :: M.Map NodeID SpagoNodeData
  , id2PackageIDMap :: M.Map NodeID NodeID
}

-- Model data types specialized with inital data
data NodeType           = IsModule | IsPackage
data LinkType           = M2M | P2P | M2P | P2M

type SpagoNodeRow row = ( 
    id       :: NodeID
  , path     :: String
  , depends  :: Array NodeID
  , nodetype :: NodeType
  , name     :: String
  , fixed    :: String
  | row )
type SpagoNodeData    = { | SpagoNodeRow () }

type SpagoTreeNode    = D3TreeRow       (EmbeddedData SpagoNodeData + ())
type SpagoSimNode     = D3SimulationRow (             SpagoNodeRow  + ())

type SpagoLinkData    = ( linktype :: LinkType )
type SpagoGraphLinkID = D3_Link NodeID SpagoLinkData
type SpagoGraphLinkObj = D3_Link SpagoNodeData SpagoLinkData

type SpagoModel = { 
    links           :: Array SpagoGraphLinkID  -- each ID will get swizzled for a SpagoGraphLinkObj_ when simulation initialized
  , nodes           :: Array SpagoSimNode      -- already upgraded to simnode as a result of positioning when building the model
  , graph           :: Graph NodeID SpagoNodeData
  , name2IdMap      :: M.Map String NodeID
  , id2NameMap      :: M.Map NodeID String
  , id2NodeMap      :: M.Map NodeID SpagoNodeData
  , id2PackageIDMap :: M.Map NodeID NodeID
  , path2LOCMap     :: M.Map String Number
  , positions       :: M.Map NodeID { x :: Number, y :: Number, isLeaf :: Boolean }
  , tree            :: Maybe (Tuple NodeID SpagoTreeNode)
}

setXY :: SpagoSimNode -> { x :: Number, y :: Number, isLeaf :: Boolean } -> SpagoSimNode
setXY (D3SimNode node) { x, y, isLeaf: true }  = D3SimNode (node { x = x, y = y, fixed = "no" })
setXY (D3SimNode node) { x, y, isLeaf: false } = D3SimNode (node { fx = notNull x, fy = notNull y, fixed = "yes" })

upgradeSpagoNodeData :: SpagoNodeData -> SpagoSimNode
upgradeSpagoNodeData node = D3SimNode { 
    depends: node.depends
  , id: node.id
  , name: node.name
  , nodetype: node.nodetype
  , path: node.path
  , index: node.id
  , fixed: "no"
  , fx: (N.null :: N.Nullable Number)
  , fy: (N.null :: N.Nullable Number)
  , vx: 0.0, vy: 0.0, x: 0.0, y: 0.0
  }

datumIsSpagoSimNode :: Datum_ -> SpagoSimNode
datumIsSpagoSimNode = unsafeCoerce

datumIsSpagoLink :: Datum_ -> D3_Link SpagoNodeData SpagoLinkData
datumIsSpagoLink = unsafeCoerce

datumIsGraphNode :: Datum_ -> SpagoSimNode
datumIsGraphNode = unsafeCoerce

getIdFromSpagoSimNode :: Datum_ -> Int
getIdFromSpagoSimNode datum = d.id
  where
    (D3SimNode d) = unsafeCoerce datum

getNameFromSpagoSimNode :: Datum_ -> String
getNameFromSpagoSimNode datum = d.name
  where
    (D3SimNode d) = unsafeCoerce datum

convertFilesToGraphModel :: forall r. 
  { body :: String | r } -> 
  { body :: String | r } -> 
  { body :: String | r } -> 
  { body :: String | r } -> SpagoModel
convertFilesToGraphModel moduleJSON packageJSON lsdepJSON locJSON = 
  makeSpagoGraphModel $ readSpago_Raw_JSON_ moduleJSON.body packageJSON.body lsdepJSON.body locJSON.body

-- TODO use a generic, per-file read for this, and lose the FFI file here
foreign import readSpago_Raw_JSON_ :: String -> String -> String -> String -> Spago_Raw_JSON_

makeSpagoGraphModel :: Spago_Raw_JSON_ -> SpagoModel
makeSpagoGraphModel json = do
  let
    { nodes, links, name2IdMap, id2NameMap, id2NodeMap, id2PackageIDMap } = getGraphJSONData json
    path2LOCMap = M.fromFoldable $ (\o -> Tuple o.path o.loc) <$> json.loc

  { links
  , nodes    : upgradeSpagoNodeData <$> nodes
  , name2IdMap
  , id2NameMap
  , id2NodeMap
  , id2PackageIDMap
  , path2LOCMap
  , graph    : makeGraph nodes
  , positions: M.empty
  , tree     : Nothing
  }

getGraphJSONData :: Spago_Raw_JSON_ -> Spago_Cooked_JSON
getGraphJSONData { packages, modules, lsDeps } = do
  let
    names = (_.key <$> modules) <> (_.key <$> packages)
    ids   = 1 `range` (length names)

    name2IdMap :: M.Map String NodeID
    name2IdMap = M.fromFoldableWith (\v1 v2 -> spy "key collision!!!!: " v1) $ zip names ids

    id2NodeMap :: M.Map NodeID SpagoNodeData
    id2NodeMap = M.fromFoldableWith (\v1 v2 -> spy "key collision!!!!: " v1) $ zip ids nodes

    id2NameMap :: M.Map NodeID String
    id2NameMap = M.fromFoldable $ zip ids names

    id2PackageIDMap :: M.Map NodeID NodeID
    id2PackageIDMap = M.fromFoldable $ catMaybes $ id2Package <$> nodes
      where
        id2Package :: SpagoNodeData -> Maybe (Tuple NodeID NodeID)
        id2Package node = do
          packageID <- case node.nodetype of
                        IsModule  -> getPackage node.path
                        IsPackage -> Just node.id
          pure $ Tuple node.id packageID

    getId :: String -> NodeID
    getId s = fromMaybe 0 (M.lookup s name2IdMap)

    depsMap :: M.Map String { version :: String, repo :: String }
    depsMap = M.fromFoldable $ (\d -> Tuple d.packageName { version: d.version, repo: d.repo.contents } ) <$> lsDeps

    makeLink :: LinkType -> Tuple NodeID NodeID -> SpagoGraphLinkID
    makeLink linktype (Tuple source target) = D3_Link { source, target, linktype }

    makeModuleToPackageLink :: forall r. { id :: NodeID | r } -> Maybe SpagoGraphLinkID
    makeModuleToPackageLink { id }  = do
      packageID <- M.lookup id id2PackageIDMap 
      Just $ D3_Link { source: id, target: packageID, linktype: M2P } 

    foldDepends :: forall r. Array (Tuple NodeID NodeID) -> { key :: String, depends :: Array String | r } -> Array (Tuple NodeID NodeID)
    foldDepends b a = (makeTuple <$> a.depends) <> b    
      where id = getId a.key 
            makeTuple :: String -> Tuple NodeID NodeID
            makeTuple s = Tuple id (getId s)   

    makeNodeFromModuleJSON :: SpagoModuleJSON -> SpagoNodeData
    makeNodeFromModuleJSON m = 
      { id       : getId m.key
      , name     : m.key
      , path     : m.path
      , nodetype : IsModule
      , depends  : getId <$> m.depends
      , fixed    : "no"
      } -- FIXME lookup the m.depends list for ids

    makeNodeFromPackageJSON :: SpagoPackageJSON -> SpagoNodeData
    makeNodeFromPackageJSON m = 
      { id       : getId m.key
      , name     : m.key
      , path
      , nodetype : IsPackage
      , depends  : getId <$> m.depends
      , fixed    : "no"
      } -- FIXME id, package and depends all need lookups
      where
        path = case M.lookup m.key depsMap of
                Nothing -> "error path not found for package key: " <> m.key
                (Just { repo }) -> repo 

    getPackage :: String -> Maybe NodeID
    getPackage path = do
      let pieces = split (Pattern "/") path
      root    <- pieces !! 0
      if root == ".spago" 
      then do 
        package <- pieces !! 1
        M.lookup package name2IdMap
      else Nothing

    moduleLinks        = (makeLink M2M) <$> (foldl foldDepends [] modules)             
    packageLinks       = (makeLink P2P) <$> (foldl foldDepends [] packages)

    packageNodes       = makeNodeFromPackageJSON <$> packages
    moduleNodes        = makeNodeFromModuleJSON  <$> modules

    modulePackageLinks = catMaybes $ makeModuleToPackageLink <$> moduleNodes
    nodes              = moduleNodes <> packageNodes
    links              = moduleLinks <> packageLinks <> modulePackageLinks

  { links, nodes, name2IdMap, id2NameMap, id2NodeMap, id2PackageIDMap }

makeGraph :: Array SpagoNodeData -> Graph NodeID SpagoNodeData
makeGraph nodes = do
  let
    graphMap = foldl addNode M.empty nodes
    addNode :: M.Map NodeID (Tuple SpagoNodeData (S.Set NodeID)) -> SpagoNodeData -> M.Map NodeID (Tuple SpagoNodeData (S.Set NodeID))
    addNode acc node = M.insert node.id (Tuple node depends) acc
      where
        depends :: S.Set Int
        depends = S.fromFoldable node.depends
  fromMap graphMap


type DepPath = Array NodeID
-- type Deps = Array NodeID
type GraphSearchRecord = {
    nodes          :: Array NodeID
  , openDepPaths   :: Array DepPath
  , closedDepPaths :: Array DepPath
  , dependencyTree :: Maybe (Tree NodeID)
}

-- TODO make this generic / independent of the SpagoGraph datatypes and extract
getReachableNodes :: NodeID -> Graph NodeID SpagoNodeData -> GraphSearchRecord
getReachableNodes id graph = go { nodes: [], openDepPaths: [[id]], closedDepPaths: [], dependencyTree: Nothing }
  where
    go :: GraphSearchRecord -> GraphSearchRecord
    go searchRecord@{ openDepPaths: [] } = searchRecord -- bottom out when all open paths are consumed
    go searchRecord = do
      case processNextOpenDepPath searchRecord of
        Nothing     -> searchRecord -- bottom out but....possibly some exceptions to be looked at here
        (Just searchRecord') -> go searchRecord'

    processNextOpenDepPath :: GraphSearchRecord -> Maybe GraphSearchRecord
    processNextOpenDepPath searchRecord = do
      x         <- uncons searchRecord.openDepPaths
      firstID   <- head x.head -- NB we're pushing onto the path, cause head is easier than tail
      firstNode <- G.lookup firstID graph

      let newDeps = 
            filter (\d -> not $ A.elem d searchRecord.nodes) firstNode.depends
          newOpenDepPaths = 
            (\d -> d : x.head) <$> newDeps -- ie [ab] with deps [bc] -> [abc, abd]

      if null newOpenDepPaths
        -- moving the open path we just processed to the list of closedDepPaths
        then Just $ searchRecord { openDepPaths   = x.tail                 
                                 , closedDepPaths = x.head : searchRecord.closedDepPaths}
        -- replace this open path with it's extension(s)
        else Just $ searchRecord { openDepPaths = x.tail <> newOpenDepPaths 
                                 , nodes     = searchRecord.nodes <> newDeps }


findGraphNodeIdFromName :: SpagoModel -> String -> Maybe NodeID
findGraphNodeIdFromName graph name = M.lookup name graph.name2IdMap



-- | boilerplate
instance showNodeType :: Show NodeType where
  show IsModule = "module"
  show IsPackage = "package"
instance showLinkType :: Show LinkType where
  show M2M = "M2M"
  show P2P = "P2P"
  show M2P = "M2P"
  show P2M = "P2M"
