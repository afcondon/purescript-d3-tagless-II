module D3.Examples.Spago.Model where

import D3.Examples.Spago.Files (SpagoModuleJSON, SpagoPackageJSON, Spago_Raw_JSON_, readSpago_Raw_JSON_)
import D3.Node (D3SimulationRow, D3TreeRow, D3_Link(..), D3_SimulationNode(..), EmbeddedData, NodeID)

import D3.Data.Types (Datum_)
import Data.Array (catMaybes, foldl, length, range, zip, (!!))
import Data.Graph (Graph, fromMap)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable, null) as N
import Data.Nullable (notNull)
import Data.Set as S
import Data.String (Pattern(..), split)
import Data.Tuple (Tuple(..))
import Debug (spy)
import Prelude (class Eq, class Show, bind, pure, ($), (<$>), (<>), (==))
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

moduleRadius = 5.0 :: Number 
packageRadius = 50.0 :: Number
packageForceRadius = 50.0 :: Number

type Spago_Cooked_JSON    = { 
    links        :: Array SpagoGraphLinkID
  , nodes        :: Array SpagoNodeData
  , name_2_ID    :: M.Map String NodeID
  , id_2_Name    :: M.Map NodeID String
  , id_2_Node    :: M.Map NodeID SpagoNodeData
  , id_2_Package :: M.Map NodeID NodeID
}

-- Model data types specialized with inital data
data NodeType        = IsModule | IsPackage
data PackageRelation = InPackage | OutPackage
data LinkType        = M2M_Tree | M2M_Graph | P2P | M2P PackageRelation
data Pinned          = Pinned | Floating -- might be more categories here, "pinned connectd", "pinned unused" whatever...

type Deps = Array NodeID

type SpagoNodeRow row = ( 
    id       :: NodeID
  , path     :: String
  , depends  :: { full       :: Deps
                , tree       :: Deps
                , inPackage  :: Deps
                , outPackage :: Deps
                }
  , nodetype :: NodeType
  , name     :: String
  , pinned   :: Pinned
  | row )
type SpagoNodeData    = { | SpagoNodeRow () }

type SpagoTreeNode    = D3TreeRow       (EmbeddedData SpagoNodeData + ())
type SpagoSimNode     = D3SimulationRow (             SpagoNodeRow  + ())

type SpagoLinkData     = ( linktype :: LinkType )
type SpagoGraphLinkID  = D3_Link NodeID SpagoLinkData
type SpagoGraphLinkObj = D3_Link SpagoNodeData SpagoLinkData

type SpagoModel = { 
    links           :: Array SpagoGraphLinkID  -- each ID will get swizzled for a SpagoGraphLinkObj_ when simulation initialized
  , prunedTreeLinks :: Array SpagoGraphLinkID  -- there are so many of these that we only use them when hovering enabled
  , nodes           :: Array SpagoSimNode      -- already upgraded to simnode as a result of positioning when building the model
  , graph           :: Graph NodeID SpagoNodeData
  , tree            :: Maybe (Tuple NodeID SpagoTreeNode)
  , maps            :: { name_2_ID    :: M.Map String NodeID
                       , id_2_Name    :: M.Map NodeID String
                       , id_2_Node    :: M.Map NodeID SpagoNodeData
                       , id_2_Package :: M.Map NodeID NodeID
                       , path_2_LOC   :: M.Map String Number
                       , id_2_XYLeaf  :: M.Map NodeID { x :: Number, y :: Number, isLeaf :: Boolean }
                       }
}

setXY :: SpagoSimNode -> { x :: Number, y :: Number, isLeaf :: Boolean } -> SpagoSimNode
setXY (D3SimNode node) { x, y, isLeaf: true }  = D3SimNode (node { x = x, y = y, pinned = Floating })
setXY (D3SimNode node) { x, y, isLeaf: false } = D3SimNode (node { fx = notNull x, fy = notNull y, pinned = Pinned })

upgradeSpagoNodeData :: SpagoNodeData -> SpagoSimNode
upgradeSpagoNodeData node = D3SimNode { 
    depends: node.depends
  , id: node.id
  , name: node.name
  , nodetype: node.nodetype
  , path: node.path
  , index: node.id
  , pinned: Floating
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


makeSpagoGraphModel :: Spago_Raw_JSON_ -> SpagoModel
makeSpagoGraphModel json = do
  let
    { nodes, links, name_2_ID, id_2_Name, id_2_Node, id_2_Package } = getGraphJSONData json
    path_2_LOC = M.fromFoldable $ (\o -> Tuple o.path o.loc) <$> json.loc

  { links
  , nodes    : upgradeSpagoNodeData <$> nodes
  , graph    : makeGraph nodes
  , tree     : Nothing  -- not present in the JSON, has to be calculated, if possible
  , prunedTreeLinks: [] -- not present in the JSON, has to be calculated, if possible
  , maps     : { name_2_ID
               , id_2_Name
               , id_2_Node
               , id_2_Package
               , path_2_LOC
               , id_2_XYLeaf: M.empty
               }
  }

getGraphJSONData :: Spago_Raw_JSON_ -> Spago_Cooked_JSON
getGraphJSONData { packages, modules, lsDeps } = do
  let
    names = (_.key <$> modules) <> (_.key <$> packages)
    ids   = 1 `range` (length names)

    name_2_ID :: M.Map String NodeID
    name_2_ID = M.fromFoldableWith (\v1 v2 -> spy "key collision!!!!: " v1) $ zip names ids

    id_2_Node :: M.Map NodeID SpagoNodeData
    id_2_Node = M.fromFoldableWith (\v1 v2 -> spy "key collision!!!!: " v1) $ zip ids nodes

    id_2_Name :: M.Map NodeID String
    id_2_Name = M.fromFoldable $ zip ids names

    id_2_Package :: M.Map NodeID NodeID
    id_2_Package = M.fromFoldable $ catMaybes $ id2Package <$> nodes
      where
        id2Package :: SpagoNodeData -> Maybe (Tuple NodeID NodeID)
        id2Package node = do
          packageID <- case node.nodetype of
                        IsModule  -> getPackage node.path
                        IsPackage -> Just node.id
          pure $ Tuple node.id packageID

    getId :: String -> NodeID
    getId s = fromMaybe 0 (M.lookup s name_2_ID)

    depsMap :: M.Map String { version :: String, repo :: String }
    depsMap = M.fromFoldable $ (\d -> Tuple d.packageName { version: d.version, repo: d.repo.contents } ) <$> lsDeps

    makeLink :: LinkType -> Tuple NodeID NodeID -> SpagoGraphLinkID
    makeLink linktype (Tuple source target) = D3_Link { source, target, linktype }

    makeModuleToPackageLink :: forall r. { id :: NodeID | r } -> Maybe SpagoGraphLinkID
    makeModuleToPackageLink { id }  = do
      packageID <- M.lookup id id_2_Package 
      Just $ D3_Link { source: id, target: packageID, linktype: M2P OutPackage } -- TODO not checking if it's InPackage at all at all

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
      , pinned   : Floating
      , depends  :  { full: (getId <$> m.depends)
                    , tree: []
                    , inPackage: []
                    , outPackage: []
                    }  
      } -- FIXME lookup the m.depends list for ids

    makeNodeFromPackageJSON :: SpagoPackageJSON -> SpagoNodeData
    makeNodeFromPackageJSON m = 
      { id       : getId m.key
      , name     : m.key
      , path
      , nodetype : IsPackage
      , pinned   : Floating
      , depends  :  { full: (getId <$> m.depends)
                    , tree: []
                    , inPackage: []
                    , outPackage: []
                    } 
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
        M.lookup package name_2_ID
      else Nothing

    moduleLinks        = (makeLink M2M_Graph) <$> (foldl foldDepends [] modules)             
    packageLinks       = (makeLink P2P) <$> (foldl foldDepends [] packages)

    packageNodes       = makeNodeFromPackageJSON <$> packages
    moduleNodes        = makeNodeFromModuleJSON  <$> modules

    modulePackageLinks = catMaybes $ makeModuleToPackageLink <$> moduleNodes
    nodes              = moduleNodes <> packageNodes
    links              = moduleLinks <> packageLinks <> modulePackageLinks

  { links, nodes, name_2_ID, id_2_Name, id_2_Node, id_2_Package }

makeGraph :: Array SpagoNodeData -> Graph NodeID SpagoNodeData
makeGraph nodes = do
  let
    graphMap = foldl addNode M.empty nodes
    addNode :: M.Map NodeID (Tuple SpagoNodeData (S.Set NodeID)) -> SpagoNodeData -> M.Map NodeID (Tuple SpagoNodeData (S.Set NodeID))
    addNode acc node = M.insert node.id (Tuple node depends) acc
      where
        depends :: S.Set Int
        depends = S.fromFoldable node.depends.full
  fromMap graphMap



findGraphNodeIdFromName :: SpagoModel -> String -> Maybe NodeID
findGraphNodeIdFromName graph name = M.lookup name graph.maps.name_2_ID



-- | boilerplate
instance showNodeType :: Show NodeType where
  show IsModule = "module"
  show IsPackage = "package"
instance showLinkType :: Show LinkType where
  show M2M_Tree  = "M2M-Tree"
  show M2M_Graph = "M2M-Graph"
  show P2P = "P2P"
  show (M2P InPackage) = "in-package dependency"
  show (M2P OutPackage) = "out-package dependency"

derive instance eqPinned :: Eq Pinned