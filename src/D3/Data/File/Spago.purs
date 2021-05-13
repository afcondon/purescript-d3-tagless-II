module D3.Data.File.Spago where

import D3.Node

import Affjax (URL)
import D3.Data.Foreign (Datum_)
import D3.Data.Types (PointXY)
import Data.Array (catMaybes, filter, foldl, head, length, null, range, uncons, zip, (!!), (:))
import Data.Array as A
import Data.Graph (Graph, fromMap)
import Data.Graph as G
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as S
import Data.String (Pattern(..), split)
import Data.Tree (Tree)
import Data.Tuple (Tuple(..))
import Debug (spy)
import Prelude (class Show, bind, not, ($), (<$>), (<>), (==))
import Unsafe.Coerce (unsafeCoerce)

-- TODO This is all specific to the Spago output, so break out into file handling modules later
type SpagoModuleJSON  = { key :: String, depends :: Array String, path :: String }
type SpagoPackageJSON = { key :: String, depends :: Array String }
type SpagoLsDepJSON   = { packageName :: String, version :: String, repo :: { tag :: String, contents :: URL } }
type SpagoLOCJSON     = { loc :: Number, path :: String }

-- TODO no error handling at all here RN (OTOH - performant!!)
type Spago_Raw_JSON_ = { 
    packages :: Array SpagoPackageJSON
  , modules  :: Array SpagoModuleJSON
  , lsDeps   :: Array SpagoLsDepJSON
  , loc      :: Array SpagoLOCJSON } 


-- *********************************************************************************************************************
-- NOTA BENE - these types are a _lie_ as stated in that the Nodes / Links are mutable and are changed when you put them
-- into the simulation, the types given here represent their form AFTER D3 has mutated them
-- *********************************************************************************************************************
type SpagoNodeData = { 
    name            :: String
  , id              :: NodeID
  , path            :: String
  , depends         :: Array NodeID
  , package         :: Maybe NodeID
  , moduleOrPackage :: NodeType
  , x               :: Number -- needed because pos set (as tree) BEFORE data put into Simulation
  , y               :: Number -- needed because pos set (as tree) BEFORE data put into Simulation
}

type SpagoLink          = ( moduleOrPackage :: LinkType )

-- | local type synonyms for the node and link NATIVE types
data NodeType           = IsModule | IsPackage
data LinkType           = M2M | P2P | M2P | P2M

type SpagoGraphNode_    = D3_Simulation_Node SpagoNodeData
type SpagoGraphLinkID_  = D3_Simulation_LinkID SpagoLink               -- this is the link before swapping IDs for obj refs
type SpagoGraphLinkObj_ = D3_Simulation_Link SpagoGraphNode_ SpagoLink -- this is the link after swapping IDs for obj refs

type SpagoJSONData    = { 
    links      :: Array SpagoGraphLinkID_
  , nodes      :: Array SpagoNodeData
  , name2IdMap :: M.Map String NodeID
}

type TreeWithRoot = Tuple NodeID (D3_Hierarchy_Node_XY NodeID)

type SpagoModel = { 
    links      :: Array SpagoGraphLinkID_ -- each ID will get swizzled for a SpagoGraphLinkObj_ when simulation initialized
  , nodes      :: Array SpagoNodeData     -- will get embedded in D3Simulation_Node when simulation initialized
  , graph      :: Graph NodeID SpagoNodeData
  , name2IdMap :: M.Map String NodeID
  , loc        :: M.Map String Number
  , positions  :: M.Map NodeID PointXY
  , tree       :: Maybe TreeWithRoot
}

datumIsGraphLink_ :: Datum_ -> SpagoGraphLinkObj_
datumIsGraphLink_ = unsafeCoerce
datumIsGraphNode_ :: Datum_ -> SpagoGraphNode_
datumIsGraphNode_ = unsafeCoerce
datumIsGraphNodeData_ :: Datum_ -> SpagoNodeData
datumIsGraphNodeData_ d = (unsafeCoerce d)."data"

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
    { nodes, links, name2IdMap } = getGraphJSONData json
    loc = M.fromFoldable $ (\o -> Tuple o.path o.loc) <$> json.loc

  { links, nodes, name2IdMap, loc
  , graph    : makeGraph nodes
  , positions: M.empty
  , tree     : Nothing
  }

getGraphJSONData :: Spago_Raw_JSON_ -> SpagoJSONData
getGraphJSONData { packages, modules, lsDeps } = do
  let
    idMap :: M.Map String NodeID
    idMap = M.fromFoldableWith (\v1 v2 -> spy "key collision!!!!: " v1) $ zip names ids
      where
        names = (_.key <$> modules) <> (_.key <$> packages)
        ids   = 1 `range` (length names)

    getId :: String -> NodeID
    getId s = fromMaybe 0 (M.lookup s idMap)

    depsMap :: M.Map String { version :: String, repo :: String }
    depsMap = M.fromFoldable $ (\d -> Tuple d.packageName { version: d.version, repo: d.repo.contents } ) <$> lsDeps

    makeLink :: LinkType -> Tuple NodeID NodeID -> SpagoGraphLinkID_
    makeLink moduleOrPackage (Tuple source target) = { source, target, moduleOrPackage }

    makeModuleToPackageLink :: forall r. { id :: NodeID, package :: Maybe NodeID | r } -> Maybe SpagoGraphLinkID_
    makeModuleToPackageLink { id, package: Just p }  = Just { source: id, target: p, moduleOrPackage: M2P } 
    makeModuleToPackageLink { id, package: Nothing } = Nothing

    foldDepends :: forall r. Array (Tuple NodeID NodeID) -> { key :: String, depends :: Array String | r } -> Array (Tuple NodeID NodeID)
    foldDepends b a = (makeTuple <$> a.depends) <> b    
      where id = getId a.key 
            makeTuple :: String -> Tuple NodeID NodeID
            makeTuple s = Tuple id (getId s)   

    makeNodeFromModuleJSON :: SpagoModuleJSON -> SpagoNodeData
    makeNodeFromModuleJSON m = 
      { id: getId m.key
      , name: m.key
      , path: m.path
      , package: getPackage m.path
      , moduleOrPackage: IsModule
      , depends: getId <$> m.depends
      , x: 0.0 -- don't have any position info yet
      , y: 0.0 -- don't have any position info yet
      } -- FIXME lookup the m.depends list for ids

    makeNodeFromPackageJSON :: SpagoPackageJSON -> SpagoNodeData
    makeNodeFromPackageJSON m = 
      { id: getId m.key
      , name: m.key
      , path
      , package: M.lookup m.key idMap
      , moduleOrPackage: IsPackage
      , depends: getId <$> m.depends
      , x: 0.0 -- don't have any position info yet
      , y: 0.0 -- don't have any position info yet
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
        M.lookup package idMap
      else Nothing

    moduleLinks = (makeLink M2M)       <$> (foldl foldDepends [] modules)             
    moduleNodes = makeNodeFromModuleJSON   <$> modules

    packageLinks = (makeLink P2P)      <$> (foldl foldDepends [] packages)
    packageNodes = makeNodeFromPackageJSON <$> packages

    modulePackageLinks = catMaybes $ makeModuleToPackageLink <$> moduleNodes

  { links: moduleLinks <> packageLinks <> modulePackageLinks
  , nodes: moduleNodes -- <> packageNodes
  , name2IdMap: idMap }

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

type GraphSearchRecord = {
    nodes          :: Array NodeID
  , openPaths      :: Array Path
  , closedPaths    :: Array Path
  , dependencyTree :: Maybe (Tree NodeID)
}

type Path = Array NodeID
type Deps = Array NodeID

-- TODO make this generic / independent of the SpagoGraph datatypes and extract
getReachableNodes :: NodeID -> Graph NodeID SpagoNodeData -> GraphSearchRecord
getReachableNodes id graph = go { nodes: [], openPaths: [[id]], closedPaths: [], dependencyTree: Nothing }
  where
    go :: GraphSearchRecord -> GraphSearchRecord
    go searchRecord@{ openPaths: [] } = searchRecord -- bottom out when all open paths are consumed
    go searchRecord = do
      case processNextOpenPath searchRecord of
        Nothing     -> searchRecord -- bottom out but....possibly some exceptions to be looked at here
        (Just searchRecord') -> go searchRecord'

    processNextOpenPath :: GraphSearchRecord -> Maybe GraphSearchRecord
    processNextOpenPath searchRecord = do
      -- let _ = spy "open paths: " searchRecord.openPaths
      x         <- uncons searchRecord.openPaths
      firstID   <- head x.head -- NB we're pushing onto the path, cause head is easier than tail
      firstNode <- G.lookup firstID graph
      -- let _ = spy "working on this node now: " firstNode

      let newDeps = -- spy "newDeps: " $ 
            filter (\d -> not $ A.elem d searchRecord.nodes) firstNode.depends
          newOpenPaths = -- spy "newOpenPaths: " $
            (\d -> d : x.head) <$> newDeps -- ie [ab] with deps [bc] -> [abc, abd]

      if null newOpenPaths
        -- moving the open path we just processed to the list of closedPaths
        then Just $ searchRecord { openPaths   = x.tail                 
                                 , closedPaths = x.head : searchRecord.closedPaths}
        -- replace this open path with it's extension(s)
        else Just $ searchRecord { openPaths = x.tail <> newOpenPaths 
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
