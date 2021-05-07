module D3.Data.File.Spago where

import Data.Array
import Prelude

import Affjax (URL)
import D3.Data.Foreign (Datum_)
import D3.FFI (D3ForceLink_, D3ForceNode_, GraphModel_, makeGraphLinks_, makeGraphNodes_)
import Data.Graph (Graph, fromMap, outEdges)
import Data.List as L
import Data.Map (lookup)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as S
import Data.String (Pattern(..), split)
import Data.Tuple (Tuple(..))
import Debug (spy)
import Unsafe.Coerce (unsafeCoerce)

-- TODO This is all specific to the Spago output, so break out into file handling modules later
type SpagoModule  = { key :: String, depends :: Array String, path :: String }
type SpagoPackage = { key :: String, depends :: Array String }
type SpagoLsDep   = { packageName :: String, version :: String, repo :: { tag :: String, contents :: URL } }

-- TODO no error handling at all here RN (OTOH - performant!!)
type SpagoDataJSON_ = { packages :: Array SpagoPackage, modules :: Array SpagoModule, lsDeps :: Array SpagoLsDep } 


-- *********************************************************************************************************************
-- NOTA BENE - these types are a _lie_ as stated in that the Nodes / Links are mutable and are changed when you put them
-- into the simulation, the types given here represent their form AFTER D3 has mutated them
-- *********************************************************************************************************************
type NodeExtension   = ( name :: String, path :: String, depends :: Array String, package :: Maybe String, moduleOrPackage :: NodeType )
type LinkExtension   = ( moduleOrPackage :: LinkType )

-- | local type synonyms for the node and link NATIVE types
type NodeID          = Int
data NodeType        = IsModule | IsPackage
instance showNodeType :: Show NodeType where
  show IsModule = "module"
  show IsPackage = "package"
data LinkType        = M2M | P2P | M2P | P2M
instance showLinkType :: Show LinkType where
  show M2M = "M2M"
  show P2P = "P2P"
  show M2P = "M2P"
  show P2M = "P2M"
type SpagoGraph      = Graph NodeID SpagoNode
type SpagoGraphNode_ = D3ForceNode_ NodeID NodeExtension
type SpagoGraphLink_ = D3ForceLink_ NodeID NodeExtension LinkExtension

type SpagoNode       = { id :: NodeID, name :: String, path :: String, package :: Maybe Int, moduleOrPackage :: NodeType, depends :: Array NodeID }
type SpagoLink       = { source :: NodeID, target :: NodeID, moduleOrPackage :: LinkType }
type SpagoRawModel   = { links :: Array SpagoLink, nodes :: Array SpagoNode, root :: Maybe NodeID }

datumIsGraphLink_ :: Datum_ -> SpagoGraphLink_
datumIsGraphLink_ = unsafeCoerce
datumIsGraphNode_ :: Datum_ -> SpagoGraphNode_
datumIsGraphNode_ = unsafeCoerce

convertFilesToGraphModel :: forall r. { body :: String | r } -> { body :: String | r } -> { body :: String | r } -> GraphModel_ SpagoGraphLink_ SpagoGraphNode_
convertFilesToGraphModel moduleJSON packageJSON lsdepJSON = 
  makeSpagoGraphModel $ readSpagoDataJSON_ moduleJSON.body packageJSON.body lsdepJSON.body

foreign import readSpagoDataJSON_ :: String -> String -> String -> SpagoDataJSON_

makeSpagoGraphModel :: SpagoDataJSON_ -> GraphModel_ SpagoGraphLink_ SpagoGraphNode_
makeSpagoGraphModel json = do
  let
    raw = makeSpagoGraphModel' json
    links :: Array SpagoGraphLink_
    links = makeGraphLinks_ $ raw.links

    nodes :: Array SpagoGraphNode_ 
    nodes = makeGraphNodes_ raw.nodes

    graph :: Graph NodeID SpagoNode
    graph = makeGraph raw.nodes

    rootConnected = spy "outEdges: " $ outEdges <$> raw.root <*> (Just graph)
  
  { links, nodes }

makeSpagoGraphModel' :: SpagoDataJSON_ -> SpagoRawModel
makeSpagoGraphModel' { packages, modules, lsDeps } = do
  let
    idMap :: M.Map String Int
    idMap = M.fromFoldableWith (\v1 v2 -> spy "key collision!!!!: " v1) $ zip names ids
      where
        names = (_.key <$> modules) <> (_.key <$> packages)
        ids   = 1 `range` (length names)

    getId :: String -> Int
    getId s = fromMaybe 0 (M.lookup s idMap)

    depsMap :: M.Map String { version :: String, repo :: String }
    depsMap = M.fromFoldable $ (\d -> Tuple d.packageName { version: d.version, repo: d.repo.contents } ) <$> lsDeps

    makeLink :: LinkType -> Tuple NodeID NodeID -> SpagoLink
    makeLink moduleOrPackage (Tuple source target) = { source, target, moduleOrPackage }

    makeModuleToPackageLink :: forall r. { id :: NodeID, package :: Maybe NodeID | r } -> Maybe SpagoLink
    makeModuleToPackageLink { id, package: Just p }  = Just { source: id, target: p, moduleOrPackage: M2P } 
    makeModuleToPackageLink { id, package: Nothing } = Nothing

    foldDepends :: forall r. Array (Tuple NodeID NodeID) -> { key :: String, depends :: Array String | r } -> Array (Tuple NodeID NodeID)
    foldDepends b a = (makeTuple <$> a.depends) <> b    
      where id = getId a.key 
            makeTuple :: String -> Tuple NodeID NodeID
            makeTuple s = Tuple id (getId s)   

    makeNodeFromModule :: SpagoModule -> SpagoNode
    makeNodeFromModule m = { id: getId m.key, name: m.key, path: m.path, package: getPackage m.path, moduleOrPackage: IsModule, depends: getId <$> m.depends } -- FIXME lookup the m.depends list for ids

    makeNodeFromPackage :: SpagoPackage -> SpagoNode
    makeNodeFromPackage m = { id: getId m.key, name: m.key, path, package: M.lookup m.key idMap, moduleOrPackage: IsPackage, depends: getId <$> m.depends } -- FIXME id, package and depends all need lookups
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
    moduleNodes = makeNodeFromModule   <$> modules

    packageLinks = (makeLink P2P)      <$> (foldl foldDepends [] packages)
    packageNodes = makeNodeFromPackage <$> packages

    modulePackageLinks = catMaybes $ makeModuleToPackageLink <$> moduleNodes

  { links: moduleLinks <> packageLinks <> modulePackageLinks
  , nodes: moduleNodes <> packageNodes
  , root: M.lookup "Main" idMap }

makeGraph :: Array SpagoNode -> SpagoGraph
makeGraph nodes = do
  let
    graphMap = foldl addNode M.empty nodes
    addNode :: M.Map NodeID (Tuple SpagoNode (S.Set NodeID)) -> SpagoNode -> M.Map NodeID (Tuple SpagoNode (S.Set NodeID))
    addNode acc node = M.insert node.id (Tuple node depends) acc
      where
        depends = S.fromFoldable node.depends
  fromMap graphMap
