module D3.Data.File.Spago where

import Data.Array
import Prelude

import Affjax (URL)
import D3.Data.Foreign (Datum_)
import D3.FFI (D3ForceLink_, D3ForceNode_, GraphModel_, makeGraphLinks_, makeGraphNodes_)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.Tuple (Tuple(..))
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
type NodeExtension = ( path :: String, depends :: Array String, package :: Maybe String, moduleOrPackage :: String )
type LinkExtension = ( moduleOrPackage :: String )

-- | local type synonyms for the node and link NATIVE types
type SpagoGraphNode_ = D3ForceNode_ String NodeExtension
type SpagoGraphLink_ = D3ForceLink_ String NodeExtension LinkExtension

datumIsGraphLink_ :: Datum_ -> SpagoGraphLink_
datumIsGraphLink_ = unsafeCoerce
datumIsGraphNode_ :: Datum_ -> SpagoGraphNode_
datumIsGraphNode_ = unsafeCoerce

convertFilesToGraphModel :: forall r. { body :: String | r } -> { body :: String | r } -> { body :: String | r } -> GraphModel_ SpagoGraphLink_ SpagoGraphNode_
convertFilesToGraphModel moduleJSON packageJSON lsdepJSON = 
  makeSpagoGraphModel $ readSpagoDataJSON_ moduleJSON.body packageJSON.body lsdepJSON.body

foreign import readSpagoDataJSON_ :: String -> String -> String -> SpagoDataJSON_

makeSpagoGraphModel :: SpagoDataJSON_ -> GraphModel_ SpagoGraphLink_ SpagoGraphNode_
makeSpagoGraphModel { packages, modules, lsDeps } = do
  let
    depsMap :: M.Map String { version :: String, repo :: String }
    depsMap = M.fromFoldable $ (\d -> Tuple d.packageName { version: d.version, repo: d.repo.contents } ) <$> lsDeps

    makeLink :: String -> Tuple String String -> { source :: String, target :: String, moduleOrPackage :: String }
    makeLink moduleOrPackage (Tuple source target) = { source, target, moduleOrPackage }

    makeModuleToPackageLink :: forall r. { id :: String, package :: Maybe String | r } -> Maybe { source :: String, target :: String, moduleOrPackage :: String }
    makeModuleToPackageLink { id, package: Just p } = Just { source: id, target: p, moduleOrPackage: "both"}
    makeModuleToPackageLink { id, package: Nothing } = Nothing

    foldDepends :: forall r. Array (Tuple String String) -> { key :: String, depends :: Array String | r } -> Array (Tuple String String)
    foldDepends b a = ((Tuple a.key) <$> a.depends) <> b

    makeNodeFromModule :: SpagoModule -> { id :: String, path :: String, package :: Maybe String, moduleOrPackage :: String }
    makeNodeFromModule m = { id: m.key, path: m.path, package: getPackage m.path, moduleOrPackage: "module" }

    makeNodeFromPackage :: SpagoPackage -> { id :: String, path :: String, package :: Maybe String, moduleOrPackage :: String }
    makeNodeFromPackage m = { id: m.key, path, package: Just m.key, moduleOrPackage: "package" } -- TODO package field here is bogus
      where
        path = case M.lookup m.key depsMap of
                Nothing -> "error path not found for package key: " <> m.key
                (Just { repo }) -> repo 

    getPackage :: String -> Maybe String
    getPackage path = do
      let pieces = split (Pattern "/") path
      root    <- pieces !! 0
      case root of
        ".spago" -> pieces !! 1
        "src"    -> pure "local"
        _        -> Nothing

    moduleLinks = (makeLink "module")   <$> (foldl foldDepends [] modules)             
    moduleNodes = makeNodeFromModule    <$> modules

    packageLinks = (makeLink "package") <$> (foldl foldDepends [] packages)
    packageNodes = makeNodeFromPackage  <$> ( [{ key: "local", depends: [] }, { key: "psci-support", depends: [] }] <> packages)

    modulePackageLinks = catMaybes $ makeModuleToPackageLink <$> moduleNodes

    links :: Array SpagoGraphLink_
    links = makeGraphLinks_ $ moduleLinks <> modulePackageLinks -- (packageLinks <> moduleLinks <> modulePackageLinks)

    nodes :: Array SpagoGraphNode_ 
    nodes = makeGraphNodes_ (packageNodes <> moduleNodes) 
  
  { links, nodes }