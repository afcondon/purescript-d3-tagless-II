module D3.Examples.Simulation.SpagoPackages where

import Prelude

import Affjax (Error, URL)
import Affjax as AJAX
import Affjax.ResponseFormat as ResponseFormat
import D3.Attributes.Instances (Datum)
import D3.Attributes.Sugar (classed, cx, cy, fill, getWindowWidthHeight, radius, strokeColor, strokeOpacity, strokeWidth, viewBox, x1, x2, y1, y2)
import D3.Examples.Simulation.LesMiserables (GraphLink)
import D3.Interpreter (class D3InterpreterM, append, attach, attachZoom, join)
import D3.Interpreter.D3 (runD3M)
import D3.Interpreter.String (runPrinter)
import D3.Layouts.Simulation (D3ForceLink_, D3ForceNode_, Force(..), ForceName(..), ForceType(..), initSimulation, makeCenterForce, startSimulation_)
import D3.Scales (d3SchemeCategory10S_)
import D3.Selection (D3Selection_, DragBehavior(..), Element(..), Join(..), Keys(..), ScaleExtent(..), ZoomExtent(..), node)
import Data.Array (foldl, (!!))
import Data.Array (fromFoldable) as A
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (tuple2)
import Debug (spy, trace)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Math (sqrt)
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML (window)
import Web.HTML.Window (innerHeight, innerWidth)

-- *********************************************************************************************************************
-- NOTA BENE - these types are a _lie_ as stated in that the Nodes / Links are mutable and are changed when you put them
-- into the simulation, the types given here represent their form AFTER D3 has mutated them
-- *********************************************************************************************************************
type NodeExtension = ( path :: String, package :: String, moduleOrPackage :: String )
type LinkExtension = ( moduleOrPackage :: String )
type GraphNode_ = D3ForceNode_ String NodeExtension
type GraphLink_ = D3ForceLink_ String NodeExtension LinkExtension
type Model = { links :: Array GraphLink_, nodes :: Array GraphNode_ }

-- this is what we can get back from the JS FFI which decodes the output of Spago for us
type Module  = { key :: String, depends :: Array String, path :: String }
type Package = { key :: String, depends :: Array String }
type LsDep   = { packageName :: String, version :: String, repo :: { tag :: String, contents :: URL } }

foreign import makeGraphLinks_ :: forall r id. Array { source :: id, target :: id | r } -> Array GraphLink_
foreign import makeGraphNodes_ :: forall r id. Array { id :: id | r }                   -> Array GraphNode_
-- TODO no error handling at all here RN (OTOH - performant!!)
foreign import readModelData_ :: String -> String -> String -> PreModel

type PreModel = { packages :: Array Package, modules :: Array Module, lsDeps :: Array LsDep } 

convertFilesToModel :: forall r. { body :: String | r } -> { body :: String | r } -> { body :: String | r } -> Model
convertFilesToModel moduleJSON packageJSON lsdepJSON = 
  makeModel $ readModelData_ moduleJSON.body packageJSON.body lsdepJSON.body

makeModel :: PreModel -> Model
makeModel { packages, modules, lsDeps } = do
  let
    depsMap :: M.Map String { version :: String, repo :: String }
    depsMap = M.fromFoldable $ spy "depsMap" $ (\d -> trace { depsMapFn: d } \_ -> Tuple d.packageName { version: d.version, repo: d.repo.contents } ) <$> lsDeps

    makeLink :: String -> Tuple String String -> { source :: String, target :: String, moduleOrPackage :: String }
    makeLink moduleOrPackage (Tuple source target) = { source, target, moduleOrPackage }

    foldDepends :: forall r. Array (Tuple String String) -> { key :: String, depends :: Array String | r } -> Array (Tuple String String)
    foldDepends b a = ((Tuple a.key) <$> a.depends) <> b

    makeNodeFromModule :: Module -> { id :: String, path :: String, package :: String, moduleOrPackage :: String }
    makeNodeFromModule m = { id: m.key, path: m.path, package: getPackage m.path, moduleOrPackage: "module" }

    makeNodeFromPackage :: Package -> { id :: String, path :: String, package :: String, moduleOrPackage :: String }
    makeNodeFromPackage m = { id: m.key, path, package: m.key, moduleOrPackage: "package" } -- TODO package field here is bogus
      where
        path = case M.lookup m.key depsMap of
                Nothing -> "error path not found for package key: " <> m.key
                (Just { repo }) -> repo 

    getPackage :: String -> String
    getPackage path = do
      let pieces = split (Pattern "/") path
      case pieces !! 1 of
        Nothing -> "error package not read correctly from path"
        (Just package) -> package

    moduleLinks = (makeLink "module")   <$> (foldl foldDepends [] modules)             
    moduleNodes = makeNodeFromModule    <$> modules

    packageLinks = (makeLink "package") <$> (foldl foldDepends [] packages)
    packageNodes = makeNodeFromPackage  <$> packages

    links :: Array GraphLink_
    links = makeGraphLinks_ (packageLinks <> moduleLinks)
    nodes :: Array GraphNode_ 
    nodes = makeGraphNodes_ (packageNodes <> moduleNodes) 
  
  { links, nodes }


drawGraph :: Aff Unit
drawGraph = do
  log "Force layout example"
  widthHeight <- liftEffect getWindowWidthHeight
  moduleJSON  <- AJAX.get ResponseFormat.string "http://localhost:1234/modules.json"
  packageJSON <- AJAX.get ResponseFormat.string "http://localhost:1234/packages.json"
  lsdepJSON   <- AJAX.get ResponseFormat.string "http://localhost:1234/lsdeps.jsonlines"
  case convertFilesToModel <$> moduleJSON <*> packageJSON <*> lsdepJSON of
    (Left error) -> log "error" -- $ ?_ error
    (Right graph) -> do
      (_ :: Tuple D3Selection_ Unit) <- liftEffect $ runD3M (enter widthHeight graph)
      printedScript <- liftEffect $ runPrinter (enter widthHeight graph) "Force Layout Script"
      log $ snd printedScript
      log $ fst printedScript
      pure unit

-- | recipe for this force layout graph
enter :: forall m link node selection r. 
  Bind m => 
  D3InterpreterM selection m => 
  Tuple Number Number ->
  { links :: Array link, nodes :: Array node | r } -> 
  m selection -- TODO is it right to return selection_ instead of simulation_? does it matter? 
enter (Tuple w h) model = do
  root       <- attach "div#spago"
  svg        <- root `append` (node Svg   [ viewBox 0.0 0.0 1000.0 1000.0 ] )
  linksGroup <- svg  `append` (node Group [ classed "link", strokeColor "#999", strokeOpacity 0.6 ])
  nodesGroup <- svg  `append` (node Group [ classed "node", strokeColor "#fff", strokeOpacity 1.5 ])

  let forces      = [ makeCenterForce 500.0 500.0
                    , Force (ForceName "charge") ForceMany ]
      simulation_ = initSimulation forces model model.nodes model.links

  links <- join linksGroup $ JoinSimulation {
      element   : Line
    , key       : UseDatumAsKey
    , "data"    : model.links
    , behaviour : [ strokeWidth 1.0 ]
    , simulation: simulation_ -- following config fields are extras for simulation
    , tickName  : "links"
    , onTick    : [ x1 setX1, y1 setY1, x2 setX2, y2 setY2 ]
    , onDrag    : NoDrag
  }

  nodes <- join nodesGroup $ JoinSimulation {
      element   : Circle
    , key       : UseDatumAsKey
    , "data"    : model.nodes
    , behaviour : [ radius 5.0, fill colorByGroup ]
    , simulation: simulation_  -- following config fields are extras for simulation
    , tickName  : "nodes"
    , onTick    : [ cx setCx, cy setCy ]
    , onDrag    : DefaultDrag
  }
  
  svg' <- svg `attachZoom`  { extent   : ZoomExtent { top: 0.0, left: 0.0 , bottom: h, right: w }
                            , scale     : ScaleExtent 1 4 -- wonder if ScaleExtent ctor could be range operator `..`
                            , qualifier : "tree"
                            }

  let _ = startSimulation_ simulation_

  pure svg'

-- this is boilerplate but...typed attribute setters facilitate typeclass based conversions
-- we give the chart our Model type but behind the scenes it is mutated by D3 and additionally
-- which projection of the "Model" is active in each Join varies so we can't have both strong
-- static type representations AND lightweight syntax with JS compatible lambdas (i think)
datumIsGraphLink_ :: Datum -> GraphLink_
datumIsGraphLink_ = unsafeCoerce
datumIsGraphNode_ :: Datum -> GraphNode_
datumIsGraphNode_ = unsafeCoerce

colorByGroup :: Datum -> String
colorByGroup datum = d3SchemeCategory10S_ d.package
  where
    d = datumIsGraphNode_ datum

setX1 :: Datum -> Number
setX1 datum = d.source.x
  where
    d = datumIsGraphLink_ datum
setY1 :: Datum -> Number
setY1 datum = d.source.y
  where
    d = datumIsGraphLink_ datum
setX2 :: Datum -> Number
setX2 datum = d.target.x
  where
    d = datumIsGraphLink_ datum
setY2 :: Datum -> Number
setY2 datum = d.target.y
  where
    d = datumIsGraphLink_ datum
setCx :: Datum -> Number
setCx datum = d.x
  where
    d = datumIsGraphNode_ datum
setCy :: Datum -> Number
setCy datum = d.y
  where
    d = datumIsGraphNode_ datum
