module D3.Examples.Simulation.SpagoPackages where

import Prelude hiding (append,join)

import Affjax (URL)
import Affjax as AJAX
import Affjax.ResponseFormat as ResponseFormat
import D3.Attributes.Instances (Datum, MouseEvent(..))
import D3.Attributes.Sugar (classed, cx, cy, fill, getWindowWidthHeight, on, opacity, radius, strokeColor, strokeOpacity, strokeWidth, text, transform', viewBox, x, x1, x2, y, y1, y2)
import D3.Interpreter (class D3InterpreterM, append, attach, attachZoom, (<+>))
import D3.Interpreter.D3 (runD3M)
import D3.Interpreter.String (runPrinter)
import D3.Layouts.Simulation (D3ForceLink_, D3ForceNode_, Force(..), ForceName(..), ForceType(..), initSimulation, makeCenterForce, startSimulation_, stopSimulation_)
import D3.Scales (d3SchemeCategory10S_)
import D3.Selection (D3Selection_, DragBehavior(..), Element(..), Join(..), Keys(..), SimulationDrag(..), node)
import D3.Zoom (ScaleExtent(..), ZoomExtent(..), ZoomTarget(..))
import Data.Array (catMaybes, foldl, (!!))
import Data.Either (Either(..))
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split)
import Data.Tuple (Tuple(..), fst, snd)
import Debug (spy, trace)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Unsafe.Coerce (unsafeCoerce)

-- *********************************************************************************************************************
-- NOTA BENE - these types are a _lie_ as stated in that the Nodes / Links are mutable and are changed when you put them
-- into the simulation, the types given here represent their form AFTER D3 has mutated them
-- *********************************************************************************************************************
type NodeExtension = ( path :: String, package :: Maybe String, moduleOrPackage :: String )
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

    makeModuleToPackageLink :: forall r. { id :: String, package :: Maybe String | r } -> Maybe { source :: String, target :: String, moduleOrPackage :: String }
    makeModuleToPackageLink { id, package: Just p } = Just { source: id, target: p, moduleOrPackage: "both"}
    makeModuleToPackageLink { id, package: Nothing } = Nothing

    foldDepends :: forall r. Array (Tuple String String) -> { key :: String, depends :: Array String | r } -> Array (Tuple String String)
    foldDepends b a = ((Tuple a.key) <$> a.depends) <> b

    makeNodeFromModule :: Module -> { id :: String, path :: String, package :: Maybe String, moduleOrPackage :: String }
    makeNodeFromModule m = { id: m.key, path: m.path, package: getPackage m.path, moduleOrPackage: "module" }

    makeNodeFromPackage :: Package -> { id :: String, path :: String, package :: Maybe String, moduleOrPackage :: String }
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

    links :: Array GraphLink_
    links = makeGraphLinks_ (packageLinks <> moduleLinks <> modulePackageLinks)
    -- links = makeGraphLinks_ packageLinks
    -- links = makeGraphLinks_ moduleLinks
    nodes :: Array GraphNode_ 
    nodes = makeGraphNodes_ (packageNodes <> moduleNodes) 
    -- nodes = makeGraphNodes_ packageNodes
    -- nodes = makeGraphNodes_ moduleNodes
  
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
      (_ :: Tuple D3Selection_ Unit) <- liftEffect $ runD3M (graphScript widthHeight graph)
      printedScript <- liftEffect $ runPrinter (graphScript widthHeight graph) "Force Layout Script"
      log $ snd printedScript
      log $ fst printedScript
      pure unit

-- | recipe for this force layout graph
graphScript :: forall m link node selection r. 
  Bind m => 
  D3InterpreterM selection m => 
  Tuple Number Number ->
  { links :: Array link, nodes :: Array node | r } -> 
  m selection -- TODO is it right to return selection_ instead of simulation_? does it matter? 
graphScript (Tuple w h) model = do
  root       <- attach "div#spago"
  svg        <- root `append` (node Svg   [ viewBox 0.0 0.0 650.0 650.0 ] )
  linksGroup <- svg  `append` (node Group [ classed "link", strokeColor "#999", strokeOpacity 0.6 ])
  nodesGroup <- svg  `append` (node Group [ classed "node" ])

  let forces      = [ makeCenterForce 325.0 325.0
                    , Force (ForceName "charge") ForceMany ]
      simulation_ = initSimulation forces model model.nodes model.links

  links <- linksGroup <+> JoinSimulation {
      element   : Line
    , key       : UseDatumAsKey
    , "data"    : model.links
    , behaviour : [ strokeWidth 1.0 ]
    , simulation: simulation_ -- following config fields are extras for simulation
    , tickName  : "links"
    , onTick    : [ x1 setX1, y1 setY1, x2 setX2, y2 setY2 ]
    , onDrag    : SimulationDrag NoDrag
  }

  nodes <- nodesGroup <+> JoinSimulation {
      element   : Group
    , key       : UseDatumAsKey
    , "data"    : model.nodes
    , behaviour : [ classed "node", transform' translateNode ]
    , simulation: simulation_  -- following config fields are extras for simulation
    , tickName  : "nodes"
    , onTick    : [ transform' translateNode  ]
    , onDrag    : SimulationDrag DefaultDrag
  }

  circle  <- nodes `append` (node Circle [ radius (\d -> if (datumIsGraphNode_ d).moduleOrPackage == "module" then 5.0 else 10.0)
                                         , fill colorByGroup
                                         , on MouseEnter (\e d t -> stopSimulation_ simulation_) 
                                         , on MouseLeave (\e d t -> startSimulation_ simulation_)
                                         , on MouseClick (\e d t -> spy "click callback" $ unit)
                                         ]) 
  labels' <- nodes `append` (node Text [ classed "label", fill "white", x 1.0, y 1.0, text (\d -> (datumIsGraphNode_ d).id)]) 
  labels  <- nodes `append` (node Text [ classed "label", fill "black", text (\d -> (datumIsGraphNode_ d).id)]) 
  
  svg' <- svg `attachZoom`  { extent    : ZoomExtent { top: 0.0, left: 0.0 , bottom: h, right: w }
                            , scale     : ScaleExtent 1 4 -- wonder if ScaleExtent ctor could be range operator `..`
                            , qualifier : "tree"
                            , target    : SelfTarget
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

translateNode :: Datum -> String
translateNode datum = "translate(" <> show d.x <> "," <> show d.y <> ")"
  where d = datumIsGraphNode_ datum


colorByGroup :: Datum -> String
colorByGroup datum = d3SchemeCategory10S_ (fromMaybe "unknown" d.package)
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
