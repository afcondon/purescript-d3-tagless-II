module D3.Examples.Simulation.SpagoPackages where

import Prelude hiding (append,join)

import Affjax (URL)
import Affjax as AJAX
import Affjax.ResponseFormat as ResponseFormat
import D3.Attributes.Sugar (classed, fill, getWindowWidthHeight, on, radius, strokeColor, strokeOpacity, text, transform', viewBox, x, x1, x2, y, y1, y2)
import D3.Data.Types (D3Selection_, Datum_, Element(..), MouseEvent(..))
import D3.FFI (D3ForceLink_, D3ForceNode_, startSimulation_, stopSimulation_)
import D3.Interpreter (class D3InterpreterM, append, attach, attachZoom, (<+>))
import D3.Interpreter.D3 (runD3M)
import D3.Interpreter.String (runPrinter)
import D3.Layouts.Simulation (Force(..), ForceName(..), ForceType(..), initSimulation)
import D3.Scales (d3SchemeCategory10S_)
import D3.Selection (DragBehavior(..), Join(..), Keys(..), SimulationDrag(..), node)
import D3.Zoom (ScaleExtent(..), ZoomExtent(..), ZoomTarget(..))
import Data.Array (catMaybes, find, foldl, (!!), (:))
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
type NodeExtension = ( path :: String, depends :: Array String, package :: Maybe String, moduleOrPackage :: String )
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

highlightNeighborhood :: Model -> String -> Unit
highlightNeighborhood { links } nodeId = markAsSpotlit_ nodeId sources targets
  where
    sources = foldl (\acc l -> if l.target.id == nodeId then l.source.id:acc else acc) [] links
    targets = foldl (\acc l -> if l.source.id == nodeId then l.target.id:acc else acc) [] links

foreign import markAsSpotlit_   :: String -> Array String -> Array String -> Unit
foreign import removeSpotlight_ :: String -> Array String -> Array String -> Unit

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
    links = makeGraphLinks_ $ moduleLinks <> modulePackageLinks -- (packageLinks <> moduleLinks <> modulePackageLinks)
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
  svg        <- root `append` (node Svg   [ viewBox (-w / 2.0) (-h / 2.0) w h ] )
  linksGroup <- svg  `append` (node Group [ classed "links", strokeColor "#999", strokeOpacity 0.6 ])
  nodesGroup <- svg  `append` (node Group [ classed "nodes" ])

  let forces      = [ Force (ForceName "charge")  ForceMany
                    , Force (ForceName "collide") (ForceCollide (\d -> chooseRadiusFn d) ) ]
      simulation_ = initSimulation forces model model.nodes model.links

  links <- linksGroup <+> JoinSimulation {
      element   : Line
    , key       : UseDatumAsKey
    , "data"    : model.links
    , behaviour : [ classed linkClass ] -- default invisible in CSS unless marked "visible"
    , simulation: simulation_ -- following config fields are extras for simulation
    , tickName  : "links"
    , onTick    : [ x1 setX1, y1 setY1, x2 setX2, y2 setY2 ]
    , onDrag    : SimulationDrag NoDrag
  }

  nodes <- nodesGroup <+> JoinSimulation {
      element   : Group
    , key       : UseDatumAsKey
    , "data"    : model.nodes
    , behaviour : [ classed nodeClass, transform' translateNode ]
    , simulation: simulation_  -- following config fields are extras for simulation
    , tickName  : "nodes"
    , onTick    : [ transform' translateNode  ]
    , onDrag    : SimulationDrag DefaultDrag
  }

  circle  <- nodes `append` (node Circle [ radius chooseRadius 
                                         , fill colorByGroup
                                         , on MouseEnter (\e d t -> stopSimulation_ simulation_) 
                                         , on MouseLeave (\e d t -> startSimulation_ simulation_)
                                         , on MouseClick (\e d t -> highlightNeighborhood (unsafeCoerce model) (datumIsGraphNode_ d).id)
                                         ]) 
  labels' <- nodes `append` (node Text [ classed "label",  x 0.2, y 0.2, text (\d -> (datumIsGraphNode_ d).id)]) 
  
  svg' <- svg `attachZoom`  { extent    : ZoomExtent { top: 0.0, left: 0.0 , bottom: h, right: w }
                            , scale     : ScaleExtent 0.2 2.0 -- wonder if ScaleExtent ctor could be range operator `..`
                            , qualifier : "tree"
                            , target    : SelfTarget
                            }
  let
    -- _ = nanNodes_ $ unsafeCoerce model.nodes
    _ = pinNode model.nodes "Main" 0.0 0.0
    _ = startSimulation_ simulation_

  pure svg'

-- TODO move to FFI
foreign import pinNode_   :: Number -> Number -> GraphNode_ -> Unit
foreign import unpinNode_ :: GraphNode_ -> Unit
foreign import nanNodes_ :: Array GraphNode_ -> Unit
pinNode :: forall node. Array node -> String -> Number -> Number -> Unit
pinNode nodes nodeName fx fy = unit
  where
    _ = (pinNode_ fx fy) <$> find (\node -> node.id == nodeName) (unsafeCoerce nodes)

-- this is boilerplate but...typed attribute setters facilitate typeclass based conversions
-- we give the chart our Model type but behind the scenes it is mutated by D3 and additionally
-- which projection of the "Model" is active in each Join varies so we can't have both strong
-- static type representations AND lightweight syntax with JS compatible lambdas (i think)
-- TODO move coerce for well defined (ie shared) types to FFI, try to use Row machinery to eliminate need for this or tighten up the type safety
datumIsGraphLink_ :: Datum_ -> GraphLink_
datumIsGraphLink_ = unsafeCoerce
datumIsGraphNode_ :: Datum_ -> GraphNode_
datumIsGraphNode_ = unsafeCoerce

moduleRadius = 5.0 :: Number 
packageRadius = 50.0 :: Number
packageForceRadius = 50.0 :: Number

chooseRadius :: Datum_ -> Number
chooseRadius datum = do
  let d = datumIsGraphNode_ datum
  case d.moduleOrPackage of
    "module" -> moduleRadius
    "package" -> packageRadius
    _ -> 10.0

chooseRadiusFn :: Datum_ -> Number
chooseRadiusFn datum = do
  let d = datumIsGraphNode_ datum
  case d.moduleOrPackage of
    "module" -> moduleRadius
    "package" -> packageRadius + packageForceRadius
    _ -> 10.0

nodeClass :: Datum_ -> String
nodeClass datum = do
  let d = datumIsGraphNode_ datum
  d.moduleOrPackage

linkClass :: Datum_ -> String
linkClass datum = do
  let d = datumIsGraphLink_ datum
  d.moduleOrPackage

translateNode :: Datum_ -> String
translateNode datum = "translate(" <> show d.x <> "," <> show d.y <> ")"
  where d = datumIsGraphNode_ datum


colorByGroup :: Datum_ -> String
colorByGroup datum = d3SchemeCategory10S_ (fromMaybe "unknown" d.package)
  where
    d = datumIsGraphNode_ datum

setX1 :: Datum_ -> Number
setX1 datum = d.source.x
  where
    d = datumIsGraphLink_ datum
setY1 :: Datum_ -> Number
setY1 datum = d.source.y
  where
    d = datumIsGraphLink_ datum
setX2 :: Datum_ -> Number
setX2 datum = d.target.x
  where
    d = datumIsGraphLink_ datum
setY2 :: Datum_ -> Number
setY2 datum = d.target.y
  where
    d = datumIsGraphLink_ datum
setCx :: Datum_ -> Number
setCx datum = d.x
  where
    d = datumIsGraphNode_ datum
setCy :: Datum_ -> Number
setCy datum = d.y
  where
    d = datumIsGraphNode_ datum
