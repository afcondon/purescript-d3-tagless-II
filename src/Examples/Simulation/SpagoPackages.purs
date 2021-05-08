module D3.Examples.Simulation.SpagoPackages where

import Prelude hiding (append,join)

import Affjax as AJAX
import Affjax.ResponseFormat as ResponseFormat
import D3.Attributes.Sugar (classed, fill, getWindowWidthHeight, on, radius, strokeColor, strokeOpacity, text, transform', viewBox, x, x1, x2, y, y1, y2)
import D3.Data.File.Spago (NodeExtension, NodeID, NodeType(..), Path, SpagoCookedModel, SpagoGraphLink_, SpagoGraphNode_, LinkExtension, convertFilesToGraphModel, datumIsGraphLink_, datumIsGraphNode_, findGraphNodeIdFromName, getReachableTree)
import D3.Data.Types (D3Selection_, Datum_, Element(..), MouseEvent(..))
import D3.FFI (GraphModel_, D3ForceLink_, pinNodeWithID, startSimulation_, stopSimulation_)
import D3.Interpreter (class D3InterpreterM, append, attach, attachZoom, (<+>))
import D3.Interpreter.D3 (runD3M)
import D3.Interpreter.String (runPrinter)
import D3.Layouts.Simulation (Force(..), ForceName(..), ForceType(..), initSimulation)
import D3.Scales (d3SchemeCategory10S_)
import D3.Selection (DragBehavior(..), Join(..), Keys(..), SimulationDrag(..), node)
import D3.Zoom (ScaleExtent(..), ZoomExtent(..), ZoomTarget(..))
import Data.Array (concatMap, cons, elem, filter, foldl, fromFoldable, length, reverse, sort)
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.List as L
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as S
import Data.Tuple (Tuple(..), fst, snd)
import Debug (spy, trace)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Unsafe.Coerce (unsafeCoerce)


highlightNeighborhood :: GraphModel_ SpagoGraphLink_ SpagoGraphNode_ -> NodeID -> Unit
highlightNeighborhood { links } nodeId = markAsSpotlit_ nodeId sources targets
  where
    sources = foldl (\acc l -> if l.target.id == nodeId then (cons l.source.id acc) else acc) [] links
    targets = foldl (\acc l -> if l.source.id == nodeId then (cons l.target.id acc) else acc) [] links

foreign import markAsSpotlit_   :: NodeID -> Array NodeID -> Array NodeID -> Unit
foreign import removeSpotlight_ :: NodeID -> Array NodeID -> Array NodeID -> Unit

drawGraph :: Aff Unit
drawGraph = do
  log "Force layout example"
  widthHeight <- liftEffect getWindowWidthHeight
  moduleJSON  <- AJAX.get ResponseFormat.string "http://localhost:1234/modules.json"
  packageJSON <- AJAX.get ResponseFormat.string "http://localhost:1234/packages.json"
  lsdepJSON   <- AJAX.get ResponseFormat.string "http://localhost:1234/lsdeps.jsonlines"
  case convertFilesToGraphModel <$> moduleJSON <*> packageJSON <*> lsdepJSON of
    (Left error)  -> log "error converting spago json file inputs"
    (Right graph) -> do
      let graph' = treeReduction graph

      (_ :: Tuple D3Selection_ Unit) <- liftEffect $ runD3M (graphScript widthHeight graph')
      printedScript <- liftEffect $ runPrinter (graphScript widthHeight graph') "Force Layout Script"
      log $ snd printedScript
      log $ fst printedScript
      pure unit

treeReduction :: SpagoCookedModel -> SpagoCookedModel
treeReduction graph = do
  case (flip getReachableTree graph.graph) <$> (findGraphNodeIdFromName graph "Main") of
    Nothing -> graph -- no change
    (Just r) -> do
      let treelinks = spy "treelinks" $ makeTreeLinks (pathsAsLists r.closedPaths)
          isTreeLink :: SpagoGraphLink_ -> Boolean -- it's not yet converted to 
          isTreeLink l = (Tuple l.sourceID l.targetID) `elem` treelinks
          links     = filter isTreeLink graph.links
          nodes     = filter (\n -> (n.id `elem` r.reachableNodes)) graph.nodes
          _         = trace { fn: "treeReduction", noOfLinksBefore: length graph.links, noOfLinksAfter: length links, noOfNodesBefore: length graph.nodes } \_ -> unit
      graph { links = links, nodes = graph.nodes }


path2Tuples :: L.List (Tuple NodeID NodeID) -> L.List NodeID -> L.List (Tuple NodeID NodeID)
path2Tuples acc Nil     = acc
path2Tuples acc (x:Nil) = acc
path2Tuples acc (s:t:tail) = path2Tuples ((Tuple s t):acc) (t:tail)

pathsAsLists :: Array (Array NodeID) -> L.List (L.List NodeID)
pathsAsLists paths = L.fromFoldable ((L.fromFoldable <<< reverse) <$> paths) -- because pattern matching lists is so much nicer for path2Tuples

makeTreeLinks :: L.List (L.List NodeID) -> Array (Tuple NodeID NodeID)
makeTreeLinks closedPaths = do
  let
    linkTuples = (L.foldl path2Tuples Nil) closedPaths
  fromFoldable $ S.fromFoldable linkTuples -- removes the duplicates while building  

-- | recipe for this force layout graph
graphScript :: forall m link node selection r. 
  Bind m => 
  D3InterpreterM selection m => 
  Tuple Number Number ->
  { links :: Array link, nodes :: Array node | r } -> 
  m selection -- TODO is it right to return selection_ instead of simulation_? think it would vary by script but Tuple selection simulation would also work as a pattern
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
  labels' <- nodes `append` (node Text [ classed "label",  x 0.2, y 0.2, text (\d -> (datumIsGraphNode_ d).name)]) 
  
  svg' <- svg `attachZoom`  { extent    : ZoomExtent { top: 0.0, left: 0.0 , bottom: h, right: w }
                            , scale     : ScaleExtent 0.2 2.0 -- wonder if ScaleExtent ctor could be range operator `..`
                            , qualifier : "tree"
                            , target    : SelfTarget
                            }
  let
    -- _ = nanNodes_ $ unsafeCoerce model.nodes
    _ = pinNodeWithID model.nodes "Main" 0.0 0.0
    _ = startSimulation_ simulation_

  pure svg'


-- this is boilerplate but...typed attribute setters facilitate typeclass based conversions
-- we give the chart our Model type but behind the scenes it is mutated by D3 and additionally
-- which projection of the "Model" is active in each Join varies so we can't have both strong
-- static type representations AND lightweight syntax with JS compatible lambdas (i think)
-- TODO move coerce for well defined (ie shared) types to FFI, try to use Row machinery to eliminate need for this or tighten up the type safety
moduleRadius = 5.0 :: Number 
packageRadius = 50.0 :: Number
packageForceRadius = 50.0 :: Number

chooseRadius :: Datum_ -> Number
chooseRadius datum = do
  let d = datumIsGraphNode_ datum
  case d.moduleOrPackage of
    IsModule   -> moduleRadius
    IsPackage -> packageRadius

chooseRadiusFn :: Datum_ -> Number
chooseRadiusFn datum = do
  let d = datumIsGraphNode_ datum
  case d.moduleOrPackage of
    IsModule  -> moduleRadius
    IsPackage -> packageRadius + packageForceRadius

nodeClass :: Datum_ -> String
nodeClass datum = do
  let d = datumIsGraphNode_ datum
  show d.moduleOrPackage

linkClass :: Datum_ -> String
linkClass datum = do
  let d = datumIsGraphLink_ datum
  show d.moduleOrPackage

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
