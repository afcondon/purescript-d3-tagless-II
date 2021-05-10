module D3.Examples.Simulation.SpagoPackages where

import D3.Attributes.Sugar
import Prelude hiding (append,join)

import Affjax as AJAX
import Affjax.ResponseFormat as ResponseFormat
import D3.Data.File.Spago (GraphSearchRecord, LinkExtension, NodeExtension, NodeID, NodeType(..), Path, SpagoCookedModel, SpagoGraphLink_, SpagoGraphNode_, convertFilesToGraphModel, datumIsGraphLink_, datumIsGraphNode_, findGraphNodeIdFromName, getReachableNodes)
import D3.Data.Types (D3HierarchicalNode(..), D3HierarchicalNode_, D3Selection_, Datum_, Element(..), Index_, MouseEvent(..), PointXY, TreeModel, datumIsTreeNode, labelName, makeD3TreeJSONFromTreeID)
import D3.FFI (D3ForceLink_, GraphModel_, descendants_, hNodeHeight_, hasChildren_, hierarchyFromJSON_, initTree_, links_, pinNode, pinNodeWithID, startSimulation_, stopSimulation_, treeMinMax_, treeSetNodeSize_, treeSetRoot_, treeSetSeparation_, treeSetSize_)
import D3.FFI.Config (defaultForceCenterConfig, defaultForceCollideConfig, defaultForceManyConfig, defaultForceRadialConfig, defaultForceRadialFixedConfig, defaultForceXConfig, defaultForceYConfig)
import D3.Interpreter (class D3InterpreterM, append, attach, attachZoom, (<+>))
import D3.Interpreter.D3 (runD3M)
import D3.Interpreter.String (runPrinter)
import D3.Layouts.Hierarchical (positionXY, radialLink, radialSeparation, verticalLink)
import D3.Layouts.Simulation (Force(..), ForceType(..), initSimulation)
import D3.Scales (d3SchemeCategory10S_)
import D3.Selection (DragBehavior(..), Join(..), Keys(..), SimulationDrag(..), node)
import D3.Zoom (ScaleExtent(..), ZoomExtent(..), ZoomTarget(..))
import Data.Array (concatMap, cons, elem, filter, find, foldl, fromFoldable, length, partition, reverse, sort)
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.List as L
import Data.Map (Map, empty)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable)
import Data.Set as S
import Data.Traversable (sequence)
import Data.Tree (Tree(..))
import Data.Tuple (Tuple(..), fst, snd)
import Debug (debugger, spy, trace)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Math (cos, pi, sin)
import Math (log, pow, sqrt) as Math
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
  locJSON     <- AJAX.get ResponseFormat.string "http://localhost:1234/loc.json"
  case convertFilesToGraphModel <$> moduleJSON <*> packageJSON <*> lsdepJSON <*> locJSON of
    (Left error)  -> log "error converting spago json file inputs"
    (Right graph) -> do
      let graph' = treeReduction graph

      (_ :: Tuple D3Selection_ Unit) <- liftEffect $ runD3M (graphScript widthHeight graph')

      (_ :: Tuple D3Selection_ Unit) <- liftEffect $ runD3M (spagoTreeScript widthHeight graph'.treeRoot_)
       
      printedScript <- liftEffect $ runPrinter (graphScript widthHeight graph') "Force Layout Script"
      log $ snd printedScript
      log $ fst printedScript
      pure unit

treeReduction :: SpagoCookedModel -> SpagoCookedModel
treeReduction graph = do
  case (flip getReachableNodes graph.graph) <$> (findGraphNodeIdFromName graph "Main") of
    Nothing -> graph -- no change
    (Just r) -> do
      let onlyTreelinks = spy "onlyTreelinks" $ makeTreeLinks (pathsAsLists r.closedPaths)
          isTreeLink :: SpagoGraphLink_ -> Boolean -- it's not yet converted to 
          isTreeLink l = (Tuple l.sourceID l.targetID) `elem` onlyTreelinks

          treelinks    = partition isTreeLink graph.links
          treenodes    = partition (\n -> (n.id `elem` r.reachableNodes) || 
                                           n.name == "Main") -- FIXME not "Main" but "whatever we gave as root of tree"
                                   graph.nodes

          idTree          = buildTree "Main" graph treelinks.yes
          jsontree        = makeD3TreeJSONFromTreeID <$> idTree
          rootTree        = hierarchyFromJSON_ <$> jsontree
          layout          = ((initTree_ unit) `treeSetSize_` [ 2.0 * pi, 500.0 ]) `treeSetSeparation_` radialSeparation
          laidOutRoot_    = (treeSetRoot_ layout) <$> rootTree
          positionMap     = getPositionMap laidOutRoot_
          positionedNodes = fromMaybe treenodes.yes $ setNodePositionsRadial treenodes.yes positionMap -- FIXME ugly code

          -- { xMin, xMax, yMin, yMax } = treeMinMax_ <$> laidOutRoot_

          _            = trace { fn: "treeReduction"
                               , noOfLinksBefore: length graph.links
                               , noOfLinksAfter: length treelinks.yes
                               , noOfNodesBefore: length graph.nodes
                               , noOfNodesAfter: length treenodes.yes
                               } \_ -> unit
      graph { links = treelinks.yes, nodes = positionedNodes, treeRoot_ = laidOutRoot_, positions = positionMap }

-- for radial positioning we treat x as angle and y as radius
radialTranslate :: PointXY -> PointXY
radialTranslate p = 
  let angle  = p.x
      radius = p.y
      x = radius * cos angle
      y = radius * sin angle
  in { x, y }

setNodePositionsRadial :: Array SpagoGraphNode_ -> Maybe (M.Map NodeID PointXY) -> Maybe (Array SpagoGraphNode_)
setNodePositionsRadial nodes maybeMap = do
  positionMap <- maybeMap
  let updateXY :: SpagoGraphNode_ -> SpagoGraphNode_
      updateXY node = 
        case M.lookup node.id positionMap of
          Nothing -> node
          -- (Just p) -> node { x = p.x, y = p.y }
          -- (Just p) -> pinNode node (radialTranslate p)
          (Just p) -> 
            let { x,y } = radialTranslate p
            in node { x = x, y = y }
  Just $ updateXY <$> nodes


-- TODO there's a clue in the name of this newtype
-- see TODO for D3HierarchicalNode d v, needs to be a row type
newtype EgregiousHackTODO = EgregiousHackTODO {
    "data"   :: { name :: Int, package :: String }
  , depth    :: Int
  , height   :: Int
  , parent   :: Nullable EgregiousHackTODO
  , children :: Array EgregiousHackTODO
  , x        :: Number
  , y        :: Number
}
getPositionMap :: Maybe D3HierarchicalNode_ -> Maybe (Map NodeID PointXY)
getPositionMap hierarchy = do
  root <- hierarchy
  let (nodes :: Array EgregiousHackTODO) = unsafeCoerce $ descendants_ root
      foldFn acc (EgregiousHackTODO n) = M.insert (n."data".name) { x: n.x, y: n.y, package: n."data".package } acc
  Just $ unsafeCoerce $ foldl foldFn empty nodes

buildTree :: String -> SpagoCookedModel -> Array SpagoGraphLink_ -> Maybe (Tree NodeID)
buildTree rootName model treelinks = do
  let 
    linksWhoseSourceIs :: NodeID -> L.List NodeID
    linksWhoseSourceIs id = L.fromFoldable $ (_.targetID) <$> (filter (\l -> l.sourceID == id) treelinks)

    go :: NodeID -> Tree NodeID
    go childID = Node childID (go <$> linksWhoseSourceIs childID)

  rootID <- M.lookup rootName model.name2IdMap
  Just $ Node rootID (go <$> (linksWhoseSourceIs rootID))

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
  { links :: Array link
  , nodes :: Array node
  , loc :: M.Map String Number
  , positions :: Maybe (M.Map NodeID PointXY) | r } -> 
  m selection -- TODO is it right to return selection_ instead of simulation_? think it would vary by script but Tuple selection simulation would also work as a pattern
graphScript (Tuple w h) model = do
  root       <- attach "div#spago"
  svg        <- root `append` (node Svg   [ viewBox (-w / 2.0) (-h / 2.0) w h ] )
  linksGroup <- svg  `append` (node Group [ classed "links", strokeColor "#999", strokeOpacity 0.6 ])
  nodesGroup <- svg  `append` (node Group [ classed "nodes" ])

  let forces      = [ Force $ ForceManyBody    $ (defaultForceManyConfig "charge") { strength = -100.0 }
                    , Force $ ForceCollide     $  defaultForceCollideConfig "collide" (\d -> chooseRadiusFn d)
                    , Force $ ForceX           $ (defaultForceXConfig "x") { strength = 0.05 }
                    , Force $ ForceY           $ (defaultForceYConfig "y") { strength = 0.05 }
                    , Force $ ForceCenter      $ (defaultForceCenterConfig "center") { strength = -1.0 }
                    -- , Force $ ForceRadialFixed $ defaultForceRadialFixedConfig "radial" 500.0
                    ]
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

  circle  <- nodes `append` (node Circle [ radius (chooseRadius model.loc) 
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

chooseRadius :: Map String Number -> Datum_ -> Number
chooseRadius locMap datum = do
  let d = datumIsGraphNode_ datum
  case d.moduleOrPackage of
    -- IsModule   -> moduleRadius
    IsModule   -> Math.sqrt (fromMaybe 10.0 $ M.lookup d.path locMap)
    IsPackage -> packageRadius

chooseRadiusFn :: Datum_ -> Index_ -> Number
chooseRadiusFn datum index = do
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



-- | **************************************************************************************************************
-- | draw the spago graph - only the tree part - as a radial tree
-- | **************************************************************************************************************


spagoTreeScript :: forall m v selection. Bind m => D3InterpreterM selection m => 
  Tuple Number Number -> Maybe D3HierarchicalNode_ -> m selection
spagoTreeScript (Tuple width height) Nothing = do
  attach "div#spagotree"            -- FIXME this is bogus but saves messing about with the Maybe root_ in the drawGraph script               

spagoTreeScript (Tuple width height) (Just root_) = do
  let 
    -- configure dimensions
    columns                    = 3.0  -- 3 columns, set in the grid CSS in index.html
    gap                        = 10.0 -- 10px set in the grid CSS in index.html
    svgWH                      = { width : ((width - ((columns - 1.0) * gap)) / columns)
                                 , height: height / 2.0 } -- 2 rows
    numberOfLevels             = (hNodeHeight_ root_) + 1.0
    spacing                    = { interChild: 120.0, interLevel: svgWH.height / numberOfLevels}
    layoutFn                   = (initTree_ unit) `treeSetNodeSize_` [ spacing.interChild, spacing.interLevel ]
    laidOutRoot_               = layoutFn `treeSetRoot_` root_
    { xMin, xMax, yMin, yMax } = treeMinMax_ laidOutRoot_
    xExtent                    = xMax - xMin -- ie if tree spans from -50 to 200, it's extent is 250
    yExtent                    = yMax - yMin -- ie if tree spans from -50 to 200, it's extent is 250


  -- "script"
  root       <- attach "div#spagotree"                           
  svg        <- root `append` (node Svg  [ viewBox (-svgWH.width / 2.0) (-20.0) svgWH.width svgWH.height ] )          
  container  <- svg  `append` (node Group [ fontFamily      "sans-serif"
                                          , fontSize        18.0
                                          ])
  links      <- container `append` (node Group [ classed "links"])
  nodes      <- container `append` (node Group [ classed "nodes"])

  theLinks_  <- links <+> Join {
      element   : Path
    , key       : UseDatumAsKey
    , "data"    : links_ root_
    , behaviour : [ strokeWidth   1.5
                  , strokeColor   "black"
                  , strokeOpacity 0.4
                  , fill          "none"
                  , radialLink _.x _.y
                  ]
  }

  nodeJoin_  <- nodes <+> Join {
      element   : Group
    , key       : UseDatumAsKey
    , "data"    : descendants_ root_
    -- there could be other stylistic stuff here but the transform is key structuring component
    , behaviour : [ transform [ radialRotateCommon, radialTreeTranslate, rotateRadialLabels ] ]
  }

  theNodes <- nodeJoin_ `append` 
                (node Circle  [ fill         "red"
                              , radius       2.5
                              , strokeColor "white"
                              ])

  theLabels <- nodeJoin_ `append`
                (node Text  [ dy         0.31
                            , x          (\datum -> if textDirection datum then 6.0 else (-6.0))
                            , textAnchor (\datum -> if textDirection datum then "start" else "end")
                            , text       labelName
                            , fill       "#555"
                            ])
                            
  pure svg

radialRotate :: Number -> String
radialRotate x = show $ (x * 180.0 / pi - 90.0)

radialRotateCommon :: forall d v. D3HierarchicalNode d v -> String
radialRotateCommon (D3HierarchicalNode d) = "rotate(" <> radialRotate d.x <> ")"

radialTreeTranslate :: forall d v. D3HierarchicalNode d v -> String
radialTreeTranslate (D3HierarchicalNode d) = "translate(" <> show d.y <> ",0)"

rotateRadialLabels :: forall d v. D3HierarchicalNode d v -> String
rotateRadialLabels (D3HierarchicalNode d) = -- TODO replace with nodeIsOnRHS 
  "rotate(" <> if d.x >= pi 
  then "180" <> ")" 
  else "0" <> ")"

nodeIsOnRHS :: Datum_ -> Boolean
nodeIsOnRHS d = node.x < pi
  where (D3HierarchicalNode node) = datumIsTreeNode d

textDirection = \d -> hasChildren_ d == nodeIsOnRHS d
