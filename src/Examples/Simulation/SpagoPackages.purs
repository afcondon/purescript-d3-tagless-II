module D3.Examples.Simulation.SpagoPackages where

import Affjax as AJAX
import Affjax.ResponseFormat as ResponseFormat
import D3.Attributes.Sugar (classed, dy, fill, fontFamily, fontSize, getWindowWidthHeight, on, radius, strokeColor, strokeOpacity, strokeWidth, text, textAnchor, transform, transform', viewBox, x, x1, x2, y, y1, y2)
import D3.Data.File.Spago (NodeType(..), SpagoModel, SpagoSimNode, SpagoTreeNode, convertFilesToGraphModel, datumIsGraphNode, datumIsSpagoLink, datumIsSpagoSimNode, findGraphNodeIdFromName, getIdFromSpagoSimNode, getNameFromSpagoSimNode, getReachableNodes, setXY)
import D3.Data.Types (D3Selection_, Datum_, Element(..), Index_, MouseEvent(..), PointXY, TreeType(..), makeD3TreeJSONFromTreeID)
import D3.Examples.Tree.Configure (datumIsTreeNode)
import D3.FFI (descendants_, getLayout, hNodeHeight_, hasChildren_, hierarchyFromJSON_, links_, runLayoutFn_, treeMinMax_, treeSetSeparation_, treeSetSize_, treeSortForTree_Spago)
import D3.FFI.Config (defaultConfigSimulation, defaultForceCenterConfig, defaultForceCollideConfig, defaultForceLinkConfig, defaultForceManyConfig, defaultForceXConfig, defaultForceYConfig)
import D3.Interpreter (class D3InterpreterM, append, attach, attachZoom, (<+>))
import D3.Interpreter.D3 (runD3M)
import D3.Interpreter.String (runPrinter)
import D3.Layouts.Hierarchical (radialLink, radialSeparation)
import D3.Layouts.Simulation (Force(..), ForceType(..), initSimulation)
import D3.Node (D3_Link(..), D3_SimulationNode(..), D3_TreeNode(..), D3_XY, NodeID, getNodeX, getNodeY, getSourceX, getSourceY, getTargetX, getTargetY)
import D3.Scales (d3SchemeCategory10N_)
import D3.Selection (DragBehavior(..), Join(..), Keys(..), SimulationDrag(..), node)
import D3.Zoom (ScaleExtent(..), ZoomExtent(..), ZoomTarget(..))
import Data.Array (cons, elem, filter, foldl, fromFoldable, partition, reverse)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.List as L
import Data.Map (Map, empty)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (nan)
import Data.Set as S
import Data.Tree (Tree(..))
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Math (cos, pi, sin)
import Math (sqrt) as Math
import Prelude (class Bind, class Eq, Unit, bind, discard, negate, pure, show, unit, ($), (*), (+), (-), (/), (<), (<$>), (<*>), (<<<), (<>), (==), (>=), (||))
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

-- | no sigs on these because they're currently called using unsafeCoerce to account for the fact that the link IDs
-- | have been swizzled for their underlying objects
highlightNeighborhood { links } nodeId = markAsSpotlit_ nodeId sources targets
  where
    sources = foldl (\acc (D3_Link l) -> if l.target.id == nodeId then (cons l.source.id acc) else acc) [] links
    targets = foldl (\acc (D3_Link l) -> if l.source.id == nodeId then (cons l.target.id acc) else acc) [] links

unhighlightNeighborhood { links } nodeId = removeSpotlight_ unit
  where
    sources = foldl (\acc l -> if l.target.id == nodeId then (cons l.source.id acc) else acc) [] links
    targets = foldl (\acc l -> if l.source.id == nodeId then (cons l.target.id acc) else acc) [] links

foreign import markAsSpotlit_   :: NodeID -> Array NodeID -> Array NodeID -> Unit
foreign import removeSpotlight_ :: Unit -> Unit

drawGraph :: Aff Unit
drawGraph = do
  log "Force layout example"
  (Tuple width height) <- liftEffect getWindowWidthHeight
  moduleJSON  <- AJAX.get ResponseFormat.string "http://localhost:1234/modules.json"
  packageJSON <- AJAX.get ResponseFormat.string "http://localhost:1234/packages.json"
  lsdepJSON   <- AJAX.get ResponseFormat.string "http://localhost:1234/lsdeps.jsonlines"
  locJSON     <- AJAX.get ResponseFormat.string "http://localhost:1234/loc.json"
  case convertFilesToGraphModel <$> moduleJSON <*> packageJSON <*> lsdepJSON <*> locJSON of
    (Left error)  -> log "error converting spago json file inputs"
    (Right graph) -> do
      let graph' = 
            case findGraphNodeIdFromName graph "Main" of
              Nothing       -> graph -- couldn't find root of tree so just skip this
              (Just rootID) -> treeReduction graph rootID

      (_ :: Tuple D3Selection_ Unit) <- liftEffect $ runD3M (graphScript (Tuple width height) graph')
      (_ :: Tuple D3Selection_ Unit) <- liftEffect $ runD3M (spagoTreeScript (Tuple (width/2.0) height) graph')
       
      printedScript <- liftEffect $ runPrinter (graphScript (Tuple width height) graph') "Force Layout Script"
      log $ snd printedScript
      log $ fst printedScript
      pure unit

-- TODO make this generic and extract from Spago example to library
treeReduction :: SpagoModel -> NodeID -> SpagoModel
treeReduction model rootID = do
      let reachable       = getReachableNodes rootID model.graph
          onlyTreelinks   = makeTreeLinks (pathsAsLists reachable.closedDepPaths)
          treelinks       = partition (\(D3_Link l) -> (Tuple l.source l.target) `elem` onlyTreelinks) model.links
          treenodes       = partition (\(D3SimNode n) -> (n.id `elem` reachable.nodes) || n.id == rootID) model.nodes
          layout          = ((getLayout TidyTree) `treeSetSize_` [ 2.0 * pi, 900.0 ]) `treeSetSeparation_` radialSeparation
          idTree          = buildTree rootID model treelinks.yes
          jsontree        = makeD3TreeJSONFromTreeID idTree model.id2NodeMap
          rootTree        = hierarchyFromJSON_       jsontree
          sortedTree      = treeSortForTree_Spago    rootTree
          laidOutRoot_    = (runLayoutFn_ layout)    sortedTree
          positionMap     = getPositionMap           laidOutRoot_
          positionedNodes = setNodePositionsRadial   treenodes.yes positionMap
          unpositionedNodes = setForPhyllotaxis  <$> treenodes.no
          tree            = Tuple rootID laidOutRoot_

      model { links = treelinks.yes, nodes = positionedNodes <> unpositionedNodes, tree = Just tree, positions = positionMap }

-- for radial positioning we treat x as angle and y as radius
radialTranslate :: PointXY -> PointXY
radialTranslate p = 
  let angle  = p.x
      radius = p.y
      x = radius * cos angle
      y = radius * sin angle
  in { x, y }

setNodePositionsRadial :: Array SpagoSimNode -> M.Map NodeID { x :: Number, y :: Number, isLeaf :: Boolean } -> Array SpagoSimNode
setNodePositionsRadial nodes positionMap = do
  let 
    updateXY (D3SimNode node) = do
      case M.lookup node.id positionMap of
        Nothing -> D3SimNode node
        (Just p) -> 
          let { x,y } = radialTranslate { x: p.x, y: p.y }
          in (D3SimNode node) `setXY` { x, y, isLeaf: p.isLeaf }
  updateXY <$> nodes

setForPhyllotaxis :: SpagoSimNode -> SpagoSimNode
setForPhyllotaxis (D3SimNode d) = D3SimNode $ d { x = nan }

getPositionMap :: SpagoTreeNode -> Map NodeID { x :: Number, y :: Number, isLeaf :: Boolean }
getPositionMap root = foldl (\acc (D3TreeNode n) -> M.insert n.data.id { x: n.x, y: n.y, isLeaf: (unsafeCoerce n).data.isLeaf } acc) empty (descendants_ root) -- TODO coerce here is pure hackery

buildTree :: forall r. NodeID -> SpagoModel -> Array (D3_Link NodeID r) -> Tree NodeID
buildTree rootID model treelinks = do
  let 
    unwrap :: D3_Link NodeID r -> { source :: NodeID, target :: NodeID | r }
    unwrap (D3_Link d) = d
    linksWhoseSourceIs :: NodeID -> L.List NodeID
    linksWhoseSourceIs id = L.fromFoldable $ (_.target) <$> (filter (\l -> l.source == id) (unwrap <$> treelinks))

    go :: NodeID -> Tree NodeID
    go childID = Node childID (go <$> linksWhoseSourceIs childID)

  Node rootID (go <$> (linksWhoseSourceIs rootID))

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
graphScript :: forall m selection. 
  Bind m => 
  D3InterpreterM selection m => 
  Tuple Number Number ->
  SpagoModel -> 
  m selection -- TODO is it right to return selection_ instead of simulation_? think it would vary by script but Tuple selection simulation would also work as a pattern
graphScript (Tuple w h) model = do
  root       <- attach "div#spago"
  svg        <- root `append` (node Svg   [ viewBox (-w / 2.0) (-h / 2.0) w h ] )
  centerDot  <- svg  `append` (node Circle [ radius 20.0, fill "red", x (w / 2.0), y h ])
  linksGroup <- svg  `append` (node Group [ classed "links", strokeColor "#999" ])
  nodesGroup <- svg  `append` (node Group [ classed "nodes" ])

  let forces      = [ Force $ ForceManyBody    $ (defaultForceManyConfig "charge") { strength = -100.0 }
                    , Force $ ForceCollide     $  defaultForceCollideConfig "collide" (\d -> chooseRadiusFn d)
                    , Force $ ForceX           $ (defaultForceXConfig "x") { strength = 0.05 }
                    , Force $ ForceY           $ (defaultForceYConfig "y") { strength = 0.05 }
                    , Force $ ForceCenter      $ (defaultForceCenterConfig "center") { strength = -1.0 }
                    , Force $ ForceLink        $ (defaultForceLinkConfig "links" model.links (\d -> d.id))
                    -- , Force $ ForceRadialFixed $ defaultForceRadialFixedConfig "radial" 500.0
                    ]
      { simulation, nodes } = initSimulation forces model.nodes defaultConfigSimulation
      -- _ = pinNodeMatchingPredicate nodes (\(D3SimNode n) -> n.name == "Main") 0.0 0.0
      -- _ = stopSimulation_ simulation

  linksSelection <- linksGroup <+> JoinSimulation {
      element   : Line
    , key       : UseDatumAsKey
    , "data"    : model.links
    , behaviour : [ classed linkClass ] -- default invisible in CSS unless marked "visible"
    , simulation: simulation
    , tickName  : "links"
    , onTick    : [ x1 setX1, y1 setY1, x2 setX2, y2 setY2 ]
    , onDrag    : SimulationDrag NoDrag
  }

  nodesSelection <- nodesGroup <+> JoinSimulation {
      element   : Group
    , key       : UseDatumAsKey
    , "data"    : nodes
    , behaviour : [ classed nodeClass, transform' translateNode ]
    , simulation: simulation
    , tickName  : "nodes"
    , onTick    : [ transform' translateNode  ]
    , onDrag    : SimulationDrag DefaultDrag
  }

  circle  <- nodesSelection `append` (node Circle [ radius (chooseRadius model.path2LOCMap) 
                                                  , fill (colorByGroup model.id2PackageIDMap)
                                                  -- , on MouseEnter (\e d t -> stopSimulation_ simulation) 
                                                  -- , on MouseLeave (\e d t -> startSimulation_ simulation)
                                                  , on MouseEnter (\e d t -> highlightNeighborhood (unsafeCoerce model) (getIdFromSpagoSimNode d))
                                                  , on MouseLeave (\e d t -> unhighlightNeighborhood (unsafeCoerce model) (getIdFromSpagoSimNode d))
                                                  ]) 
  labels' <- nodesSelection `append` (node Text [ classed "label",  x 0.2, y (positionLabel model.path2LOCMap), text getNameFromSpagoSimNode]) 
  
  svg' <- svg `attachZoom`  { extent    : ZoomExtent { top: 0.0, left: 0.0 , bottom: h, right: w }
                            , scale     : ScaleExtent 0.2 2.0 -- wonder if ScaleExtent ctor could be range operator `..`
                            , qualifier : "tree"
                            , target    : SelfTarget
                            }
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
  let (D3SimNode d) = datumIsSpagoSimNode datum
  case d.nodetype of
    IsModule   -> Math.sqrt (fromMaybe 10.0 $ M.lookup d.path locMap)
    IsPackage -> packageRadius

chooseRadiusTree :: Map String Number -> Datum_ -> Number
chooseRadiusTree locMap datum = do
  let (D3SimNode d) = unsafeCoerce datum -- TODO unsafe because despite all the row types this still cashes out to a simnode not a treenode here which must be fixed
  case d.data.nodetype of
    IsModule   -> Math.sqrt (fromMaybe 10.0 $ M.lookup d.data.path locMap)
    IsPackage -> packageRadius

positionLabel :: Map String Number -> Datum_ -> Number
positionLabel locMap datum = do
  let (D3SimNode d) = datumIsSpagoSimNode datum
  case d.nodetype of
    IsModule -> negate $ Math.sqrt (fromMaybe 10.0 $ M.lookup d.path locMap)
    IsPackage -> 0.0

chooseRadiusFn :: Datum_ -> Index_ -> Number
chooseRadiusFn datum index = do
  let (D3SimNode d) = datumIsSpagoSimNode datum
  case d.nodetype of
    IsModule  -> moduleRadius
    IsPackage -> packageRadius + packageForceRadius

nodeClass :: Datum_ -> String
nodeClass datum = do
  let (D3SimNode d) = datumIsSpagoSimNode datum
  show d.nodetype

linkClass :: Datum_ -> String
linkClass datum = do
  let (D3_Link d) = datumIsSpagoLink datum
  show d.linktype

translateNode :: Datum_ -> String
translateNode datum = "translate(" <> show x <> "," <> show y <> ")"
  where 
    d = datumIsGraphNode datum
    (x :: Number) = (unsafeCoerce datum).x
    (y :: Number) = (unsafeCoerce datum).y

colorByGroup :: M.Map NodeID NodeID -> Datum_ -> String
colorByGroup packageMap datum = d3SchemeCategory10N_ (toNumber $ fromMaybe 0 packageID)
  where
    (D3SimNode d) = unsafeCoerce datum
    packageID     = M.lookup d.id packageMap

colorByGroupTree :: M.Map NodeID NodeID -> Datum_ -> String
colorByGroupTree packageMap datum = d3SchemeCategory10N_ (toNumber $ fromMaybe 0 packageID)
  where
    (D3SimNode d) = unsafeCoerce datum
    packageID     = M.lookup d.data.id packageMap

setX1 :: Datum_ -> Number
setX1 = getSourceX
setY1 :: Datum_ -> Number
setY1 = getSourceY
setX2 :: Datum_ -> Number
setX2 = getTargetX
setY2 :: Datum_ -> Number
setY2 = getTargetY
setCx :: Datum_ -> Number
setCx = getNodeX
setCy :: Datum_ -> Number
setCy = getNodeY



-- | **************************************************************************************************************
-- | draw the spago graph - only the tree part - as a radial tree
-- | **************************************************************************************************************

-- TODO forall d should be explicit, this script requires certain data structures, fix sig to specify
spagoTreeScript :: forall m selection. Bind m => D3InterpreterM selection m => 
  Tuple Number Number -> SpagoModel -> m selection
spagoTreeScript _ model@{ tree: Nothing } = do
  attach "div#spagotree"            -- FIXME this is bogus but saves messing about with the Maybe tree in the drawGraph script for now            

spagoTreeScript (Tuple width height) model@{ tree: Just (Tuple _ theTree)} = do
  let 
    -- configure dimensions
    columns                    = 2.0  -- 3 columns, set in the grid CSS in index.html
    rows                       = 1.0
    gap                        = 10.0 -- 10px set in the grid CSS in index.html
    svgWH                      = { width : ((width - ((columns - 1.0) * gap)) / columns)
                                 , height: height / rows }
    numberOfLevels             = (hNodeHeight_ theTree) + 1.0
    spacing                    = { interChild: 120.0, interLevel: height / numberOfLevels}
    layoutFn                   = ((getLayout TidyTree) `treeSetSize_`       [ 2.0 * pi, width ]) 
                                                       `treeSetSeparation_` radialSeparation
    laidOutRoot_               = layoutFn `runLayoutFn_` theTree
    { xMin, xMax, yMin, yMax } = treeMinMax_ laidOutRoot_
    xExtent                    = xMax - xMin -- ie if tree spans from -50 to 200, it's extent is 250
    yExtent                    = yMax - yMin -- ie if tree spans from -50 to 200, it's extent is 250


  -- "script"
  root       <- attach "div#spagotree"                           
  svg        <- root `append` (node Svg  [ viewBox (-width / 2.0) (-height / 2.0) width height ] )          
  container  <- svg  `append` (node Group [ fontFamily      "sans-serif"
                                          , fontSize        18.0
                                          ])
  links      <- container `append` (node Group [ classed "links"])
  nodes      <- container `append` (node Group [ classed "nodes"])

  theLinks_  <- links <+> Join {
      element   : Path
    , key       : UseDatumAsKey
    , "data"    : links_ theTree
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
    , "data"    : descendants_ theTree
    -- there could be other stylistic stuff here but the transform is key structuring component
    , behaviour : [ transform [ radialRotateCommon, radialTreeTranslate, rotateRadialLabels ] ]
  }

  theNodes <- nodeJoin_ `append` 
                (node Circle  [ fill         (colorByGroupTree model.id2PackageIDMap)
                              , radius       (chooseRadiusTree model.path2LOCMap)
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

radialRotateCommon :: forall r. D3_TreeNode (D3_XY + r) -> String
radialRotateCommon (D3TreeNode d) = "rotate(" <> radialRotate d.x <> ")"

radialTreeTranslate :: forall r. D3_TreeNode (D3_XY + r) -> String
radialTreeTranslate (D3TreeNode d) = "translate(" <> show d.y <> ",0)"

rotateRadialLabels :: forall r. D3_TreeNode (D3_XY + r) -> String
rotateRadialLabels (D3TreeNode d) = -- TODO replace with nodeIsOnRHS 
  "rotate(" <> if d.x >= pi 
  then "180" <> ")" 
  else "0" <> ")"

nodeIsOnRHS :: Datum_ -> Boolean
nodeIsOnRHS d = node.x < pi
  where (D3TreeNode node) = datumIsTreeNode d

textDirection :: Datum_ -> Boolean
textDirection = \d -> hasChildren_ d == nodeIsOnRHS d

labelName :: Datum_ -> String
labelName d = node."data".name
  where node = unsafeCoerce d
