module D3.Examples.Simulation.SpagoPackages where

import D3.Data.File.Spago
import D3.Node

import Affjax as AJAX
import Affjax.ResponseFormat as ResponseFormat
import D3.Attributes.Sugar (classed, dy, fill, fontFamily, fontSize, getWindowWidthHeight, on, radius, strokeColor, strokeOpacity, strokeWidth, text, textAnchor, transform, transform', viewBox, x, x1, x2, y, y1, y2)
import D3.Data.Types (D3Selection_, Datum_, Element(..), Index_, MouseEvent(..), PointXY, TreeType(..), makeD3TreeJSONFromTreeID)
import D3.Examples.Tree.Configure (datumIsTreeNode)
import D3.FFI (GraphModel_, descendants_, getLayout, hNodeHeight_, hasChildren_, hierarchyFromJSON_, links_, nanNodes_, pinNodeMatchingPredicate, runLayoutFn_, startSimulation_, stopSimulation_, treeMinMax_, treeSetNodeSize_, treeSetSeparation_, treeSetSize_)
import D3.FFI.Config (defaultConfigSimulation, defaultForceCenterConfig, defaultForceCollideConfig, defaultForceLinkConfig, defaultForceManyConfig, defaultForceXConfig, defaultForceYConfig)
import D3.Interpreter (class D3InterpreterM, append, attach, attachZoom, (<+>))
import D3.Interpreter.D3 (runD3M)
import D3.Interpreter.String (runPrinter)
import D3.Layouts.Hierarchical (radialLink, radialSeparation)
import D3.Layouts.Simulation (Force(..), ForceType(..), initSimulation)
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
import Data.Set as S
import Data.Tree (Tree(..))
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Math (cos, pi, sin)
import Math (sqrt) as Math
import Prelude (class Bind, Unit, bind, discard, negate, pure, show, unit, ($), (*), (+), (-), (/), (<), (<$>), (<*>), (<<<), (<>), (==), (>=), (||))
import Unsafe.Coerce (unsafeCoerce)
import Type.Row (type (+))


-- highlightNeighborhood :: forall d r. GraphModel_ (D3_Link (D3_Simulation_Node d) r) (D3_Simulation_Node d) -> NodeID -> Unit
highlightNeighborhood { links } nodeId = markAsSpotlit_ nodeId sources targets
  where
    sources = foldl (\acc l -> if l.target.index == nodeId then (cons l.source.index acc) else acc) [] links
    targets = foldl (\acc l -> if l.source.index == nodeId then (cons l.target.index acc) else acc) [] links

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
      let graph' = 
            case findGraphNodeIdFromName graph "Main" of
              Nothing       -> graph -- couldn't find root of tree so just skip this
              (Just rootID) -> treeReduction graph rootID

      (_ :: Tuple D3Selection_ Unit) <- liftEffect $ runD3M (graphScript widthHeight graph')
      (_ :: Tuple D3Selection_ Unit) <- liftEffect $ runD3M (spagoTreeScript widthHeight graph'.tree)
       
      printedScript <- liftEffect $ runPrinter (graphScript widthHeight graph') "Force Layout Script"
      log $ snd printedScript
      log $ fst printedScript
      pure unit

-- TODO make this generic and extract from Spago example to library
treeReduction :: SpagoModel -> NodeID -> SpagoModel
treeReduction model rootID = do
      let reachable       = getReachableNodes rootID model.graph
          onlyTreelinks   = makeTreeLinks (pathsAsLists reachable.closedPaths)
          treelinks       = partition (\l -> (Tuple l.source l.target) `elem` onlyTreelinks) model.links
          treenodes       = partition (\n -> (n.id `elem` reachable.nodes) || n.id == rootID) model.nodes
          layout          = ((getLayout TidyTree) `treeSetSize_` [ 2.0 * pi, 1000.0 ]) `treeSetSeparation_` radialSeparation
          idTree          = buildTree rootID model treelinks.yes
          jsontree        = makeD3TreeJSONFromTreeID idTree
          rootTree        = hierarchyFromJSON_       jsontree
      -- sortedTree       = treeSortForTree_      rootTree
          laidOutRoot_    = (runLayoutFn_ layout)    rootTree -- sortedTree
          positionMap     = getPositionMap           laidOutRoot_
          positionedNodes = setNodePositionsRadial   treenodes.yes positionMap
          tree            = Tuple rootID laidOutRoot_

      model { links = treelinks.yes, nodes = positionedNodes, tree = Just tree, positions = positionMap }

-- for radial positioning we treat x as angle and y as radius
radialTranslate :: PointXY -> PointXY
radialTranslate p = 
  let angle  = p.x
      radius = p.y
      x = radius * cos angle
      y = radius * sin angle
  in { x, y }

setNodePositionsRadial :: Array SpagoSimNode -> M.Map NodeID PointXY -> Array SpagoSimNode
setNodePositionsRadial nodes positionMap = do
  let updateXY :: SpagoSimNode -> SpagoSimNode
      updateXY (D3SimNode node) = 
        case M.lookup node.id positionMap of
          Nothing -> (D3SimNode node)
          (Just p) -> 
            let { x,y } = radialTranslate p
            in D3SimNode $ node { x = x, y = y }
  updateXY <$> nodes

getPositionMap :: SpagoTreeNode -> Map NodeID PointXY
getPositionMap root = foldl (\acc n -> M.insert n.id { x: n.x, y: n.y } acc) empty (descendants_ root)

buildTree :: forall r. NodeID -> SpagoModel -> Array (D3_Link NodeID r) -> Tree NodeID
buildTree rootID model treelinks = do
  let 
    linksWhoseSourceIs :: NodeID -> L.List NodeID
    linksWhoseSourceIs id = L.fromFoldable $ (_.target) <$> (filter (\l -> l.source == id) treelinks)

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
  linksGroup <- svg  `append` (node Group [ classed "links", strokeColor "#999", strokeOpacity 0.6 ])
  nodesGroup <- svg  `append` (node Group [ classed "nodes" ])

  let forces      = [ Force $ ForceManyBody    $ (defaultForceManyConfig "charge") { strength = -100.0 }
                    , Force $ ForceCollide     $  defaultForceCollideConfig "collide" (\d -> chooseRadiusFn d)
                    , Force $ ForceX           $ (defaultForceXConfig "x") { strength = 0.05 }
                    , Force $ ForceY           $ (defaultForceYConfig "y") { strength = 0.05 }
                    , Force $ ForceCenter      $ (defaultForceCenterConfig "center") { strength = -1.0 }
                    , Force $ ForceLink        $ (defaultForceLinkConfig "links" model.links (\d i -> d.id))
                    -- , Force $ ForceRadialFixed $ defaultForceRadialFixedConfig "radial" 500.0
                    ]
      { simulation, nodes } = initSimulation forces model.nodes defaultConfigSimulation

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

  circle  <- nodesSelection `append` (node Circle [ radius (chooseRadius model.loc) 
                                                  , fill colorByGroup
                                                  , on MouseEnter (\e d t -> stopSimulation_ simulation) 
                                                  , on MouseLeave (\e d t -> startSimulation_ simulation)
                                                  , on MouseClick (\e d t -> highlightNeighborhood (unsafeCoerce model) (datumIsSpagoSimNode d).index)
                                                  ]) 
  labels' <- nodesSelection `append` (node Text [ classed "label",  x 0.2, y 0.2, text (\d -> (datumIsSpagoSimNode d).data.name)]) 
  
  svg' <- svg `attachZoom`  { extent    : ZoomExtent { top: 0.0, left: 0.0 , bottom: h, right: w }
                            , scale     : ScaleExtent 0.2 2.0 -- wonder if ScaleExtent ctor could be range operator `..`
                            , qualifier : "tree"
                            , target    : SelfTarget
                            }
  let
    _ = nanNodes_ $ unsafeCoerce model.nodes
    _ = pinNodeMatchingPredicate nodes (\n -> n.data.name == "Main") 0.0 0.0
    _ = startSimulation_ simulation

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
  let d = datumIsSpagoSimNode datum
  case d.data.moduleOrPackage of
    -- IsModule   -> moduleRadius
    IsModule   -> Math.sqrt (fromMaybe 10.0 $ M.lookup d.data.path locMap)
    IsPackage -> packageRadius

chooseRadiusFn :: Datum_ -> Index_ -> Number
chooseRadiusFn datum index = do
  let d = datumIsSpagoSimNode datum
  case d.data.moduleOrPackage of
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

colorByGroup :: Datum_ -> String
colorByGroup datum = d3SchemeCategory10N_ (toNumber $ fromMaybe 0 d.package)
  where
    d = datumIsSpagoSimNode datum

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
spagoTreeScript :: forall m d selection. Bind m => D3InterpreterM selection m => 
  Tuple Number Number -> Maybe SpagoTreeNode -> m selection
spagoTreeScript (Tuple width height) Nothing = do
  attach "div#spagotree"            -- FIXME this is bogus but saves messing about with the Maybe tree in the drawGraph script               

spagoTreeScript (Tuple width height) (Just tree) = do
  let 
    -- configure dimensions
    columns                    = 3.0  -- 3 columns, set in the grid CSS in index.html
    gap                        = 10.0 -- 10px set in the grid CSS in index.html
    svgWH                      = { width : ((width - ((columns - 1.0) * gap)) / columns)
                                 , height: height / 2.0 } -- 2 rows
    numberOfLevels             = (hNodeHeight_ tree) + 1.0
    spacing                    = { interChild: 120.0, interLevel: svgWH.height / numberOfLevels}
    layoutFn                   = (getLayout TidyTree) `treeSetNodeSize_` [ spacing.interChild, spacing.interLevel ]
    laidOutRoot_               = layoutFn `runLayoutFn_` tree
    { xMin, xMax, yMin, yMax } = treeMinMax_ laidOutRoot_
    xExtent                    = xMax - xMin -- ie if tree spans from -50 to 200, it's extent is 250
    yExtent                    = yMax - yMin -- ie if tree spans from -50 to 200, it's extent is 250


  -- "script"
  root       <- attach "div#spagotree"                           
  svg        <- root `append` (node Svg  [ viewBox (-svgWH.width / 2.0) (-svgWH.height / 2.0) (svgWH.width * 2.0) (svgWH.height * 2.0) ] )          
  container  <- svg  `append` (node Group [ fontFamily      "sans-serif"
                                          , fontSize        18.0
                                          ])
  links      <- container `append` (node Group [ classed "links"])
  nodes      <- container `append` (node Group [ classed "nodes"])

  theLinks_  <- links <+> Join {
      element   : Path
    , key       : UseDatumAsKey
    , "data"    : links_ tree
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
    , "data"    : descendants_ tree
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
labelName d = node."data".label
  where node = unsafeCoerce d
