module D3.Examples.Spago where

import Affjax as AJAX
import Affjax.ResponseFormat as ResponseFormat
import Utility (getWindowWidthHeight)
import D3.Examples.Spago.File (NodeType(..), SpagoModel, SpagoSimNode, SpagoTreeNode, convertFilesToGraphModel, datumIsGraphNode, datumIsSpagoLink, datumIsSpagoSimNode, findGraphNodeIdFromName, getReachableNodes, setXY)
import D3.Data.Types (D3Selection_, Datum_, Index_, PointXY)
import D3.Data.Tree (TreeType(..), makeD3TreeJSONFromTreeID)
import D3.Examples.Spago.Graph (graphScript)
import D3.Examples.Spago.Tree (treeScript)
import D3.Examples.Tree.Configure (datumIsTreeNode)
import D3.FFI (descendants_, getLayout, hasChildren_, hierarchyFromJSON_, runLayoutFn_, treeSetSeparation_, treeSetSize_, treeSortForTree_Spago)
import D3.Interpreter.D3 (runD3M)
import D3.Interpreter.String (runPrinter)
import D3.Layouts.Hierarchical (radialSeparation)
import D3.Node (D3_Link(..), D3_SimulationNode(..), D3_TreeNode(..), D3_XY, NodeID, getNodeX, getNodeY, getSourceX, getSourceY, getTargetX, getTargetY)
import D3.Scales (d3SchemeCategory10N_)
import Data.Array (elem, filter, foldl, fromFoldable, partition, reverse)
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
import Prelude (Unit, bind, discard, negate, pure, show, unit, ($), (*), (+), (-), (/), (<), (<$>), (<*>), (<<<), (<>), (==), (>=), (||))
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

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
      (_ :: Tuple D3Selection_ Unit) <- liftEffect $ runD3M (treeScript (Tuple (width/2.0) height) graph')
       
      printedScript <- liftEffect $ runPrinter (graphScript (Tuple width height) graph') "Force Layout Script"
      log $ snd printedScript
      log $ fst printedScript
      pure unit

-- TODO make this generic and extract from Spago example to library
treeReduction :: SpagoModel -> NodeID -> SpagoModel
treeReduction model rootID  = do
      let reachable         = getReachableNodes rootID model.graph
          onlyTreelinks     = makeTreeLinks (pathsAsLists reachable.closedDepPaths)
          treelinks         = partition (\(D3_Link l) -> (Tuple l.source l.target) `elem` onlyTreelinks) model.links
          treenodes         = partition (\(D3SimNode n) -> (n.id `elem` reachable.nodes) || n.id == rootID) model.nodes
          layout            = ((getLayout TidyTree) `treeSetSize_` [ 2.0 * pi, 900.0 ]) `treeSetSeparation_` radialSeparation
          idTree            = buildTree rootID model treelinks.yes
          jsontree          = makeD3TreeJSONFromTreeID idTree model.id2NodeMap
          rootTree          = hierarchyFromJSON_       jsontree
          sortedTree        = treeSortForTree_Spago    rootTree
          laidOutRoot_      = (runLayoutFn_ layout)    sortedTree
          positionMap       = getPositionMap           laidOutRoot_
          positionedNodes   = setNodePositionsRadial   treenodes.yes positionMap
          unpositionedNodes = setForPhyllotaxis  <$> treenodes.no
          tree              = Tuple rootID laidOutRoot_

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
-- TODO coerce here is pure hackery because reference swizzling not properly modeled in type system
getPositionMap root = foldl (\acc (D3TreeNode n) -> M.insert n.data.id { x: n.x, y: n.y, isLeaf: (unsafeCoerce n).data.isLeaf } acc) empty (descendants_ root) 

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
