module D3.Examples.Spago.Model where

import D3.Examples.Spago.Files
import Prelude

import D3.Data.Types (Datum_, Index_, PointXY)
import D3.Node (D3SimulationRow, D3TreeRow, D3_FocusXY, D3_Link(..), D3_Radius, D3_SimulationNode(..), EmbeddedData, NodeID)
import Data.Array (catMaybes, cons, foldl, length, range, zip, (!!), (:))
import Data.Foldable (sum)
import Data.Generic.Rep (Sum)
import Data.Graph (Graph, fromMap)
import Data.Int (toNumber)
import Data.Map (toUnfoldable)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable, null) as N
import Data.Nullable (notNull)
import Data.Set as S
import Data.String (Pattern(..), split)
import Data.Tuple (Tuple(..))
import Debug (spy, trace)
import Math (sqrt, (%))
import Math as Math
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

moduleRadius = 5.0 :: Number 
packageRadius = 50.0 :: Number
packageForceRadius = 50.0 :: Number

-- Model data types specialized with inital data


type SpagoTreeNode    = D3TreeRow       (EmbeddedData SpagoNodeData + ())
type SpagoSimNode     = D3SimulationRow (             SpagoNodeRow  + D3_FocusXY + D3_Radius + ()) -- note we've woven in focusXY so that we can cluster the nodes

type SpagoGraphLinkObj = D3_Link SpagoNodeData SpagoLinkData 

type SpagoModel = { 
    links           :: Array SpagoGraphLinkID  -- each ID will get swizzled for a SpagoGraphLinkObj_ when simulation initialized
  , prunedTreeLinks :: Array SpagoGraphLinkID  -- there are so many of these that we only use them when hovering enabled
  , nodes           :: Array SpagoSimNode      -- already upgraded to simnode as a result of positioning when building the model
  , graph           :: Graph NodeID SpagoNodeData
  , tree            :: Maybe (Tuple NodeID SpagoTreeNode)
  , maps            :: { name2ID    :: M.Map String NodeID
                       , id2Name    :: M.Map NodeID String
                       , id2Node    :: M.Map NodeID SpagoNodeData
                       , id2Package :: M.Map NodeID NodeID
                       , id2LOC     :: M.Map NodeID Number
                       , id2XYLeaf  :: M.Map NodeID { x :: Number, y :: Number, isLeaf :: Boolean }
                       }
}

upgradeSpagoNodeData :: SpagoNodeData -> SpagoSimNode
upgradeSpagoNodeData node = D3SimNode { 
    links      : node.links
  , id           : node.id
  , name         : node.name
  , nodetype     : node.nodetype
  , containerID  : node.containerID
  , containerName: node.containerName
  , containsMany : node.containsMany
  , cluster      : node.containerID -- TODO cluster all the single module packages together
  , index        : node.id
  , pinned       : Floating
  , loc          : node.loc
  , focusX       : 0.0
  , focusY       : 0.0
  , fx           : (N.null :: N.Nullable Number)
  , fy           : (N.null :: N.Nullable Number)
  , vx           : 0.0
  , vy           : 0.0
  , x            : 0.0
  , y            : 0.0
  , r            : sqrt node.loc
  }

numberToGridPoint :: Int -> Int -> PointXY
numberToGridPoint columns i = do
  let
    c = toNumber columns
    d = toNumber i
    x = (d % c)
    y = Math.floor (d / c)
  { x, y }

scalePoint :: Number -> Number -> PointXY -> PointXY
scalePoint xFactor yFactor xy = { x: xy.x * xFactor, y: xy.y * yFactor }

offsetXY :: PointXY -> PointXY -> PointXY
offsetXY offset xy = { x: xy.x + offset.x, y: xy.y + offset.y }

offsetX :: Number -> PointXY -> PointXY
offsetX xOffset xy = xy { x = xy.x + xOffset }

offsetY :: Number -> PointXY -> PointXY
offsetY yOffset xy = xy { y = xy.y + yOffset }

pinNode :: SpagoSimNode -> PointXY -> SpagoSimNode
pinNode (D3SimNode node) xy = D3SimNode (node { fx = notNull xy.x, fy = notNull xy.y } )

pinIfPackage :: SpagoSimNode -> SpagoSimNode
pinIfPackage n@(D3SimNode node) = do
  let xy = offsetXY { x: (-900.0), y: (-5000.0) } $
            scalePoint 180.0 100.0 $
            numberToGridPoint 10 node.cluster
      -- _ = trace { pin: node.name, cluster: node.cluster, x: xy.x, y: xy.y } \_ -> unit
  case node.nodetype of
    (IsModule _)  -> setXY   n xy -- only setting intial position of module
    (IsPackage _) -> pinNode n xy -- but fixing the position of packages

gridifyByNodeID :: SpagoSimNode -> SpagoSimNode
gridifyByNodeID n@(D3SimNode node) = do
  let xy = offsetXY { x: (-1000.0), y: (-500.0) } $
            scalePoint 100.0 20.0 $
            numberToGridPoint 10 node.id
      _ = trace { pin: node.name, cluster: node.cluster, x: xy.x, y: xy.y } \_ -> unit
  pinNode n xy

gridifyByCluster :: SpagoSimNode -> SpagoSimNode
gridifyByCluster n@(D3SimNode node) = do
  let xy = offsetXY { x: (-800.0), y: (-5000.0) } $
           scalePoint 180.0 100.0 $
           numberToGridPoint 10 node.cluster
      _ = trace { pin: node.name, cluster: node.cluster, x: xy.x, y: xy.y } \_ -> unit
  pinNode n xy

setXY :: SpagoSimNode -> { x :: Number, y :: Number } -> SpagoSimNode
setXY (D3SimNode node) { x, y } = D3SimNode (node { x = x, y = y })

setXYExceptLeaves :: SpagoSimNode -> { x :: Number, y :: Number, isLeaf :: Boolean } -> SpagoSimNode
setXYExceptLeaves (D3SimNode node) { x, y, isLeaf: true }  = D3SimNode node
setXYExceptLeaves (D3SimNode node) { x, y, isLeaf: false } = D3SimNode (node { fx = notNull x, fy = notNull y, pinned = Pinned })

datumIsSpagoSimNode :: Datum_ -> SpagoSimNode
datumIsSpagoSimNode = unsafeCoerce

datumIsSpagoLink :: Datum_ -> D3_Link SpagoNodeData SpagoLinkData
datumIsSpagoLink = unsafeCoerce

datumIsGraphNode :: Datum_ -> SpagoSimNode
datumIsGraphNode = unsafeCoerce

getIdFromSpagoSimNode :: Datum_ -> Int
getIdFromSpagoSimNode datum = d.id
  where
    (D3SimNode d) = unsafeCoerce datum

getRadiusFromSpagoSimNode :: Datum_ -> Number
getRadiusFromSpagoSimNode datum = d.radius
  where
    (D3SimNode d) = unsafeCoerce datum

cluster2Point :: Int -> PointXY
cluster2Point v = 
  offsetXY { x: (-800.0), y: (-5000.0) } $
  scalePoint 180.0 100.0 $
  numberToGridPoint 10 v


chooseFocusFromSpagoSimNodeX :: Datum_ -> Number
chooseFocusFromSpagoSimNodeX datum = x 
  where
    (D3SimNode d) = unsafeCoerce datum
    { x } = cluster2Point d.cluster

chooseFocusFromSpagoSimNodeY :: Datum_ -> Number
chooseFocusFromSpagoSimNodeY datum = y 
  where
    (D3SimNode d) = unsafeCoerce datum
    { y } = cluster2Point d.cluster

getNameFromSpagoSimNode :: Datum_ -> String
getNameFromSpagoSimNode datum = d.name
  where
    (D3SimNode d) = unsafeCoerce datum

convertFilesToGraphModel :: forall r. 
  { body :: String | r } -> 
  { body :: String | r } -> 
  { body :: String | r } -> 
  { body :: String | r } -> SpagoModel
convertFilesToGraphModel moduleJSON packageJSON lsdepJSON locJSON = 
  makeSpagoGraphModel $ readSpago_Raw_JSON_ moduleJSON.body packageJSON.body lsdepJSON.body locJSON.body


makeSpagoGraphModel :: Spago_Raw_JSON_ -> SpagoModel
makeSpagoGraphModel json = do
  let { nodes, links, name2ID, id2Name, id2Node, id2Package, id2LOC } 
        = getGraphJSONData json

  { links
  , nodes    : upgradeSpagoNodeData <$> nodes
  , graph    : makeGraph nodes
  , tree     : Nothing  -- not present in the JSON, has to be calculated, if possible
  , prunedTreeLinks: [] -- not present in the JSON, has to be calculated, if possible
  , maps     : { name2ID
               , id2Name
               , id2Node
               , id2Package
               , id2LOC
               , id2XYLeaf: M.empty
               }
  }

makeGraph :: Array SpagoNodeData -> Graph NodeID SpagoNodeData
makeGraph nodes = do
  let
    graphMap = foldl addNode M.empty nodes
    addNode :: M.Map NodeID (Tuple SpagoNodeData (S.Set NodeID)) -> SpagoNodeData -> M.Map NodeID (Tuple SpagoNodeData (S.Set NodeID))
    addNode acc node = M.insert node.id (Tuple node depends) acc
      where
        depends :: S.Set Int
        depends = S.fromFoldable node.links.targets
  fromMap graphMap



