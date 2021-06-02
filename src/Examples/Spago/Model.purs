module D3.Examples.Spago.Model where

import Prelude

import D3.Data.Tree (TreeLayout(..))
import D3.Data.Types (D3Simulation_, Datum_, PointXY, chooseX, chooseY)
import D3.Examples.Spago.Files (NodeType(..), Pinned(..), SpagoGraphLinkID, SpagoNodeData, SpagoNodeRow, Spago_Raw_JSON_, getGraphJSONData, readSpago_Raw_JSON_, unboxD3SimLink, unboxD3SimNode, unboxD3TreeNode)
import D3.FFI (hasChildren_)
import D3.Node (D3SimulationRow, D3TreeRow, D3_FocusXY, D3_Radius, D3_SimulationNode(..), EmbeddedData, NodeID)
import D3.Scales (d3SchemeCategory10N_)
import Data.Array (foldl)
import Data.Graph (Graph, fromMap)
import Data.Int (toNumber)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable, notNull, toMaybe)
import Data.Nullable (Nullable, null) as N
import Data.Set as S
import Data.Tuple (Tuple(..))
import Debug (trace)
import Math (pi, sqrt, (%))
import Math as Math
import Type.Row (type (+))
import Web.Event.Internal.Types (Event)

moduleRadius       =  5.0 :: Number 
packageRadius      = 50.0 :: Number
packageForceRadius = 50.0 :: Number

tree_datum_ = {
    x           : (\d -> (unboxD3TreeNode d).x)
  , y           : (\d -> (unboxD3TreeNode d).y)
  , isLeaf      : (\d -> (unboxD3TreeNode d).isLeaf)
  , containerID : (\d -> (unboxD3TreeNode d).data.containerID)
  , name        : (\d -> (unboxD3TreeNode d).data.name)
  , loc         : (\d -> (unboxD3TreeNode d).data.loc)
  , colorByGroup: (\d -> d3SchemeCategory10N_ (toNumber $ tree_datum_.containerID d))
  , textAnchor  : (\l d -> case l of
                            Radial ->
                              if (hasChildren_ d) == (datum_.x d < pi)
                              then "start"
                              else "end"
                            _ -> 
                              if (hasChildren_ d)
                              then "start"
                              else "end"
                        )
  , textX       : (\l d -> case l of
                      Radial ->
                        if (hasChildren_ d) == (datum_.x d < pi) -- d.x < pi => node is on the RHS of Radial tree
                        then 6.0
                        else (-6.0)
                      _ -> 
                        if (hasChildren_ d)
                        then 6.0
                        else (-6.0)
                  )
  , onRHS       : (\l d -> if l == Radial && (datum_.x d >= pi)
                        then true
                        else false
                  )
}

link_ = {
    source: (\d -> (unboxD3SimLink d).source)
  , target: (\d -> (unboxD3SimLink d).target)
  , linkClass: (\d -> show (unboxD3SimLink d).linktype)
}

cluster2Point :: Int -> PointXY
cluster2Point v = 
  offsetXY { x: (-800.0), y: (-5000.0) } $
  scalePoint 180.0 100.0 $
  numberToGridPoint 10 v

tree2Point :: Nullable Number -> Nullable Number -> Maybe PointXY
tree2Point x' y' = do
  x <- toMaybe x'
  y <- toMaybe y'
  Just { x, y } 

-- | all the coercions in one place
datum_ = {
-- direct accessors to fields of the datum (BOILERPLATE)
    radius        : (\d -> (unboxD3SimNode d).r)
  , id            : (\d -> (unboxD3SimNode d).id)
  , loc           : (\d -> (unboxD3SimNode d).loc)
  , containerID   : (\d -> (unboxD3SimNode d).containerID)
  , containerName : (\d -> (unboxD3SimNode d).containerName)
  , name          : (\d -> (unboxD3SimNode d).name)
  , x             : (\d -> (unboxD3SimNode d).x)
  , y             : (\d -> (unboxD3SimNode d).y)
  , treeX         : (\d -> (unboxD3SimNode d).treeX)
  , treeY         : (\d -> (unboxD3SimNode d).treeY)
  , nodetype      : (\d -> (unboxD3SimNode d).nodetype)
  , cluster       : (\d -> (unboxD3SimNode d).cluster)
  , links         : (\d -> (unboxD3SimNode d).links)
  , connected     : (\d -> (unboxD3SimNode d).connected)

  , clusterPoint  : (\d -> cluster2Point (unboxD3SimNode d).cluster)
  , clusterPointX : (\d -> chooseX $ datum_.clusterPoint d)
  , clusterPointY : (\d -> chooseY $ datum_.clusterPoint d)
  , treePoint     : (\d -> fromMaybe (datum_.clusterPoint d) (tree2Point (datum_.treeX d) (datum_.treeY d)))
  , treePointX    : (\d -> chooseX $ datum_.treePoint d)
  , treePointY    : (\d -> chooseY $ datum_.treePoint d)

-- more complicated calculations (CONVENIENCE)
  , positionLabel:
    (\d -> case datum_.nodetype d of
            (IsModule _)  -> negate $ datum_.loc d
            (IsPackage _) -> 0.0
    )
  , collideRadius:
      (\d -> 
        if datum_.id d == datum_.containerID d
        then 10.0
        else datum_.radius d
      )
  , nodeClass:
      (\d -> show (datum_.nodetype d) <> " " <> (datum_.containerName d) <> " " <> (datum_.name d))
  , colorByGroup:
      (\d -> d3SchemeCategory10N_ (toNumber $ datum_.cluster d))
  , translateNode:
      (\d -> "translate(" <> show (datum_.x d) <> "," <> show (datum_.y d) <> ")")
-- accessors to provide different force settings for different cohorts, quite possible that this should go thru a similar but different route from `datum`
  , onlyPackages:
      (\d -> case datum_.nodetype d of
              (IsModule _)  -> 0.0 -- we don't want modules to respond to this force at all
              (IsPackage _) -> 0.5)
  , onlyUnused:
      (\d -> case datum_.nodetype d of
              (IsModule _)  -> if datum_.connected d 
                               then 0.0 
                               else 0.8  -- this should put the only unused modules in a different orbit from the packages
              (IsPackage _) -> 0.0)
}

              
-- Model data types specialized with inital data
type SpagoTreeNode    = D3TreeRow       (EmbeddedData SpagoNodeData + ())
type SpagoSimNode     = D3SimulationRow (             SpagoNodeRow  + D3_FocusXY + D3_Radius + ()) -- note we've woven in focusXY so that we can cluster the nodes

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

upgradeSpagoNodeData :: M.Map NodeID (Array NodeID) -> SpagoNodeData -> SpagoSimNode
upgradeSpagoNodeData sourcesMap node = D3SimNode { 
    links        : node.links { sources = fromMaybe [] $ M.lookup node.id sourcesMap }
  , id           : node.id
  , name         : node.name
  , nodetype     : node.nodetype
  , containerID  : node.containerID
  , containerName: node.containerName
  , connected    : node.connected
  , containsMany : node.containsMany
  , cluster      : node.containerID -- TODO cluster all the single module packages together
  , index        : node.id
  , pinned       : Floating
  , loc          : node.loc
  , focusX       : 0.0
  , focusY       : 0.0
  , fx           : (N.null :: N.Nullable Number)
  , fy           : (N.null :: N.Nullable Number)
  , treeX        : (N.null :: N.Nullable Number)
  , treeY        : (N.null :: N.Nullable Number)
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
      _ =  trace { pin: node.name, cluster: node.cluster, x: xy.x, y: xy.y } \_ -> unit
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
setXYExceptLeaves (D3SimNode node) { x, y, isLeaf: true }  = D3SimNode node { connected = true }
setXYExceptLeaves (D3SimNode node) { x, y, isLeaf: false } = D3SimNode (node { treeX = notNull x, treeY = notNull y, connected = true, pinned = Forced })



convertFilesToGraphModel :: forall r. 
  { body :: String | r } -> 
  { body :: String | r } -> 
  { body :: String | r } -> 
  { body :: String | r } -> SpagoModel
convertFilesToGraphModel moduleJSON packageJSON lsdepJSON locJSON = 
  makeSpagoGraphModel $ readSpago_Raw_JSON_ moduleJSON.body packageJSON.body lsdepJSON.body locJSON.body


makeSpagoGraphModel :: Spago_Raw_JSON_ -> SpagoModel
makeSpagoGraphModel json = do
  let { nodes, links, name2ID, id2Name, id2Node, id2Package, id2LOC, sourceLinksMap } 
        = getGraphJSONData json

  { links
  , nodes    : (upgradeSpagoNodeData sourceLinksMap) <$> nodes
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



toggleSpotlight :: Event -> D3Simulation_ -> Datum_ -> Unit
toggleSpotlight event simulation d = toggleSpotlight_ event simulation nodeID nodeType
  where
    nodeID   = datum_.id d
    nodeType = show $ datum_.nodetype d

foreign import toggleSpotlight_ :: Event -> D3Simulation_ -> NodeID -> String -> Unit
foreign import cancelSpotlight_ :: D3Simulation_ -> Unit
