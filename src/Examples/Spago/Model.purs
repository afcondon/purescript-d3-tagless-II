module D3.Examples.Spago.Model where

import Prelude

import D3.Data.Tree (TreeLayout(..))
import D3.Data.Types (D3Simulation_, Datum_, Index_, PointXY, index_ToInt, intToIndex_)
import D3.Examples.Spago.Files (NodeType(..), Pinned(..), SpagoGraphLinkID, SpagoNodeData, SpagoNodeRow, Spago_Raw_JSON_, getGraphJSONData, readSpago_Raw_JSON_)
import D3.Examples.Spago.Unsafe (unboxD3SimLink, unboxD3SimNode, unboxD3TreeNode)
import D3.FFI (getIndexFromDatum_, hasChildren_, pinTreeNode_, setInSimNodeFlag)
import D3.Node (D3TreeRow, D3_FocusXY, D3_Radius, D3_SimulationNode(..), D3_VxyFxy, D3_XY, EmbeddedData, NodeID)
import D3.Scales (d3SchemeCategory10N_, d3SchemeDiverging10N_)
import Data.Array (foldl, partition)
import Data.Graph (Graph, fromMap)
import Data.Int (toNumber)
import Data.Lens.Record (prop)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Nullable (Nullable, notNull, toMaybe)
import Data.Nullable (Nullable, null) as N
import Data.Set as S
import Data.Tuple (Tuple(..))
import Math (pi, sqrt, (%))
import Math as Math
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Web.Event.Internal.Types (Event)

-- Model data types specialized with inital data
type SpagoTreeNode = D3TreeRow         (EmbeddedData SpagoNodeData                                  + ())
type SpagoSimNode  = D3_SimulationNode ( SpagoNodeRow  + D3_XY + D3_VxyFxy + D3_FocusXY + D3_Radius + ()) -- note we've woven in focusXY so that we can cluster the nodes

type SpagoModel = { 
    links :: Array SpagoGraphLinkID
  , nodes :: Array SpagoSimNode      -- already upgraded to simnode as a result of positioning when building the model
  , graph :: Graph NodeID SpagoNodeData
  , tree  :: Maybe (Tuple NodeID SpagoTreeNode)
  , maps  :: { name2ID    :: M.Map String NodeID
             , id2Name    :: M.Map NodeID String
             , id2Node    :: M.Map NodeID SpagoNodeData
             , id2Package :: M.Map NodeID NodeID
             , id2LOC     :: M.Map NodeID Number
             , id2XYLeaf  :: M.Map NodeID { x :: Number, y :: Number, isLeaf :: Boolean }
             }
}

tree_datum_ = {
    x           : _.x                <<< unboxD3TreeNode
  , y           : _.y                <<< unboxD3TreeNode
  , isLeaf      : _.isLeaf           <<< unboxD3TreeNode
  , containerID : _.data.containerID <<< unboxD3TreeNode
  , name        : _.data.name        <<< unboxD3TreeNode
  , loc         : _.data.loc         <<< unboxD3TreeNode
  , colorByGroup: d3SchemeCategory10N_ <<< toNumber <<< _.data.containerID <<< unboxD3TreeNode
  , textAnchor  : \l d -> case l of
                            Radial ->
                              if (hasChildren_ d) == (datum_.x d < pi)
                              then "start"
                              else "end"
                            _ -> 
                              if (hasChildren_ d)
                              then "start"
                              else "end"

  , textX       : \l d -> case l of
                      Radial ->
                        if (hasChildren_ d) == (datum_.x d < pi) -- d.x < pi => node is on the RHS of Radial tree
                        then 6.0
                        else (-6.0)
                      _ -> 
                        if (hasChildren_ d)
                        then 6.0
                        else (-6.0)

  , onRHS       : \l d -> if l == Radial && (datum_.x d >= pi)
                        then true
                        else false
}


link_ = {
    source    : _.source <<< unboxD3SimLink 
  , target    : _.target <<< unboxD3SimLink

  , linkClass :                          show     <<< _.linktype           <<< unboxD3SimLink
  , linkClass2: (append "updated ")  <<< show     <<< _.linktype           <<< unboxD3SimLink
  , color     : d3SchemeCategory10N_ <<< toNumber <<< _.target.containerID <<< unboxD3SimLink
}

-- TODO this is a ridiculously brittle and specific function to distribute package nodes on the screen, general solution needed here
cluster2Point :: Index_ -> PointXY
cluster2Point i =  
  scalePoint 200.0 200.0 $
  offsetXY { x: (-4.5), y: (-2.5) } $ -- center the grid on the (already centered) origin
  numberToGridPoint 10 (index_ToInt i)

tree2Point :: Nullable Number -> Nullable Number -> Maybe PointXY
tree2Point x' y' = do
  x <- toMaybe x'
  y <- toMaybe y'
  Just { x, y } 

-- | all the coercions in one place
datum_ = {
-- direct accessors to fields of the datum (BOILERPLATE)
    radius        : _.r             <<< unboxD3SimNode
  , id            : _.id            <<< unboxD3SimNode
  , loc           : _.loc           <<< unboxD3SimNode
  , containerID   : _.containerID   <<< unboxD3SimNode
  , containerName : _.containerName <<< unboxD3SimNode
  , name          : _.name          <<< unboxD3SimNode
  , x             : _.x             <<< unboxD3SimNode
  , y             : _.y             <<< unboxD3SimNode
  , treeX         : _.treeX         <<< unboxD3SimNode
  , treeY         : _.treeY         <<< unboxD3SimNode
  , nodetype      : _.nodetype      <<< unboxD3SimNode
  , cluster       : _.cluster       <<< unboxD3SimNode
  , links         : _.links         <<< unboxD3SimNode
  , connected     : _.connected     <<< unboxD3SimNode

  , nameAndID     : \d -> (unboxD3SimNode d).name <> " " <> show (unboxD3SimNode d).id
  , indexAndID    : \d -> (unboxD3SimNode d).name <> " " <> show (getIndexFromDatum_ d) <> " " <>  show (unboxD3SimNode d).id
  , namePos       : \d -> "(" <> show (Math.floor $ datum_.x d) <> "," <> show (Math.floor $ datum_.y d) <> ")" -- for debugging position

  , clusterPoint  : \d -> cluster2Point (intToIndex_ $ (unboxD3SimNode d).cluster - 5) -- TODO fix this dirty hack (magic number for start of package ids)
  , clusterPointX : \d -> _.x $ datum_.clusterPoint d
  , clusterPointY : \d -> _.y $ datum_.clusterPoint d

  , treePoint     : \d -> fromMaybe (datum_.clusterPoint d) (tree2Point (datum_.treeX d) (datum_.treeY d))
  , treePointX    : \d -> _.x $ datum_.treePoint d
  , treePointY    : \d -> _.y $ datum_.treePoint d

-- the crucial index function which allows us to reference Nodes from Links in JSON
  , indexFunction : _.id <<< unboxD3SimNode

-- more complicated calculations (CONVENIENCE)
  , positionLabel:
    \d -> case datum_.nodetype d of
            (IsModule _)  -> negate $ datum_.radius d
            (IsPackage _) -> 0.0 -- TODO move the magic numbers out by making the filter / groupFn available

  , collideRadius:
      \d -> 
        if datum_.id d == datum_.containerID d
        then 10.0
        else datum_.radius d

  , collideRadiusBig:
      \d -> (datum_.radius d) + 50.0

  , nodeClass:
      \d -> show (datum_.nodetype d) <> " " <> (datum_.containerName d) <> " " <> (datum_.name d) <> (if (datum_.connected d) then " connected" else "")
  , nodeClass':
      \d -> "updated" <> show (datum_.nodetype d) <> " " <> (datum_.containerName d) <> " " <> (datum_.name d) <> (if (datum_.connected d) then " connected" else "")
  , colorByGroup:
      \d -> d3SchemeCategory10N_ (toNumber $ datum_.cluster d)
  , colorByLevel:
      \d -> d3SchemeDiverging10N_ (toNumber $ datum_.id d) -- we don't have level in SimNode yet so let's test with id
  , colorByUsage:
      \d -> if (datum_.connected d) then "red" else "blue"
  , translateNode:
      \d -> "translate(" <> show (datum_.x d) <> "," <> show (datum_.y d) <> ")"
      
-- accessors to provide different force settings for different cohorts, quite possible that this should go thru a similar but different route from `datum`
  , isNamed: \name d -> \d -> datum_.name d == name
  , isPackage:
      \d -> case datum_.nodetype d of
              (IsModule _) -> false
              (IsPackage _) -> true
  , isModule:
      \d -> case datum_.nodetype d of
              (IsModule _) -> true
              (IsPackage _) -> false
  , isUnusedModule:
      \d -> case datum_.nodetype d of
              (IsPackage _) -> false
              (IsModule _)  -> if datum_.connected d 
                               then false
                               else true
              
  , isUsedModule:
      \d -> case datum_.nodetype d of
              (IsPackage _) -> false
              (IsModule _)  -> if datum_.connected d 
                               then true
                               else false
              
}

-- type LinkFilter = forall r. D3_SimulationNode r -> Boolean
noFilter :: SpagoSimNode -> Boolean
noFilter = const true
isPackage :: SpagoSimNode -> Boolean
isPackage (D3SimNode d) =
  case d.nodetype of
    (IsModule _) -> false
    (IsPackage _) -> true
isModule :: SpagoSimNode -> Boolean
isModule (D3SimNode d) =
  case d.nodetype of
    (IsModule _) -> true
    (IsPackage _) -> false
isUsedModule :: SpagoSimNode -> Boolean
isUsedModule (D3SimNode d) =
  case d.nodetype of
    (IsPackage _) -> false
    (IsModule _) -> if d.connected 
                    then true
                    else false
              
upgradeSpagoNodeData :: M.Map NodeID (Array NodeID) -> SpagoNodeData -> SpagoSimNode
upgradeSpagoNodeData sourcesMap node = D3SimNode { 
    links        : node.links { sources = fromMaybe [] $ M.lookup node.id sourcesMap }
  , id           : node.id
  , cluster      : node.containerID -- TODO cluster all the single module packages together
  , connected    : node.connected
  , containerID  : node.containerID
  , containerName: node.containerName
  , containsMany : node.containsMany
  , focusX       : 0.0
  , focusY       : 0.0
  , fx           : (N.null :: N.Nullable Number)
  , fy           : (N.null :: N.Nullable Number)
  , inSim        : true
  , loc          : node.loc
  , name         : node.name
  , nodetype     : node.nodetype
  , pinned       : Floating
  , r            : sqrt node.loc
  , treeX        : (N.null :: N.Nullable Number)
  , treeY        : (N.null :: N.Nullable Number)
  , vx           : 0.0
  , vy           : 0.0
  , x            : 0.0
  , y            : 0.0
  }

numberToGridPoint :: Int -> Int -> PointXY
numberToGridPoint columns i = do
  let
    c = toNumber columns
    d = toNumber i
    x = (d % c)
    y = Math.floor (d / c)
    -- _ = trace { numberToGridPoint: i, columns, x, y } \_ -> unit
  { x, y }

scalePoint :: Number -> Number -> PointXY -> PointXY
scalePoint xFactor yFactor xy = { x: xy.x * xFactor, y: xy.y * yFactor }

offsetXY :: PointXY -> PointXY -> PointXY
offsetXY offset xy = { x: xy.x + offset.x, y: xy.y + offset.y }

offsetX :: Number -> PointXY -> PointXY
offsetX xOffset xy = xy { x = xy.x + xOffset }

offsetY :: Number -> PointXY -> PointXY
offsetY yOffset xy = xy { y = xy.y + yOffset }

pinNode :: PointXY -> SpagoSimNode -> SpagoSimNode
pinNode xy (D3SimNode node) = D3SimNode (node { fx = notNull xy.x, fy = notNull xy.y } )

setXY :: SpagoSimNode -> { x :: Number, y :: Number } -> SpagoSimNode
setXY (D3SimNode node) { x, y } = D3SimNode (node { x = x, y = y })

setXYIncludingLeaves :: SpagoSimNode -> { x :: Number, y :: Number, isLeaf :: Boolean } -> SpagoSimNode
setXYIncludingLeaves (D3SimNode node) { x, y } =
  D3SimNode (node { x = x, y = y, treeX = notNull x, treeY = notNull y, connected = true, pinned = Forced })

setXYExceptLeaves :: SpagoSimNode -> { x :: Number, y :: Number, isLeaf :: Boolean } -> SpagoSimNode
setXYExceptLeaves (D3SimNode node) { x, y, isLeaf: true }  = 
  D3SimNode node { treeX = (N.null :: Nullable Number), treeY = (N.null :: Nullable Number), connected = true }
setXYExceptLeaves (D3SimNode node) { x, y, isLeaf: false } =
  D3SimNode (node { x = x, y = y, treeX = notNull x, treeY = notNull y, connected = true, pinned = Forced })

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

  { links    : links
  , nodes    : nodes <#> upgradeSpagoNodeData sourceLinksMap
  , graph    : makeGraph nodes
  , tree     : Nothing  -- not present in the JSON, has to be calculated, if possible
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

-- foreign functions to implement Spotlight, essentially because it's easier to prototype some 
-- behaviors in D3js while deciding whether/how to express them in PureScript
foreign import toggleSpotlight_ :: Event -> D3Simulation_ -> NodeID -> String -> Unit
foreign import cancelSpotlight_ :: D3Simulation_ -> Unit

-- this is going to be another side-effecting function since it will change the fx/fy of selected nodes
modifyModelNodesInPlace :: (SpagoSimNode -> SpagoSimNode) -> SpagoModel -> (SpagoSimNode -> Boolean) ->  SpagoModel
modifyModelNodesInPlace fn model predicate = model { nodes = updatedNodes }
  where
    nodes = partition predicate model.nodes 
    updatedNodes = (fn <$> nodes.yes) <> nodes.no

markNodesToAddToSimulation :: SpagoModel -> (SpagoSimNode -> Boolean) -> SpagoModel
markNodesToAddToSimulation = modifyModelNodesInPlace setInSimNodeFlag

pinNodesInModel :: PointXY -> SpagoModel -> (SpagoSimNode -> Boolean) -> SpagoModel
pinNodesInModel xy = modifyModelNodesInPlace (pinNode xy)


modifyNodesInPlace :: (SpagoSimNode -> SpagoSimNode) -> Array SpagoSimNode -> (SpagoSimNode -> Boolean) ->  Array SpagoSimNode
modifyNodesInPlace fn nodes predicate = updatedNodes
  where
    nodes' = partition predicate nodes 
    updatedNodes = (fn <$> nodes'.yes) <> nodes'.no

pinNodesByPredicate :: PointXY -> Array SpagoSimNode -> (SpagoSimNode -> Boolean) -> Array SpagoSimNode
pinNodesByPredicate xy = modifyNodesInPlace (pinNode xy)
