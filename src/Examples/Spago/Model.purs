module D3.Examples.Spago.Model where

import Prelude

import D3.Data.Tree (TreeLayout(..))
import D3.Data.Types (D3Simulation_, Datum_, Index_, PointXY, index_ToInt, intToIndex_)
import D3.Examples.Spago.Files (LinkType(..), NodeType(..), Pinned(..), SpagoGraphLinkID, SpagoNodeData, SpagoNodeRow, Spago_Raw_JSON_, getGraphJSONData, readSpago_Raw_JSON_)
import D3.Examples.Spago.Unsafe (unboxD3SimLink, unboxD3SimNode, unboxD3TreeNode)
import D3.FFI (hasChildren_)
import D3.Node (D3SimulationRow, D3TreeRow, D3_FocusXY, D3_Link(..), D3_Radius, D3_SimulationNode(..), EmbeddedData, NodeID)
import D3.Scales (d3SchemeCategory10N_)
import Data.Array (filter, foldl)
import Data.Graph (Graph, fromMap)
import Data.Int (toNumber)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable, notNull, toMaybe)
import Data.Nullable (Nullable, null) as N
import Data.Set as S
import Data.Tuple (Tuple(..))
import Debug (trace)
import Effect.Class.Console (log)
import Math (pi, sqrt, (%))
import Math as Math
import Type.Row (type (+))
import Web.Event.Internal.Types (Event)

moduleRadius       =  5.0 :: Number 
packageRadius      = 50.0 :: Number
packageForceRadius = 50.0 :: Number

tree_datum_ = {
    x           : \d -> (unboxD3TreeNode d).x
  , y           : \d -> (unboxD3TreeNode d).y
  , isLeaf      : \d -> (unboxD3TreeNode d).isLeaf
  , containerID : \d -> (unboxD3TreeNode d).data.containerID
  , name        : \d -> (unboxD3TreeNode d).data.name
  , loc         : \d -> (unboxD3TreeNode d).data.loc
  , colorByGroup: \d -> d3SchemeCategory10N_ (toNumber $ tree_datum_.containerID d)
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
    source    : \d -> (unboxD3SimLink d).source
  , target    : \d -> (unboxD3SimLink d).target
  , linkClass : \d -> show (unboxD3SimLink d).linktype
  , linkClass2: \d -> "updated " <> show (unboxD3SimLink d).linktype
  , color     : \d -> d3SchemeCategory10N_ (toNumber $ (unboxD3SimLink d).target.containerID)
}

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
    radius        : \d -> (unboxD3SimNode d).r
  , id            : \d -> (unboxD3SimNode d).id
  , loc           : \d -> (unboxD3SimNode d).loc
  , containerID   : \d -> (unboxD3SimNode d).containerID
  , containerName : \d -> (unboxD3SimNode d).containerName
  , name          : \d -> (unboxD3SimNode d).name
  , namePos       : \d -> "(" <> show (Math.floor $ datum_.x d) <> "," <> show (Math.floor $ datum_.y d) <> ")" -- for debugging position
  , x             : \d -> (unboxD3SimNode d).x
  , y             : \d -> (unboxD3SimNode d).y
  , treeX         : \d -> (unboxD3SimNode d).treeX
  , treeY         : \d -> (unboxD3SimNode d).treeY
  , nodetype      : \d -> (unboxD3SimNode d).nodetype
  , cluster       : \d -> (unboxD3SimNode d).cluster
  , links         : \d -> (unboxD3SimNode d).links
  , connected     : \d -> (unboxD3SimNode d).connected

  , clusterPoint  : \d -> cluster2Point (intToIndex_ $ (unboxD3SimNode d).cluster - 462) -- TODO fix this dirty hack
  , clusterPointX : \d -> _.x $ datum_.clusterPoint d
  , clusterPointY : \d -> _.y $ datum_.clusterPoint d
  , treePoint     : \d -> fromMaybe (datum_.clusterPoint d) (tree2Point (datum_.treeX d) (datum_.treeY d))
  , treePointX    : \d -> _.x $ datum_.treePoint d
  , treePointY    : \d -> _.y $ datum_.treePoint d

-- the crucial index function which allows us to reference Nodes from Links in JSON
  , indexFunction : \d -> (unboxD3SimNode d).id

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
      \d -> (datum_.radius d) + 30.0

  , nodeClass:
      \d -> show (datum_.nodetype d) <> " " <> (datum_.containerName d) <> " " <> (datum_.name d)
  , colorByGroup:
      \d -> d3SchemeCategory10N_ (toNumber $ datum_.cluster d)
  , translateNode:
      \d -> "translate(" <> show (datum_.x d) <> "," <> show (datum_.y d) <> ")"
      
-- accessors to provide different force settings for different cohorts, quite possible that this should go thru a similar but different route from `datum`
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

isPackage (D3SimNode d) =
  case d.nodetype of
    (IsModule _) -> false
    (IsPackage _) -> true
isModule (D3SimNode d) =
  case d.nodetype of
    (IsModule _) -> true
    (IsPackage _) -> false
isUsedModule (D3SimNode d) =
  case d.nodetype of
    (IsPackage _) -> false
    (IsModule _) -> if d.connected 
                    then true
                    else false
              
-- Model data types specialized with inital data
type SpagoTreeNode    = D3TreeRow       (EmbeddedData SpagoNodeData + ())
type SpagoSimNode     = D3SimulationRow (             SpagoNodeRow  + D3_FocusXY + D3_Radius + ()) -- note we've woven in focusXY so that we can cluster the nodes

type SpagoModel = { 
    links           :: {
        allLinks     :: Array SpagoGraphLinkID
      , treeLinks    :: Array SpagoGraphLinkID  -- each ID will get swizzled for a SpagoGraphLinkObj_ when simulation initialized
      , prunedLinks  :: Array SpagoGraphLinkID  -- there are so many of these that we only use them when hovering enabled
      , packageLinks :: Array SpagoGraphLinkID
    }
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

pinNode :: SpagoSimNode -> PointXY -> SpagoSimNode
pinNode (D3SimNode node) xy = D3SimNode (node { fx = notNull xy.x, fy = notNull xy.y } )

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

  { links : {
      allLinks: links
    , treeLinks: []
    , prunedLinks: []
    , packageLinks: filter (\(D3_Link l) -> l.linktype == P2P) links
  }
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
