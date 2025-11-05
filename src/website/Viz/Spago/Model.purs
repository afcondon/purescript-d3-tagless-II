module D3.Viz.Spago.Model where

import Prelude

import PSD3.Internal.Attributes.Instances (Label)
import PSD3.Internal.Types (D3Simulation_, Datum_, PointXY)
import D3.Viz.Spago.Files (LinkType(..), NodeType(..), SpagoGraphLinkID, SpagoNodeData, SpagoNodeRow, Spago_Raw_JSON_, getGraphJSONData, readSpago_Raw_JSON_)
import D3.Viz.Spago.Unsafe (unboxD3SimLink, unboxD3SimNode)
import PSD3.Internal.FFI (getIndexFromDatum_, setInSimNodeFlag)
import PSD3.Data.Node (D3TreeRow, D3_FocusXY, D3_Radius, D3_SimulationNode(..), D3_VxyFxy, D3_XY, EmbeddedData, NodeID)
import PSD3.Internal.Scales.Scales (d3SchemeCategory10N_, d3SchemeSequential10N_)
import Data.Array (foldl, length, mapWithIndex, partition, (:))
import Data.Array (null) as A
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Graph (Graph, fromMap)
import Data.Int (toNumber)
import Data.Int (floor) as Int
import Data.List as L
import Data.Map (fromFoldable, lookup)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable, notNull, null, toMaybe)
import Data.Nullable (Nullable, null) as N
import Data.Number (ceil, cos, sin, sqrt, pi, (%))
import Data.Number (floor) as Number
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Type.Row (type (+))
import Web.Event.Internal.Types (Event)


-- Model data types specialized with inital data
type SpagoTreeNode = D3TreeRow         (EmbeddedData SpagoNodeData                                  + ())
type SpagoSimNode  = D3_SimulationNode ( SpagoNodeRow  + D3_XY + D3_VxyFxy + D3_FocusXY + D3_Radius + ()) -- note we've woven in focusXY so that we can cluster the nodes
-- | this is the only data that we're bringing over from the SpagoTreeNode to SpagoSimNode (at the momment)
type TreeFields = { x :: Number, y :: Number, isTreeLeaf :: Boolean, depth :: Int, childIDs :: Array NodeID }

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
             , id2TreeData :: M.Map NodeID TreeFields
             }
}


link_ = {
    source    : _.source <<< unboxD3SimLink 
  , target    : _.target <<< unboxD3SimLink
  , linkType  : _.linktype <<< unboxD3SimLink

  , linkClass :                          show     <<< _.linktype           <<< unboxD3SimLink
  , linkClass2: (append "updated ")  <<< show     <<< _.linktype           <<< unboxD3SimLink
  , color     : d3SchemeCategory10N_ <<< toNumber <<< _.target.containerID <<< unboxD3SimLink
}

isP2P_Link_ l = link_.linkType l == P2P
isM2P_Link_ l = link_.linkType l == M2P
isM2M_Graph_Link_ l = link_.linkType l == M2M_Graph
isM2M_Tree_Link_ l = link_.linkType l == M2M_Tree

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
  , fx            : _.fx            <<< unboxD3SimNode
  , fy            : _.fy            <<< unboxD3SimNode
  , treeXY        : _.treeXY        <<< unboxD3SimNode
  , treeDepth     : _.treeDepth     <<< unboxD3SimNode
  , gridXY        : _.gridXY        <<< unboxD3SimNode
  , nodetype      : _.nodetype      <<< unboxD3SimNode
  , cluster       : _.cluster       <<< unboxD3SimNode
  , links         : _.links         <<< unboxD3SimNode
  , connected     : _.connected     <<< unboxD3SimNode

  , nameAndID     : \d -> (unboxD3SimNode d).name <> " " <> show (unboxD3SimNode d).id
  , indexAndID    : \d -> (unboxD3SimNode d).name <> " " <> show (getIndexFromDatum_ d) <> " " <>  show (unboxD3SimNode d).id
  , namePos       : \d -> "(" <> show (Number.floor $ datum_.x d) <> "," <> show (Number.floor $ datum_.y d) <> ")" -- for debugging position

  , gridPoint     : \d -> fromMaybe { x: datum_.x d, y: datum_.y d} $ toMaybe (datum_.gridXY d)
  , gridPointX    : \d -> (_.x $ datum_.gridPoint d)
  , gridPointY    : \d -> (_.y $ datum_.gridPoint d)

  , treePoint     : \d -> fromMaybe { x: datum_.x d, y: datum_.y d} $ toMaybe (datum_.treeXY d)
  , treePointX    : \d -> _.x $ datum_.treePoint d
  , treePointY    : \d -> _.y $ datum_.treePoint d

-- the crucial index function which allows us to reference Nodes from Links in JSON
  , indexFunction : (_.id <<< unboxD3SimNode)

-- more complicated calculations (CONVENIENCE)
  , positionLabel:
    \d -> case datum_.nodetype d of
            (IsModule _)  -> negate $ datum_.radius d  -- position below center
            (IsPackage _) -> 0.0  -- position at center

  , collideRadius:
      \d -> 
        if datum_.id d == datum_.containerID d
        then 10.0
        else datum_.radius d

  , collideRadiusBig:
      \d -> (datum_.radius d) + 10.0

  , nodeClass:
      \d -> show (datum_.nodetype d) <> " " <> (datum_.containerName d) <> " " <> (datum_.name d) <> (if (datum_.connected d) then " connected" else "")
  , nodeClass':
      \d -> "updated" <> show (datum_.nodetype d) <> " " <> (datum_.containerName d) <> " " <> (datum_.name d) <> (if (datum_.connected d) then " connected" else "")
  , colorByGroup:
      \d -> d3SchemeCategory10N_ (toNumber $ datum_.cluster d)
  , colorByDepth:
      \d -> case toMaybe $ datum_.treeDepth d of
              Nothing -> "none"
              Just depth -> d3SchemeSequential10N_ (toNumber depth)
  , fillByUsage:
      \d -> if (datum_.connected d) then datum_.colorByGroup d else "none"
  , strokeByUsage:
      \d -> if (datum_.connected d) then "none" else datum_.colorByGroup d
  , colorByUsage:
      \d -> if (datum_.connected d) then "red" else "blue"
  , opacityByType:
      \d -> if (datum_.isPackage d) then 0.4 else 0.7
  , translateNode:
      \d -> "translate(" <> show (datum_.x d) <> "," <> show (datum_.y d) <> ")"
      
-- accessors to provide different force settings for different cohorts, quite possible that this should go thru a similar but different route from `datum`
  , isNamed: \name _ -> \d -> datum_.name d == name
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
  , treeChildren:
      \d -> (datum_.links d).treeChildren              
  , isTreeParent: -- simplifying assumption here that we don't need or care to check if its actually a tree node or not, just that it has tree children
      \d -> not $ A.null $ datum_.treeChildren d
              
}

-- type LinkFilter = forall r. D3_SimulationNode r -> Boolean
allNodes :: SpagoSimNode -> Boolean
allNodes = const true
isPackage :: SpagoSimNode -> Boolean
isPackage (D3SimNode d) =
  case d.nodetype of
    (IsModule _) -> false
    (IsPackage _) -> true
isPackageOrVisibleModule :: NodeID -> SpagoSimNode -> Boolean
isPackageOrVisibleModule id (D3SimNode d) =
  case d.nodetype of
    (IsModule _) -> d.containerID == id -- include modules whose
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

sourcePackageIs :: String -> Datum_ -> Boolean
sourcePackageIs name link = (link_.source link).name == name
              
upgradeSpagoNodeData :: M.Map NodeID (Array NodeID) -> SpagoNodeData -> SpagoSimNode
upgradeSpagoNodeData sourcesMap node = D3SimNode {
    links        : node.links { sources = fromMaybe [] $ M.lookup node.id sourcesMap }
  , id           : node.id
  , cluster      : node.containerID  -- packages cluster by their own ID, modules by their container
  , connected    : node.connected
  , showChildren : case node.nodetype of
                      (IsPackage _) -> true
                      (IsModule _)  -> false
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
  , r            : sqrt node.loc
  , treeXY       : (N.null :: N.Nullable PointXY)
  , treeDepth    : (N.null :: N.Nullable Int)
  , gridXY       : (N.null :: N.Nullable PointXY)
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
    y = Number.floor (d / c)
    -- _ = trace { numberToGridPoint: i, columns, x, y } \_ -> unit
  { x, y }

-- | ==================================================================================================================
-- | collection of functions for initializing nodes prior to putting them in the simulation
-- | ==================================================================================================================

-- | setting the fx / fy to null ensures that D3 will calculate their positions as normal
unpinAllNodes :: Array SpagoSimNode -> Array SpagoSimNode
unpinAllNodes nodes = unpin <$> nodes
  where
    -- unpin (D3SimNode d) = D3SimNode $ unpinNode_ d
    unpin :: SpagoSimNode -> SpagoSimNode
    unpin (D3SimNode d) = D3SimNode d { fx = (null :: Nullable Number), fy = (null :: Nullable Number) }

-- | put (only) package nodes on a fixed grid
packageNodesToGridXY :: Array SpagoSimNode -> Array SpagoSimNode
packageNodesToGridXY nodes = partitioned.no <> packagesWithGrid
  where
    -- we're going to set gridXY of packages and then make modules have gridXY of their containing package
    partitioned = partition isPackage nodes
    packageCount = length partitioned.yes
    -- | we want a square (eventually rect) that is large enough to hold all the packages
    -- nearestSquare = pow (ceil $ sqrt packageCount) 2.0
    -- | columns would be sqrt of nearestSquare, so we simply don't square it
    -- | when extending this to a rectangle we will actually need the square tho
    columns = Int.floor $ ceil $ sqrt $ toNumber packageCount -- we don't actually ever need rows
    offset  = -((toNumber columns) / 2.0)

    packagesWithGrid = foldlWithIndex (\i b a -> (setGridXY a i) : b) [] partitioned.yes
      where 
        setGridXY (D3SimNode p) i = do
          let gridXY = scalePoint 200.0 200.0 $ offsetXY { x: offset, y: offset } $ numberToGridPoint columns i 
          D3SimNode p { gridXY = notNull gridXY } -- , fx = notNull gridXY.x, fy = notNull gridXY.y }

moduleNodesToContainerXY :: Array SpagoSimNode -> Array SpagoSimNode
moduleNodesToContainerXY nodes = modulesWithGrid <> partitioned.yes
  where
    -- we're going to set gridXY of packages and then make modules have gridXY of their containing package
    partitioned = partition isPackage nodes

    packagesIndexMap = 
      fromFoldable $
      foldl (\b (D3SimNode a) -> (Tuple a.id a.gridXY) : b) [] partitioned.yes

    modulesWithGrid = map setModuleGridXY partitioned.no

    setModuleGridXY (D3SimNode m) =
      case lookup m.containerID packagesIndexMap of
        Nothing -> D3SimNode m -- shouldn't be possible, but a noop is fine if not found
        Just gridXY -> do
          case toMaybe gridXY of
            Nothing ->  D3SimNode m { x = 0.0, y = 0.0, gridXY = gridXY }
            Just xy ->  D3SimNode m { x = xy.x, y = xy.y, gridXY = gridXY }

packagesNodesToPhyllotaxis :: Array SpagoSimNode -> Array SpagoSimNode
packagesNodesToPhyllotaxis = nodesToPhyllotaxis isPackage

modulesNodesToPhyllotaxis :: Array SpagoSimNode -> Array SpagoSimNode
modulesNodesToPhyllotaxis = nodesToPhyllotaxis isModule

-- | layout nodes in "sunflower pattern", both pleasing to the eye and good as a starting position for sim
-- | (no two nodes in same spot). It's impossible to do this using D3 in simulation update situation
nodesToPhyllotaxis :: (SpagoSimNode -> Boolean) -> Array SpagoSimNode -> Array SpagoSimNode
nodesToPhyllotaxis predicate nodes = partitioned.no <> (setForPhyllotaxis `mapWithIndex` partitioned.yes)
  where
    partitioned = partition predicate nodes

initialRadius = 10.0
initialAngle = pi * (3.0 - sqrt 5.0)

-- | Position a node in a sunflower/phyllotaxis pattern based on its index.
-- | This creates aesthetically pleasing, evenly-distributed circular layouts.
setForPhyllotaxis :: Int -> SpagoSimNode -> SpagoSimNode
setForPhyllotaxis index (D3SimNode d) = D3SimNode $ d { x = (radius * cos angle), y = (radius * sin angle) }
  where
    i = toNumber index
    radius = initialRadius * sqrt (0.5 + i)
    angle  = i * initialAngle

treeNodesToTreeXY_H :: Array SpagoSimNode -> Array SpagoSimNode
treeNodesToTreeXY_H nodes = partitioned.no <> (setXYtoTreeXY <$> partitioned.yes)
  where
    partitioned = partition isUsedModule nodes
    setXYtoTreeXY :: SpagoSimNode -> SpagoSimNode
    setXYtoTreeXY (D3SimNode d) = D3SimNode $ d { fx = notNull treeXY.x, fy = notNull treeXY.y }
      where treeXY = fromMaybe { x: d.x, y: d.y } $ toMaybe d.treeXY

-- same as horizontal tree but uses x and y as polar coordinates, computes fx/fy from them
treeNodesToTreeXY_R :: Array SpagoSimNode -> Array SpagoSimNode
treeNodesToTreeXY_R nodes = partitioned.no <> (setXYtoTreeXY <$> partitioned.yes)
  where
    partitioned = partition isUsedModule nodes
    setXYtoTreeXY :: SpagoSimNode -> SpagoSimNode
    setXYtoTreeXY (D3SimNode d) = D3SimNode $ d { fx = notNull radialXY.x, fy = notNull radialXY.y }
      where treeXY = fromMaybe { x: d.x, y: d.y } $ toMaybe d.treeXY
            radialXY = radialTranslate treeXY
            -- for radial positioning we treat x as angle and y as radius
            radialTranslate :: PointXY -> PointXY
            radialTranslate p = 
              let angle  = p.y -- reversed because horizontal tree is the default this should change
                  radius = p.x
                  x = radius * cos angle
                  y = radius * sin angle
              in { x, y }

-- same as horizontal tree but reverses {x,y} and {fx,fy}
treeNodesToTreeXY_V :: Array SpagoSimNode -> Array SpagoSimNode
treeNodesToTreeXY_V nodes = partitioned.no <> (setXYtoTreeXY <$> partitioned.yes)
  where
    partitioned = partition isUsedModule nodes
    setXYtoTreeXY :: SpagoSimNode -> SpagoSimNode
    setXYtoTreeXY (D3SimNode d) = D3SimNode $ d { fx = notNull treeXY.y, fy = notNull treeXY.x }
      where treeXY = fromMaybe { x: d.y, y: d.x } $ toMaybe d.treeXY

fixNamedNodeTo :: Label -> PointXY -> Array SpagoSimNode -> Array SpagoSimNode
fixNamedNodeTo label point nodes = fixNamedNode' <$> nodes
  where
    fixNamedNode' (D3SimNode d) = if d.name == label
                                  then D3SimNode d { fx = notNull point.x, fy = notNull point.y }
                                  else D3SimNode d

fixNamedNode :: Label -> Array SpagoSimNode -> Array SpagoSimNode
fixNamedNode label nodes = fixNamedNode' <$> nodes
  where
    fixNamedNode' (D3SimNode d) = if d.name == label
                                  then D3SimNode d { fx = notNull d.x, fy = notNull d.y }
                                  else D3SimNode d

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

setTreeXYIncludingLeaves :: SpagoSimNode -> TreeFields -> SpagoSimNode
setTreeXYIncludingLeaves (D3SimNode node) { x, y, depth, childIDs } =
  D3SimNode (node { treeXY = notNull {x, y}, treeDepth = notNull depth, connected = true, links { treeChildren = childIDs } })

setTreeXYExceptLeaves :: SpagoSimNode -> TreeFields -> SpagoSimNode
setTreeXYExceptLeaves (D3SimNode node) { depth, isTreeLeaf: true }  = 
  D3SimNode node { treeXY = (N.null :: Nullable PointXY), treeDepth = notNull depth, connected = true }
setTreeXYExceptLeaves (D3SimNode node) { x, y, depth, isTreeLeaf: false, childIDs } =
  D3SimNode (node { treeXY = notNull { x,y }, treeDepth = notNull depth, connected = true, links { treeChildren = childIDs } })

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
               , id2TreeData: M.empty
               }
  }

makeGraph :: Array SpagoNodeData -> Graph NodeID SpagoNodeData
makeGraph nodes = do
  let
    graphMap = foldl addNode M.empty nodes
    -- addNode :: M.Map NodeID (Tuple SpagoNodeData (S.Set NodeID)) -> SpagoNodeData -> M.Map NodeID (Tuple SpagoNodeData (S.Set NodeID))
    addNode acc node = M.insert node.id (Tuple node depends) acc
      where
        depends :: L.List Int
        depends = L.fromFoldable node.links.targets
  fromMap graphMap


-- explodePackages :: Event -> D3Simulation_ -> Datum_ -> Unit
-- explodePackages event simulation d = explodePackages_ event simulation nodeID nodeType
--   where
--     nodeID   = datum_.id d
--     nodeType = show $ datum_.nodetype d

-- toggleSpotlight :: Event -> D3Simulation_ -> Datum_ -> Unit
-- toggleSpotlight event simulation d = toggleSpotlight_ event simulation nodeID nodeType
--   where
--     nodeID   = datum_.id d
--     nodeType = show $ datum_.nodetype d

-- foreign functions to implement Spotlight, essentially because it's easier to prototype some 
-- behaviors in D3js while deciding whether/how to express them in PureScript
-- REVIEW make a common signature type for click handlers
foreign import explodePackages_ :: Event -> D3Simulation_ -> NodeID -> String -> Unit
foreign import toggleSpotlight_ :: Event -> D3Simulation_ -> NodeID -> String -> Unit
foreign import cancelSpotlight_ :: D3Simulation_ -> Unit

-- | FFI: Transition nodes to pinned positions with smooth D3 animation
-- | Works with Group elements positioned via transform attribute
-- | Used for scene transitions to grid and tree layouts
foreign import transitionNodesToPinnedPositions_
  :: forall d.
     String                        -- SVG selector
  -> String                        -- Node selector (Groups)
  -> String                        -- Link selector (Lines)
  -> Array (D3_SimulationNode d)   -- Nodes with target fx/fy set
  -> Effect Unit                   -- Completion callback
  -> Effect Unit

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
