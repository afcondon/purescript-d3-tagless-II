module D3.Viz.Spago.Model where

import Prelude

import PSD3.Internal.Attributes.Instances (Label)
import PSD3.Internal.Types (D3Simulation_, PointXY)
import D3.Viz.Spago.Files (D3_Radius, D3TreeRow, EmbeddedData, LinkType(..), NodeType(..), SpagoNodeData, SpagoNodeRow, Spago_Raw_JSON_, getGraphJSONData, readSpago_Raw_JSON_)
import PSD3.Internal.FFI (getIndexFromDatum_, setInSimNodeFlag)
import PSD3.Data.Node (D3Link_Unswizzled, D3Link_Swizzled, D3_FocusXY, SimulationNode, D3_VxyFxy, D3_XY, NodeID)
import PSD3.Internal.Scales.Scales (d3SchemeCategory10N_, d3SchemeSequential10N_)
import Data.Array (foldl, length, mapWithIndex, partition, (:))
import Data.Array (null) as A
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Graph (Graph, fromMap)
import PSD3.Data.Graph (GraphConfig, GraphModel, buildGraphModel, toDataGraph)
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
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Internal.Types (Event)


-- Model data types specialized with inital data
type SpagoTreeNode = Record (D3TreeRow (EmbeddedData SpagoNodeData + ()))
type SpagoSimNode  = SimulationNode ( SpagoNodeRow  + D3_FocusXY + D3_Radius + ()) -- SimulationNode already includes D3_XY + D3_VxyFxy
-- | this is the only data that we're bringing over from the SpagoTreeNode to SpagoSimNode (at the momment)
type TreeFields = { x :: Number, y :: Number, isTreeLeaf :: Boolean, depth :: Int, childIDs :: Array NodeID }

type SpagoModel = {
    links :: Array D3Link_Unswizzled
  , nodes :: Array SpagoSimNode      -- already upgraded to simnode as a result of positioning when building the model
  , graph :: Graph NodeID SpagoNodeData
  , graphModel :: GraphModel SpagoNodeData D3Link_Unswizzled  -- Generic graph infrastructure
  , tree  :: Maybe (Tuple NodeID SpagoTreeNode)
  , maps  :: { name2ID    :: M.Map String NodeID
             , id2Name    :: M.Map NodeID String
             -- id2Node removed: use graphModel.maps.nodeById instead
             , id2Package :: M.Map NodeID NodeID
             , id2LOC     :: M.Map NodeID Number
             , id2TreeData :: M.Map NodeID TreeFields
             }
}


-- PHANTOM TYPES: No more link_/datum_ accessor modules!
-- Use inline type annotations with DatumFn when needed.
-- Helper type aliases for swizzled links (what D3 returns after swizzling)
type SpagoSwizzledLink = { source :: SpagoSimNode, target :: SpagoSimNode, linktype :: LinkType }

-- Helper functions for link predicates (using phantom types)
isP2P_Link :: SpagoSwizzledLink -> Boolean
isP2P_Link l = l.linktype == P2P

isM2P_Link :: SpagoSwizzledLink -> Boolean
isM2P_Link l = l.linktype == M2P

isM2M_Graph_Link :: SpagoSwizzledLink -> Boolean
isM2M_Graph_Link l = l.linktype == M2M_Graph

isM2M_Tree_Link :: SpagoSwizzledLink -> Boolean
isM2M_Tree_Link l = l.linktype == M2M_Tree

-- OLD BOILERPLATE REMOVED - use phantom types instead:
{-
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
-}

-- Node predicate functions (phantom type friendly)
allNodes :: SpagoSimNode -> Boolean
allNodes = const true

isPackage :: SpagoSimNode -> Boolean
isPackage d = case d.nodetype of
  (IsModule _) -> false
  (IsPackage _) -> true

isPackageOrVisibleModule :: NodeID -> SpagoSimNode -> Boolean
isPackageOrVisibleModule id d = case d.nodetype of
  (IsModule _) -> d.containerID == id
  (IsPackage _) -> true

isModule :: SpagoSimNode -> Boolean
isModule d = case d.nodetype of
  (IsModule _) -> true
  (IsPackage _) -> false

isUsedModule :: SpagoSimNode -> Boolean
isUsedModule d = case d.nodetype of
  (IsPackage _) -> false
  (IsModule _) -> d.connected
              
upgradeSpagoNodeData :: M.Map NodeID (Array NodeID) -> SpagoNodeData -> SpagoSimNode
upgradeSpagoNodeData sourcesMap node = {
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
    unpin :: SpagoSimNode -> SpagoSimNode
    unpin d = d { fx = (null :: Nullable Number), fy = (null :: Nullable Number) }

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
        setGridXY p i = do
          let gridXY = scalePoint 200.0 200.0 $ offsetXY { x: offset, y: offset } $ numberToGridPoint columns i
          -- Set x/y for transition, gridXY for cluster forces
          -- Don't pin (fx/fy) - let forces position nodes during simulation
          p { gridXY = notNull gridXY, x = gridXY.x, y = gridXY.y }

moduleNodesToContainerXY :: Array SpagoSimNode -> Array SpagoSimNode
moduleNodesToContainerXY nodes = modulesWithGrid <> partitioned.yes
  where
    -- we're going to set gridXY of packages and then make modules have gridXY of their containing package
    partitioned = partition isPackage nodes

    packagesIndexMap =
      fromFoldable $
      foldl (\b a -> (Tuple a.id a.gridXY) : b) [] partitioned.yes

    modulesWithGrid = map setModuleGridXY partitioned.no

    setModuleGridXY m =
      case lookup m.containerID packagesIndexMap of
        Nothing -> m -- shouldn't be possible, but a noop is fine if not found
        Just gridXY -> do
          case toMaybe gridXY of
            Nothing -> m { x = 0.0, y = 0.0, gridXY = gridXY }
            Just xy -> m { x = xy.x, y = xy.y, gridXY = gridXY }

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
setForPhyllotaxis index d = d { x = (radius * cos angle), y = (radius * sin angle) }
  where
    i = toNumber index
    radius = initialRadius * sqrt (0.5 + i)
    angle  = i * initialAngle

treeNodesToTreeXY_H :: Array SpagoSimNode -> Array SpagoSimNode
treeNodesToTreeXY_H nodes = partitioned.no <> (setXYtoTreeXY <$> partitioned.yes)
  where
    partitioned = partition isUsedModule nodes
    setXYtoTreeXY :: SpagoSimNode -> SpagoSimNode
    -- Horizontal tree: swap D3's (breadth=x, depth=y) to screen (depth=x, breadth=y)
    setXYtoTreeXY d = d { treeXY = notNull treeXY, x = treeXY.y, y = treeXY.x }
      where treeXY = fromMaybe { x: d.y, y: d.x } $ toMaybe d.treeXY

-- same as horizontal tree but uses x and y as polar coordinates, computes fx/fy from them
treeNodesToTreeXY_R :: Array SpagoSimNode -> Array SpagoSimNode
treeNodesToTreeXY_R nodes = partitioned.no <> (setXYtoTreeXY <$> partitioned.yes)
  where
    partitioned = partition isUsedModule nodes
    setXYtoTreeXY :: SpagoSimNode -> SpagoSimNode
    setXYtoTreeXY d = d { treeXY = notNull treeXY, x = radialXY.x, y = radialXY.y }
      where treeXY = fromMaybe { x: d.x, y: d.y } $ toMaybe d.treeXY
            radialXY = radialTranslate treeXY
            -- for radial positioning: breadth (x) becomes angle, depth (y) becomes radius
            radialTranslate :: PointXY -> PointXY
            radialTranslate p =
              let angle  = p.x  -- breadth determines angular position around the circle
                  radius = p.y  -- depth determines distance from center
                  x = radius * cos angle
                  y = radius * sin angle
              in { x, y }

-- Vertical tree uses D3's natural coordinates (breadth=x, depth=y)
treeNodesToTreeXY_V :: Array SpagoSimNode -> Array SpagoSimNode
treeNodesToTreeXY_V nodes = partitioned.no <> (setXYtoTreeXY <$> partitioned.yes)
  where
    partitioned = partition isUsedModule nodes
    setXYtoTreeXY :: SpagoSimNode -> SpagoSimNode
    -- Vertical tree: use D3's coordinates directly (breadth=x, depth=y)
    setXYtoTreeXY d = d { treeXY = notNull treeXY, x = treeXY.x, y = treeXY.y }
      where treeXY = fromMaybe { x: d.x, y: d.y } $ toMaybe d.treeXY

-- | Set nodes to their tree depth position with Y at 0 (for swarm diagrams)
-- | This creates a starting line for the swarm effect where nodes begin
-- | horizontally positioned by tree depth and then spread vertically due to forces
treeNodesToSwarmStart :: Array SpagoSimNode -> Array SpagoSimNode
treeNodesToSwarmStart nodes = partitioned.no <> (setSwarmStart <$> partitioned.yes)
  where
    partitioned = partition isUsedModule nodes
    setSwarmStart :: SpagoSimNode -> SpagoSimNode
    -- Use treeXY.y (depth) for x position, since D3 tree has depth on y-axis
    setSwarmStart d = d { x = treeXY.y, y = 0.0, fx = null, fy = null }
      where treeXY = fromMaybe { x: d.x, y: d.y } $ toMaybe d.treeXY

fixNamedNodeTo :: Label -> PointXY -> Array SpagoSimNode -> Array SpagoSimNode
fixNamedNodeTo label point nodes = fixNamedNode' <$> nodes
  where
    fixNamedNode' d = if d.name == label
                      then d { fx = notNull point.x, fy = notNull point.y }
                      else d

fixNamedNode :: Label -> Array SpagoSimNode -> Array SpagoSimNode
fixNamedNode label nodes = fixNamedNode' <$> nodes
  where
    fixNamedNode' d = if d.name == label
                      then d { fx = notNull d.x, fy = notNull d.y }
                      else d

scalePoint :: Number -> Number -> PointXY -> PointXY
scalePoint xFactor yFactor xy = { x: xy.x * xFactor, y: xy.y * yFactor }

offsetXY :: PointXY -> PointXY -> PointXY
offsetXY offset xy = { x: xy.x + offset.x, y: xy.y + offset.y }

offsetX :: Number -> PointXY -> PointXY
offsetX xOffset xy = xy { x = xy.x + xOffset }

offsetY :: Number -> PointXY -> PointXY
offsetY yOffset xy = xy { y = xy.y + yOffset }

pinNode :: PointXY -> SpagoSimNode -> SpagoSimNode
pinNode xy node = node { fx = notNull xy.x, fy = notNull xy.y }

setXY :: SpagoSimNode -> { x :: Number, y :: Number } -> SpagoSimNode
setXY node { x, y } = node { x = x, y = y }

setTreeXYIncludingLeaves :: SpagoSimNode -> TreeFields -> SpagoSimNode
setTreeXYIncludingLeaves node { x, y, depth, childIDs } =
  node { treeXY = notNull {x, y}, treeDepth = notNull depth, connected = true, links { treeChildren = childIDs } }

setTreeXYExceptLeaves :: SpagoSimNode -> TreeFields -> SpagoSimNode
setTreeXYExceptLeaves node { depth, isTreeLeaf: true } =
  node { treeXY = (N.null :: Nullable PointXY), treeDepth = notNull depth, connected = true }
setTreeXYExceptLeaves node { x, y, depth, isTreeLeaf: false, childIDs } =
  node { treeXY = notNull { x,y }, treeDepth = notNull depth, connected = true, links { treeChildren = childIDs } }

convertFilesToGraphModel :: forall r. 
  { body :: String | r } -> 
  { body :: String | r } -> 
  { body :: String | r } -> 
  { body :: String | r } -> SpagoModel
convertFilesToGraphModel moduleJSON packageJSON lsdepJSON locJSON = 
  makeSpagoGraphModel $ readSpago_Raw_JSON_ moduleJSON.body packageJSON.body lsdepJSON.body locJSON.body


makeSpagoGraphModel :: Spago_Raw_JSON_ -> SpagoModel
makeSpagoGraphModel json = do
  let { nodes, links, name2ID, id2Name, id2Package, id2LOC, sourceLinksMap }
        = getGraphJSONData json

      { graph, graphModel } = buildSpagoGraph nodes

  { links
  , nodes    : nodes <#> upgradeSpagoNodeData sourceLinksMap
  , graph
  , graphModel
  , tree     : Nothing  -- not present in the JSON, has to be calculated, if possible
  , maps     : { name2ID
               , id2Name
               -- id2Node removed: use graphModel.maps.nodeById instead
               , id2Package
               , id2LOC
               , id2TreeData: M.empty
               }
  }

-- | Configuration for using generic graph infrastructure with Spago data
spagoGraphConfig :: GraphConfig SpagoNodeData D3Link_Unswizzled
spagoGraphConfig =
  { getNodeId: _.id
  , getLinkSource: \link -> (unsafeCoerce link).source
  , getLinkTarget: \link -> (unsafeCoerce link).target
  }

-- | Build both GraphModel and Data.Graph from node data
-- | Returns both representations for maximum flexibility
buildSpagoGraph :: Array SpagoNodeData ->
  { graph :: Graph NodeID SpagoNodeData
  , graphModel :: GraphModel SpagoNodeData D3Link_Unswizzled
  }
buildSpagoGraph nodes =
  let
    -- Create links array from node dependencies
    -- Each node has a links.targets array that specifies its outgoing edges
    links = nodes >>= \node ->
      node.links.targets <#> \target ->
        unsafeCoerce { source: node.id, target: target, linktype: M2M_Graph, inSim: true }

    -- Build graph model using generic infrastructure
    graphModel = buildGraphModel spagoGraphConfig nodes links

    -- Convert to Data.Graph format for compatibility with existing code
    graph = toDataGraph spagoGraphConfig graphModel
  in
    { graph, graphModel }

-- | Legacy function for compatibility - returns only Data.Graph
makeGraph :: Array SpagoNodeData -> Graph NodeID SpagoNodeData
makeGraph nodes = (buildSpagoGraph nodes).graph


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
  :: forall row.
     String                           -- SVG selector
  -> String                           -- Node selector (Groups)
  -> String                           -- Link selector (Lines)
  -> Array (SimulationNode row)      -- Nodes with target fx/fy set
  -> Effect Unit                      -- Completion callback
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
