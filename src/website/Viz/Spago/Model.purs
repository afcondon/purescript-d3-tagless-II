module D3.Viz.Spago.Model where

import Prelude

import D3.Viz.Spago.Files (D3TreeRow, D3_Radius, EmbeddedData, LinkType(..), NodeType(..), SpagoLink, SpagoNodeData, SpagoNodeRow, Spago_Raw_JSON_, getGraphJSONData, readSpago_Raw_JSON_)
import Data.Array (foldl, length, mapMaybe, mapWithIndex, partition, (:))
import Data.FoldableWithIndex (foldlWithIndex)
import Debug (spy)
import Data.Graph (Graph)
import Data.Int (floor) as Int
import Data.Int (toNumber)
import Data.Map (fromFoldable, lookup)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable, notNull, null, toMaybe)
import Data.Nullable (Nullable, null) as N
import Data.Number (ceil, cos, sin, sqrt, pi, (%))
import Data.Number (floor) as Number
import Data.Tuple (Tuple(..))
import Effect (Effect)
import PSD3.Data.Graph (GraphConfig, GraphModel, buildGraphModel, toDataGraph)
import PSD3.Data.Node (D3_FocusXY, NodeID, SimulationNode)
import PSD3.Internal.Attributes.Instances (Label)
import PSD3.Internal.FFI (setInSimNodeFlag)
import PSD3.Internal.Types (D3Simulation_, PointXY)
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Internal.Types (Event)


-- Model data types specialized with inital data
type SpagoTreeNode = Record (D3TreeRow (EmbeddedData SpagoNodeData + ()))
type SpagoSimNode  = SimulationNode ( SpagoNodeRow  + D3_FocusXY + D3_Radius + ()) -- SimulationNode already includes D3_XY + D3_VxyFxy
-- | this is the only data that we're bringing over from the SpagoTreeNode to SpagoSimNode (at the momment)
type TreeFields = { x :: Number, y :: Number, isTreeLeaf :: Boolean, depth :: Int, childIDs :: Array NodeID }

type SpagoModel = {
    links :: Array SpagoLink
  , nodes :: Array SpagoSimNode      -- already upgraded to simnode as a result of positioning when building the model
  , graph :: Graph NodeID SpagoNodeData
  , graphModel :: GraphModel SpagoNodeData SpagoLink  -- Generic graph infrastructure
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

-- | Check if node is a module from my-project (our source code)
isMyProjectModule :: SpagoSimNode -> Boolean
isMyProjectModule d = case d.nodetype of
  (IsPackage _) -> false
  (IsModule _) -> d.containerName == "my-project" && d.connected
              
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
  , r            : case node.nodetype of
                     IsPackage _ -> 20.0  -- Fixed size for packages
                     IsModule _ -> max 5.0 (sqrt node.loc)  -- Modules sized by LOC, min 5
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

-- | Position packages in phyllotaxis pattern AND pin them (fx, fy)
-- | Also stores position in gridXY so modules can cluster around them
-- | Uses larger radius than standard phyllotaxis for grid-like spacing
pinnedPackagesToPhyllotaxis :: Array SpagoSimNode -> Array SpagoSimNode
pinnedPackagesToPhyllotaxis nodes = partitioned.no <> (setPinnedPhyllotaxis `mapWithIndex` partitioned.yes)
  where
    partitioned = partition isPackage nodes
    -- Scale factor to spread packages out like grid layout (standard is 10.0)
    phyllotaxisScale = 100.0
    setPinnedPhyllotaxis :: Int -> SpagoSimNode -> SpagoSimNode
    setPinnedPhyllotaxis index d =
      let i = toNumber index
          radius = phyllotaxisScale * sqrt (0.5 + i)
          angle  = i * initialAngle
          x = radius * cos angle
          y = radius * sin angle
      in d { x = x, y = y, fx = notNull x, fy = notNull y, gridXY = notNull { x, y } }

modulesNodesToPhyllotaxis :: Array SpagoSimNode -> Array SpagoSimNode
modulesNodesToPhyllotaxis = nodesToPhyllotaxis isModule

-- | Position nodes for orbit view initial state
-- |
-- | Packages are spread in a large phyllotaxis pattern (scale 100)
-- | Modules are placed at their container package positions
-- | This gives a good starting point for radial forces to work from
nodesToOrbitStart :: Array SpagoSimNode -> Array SpagoSimNode
nodesToOrbitStart nodes = modulesAtContainers
  where
    -- First pass: position packages in spread phyllotaxis (no pinning!) and store in gridXY
    packagesPositioned = spreadPackagesToPhyllotaxis nodes

    -- Second pass: place modules at their container's position
    modulesAtContainers = moduleNodesToContainerXY packagesPositioned

-- | Position packages in spread phyllotaxis WITHOUT pinning
-- | Stores position in gridXY so modules can cluster around them
spreadPackagesToPhyllotaxis :: Array SpagoSimNode -> Array SpagoSimNode
spreadPackagesToPhyllotaxis nodes = partitioned.no <> (setSpreadPhyllotaxis `mapWithIndex` partitioned.yes)
  where
    partitioned = partition isPackage nodes
    phyllotaxisScale = 100.0  -- Spread out for large number of packages
    setSpreadPhyllotaxis :: Int -> SpagoSimNode -> SpagoSimNode
    setSpreadPhyllotaxis index d =
      let i = toNumber index
          radius = phyllotaxisScale * sqrt (0.5 + i)
          angle  = i * initialAngle
          x = radius * cos angle
          y = radius * sin angle
      in d { x = x, y = y, gridXY = notNull { x, y } }  -- No fx/fy - let forces work

-- | Distribute nodes evenly around a circle at given radius
-- |
-- | Unlike phyllotaxis which spirals outward, this pins all package nodes at exactly
-- | the same radius, evenly spaced by angle and with modules tied to them but not pinned
nodesToCircle :: (SpagoSimNode -> Boolean) -> Number -> Array SpagoSimNode -> Array SpagoSimNode
nodesToCircle predicate radius nodes = setPositionByPackage <$> nodes
  where
    partitioned = partition predicate nodes

    -- Map package name to position (use String key since containerID is 0 for all modules)
    packagePositions :: M.Map String { x :: Number, y :: Number }
    packagePositions = fromFoldable $ positionOnCircle  `mapWithIndex` partitioned.yes

    count = length partitioned.yes
    angleStep = if count == 0 then 0.0 else (2.0 * pi) / toNumber count

    positionOnCircle :: Int -> SpagoSimNode -> Tuple String { x :: Number, y :: Number }
    positionOnCircle index node =
      let angle = toNumber index * angleStep
          x = radius * cos angle
          y = radius * sin angle
      in  Tuple node.name { x,y }  -- Use package name as key

    setPositionByPackage :: SpagoSimNode -> SpagoSimNode
    setPositionByPackage node =
      let
        maybePos = lookup node.containerName packagePositions  -- Look up by containerName
        { x, y } = fromMaybe { x: 0.0, y: 0.0 } maybePos
        _ = case node.nodetype, maybePos of
              IsModule _, Nothing -> spy ("Module " <> node.name <> " container '" <> node.containerName <> "' not found in packagePositions") unit
              _, _ -> unit
      in
      case node.nodetype of
        (IsModule _) -> node { x = x, y = y, gridXY = notNull { x, y } } -- start modules at center of their packages, use clusterX_M and clusterY_M forces to keep them near their container
        -- (IsModule _) -> node { x = x, y = y, fx = notNull x, fy = notNull y, gridXY = notNull { x, y } } -- start modules at center of their packages, use clusterX_M and clusterY_M forces to keep them near their container
        (IsPackage _) -> node { x = x, y = y, fx = notNull x, fy = notNull y } -- modified to pin nodes


-- | Distribute packages evenly around a circle
packagesToCircle :: Number -> Array SpagoSimNode -> Array SpagoSimNode
packagesToCircle = nodesToCircle isPackage

-- | Distribute used modules evenly around a circle
modulesToCircle :: Number -> Array SpagoSimNode -> Array SpagoSimNode
modulesToCircle = nodesToCircle isModule

-- | Convenience function to pin main node to a particular spot
pinMainAtXY :: Number -> Number -> Array SpagoSimNode -> Array SpagoSimNode
pinMainAtXY x y nodes = f <$> nodes
  where f n = if n.name == "PSD3.Main" then n { fx = notNull x, fy = notNull y } else n

-- | Pin modules to their containing package's position
-- | Assumes packages already have gridXY set (e.g., from nodesToCircle)
pinModulesToPackages :: Array SpagoSimNode -> Array SpagoSimNode
pinModulesToPackages nodes = pinModule <$> nodes
  where
    -- Build map of package ID -> position
    packagePositions :: M.Map Int { x :: Number, y :: Number }
    packagePositions = fromFoldable $ mapMaybe getPackagePos nodes

    getPackagePos :: SpagoSimNode -> Maybe (Tuple Int { x :: Number, y :: Number })
    getPackagePos node = case node.nodetype of
      IsPackage _ ->
        case toMaybe node.gridXY of
          Just pos -> Just (Tuple node.id pos)
          Nothing -> Nothing
      _ -> Nothing

    pinModule :: SpagoSimNode -> SpagoSimNode
    pinModule node = case node.nodetype of
      IsModule _ ->
        case lookup node.containerID packagePositions of
          Just pos -> node
            { fx = notNull pos.x
            , fy = notNull pos.y
            , x = pos.x
            , y = pos.y
            }
          Nothing -> node  -- No package found, leave as is
      _ -> node  -- Not a module, leave as is

-- | Position nodes in orbit pattern: packages outer ring, modules inner ring, main pinned center
-- |
-- | This creates concentric rings:
-- | - Main module (PSD3.Main) pinned at center (0, 0)
-- | - Modules spread in phyllotaxis at inner radius
-- | - Packages spread in phyllotaxis at outer radius
nodesToOrbit :: Number -> Number -> Array SpagoSimNode -> Array SpagoSimNode
nodesToOrbit innerRadius outerRadius nodes = map positionNode nodes
  where
    -- Separate by type for phyllotaxis indexing
    packages = partition isPackage nodes
    modules = partition isModule packages.no

    -- Build index maps for phyllotaxis positioning
    packageIndices = foldlWithIndex (\i acc n -> M.insert n.id i acc) M.empty packages.yes
    moduleIndices = foldlWithIndex (\i acc n -> M.insert n.id i acc) M.empty modules.yes

    positionNode :: SpagoSimNode -> SpagoSimNode
    positionNode node
      | node.name == "PSD3.Main" =
          node { x = 0.0, y = 0.0, fx = notNull 0.0, fy = notNull 0.0 }  -- Main pinned at center
      | isPackage node =
          case M.lookup node.id packageIndices of
            Just i -> positionInRing outerRadius i node
            Nothing -> node
      | otherwise =
          case M.lookup node.id moduleIndices of
            Just i -> positionInRing innerRadius i node
            Nothing -> node

    positionInRing :: Number -> Int -> SpagoSimNode -> SpagoSimNode
    positionInRing ringRadius index node =
      let i = toNumber index
          radius = ringRadius + (10.0 * sqrt (0.5 + i))  -- Slight spread within ring
          angle = i * initialAngle
          x = radius * cos angle
          y = radius * sin angle
      in node { x = x, y = y }

-- | Position all tree modules to their radial tree positions and pin them
-- |
-- | Converts treeXY (angle, radius) to cartesian and sets fx/fy to pin in place.
-- | Non-tree nodes (packages, unused modules) are left unchanged.
nodesToRadialTree :: Array SpagoSimNode -> Array SpagoSimNode
nodesToRadialTree nodes = map positionNode nodes
  where
    positionNode :: SpagoSimNode -> SpagoSimNode
    positionNode node
      | node.name == "PSD3.Main" =
          node { x = 0.0, y = 0.0, fx = notNull 0.0, fy = notNull 0.0 }
      | isUsedModule node =
          case toMaybe node.treeXY of
            Just treeXY ->
              let angle = treeXY.x
                  radius = treeXY.y * treeDepthMultiplier
                  x = radius * cos angle
                  y = radius * sin angle
              in node { x = x, y = y, fx = notNull x, fy = notNull y }
            Nothing -> node
      | otherwise = node  -- Packages and unused modules unchanged

-- | layout nodes in "sunflower pattern", both pleasing to the eye and good as a starting position for sim
-- | (no two nodes in same spot). It's impossible to do this using D3 in simulation update situation
nodesToPhyllotaxis :: (SpagoSimNode -> Boolean) -> Array SpagoSimNode -> Array SpagoSimNode
nodesToPhyllotaxis predicate nodes = partitioned.no <> (setForPhyllotaxis `mapWithIndex` partitioned.yes)
  where
    partitioned = partition predicate nodes

initialRadius :: Number
initialRadius = 10.0
initialAngle :: Number
initialAngle = pi * (3.0 - sqrt 5.0)

-- | Position a node in a sunflower/phyllotaxis pattern based on its index.
-- | This creates aesthetically pleasing, evenly-distributed circular layouts.
setForPhyllotaxis :: Int -> SpagoSimNode -> SpagoSimNode
setForPhyllotaxis index d = d { x = (radius * cos angle), y = (radius * sin angle) }
  where
    i = toNumber index
    radius = initialRadius * sqrt (0.5 + i)
    angle  = i * initialAngle

-- | Depth multiplier for tree layouts - stretches depth dimension for better force layout relaxation
treeDepthMultiplier :: Number
treeDepthMultiplier = 2.0

treeNodesToTreeXY_H :: Array SpagoSimNode -> Array SpagoSimNode
treeNodesToTreeXY_H nodes = partitioned.no <> (setXYtoTreeXY <$> partitioned.yes)
  where
    partitioned = partition isUsedModule nodes
    setXYtoTreeXY :: SpagoSimNode -> SpagoSimNode
    -- Horizontal tree: swap D3's (breadth=x, depth=y) to screen (depth=x, breadth=y)
    -- Depth (now x) is stretched by multiplier
    setXYtoTreeXY d = d { treeXY = notNull treeXY, x = treeXY.y * treeDepthMultiplier, y = treeXY.x }
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
            -- Radius is stretched by depth multiplier
            radialTranslate :: PointXY -> PointXY
            radialTranslate p =
              let angle  = p.x  -- breadth determines angular position around the circle
                  radius = p.y * treeDepthMultiplier  -- depth determines distance from center, stretched
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
    -- Depth (y) is stretched by multiplier
    setXYtoTreeXY d = d { treeXY = notNull treeXY, x = treeXY.x, y = treeXY.y * treeDepthMultiplier }
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

-- | Position nodes for progressive tree revelation
-- | Modules at depth <= step get radial tree position
-- | Modules at depth > step stay at their package's grid position
-- | Packages stay at their grid positions
nodesToRevelationXY :: Int -> Array SpagoSimNode -> Array SpagoSimNode
nodesToRevelationXY step nodes = map positionNode nodes
  where
    positionNode :: SpagoSimNode -> SpagoSimNode
    positionNode node
      | isPackage node = node  -- Packages already positioned by packageNodesToGridXY
      | otherwise = positionModule node

    positionModule :: SpagoSimNode -> SpagoSimNode
    positionModule node =
      let nodeDepth = fromMaybe 0 $ toMaybe node.treeDepth
      in if node.name == "PSD3.Main"
         then node { x = 0.0, y = 0.0, fx = notNull 0.0, fy = notNull 0.0 }  -- Main pinned at center
         else if nodeDepth == step && nodeDepth > 0
              then setToRadialTreeXY node  -- NEWLY revealed: move to tree position
              else node  -- Everyone else: leave exactly as-is

    -- Set module to radial tree position and PIN it there
    setToRadialTreeXY :: SpagoSimNode -> SpagoSimNode
    setToRadialTreeXY node =
      let treeXY = fromMaybe { x: node.x, y: node.y } $ toMaybe node.treeXY
          angle = treeXY.x
          radius = treeXY.y * treeDepthMultiplier
          x = radius * cos angle
          y = radius * sin angle
      in node { x = x, y = y, fx = notNull x, fy = notNull y }  -- Pin to prevent forces pulling back

    -- Set module to its container (package) grid position and PIN it there
    setToContainerXY :: SpagoSimNode -> SpagoSimNode
    setToContainerXY node =
      case toMaybe node.gridXY of
        Just gridXY -> node { x = gridXY.x, y = gridXY.y, fx = notNull gridXY.x, fy = notNull gridXY.y }  -- Pin at container
        Nothing -> node  -- Fallback: keep current position

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
spagoGraphConfig :: GraphConfig SpagoNodeData SpagoLink
spagoGraphConfig =
  { getNodeId: _.id
  , getLinkSource: _.source
  , getLinkTarget: _.target
  }

-- | Build both GraphModel and Data.Graph from node data
-- | Returns both representations for maximum flexibility
buildSpagoGraph :: Array SpagoNodeData ->
  { graph :: Graph NodeID SpagoNodeData
  , graphModel :: GraphModel SpagoNodeData SpagoLink
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
