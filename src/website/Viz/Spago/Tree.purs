module D3.Viz.Spago.Tree where

import Prelude

import Data.DependencyGraph (getReachableNodes)
import PSD3.Data.Tree (TreeType(..), makeD3TreeJSONFromTreeID)
import PSD3.Internal.Types (PointXY)
import D3.Viz.Spago.Files (LinkType(..), isP2P_Link)
import D3.Viz.Spago.Model (SpagoModel, SpagoSimNode, SpagoTreeNode, TreeFields, setTreeXYExceptLeaves, setTreeXYIncludingLeaves)
import PSD3.Internal.FFI (descendants_, getHierarchyChildren_, getLayout, hNodeHeight_, hasChildren_, hierarchyFromJSON_, runLayoutFn_, treeSetNodeSize_, treeSortForTree_Spago)
import PSD3.Data.Node (D3Link(..), D3_SimulationNode(..), D3_TreeNode(..), NodeID)
import Data.Array (elem, filter, foldl, fromFoldable, partition, reverse)
import Data.List (List(..), (:))
import Data.List as L
import Data.Map (Map, empty)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Number (cos, sin)
import Data.Set as S
import Data.Tree (Tree(..))
import Data.Tuple (Tuple(..))

tupleToLink :: forall t1 t6.
  t1
  -> Tuple t6 t6
     -> D3Link t6
          ( inSim :: Boolean
          , linktype :: t1
          )
tupleToLink linktype (Tuple source target) = D3LinkID { source, target, linktype, inSim: true }
changeLinkType :: forall t149 t154 t158.
  t149
  -> D3Link t154
       ( linktype :: t149
       | t158
       )
     -> D3Link t154
          ( linktype :: t149
          | t158
          )
changeLinkType linktype (D3LinkID l) = D3LinkID l { linktype = linktype }

-- TODO make this generic and extract from Spago example to library
treeReduction :: NodeID -> SpagoModel -> SpagoModel
treeReduction rootID model = do
      let reachable         = getReachableNodes rootID model.graph
          onlyPackageLinks  = filter isP2P_Link model.links
          onlyTreelinks     = makeTreeLinkTuples (pathsAsLists reachable.closedDepPaths)
          prunedTreeLinks   = (tupleToLink M2M_Graph ) <$> reachable.redundantLinks
          partitionedLinks  = partition (\(D3LinkID l) -> (Tuple l.source l.target) `elem` onlyTreelinks) model.links
          treelinks         = (changeLinkType M2M_Tree) <$> partitionedLinks.yes
          treenodes         = partition (\(D3SimNode n) -> (n.id `elem` reachable.nodes) || n.id == rootID) model.nodes

          -- layout            = ((getLayout TidyTree) `treeSetSize_` [ 2.0 * pi, 900.0 ]) `treeSetSeparation_` radialSeparation
          numberOfLevels    = (hNodeHeight_ rootTree) + 1.0
          -- layout            = (getLayout TidyTree) `treeSetNodeSize_` [ 10.0, svg.width / numberOfLevels]
          layout            = (getLayout TidyTree) `treeSetNodeSize_` [ 8.0, 4000.0 / numberOfLevels]

          idTree            = buildTree rootID treelinks
          jsontree          = makeD3TreeJSONFromTreeID idTree model.maps.id2Node
          rootTree          = hierarchyFromJSON_       jsontree
          sortedTree        = treeSortForTree_Spago    rootTree
          laidOutRoot_      = (runLayoutFn_ layout)    sortedTree
          treeDerivedDataMap = getTreeDerivedData      laidOutRoot_
          -- positionedNodes   = setNodeXY_ForRadialTree   treenodes.yes treeDerivedDataMap
          positionedNodes   = setNodeXY_ForHorizontalTree   treenodes.yes treeDerivedDataMap
          -- TODO seems we can't position for Phyllotaxis here because we need to put nodes into the DOM before putting them into simulation (to support update pattern)
          -- unpositionedNodes = setForPhyllotaxis  <$> treenodes.no
          unpositionedNodes = treenodes.no
          tree              = Tuple rootID laidOutRoot_

          links = treelinks <> prunedTreeLinks <> onlyPackageLinks -- now all the links should have the right type, M2M_Graph / M2M_Tree / P2P 

      model { links = links, nodes = positionedNodes <> unpositionedNodes, tree = Just tree, maps { id2TreeData = treeDerivedDataMap } }

-- for radial positioning we treat x as angle and y as radius
radialTranslate :: PointXY -> PointXY -- TODO move this to more basic library so it can be used without cycles
radialTranslate p = 
  let angle  = p.x
      radius = p.y
      x = radius * cos angle
      y = radius * sin angle
  in { x, y }

setNodeXY_ForRadialTree :: Array SpagoSimNode -> M.Map NodeID TreeFields -> Array SpagoSimNode
setNodeXY_ForRadialTree nodes treeDerivedDataMap = do
  let 
    pinLeaves = false
    updateXY (D3SimNode node) = do
      case M.lookup node.id treeDerivedDataMap of
        Nothing -> D3SimNode node
        (Just p) -> 
          let { x,y } = radialTranslate { x: p.x, y: p.y }
          in 
            if pinLeaves
            then (D3SimNode node) `setTreeXYIncludingLeaves` { x, y, depth: p.depth, isTreeLeaf: p.isTreeLeaf, childIDs: p.childIDs } -- only pin parents
            else (D3SimNode node) `setTreeXYExceptLeaves`    { x, y, depth: p.depth, isTreeLeaf: p.isTreeLeaf, childIDs: p.childIDs } -- only pin parents
  updateXY <$> nodes

setNodeXY_ForHorizontalTree :: Array SpagoSimNode -> M.Map NodeID TreeFields -> Array SpagoSimNode
setNodeXY_ForHorizontalTree nodes treeDerivedDataMap = do
  let 
    pinLeaves = true
    updateXY (D3SimNode node) = do
      case M.lookup node.id treeDerivedDataMap of
        Nothing -> D3SimNode node
        (Just p) -> 
          let { x,y } = { x: p.y - 1200.0 , y: p.x } -- TODO MAGIC NUMBER just shifting left because origin is in center
          in 
            if pinLeaves
            then (D3SimNode node) `setTreeXYIncludingLeaves` { x, y, depth: p.depth, isTreeLeaf: p.isTreeLeaf, childIDs: p.childIDs } -- only pin parents
            else (D3SimNode node) `setTreeXYExceptLeaves`    { x, y, depth: p.depth, isTreeLeaf: p.isTreeLeaf, childIDs: p.childIDs } -- only pin parents
  updateXY <$> nodes

-- | having calculated tree from graph at origin Main, extract the information that we need in visualisation
getTreeDerivedData :: SpagoTreeNode -> Map NodeID TreeFields
-- TODO coerce here is because the transformation done by hierarchyFromJSON_ is not yet modelled in the type system
-- ideally you'd want to be able to do a (slightly) more principled cast as shown in commented out line below
-- getTreeDerivedData root = foldl (\acc (D3TreeNode n) -> M.insert n.data.id { x: n.x, y: n.y, isTreeLeaf: (tree_datum_.isTreeLeaf n) } acc) empty (descendants_ root) 
getTreeDerivedData root = 
  foldl (\acc treeNode@(D3TreeNode n) -> M.insert n.data.id
          { x: n.x
          , y: n.y
          , depth: n.depth
          , isTreeLeaf: hasChildren_ treeNode
          , childIDs: (\(D3TreeNode n) -> n.id) <$> (getHierarchyChildren_ treeNode) }
          acc)
        empty
        (descendants_ root) 

buildTree :: forall r. NodeID -> Array (D3Link NodeID r) -> Tree NodeID
buildTree rootID treelinks = do
  let 
    unwrap :: D3Link NodeID r -> { source :: NodeID, target :: NodeID | r }
    unwrap (D3LinkID d) = d
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

makeTreeLinkTuples :: L.List (L.List NodeID) -> Array (Tuple NodeID NodeID)
makeTreeLinkTuples closedPaths = do
  let
    linkTuples = (L.foldl path2Tuples Nil) closedPaths
  fromFoldable $ S.fromFoldable linkTuples -- removes the duplicates while building, but if we nubbed instead we could get count also
