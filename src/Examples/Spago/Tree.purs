module D3.Examples.Spago.Tree where

import D3.Data.Graph (getReachableNodes)
import D3.Data.Tree (TreeType(..), makeD3TreeJSONFromTreeID)
import D3.Data.Types (PointXY)
import D3.Examples.Spago.Files (LinkType(..))
import D3.Examples.Spago.Model (SpagoModel, SpagoSimNode, SpagoTreeNode, setXYExceptLeaves)
import D3.FFI 
import D3.Layouts.Hierarchical (radialSeparation)
import D3.Node (D3_Link(..), D3_SimulationNode(..), D3_TreeNode(..), NodeID)
import Data.Array (elem, filter, foldl, fromFoldable, partition, reverse)
import Data.List (List(..), (:))
import Data.List as L
import Data.Map (Map, empty)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Number (nan)
import Data.Set as S
import Data.Tree (Tree(..))
import Data.Tuple (Tuple(..))
import Math (cos, pi, sin)
import Prelude (($), (*), (<$>), (<<<), (<>), (==), (||))
import Unsafe.Coerce (unsafeCoerce)

-- TODO make this generic and extract from Spago example to library
treeReduction :: SpagoModel -> NodeID -> SpagoModel
treeReduction model rootID  = do
      let reachable         = getReachableNodes rootID model.graph
          onlyTreelinks     = makeTreeLinks (pathsAsLists reachable.closedDepPaths)
          prunedTreeLinks   = (\(Tuple source target) -> D3_Link { source, target, linktype: M2M_Graph }) <$> reachable.redundantLinks
          treelinks         = partition (\(D3_Link l) -> (Tuple l.source l.target) `elem` onlyTreelinks) model.links.allLinks
          treenodes         = partition (\(D3SimNode n) -> (n.id `elem` reachable.nodes) || n.id == rootID) model.nodes
          layout            = ((getLayout TidyTree) `treeSetSize_` [ 2.0 * pi, 900.0 ]) `treeSetSeparation_` radialSeparation

          idTree            = buildTree rootID treelinks.yes
          jsontree          = makeD3TreeJSONFromTreeID idTree model.maps.id2Node
          rootTree          = hierarchyFromJSON_       jsontree
          sortedTree        = treeSortForTree_Spago    rootTree
          laidOutRoot_      = (runLayoutFn_ layout)    sortedTree
          positionMap       = getPositionMap           laidOutRoot_
          positionedNodes   = setNodePositionsRadial   treenodes.yes positionMap
          unpositionedNodes = setForPhyllotaxis  <$> treenodes.no
          tree              = Tuple rootID laidOutRoot_

          packageLinks      = []
          links = model.links { treeLinks = treelinks.yes, prunedLinks = prunedTreeLinks }

      model { links = links, nodes = positionedNodes <> unpositionedNodes, tree = Just tree, maps { id2XYLeaf = positionMap } }

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
          -- in (D3SimNode node) `setXYExceptLeaves` { x, y, isLeaf: p.isLeaf } -- only pin parents
          in (D3SimNode node) `setXYExceptLeaves` { x, y, isLeaf: false }
  updateXY <$> nodes

setForPhyllotaxis :: SpagoSimNode -> SpagoSimNode
setForPhyllotaxis (D3SimNode d) = D3SimNode $ d { x = nan }

getPositionMap :: SpagoTreeNode -> Map NodeID { x :: Number, y :: Number, isLeaf :: Boolean }
-- TODO coerce here is because the transformation done by hierarchyFromJSON_ is not yet modelled in the type system
-- ideally you'd want to be able to do a (slightly) more principled cast as shown in commented out line below
-- getPositionMap root = foldl (\acc (D3TreeNode n) -> M.insert n.data.id { x: n.x, y: n.y, isLeaf: (tree_datum_.isLeaf n) } acc) empty (descendants_ root) 
getPositionMap root = foldl (\acc (D3TreeNode n) -> M.insert n.data.id { x: n.x, y: n.y, isLeaf: (unsafeCoerce n).data.isLeaf } acc) empty (descendants_ root) 

buildTree :: forall r. NodeID -> Array (D3_Link NodeID r) -> Tree NodeID
buildTree rootID treelinks = do
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
  fromFoldable $ S.fromFoldable linkTuples -- removes the duplicates while building, but if we nubbed instead we could get count also
