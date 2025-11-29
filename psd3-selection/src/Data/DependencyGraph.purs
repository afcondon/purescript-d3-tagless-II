module Data.DependencyGraph
  ( DepPath
  , GraphSearchRecord
  , getReachableNodes
  ) where

import Data.Array (elem, head, null, partition, uncons, (:))
import Data.Graph (Graph)
import Data.Graph as G
import Data.Maybe (Maybe(..))
import Data.Tree (Tree)
import Data.Tuple (Tuple(..))
import Prelude

type DepPath id = Array id
type GraphSearchRecord id = {
    nodes          :: Array id -- potentially confusingly this is just a list of nodes, not a list of dependencies
  , openDepPaths   :: Array (DepPath id)
  , closedDepPaths :: Array (DepPath id)
  , dependencyTree :: Maybe (Tree id)
  , redundantLinks :: Array (Tuple id id)
}

getReachableNodes :: forall id r1 r2. (Ord id) => id -> Graph id { links :: { targets :: Array id | r1 } | r2 } ->  GraphSearchRecord id
getReachableNodes id graph = go { nodes: [], openDepPaths: [[id]], closedDepPaths: [], redundantLinks: [], dependencyTree: Nothing }
  where
    go :: GraphSearchRecord id -> GraphSearchRecord id
    go searchRecord@{ openDepPaths: [] } = searchRecord -- bottom out when all open paths are consumed
    go searchRecord = do
      case processNextOpenDepPath searchRecord of
        Nothing     -> searchRecord -- bottom out but....possibly some exceptions to be looked at here
        (Just searchRecord') -> go searchRecord'

    processNextOpenDepPath :: GraphSearchRecord id -> Maybe (GraphSearchRecord id)
    processNextOpenDepPath searchRecord = do
      x         <- uncons searchRecord.openDepPaths
      firstID   <- head x.head -- NB we're pushing onto the path, cause head is easier than tail
      firstNode <- G.lookup firstID graph

      let newDeps         = partition (\d -> not $ elem d searchRecord.nodes) firstNode.links.targets
          newOpenDepPaths = newDeps.yes <#> \d -> d : x.head -- ie [ab] with deps [bc] -> [abc, abd]
          prunedLinks     = newDeps.no  <#> \d -> Tuple firstID d -- these are the links that we dropped to make tree

      if null newOpenDepPaths
        -- moving the open path we just processed to the list of closedDepPaths
        then Just $ searchRecord { openDepPaths   = x.tail                 
                                 , closedDepPaths = x.head : searchRecord.closedDepPaths
                                 , redundantLinks = searchRecord.redundantLinks <> prunedLinks }
        -- replace this open path with it's extension(s)
        else Just $ searchRecord { openDepPaths = x.tail <> newOpenDepPaths 
                                 , nodes     = searchRecord.nodes <> newDeps.yes
                                 , redundantLinks = searchRecord.redundantLinks <> prunedLinks }

