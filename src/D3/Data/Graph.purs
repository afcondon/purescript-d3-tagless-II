module D3.Data.Graph where

import D3.Node (NodeID)
import Data.Array (elem, filter, head, null, partition, uncons, (:))
import Data.Graph (Graph)
import Data.Graph as G
import Data.Maybe (Maybe(..))
import Data.Tree (Tree)
import Data.Tuple (Tuple(..))
import Prelude (bind, not, ($), (<$>), (<>))

type DepPath = Array NodeID
type GraphSearchRecord = {
    nodes          :: Array NodeID -- potentially confusingly this is just a list of nodes, not a list of dependencies
  , openDepPaths   :: Array DepPath
  , closedDepPaths :: Array DepPath
  , dependencyTree :: Maybe (Tree NodeID)
  , redundantLinks :: Array (Tuple NodeID NodeID)
}

getReachableNodes :: forall r1 r2. NodeID -> Graph Int { links :: { targets :: Array NodeID | r1 } | r2 } ->  GraphSearchRecord
getReachableNodes id graph = go { nodes: [], openDepPaths: [[id]], closedDepPaths: [], redundantLinks: [], dependencyTree: Nothing }
  where
    go :: GraphSearchRecord -> GraphSearchRecord
    go searchRecord@{ openDepPaths: [] } = searchRecord -- bottom out when all open paths are consumed
    go searchRecord = do
      case processNextOpenDepPath searchRecord of
        Nothing     -> searchRecord -- bottom out but....possibly some exceptions to be looked at here
        (Just searchRecord') -> go searchRecord'

    processNextOpenDepPath :: GraphSearchRecord -> Maybe GraphSearchRecord
    processNextOpenDepPath searchRecord = do
      x         <- uncons searchRecord.openDepPaths
      firstID   <- head x.head -- NB we're pushing onto the path, cause head is easier than tail
      firstNode <- G.lookup firstID graph

      let newDeps = 
            partition (\d -> not $ elem d searchRecord.nodes) firstNode.links.targets
          newOpenDepPaths = 
            (\d -> d : x.head) <$> newDeps.yes -- ie [ab] with deps [bc] -> [abc, abd]
          prunedLinks =
            (\d -> Tuple firstID d) <$> newDeps.no -- these are the links that we dropped to make tree

      if null newOpenDepPaths
        -- moving the open path we just processed to the list of closedDepPaths
        then Just $ searchRecord { openDepPaths   = x.tail                 
                                 , closedDepPaths = x.head : searchRecord.closedDepPaths
                                 , redundantLinks = searchRecord.redundantLinks <> prunedLinks }
        -- replace this open path with it's extension(s)
        else Just $ searchRecord { openDepPaths = x.tail <> newOpenDepPaths 
                                 , nodes     = searchRecord.nodes <> newDeps.yes
                                 , redundantLinks = searchRecord.redundantLinks <> prunedLinks }

