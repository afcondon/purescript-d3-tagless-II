module D3.Data.Graph where

import Prelude (bind, not, ($), (<$>), (<>))

import D3.Node (NodeID)
import Data.Array (filter, elem, head, null, uncons, (:)) 
import Data.Graph (Graph)
import Data.Graph as G
import Data.Maybe (Maybe(..))
import Data.Tree (Tree)

type DepPath = Array NodeID
type GraphSearchRecord = {
    nodes          :: Array NodeID -- potentially confusingly this is just a list of nodes, not a list of dependencies
  , openDepPaths   :: Array DepPath
  , closedDepPaths :: Array DepPath
  , dependencyTree :: Maybe (Tree NodeID)
}

getReachableNodes :: forall r1 r2. NodeID -> Graph Int { depends :: { full :: Array NodeID | r1 } | r2 }
  -> { closedDepPaths :: Array (Array NodeID)
    , dependencyTree  :: Maybe (Tree NodeID)
    , nodes           :: Array NodeID
    , openDepPaths    :: Array (Array NodeID)
    }
getReachableNodes id graph = go { nodes: [], openDepPaths: [[id]], closedDepPaths: [], dependencyTree: Nothing }
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
            filter (\d -> not $ elem d searchRecord.nodes) firstNode.depends.full
          newOpenDepPaths = 
            (\d -> d : x.head) <$> newDeps -- ie [ab] with deps [bc] -> [abc, abd]

      if null newOpenDepPaths
        -- moving the open path we just processed to the list of closedDepPaths
        then Just $ searchRecord { openDepPaths   = x.tail                 
                                 , closedDepPaths = x.head : searchRecord.closedDepPaths}
        -- replace this open path with it's extension(s)
        else Just $ searchRecord { openDepPaths = x.tail <> newOpenDepPaths 
                                 , nodes     = searchRecord.nodes <> newDeps }

