module PSD3.ForceNavigator.State where

import Prelude

import D3.Attributes.Instances (Label)
import D3.Data.Types (D3Selection_)
import D3.Node (D3Link(..), D3_SimulationNode(..))
import D3.Simulation.Types (D3SimulationState_, Force, ForceStatus, getStatusMap, initialSimulationState)
import D3.Viz.ForceNavigator.Model (NavigationSimNode, NodeType(..))
import Data.Array (elem, filter)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Map (Map) as M
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Type.Proxy (Proxy(..))

type State = {
  simulation :: D3SimulationState_
, expandedNodes :: Set String  -- IDs of expanded section nodes
, openSelections :: Maybe { nodes :: Maybe D3Selection_, links :: Maybe D3Selection_ }
, forceStatuses :: M.Map Label ForceStatus
}

initialState :: M.Map Label Force -> State
initialState forceLibrary = {
  simulation: initialSimulationState forceLibrary
, expandedNodes: Set.singleton "PS<$>D3"  -- Center node starts expanded
, openSelections: Nothing
, forceStatuses: getStatusMap forceLibrary
}

-- | Get the currently visible nodes based on expansion state
visibleNodes :: Set String -> Array NavigationSimNode -> Array NavigationSimNode
visibleNodes expanded allNodes =
  let
    -- Start with center node
    centerNode = allNodes # filter (\(D3SimNode n) -> n.id == "purescript-d3")

    -- Get all section nodes (children of center)
    sectionNodes = allNodes # filter (\(D3SimNode n) -> n.nodeType == Section)

    -- For each expanded section, get its children
    expandedChildren = do
      (D3SimNode node) <- allNodes
      if Set.member node.id expanded
        then case node.children of
          Just childIds -> allNodes # filter (\(D3SimNode n) -> elem n.id childIds)
          Nothing -> []
        else []
  in
    centerNode <> sectionNodes <> expandedChildren

-- | Get the visible links based on which nodes are visible
visibleLinks :: Array NavigationSimNode -> Array (D3Link String ()) -> Array (D3Link String ())
visibleLinks nodes allLinks =
  let
    visibleIds = Set.fromFoldable $ map (\(D3SimNode n) -> n.id) nodes
    isVisible (D3LinkID link) = Set.member link.source visibleIds && Set.member link.target visibleIds
  in
    filter isVisible allLinks

-- Lenses
_simulation :: forall a r. Lens' { simulation :: a | r } a
_simulation = prop (Proxy :: Proxy "simulation")

_expandedNodes :: forall a r. Lens' { expandedNodes :: a | r } a
_expandedNodes = prop (Proxy :: Proxy "expandedNodes")

_openSelections :: forall a r. Lens' { openSelections :: a | r } a
_openSelections = prop (Proxy :: Proxy "openSelections")

_forceStatuses :: Lens' State (M.Map Label ForceStatus)
_forceStatuses = prop (Proxy :: Proxy "forceStatuses")
