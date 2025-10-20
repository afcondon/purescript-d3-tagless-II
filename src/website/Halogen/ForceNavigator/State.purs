module PSD3.App.ForceNavigator.State where

import Prelude

import D3.Attributes.Instances (Label)
import D3.Data.Types (D3Selection_)
import D3.Viz.Navigation.Model (NavigationRawModel, NavigationSimNode, NodeType(..))
import D3.Node (D3Link(..), D3_SimulationNode(..))
import D3.Selection (SelectionAttribute)
import D3.Simulation.Types (D3SimulationState_, Force, ForceStatus, initialSimulationState)
import D3Tagless.Capabilities (Staging)
import Data.Array (elem, filter)
import Data.Functor (map)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), isJust)
import Data.Set (Set)
import Data.Set as Set
import Type.Proxy (Proxy(..))

type State = {
  simulation :: D3SimulationState_
, expandedNodes :: Set String  -- IDs of expanded section nodes
, openSelections :: Maybe { nodes :: Maybe D3Selection_, links :: Maybe D3Selection_ }
}

initialState :: Map Label Force -> State
initialState forceLibrary = {
  simulation: initialSimulationState forceLibrary
, expandedNodes: Set.singleton "purescript-d3"  -- Center node starts expanded
, openSelections: Nothing
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
