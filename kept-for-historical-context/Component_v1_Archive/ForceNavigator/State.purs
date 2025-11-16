module PSD3.ForceNavigator.State where

import Prelude

import PSD3.Internal.Attributes.Instances (Label)
import PSD3.Internal.Types (D3Selection_)
import PSD3.Data.Node (D3Link_Unswizzled, D3_SimulationNode(..))
import Unsafe.Coerce (unsafeCoerce)
import PSD3.Internal.Simulation.Types (D3SimulationState_, Force, initialSimulationState)
import D3.Viz.ForceNavigator.Model (NavigationSimNode, NodeType(..))
import Data.Array (elem, filter, length) as Array
import Data.Lens (Lens')
import Debug (spy) as Debug
import Data.Lens.Record (prop)
import Data.Map (Map, keys) as M
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Halogen.Subscription as HS
import PSD3.ForceNavigator.Actions (Action)
import Type.Proxy (Proxy(..))

type State = {
    simulation :: D3SimulationState_ NavigationSimNode
  , expandedNodes :: Set String  -- IDs of expanded section nodes
  , openSelections :: Maybe { nodes :: Maybe (D3Selection_ NavigationSimNode), links :: Maybe (D3Selection_ NavigationSimNode) }
  , activeForces :: Set Label  -- Labels of forces currently enabled (parallel to expandedNodes for data)
  , eventListener :: Maybe (HS.Listener Action)  -- Component infrastructure, not scene config
}

initialState :: M.Map Label (Force NavigationSimNode) -> State
initialState forceLibrary = {
    simulation: initialSimulationState forceLibrary
  , expandedNodes: Set.empty  -- Start with no sections expanded (just center + sections visible)
  , openSelections: Nothing
  , activeForces: Set.fromFoldable (M.keys forceLibrary)  -- Start with all forces enabled
  , eventListener: Nothing
}

-- | Get the currently visible nodes based on expansion state
visibleNodes :: Set String -> Array NavigationSimNode -> Array NavigationSimNode
visibleNodes expanded allNodes =
  let
    -- Start with center node
    centerNode = allNodes # Array.filter (\n -> n.id == "purescript-d3")

    -- Get all section nodes (children of center)
    sectionNodes = allNodes # Array.filter (\n -> n.nodeType == Section)

    -- For each expanded section, get its children
    expandedChildren = do
      node <- allNodes
      if Set.member node.id expanded
        then case node.children of
          Just childIds -> allNodes # Array.filter (\n -> Array.elem n.id childIds)
          Nothing -> []
        else []
  in
    centerNode <> sectionNodes <> expandedChildren

-- | Clone a link to create a fresh object (prevents mutation of static data)
-- | Specific to String IDs with no extra fields for NavigationRawModel
cloneLink :: D3Link_Unswizzled -> D3Link_Unswizzled
cloneLink link =
  let { source, target } = (unsafeCoerce link :: { source :: String, target :: String })
  in unsafeCoerce { source, target }

-- | Get the visible links based on which nodes are visible
-- | CRITICAL: Must clone links to prevent FFI swizzling from mutating static navigationData
visibleLinks :: Array NavigationSimNode -> Array D3Link_Unswizzled -> Array D3Link_Unswizzled
visibleLinks nodes allLinks =
  let
    visibleIds = Set.fromFoldable $ map (\n -> n.id) nodes
    unpackLink :: D3Link_Unswizzled -> { source :: String, target :: String }
    unpackLink = unsafeCoerce

    -- Show ALL links in the input to see if skeleton links are even there
    _ = Debug.spy "📊 ALL INPUT LINKS" $ map (\l -> let link = unpackLink l in link.source <> " → " <> link.target) allLinks

    -- Test the skeleton links specifically
    skeletonTests = map (\target ->
      let sourceInSet = Set.member "purescript-d3" visibleIds
          targetInSet = Set.member target visibleIds
      in { target, sourceInSet, targetInSet }
    ) ["gallery", "about", "spago", "interpreters", "github"]
    _ = Debug.spy "🔍 Skeleton link tests" skeletonTests

    isVisible link =
      let linkRec = unpackLink link
          sourceIn = Set.member linkRec.source visibleIds
          targetIn = Set.member linkRec.target visibleIds
          visible = sourceIn && targetIn
      in visible

    -- Filter visible links and CLONE them to prevent mutation of static data
    filtered = Array.filter isVisible allLinks
    result = map cloneLink filtered

    _ = Debug.spy "📊 Total links in navigationData" $ Array.length allLinks
    _ = Debug.spy "📊 visibleNodes IDs" $ Set.toUnfoldable visibleIds :: Array String
    _ = Debug.spy "📊 visibleLinks (filtered)" $ map (\l -> let link = unpackLink l in link.source <> " → " <> link.target) result
    _ = Debug.spy "📊 visibleLinks count" $ Array.length result
  in
    result

-- Lenses
_simulation :: Lens' State (D3SimulationState_ NavigationSimNode)
_simulation = prop (Proxy :: Proxy "simulation")

_expandedNodes :: Lens' State (Set String)
_expandedNodes = prop (Proxy :: Proxy "expandedNodes")

_openSelections :: Lens' State (Maybe { nodes :: Maybe (D3Selection_ NavigationSimNode), links :: Maybe (D3Selection_ NavigationSimNode) })
_openSelections = prop (Proxy :: Proxy "openSelections")

_activeForces :: Lens' State (Set Label)
_activeForces = prop (Proxy :: Proxy "activeForces")

_eventListener :: Lens' State (Maybe (HS.Listener Action))
_eventListener = prop (Proxy :: Proxy "eventListener")
