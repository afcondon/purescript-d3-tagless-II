module D3.Viz.LesMis.LesMisScenes where

import Prelude

import D3.Viz.LesMiserables.Model (LesMisNodeRow)
import D3.Viz.LesMis.LesMisRenderCallbacks (LesMisAttributes, defaultLesMisAttributes)
import Data.Array as Array
import Data.Int (toNumber, floor)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Data.Number (sqrt, cos, sin, pi, ceil, floor, (%)) as Number
import Data.Set as Set
import PSD3.Internal.Attributes.Instances (Label)
import PSD3v2.Simulation.Scene (SceneConfig, smoothTransition, smoothTransitionPinned)

-- ============================================================================
-- Node Initializer Functions
-- ============================================================================

-- | Golden angle for phylotaxis (sunflower spiral) layout
initialRadius :: Number
initialRadius = 10.0

initialAngle :: Number
initialAngle = Number.pi * (3.0 - Number.sqrt 5.0)  -- Golden angle

-- | Apply phylotaxis positions to nodes for initial layout (does NOT pin with fx/fy)
setPhyllotaxisInitialPositions :: forall r. Array (Record (x :: Number, y :: Number | r)) -> Array (Record (x :: Number, y :: Number | r))
setPhyllotaxisInitialPositions nodes = Array.mapWithIndex setPosition nodes
  where
    setPosition :: Int -> Record (x :: Number, y :: Number | r) -> Record (x :: Number, y :: Number | r)
    setPosition index node =
      let
        i = toNumber index
        rad = initialRadius * Number.sqrt (0.5 + i)
        angle = i * initialAngle
        newX = rad * Number.cos angle
        newY = rad * Number.sin angle
      in
        node { x = newX, y = newY }

-- | Apply phylotaxis positions to nodes (sets fx/fy to pin them)
setPhyllotaxisPositions :: forall r. Array (Record (x :: Number, y :: Number, fx :: Nullable Number, fy :: Nullable Number | r)) -> Array (Record (x :: Number, y :: Number, fx :: Nullable Number, fy :: Nullable Number | r))
setPhyllotaxisPositions nodes = Array.mapWithIndex setPosition nodes
  where
    setPosition :: Int -> Record (x :: Number, y :: Number, fx :: Nullable Number, fy :: Nullable Number | r) -> Record (x :: Number, y :: Number, fx :: Nullable Number, fy :: Nullable Number | r)
    setPosition index node =
      let
        i = toNumber index
        rad = initialRadius * Number.sqrt (0.5 + i)
        angle = i * initialAngle
        newX = rad * Number.cos angle
        newY = rad * Number.sin angle
      in
        node { x = newX, y = newY, fx = toNullable (Just newX), fy = toNullable (Just newY) }

-- | Calculate grid position from index
numberToGridPoint :: Int -> Int -> { x :: Number, y :: Number }
numberToGridPoint columns i = do
  let
    c = toNumber columns
    d = toNumber i
    x = (d Number.% c)
    y = Number.floor (d / c)
  { x, y }

-- | Apply grid positions to nodes (sets fx/fy to pin them)
setGridPositions :: forall r. Number -> Array (Record (x :: Number, y :: Number, fx :: Nullable Number, fy :: Nullable Number | r)) -> Array (Record (x :: Number, y :: Number, fx :: Nullable Number, fy :: Nullable Number | r))
setGridPositions gridSpacing nodes = Array.mapWithIndex setPosition nodes
  where
    nodeCount = Array.length nodes
    columns = floor $ Number.ceil $ Number.sqrt $ toNumber nodeCount
    offset = -(toNumber columns * gridSpacing) / 2.0

    setPosition i node =
      let gridPt = numberToGridPoint columns i
          newX = gridPt.x * gridSpacing + offset
          newY = gridPt.y * gridSpacing + offset
      in node { x = newX, y = newY, fx = toNullable (Just newX), fy = toNullable (Just newY) }

-- | Unpin all nodes (clear fx/fy to allow forces to control them)
unpinAllNodes :: forall r. Array (Record (fx :: Nullable Number, fy :: Nullable Number | r)) -> Array (Record (fx :: Nullable Number, fy :: Nullable Number | r))
unpinAllNodes nodes = nodes <#> \node -> node { fx = toNullable Nothing, fy = toNullable Nothing }

-- ============================================================================
-- Scene Configurations
-- ============================================================================

-- | Full graph scene - force-directed layout with all nodes
-- |
-- | Shows all nodes with standard force-directed physics.
-- | Nodes start in phylotaxis positions but are unpinned so forces can move them.
fullGraphScene :: Set.Set Label -> SceneConfig LesMisNodeRow LesMisAttributes
fullGraphScene activeForces = {
  chooseNodes: const true  -- Show all nodes
, linksShown: const true   -- Show all links
, linksActive: const true  -- All links apply force
, cssClass: "full-graph"
, attributes: defaultLesMisAttributes
, activeForces: activeForces
, nodeInitializerFunctions: [unpinAllNodes, setPhyllotaxisInitialPositions]
, transitionConfig: Just smoothTransition
}

-- | Filtered graph scene - show only nodes with group >= minGroup
-- |
-- | Useful for creating subsets of the graph.
-- | Links are automatically filtered to only connect visible nodes.
filteredGraphScene :: Int -> Set.Set Label -> SceneConfig LesMisNodeRow LesMisAttributes
filteredGraphScene minGroup activeForces = {
  chooseNodes: \n -> n.group >= minGroup  -- Filter by group
, linksShown: const true
, linksActive: const true
, cssClass: "filtered-graph"
, attributes: defaultLesMisAttributes
, activeForces: activeForces
, nodeInitializerFunctions: [unpinAllNodes, setPhyllotaxisInitialPositions]
, transitionConfig: Just smoothTransition
}

-- | Grid layout scene - nodes arranged in a grid (pinned)
-- |
-- | Nodes are pinned in grid positions.
-- | No forces are active (empty set).
gridScene :: Number -> SceneConfig LesMisNodeRow LesMisAttributes
gridScene gridSpacing = {
  chooseNodes: const true
, linksShown: const true
, linksActive: const false  -- Links don't apply force in grid layout
, cssClass: "grid-layout"
, attributes: defaultLesMisAttributes
, activeForces: Set.empty  -- No forces in grid layout
, nodeInitializerFunctions: [setGridPositions gridSpacing]
, transitionConfig: Just smoothTransitionPinned  -- Pin after transition
}

-- | Phylotaxis layout scene - sunflower spiral (pinned)
-- |
-- | Nodes are pinned in phylotaxis (sunflower spiral) positions.
-- | No forces are active.
phylotaxisScene :: SceneConfig LesMisNodeRow LesMisAttributes
phylotaxisScene = {
  chooseNodes: const true
, linksShown: const true
, linksActive: const false  -- Links don't apply force
, cssClass: "phylotaxis-layout"
, attributes: defaultLesMisAttributes
, activeForces: Set.empty  -- No forces in phylotaxis layout
, nodeInitializerFunctions: [setPhyllotaxisPositions]
, transitionConfig: Just smoothTransitionPinned  -- Pin after transition
}
