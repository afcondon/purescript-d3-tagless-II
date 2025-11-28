-- | Scene Definitions for CodeExplorer
-- |
-- | This module defines all the visualization scenes as SceneConfig constants.
-- | Each scene is a complete, self-contained configuration specifying:
-- | - Data filters (which nodes/links to show)
-- | - Force configuration (which forces to activate)
-- | - Visual styling (CSS classes and D3 attributes)
-- | - Node initialization (positioning functions)
-- |
-- | This approach complements Forces.purs and makes scene configuration
-- | declarative and composable. Scenes can be easily shared, modified,
-- | or extended without understanding the internals of the simulation.
module PSD3.CodeExplorer.Scenes where

import Prelude

import D3.Viz.Spago.Draw.Attributes (clusterSceneAttributes, graphSceneAttributes, treeSceneAttributes)
import D3.Viz.Spago.Files (isM2M_Tree_Link, isP2P_Link, isM2P_Link)
import D3.Viz.Spago.Model (allNodes, fixNamedNodeTo, isPackage, isUsedModule, moduleNodesToContainerXY, packageNodesToGridXY, packagesNodesToPhyllotaxis, pinnedPackagesToPhyllotaxis, treeNodesToSwarmStart, treeNodesToTreeXY_H, treeNodesToTreeXY_R, treeNodesToTreeXY_V, unpinAllNodes, nodesToRevelationXY)
import PSD3.CodeExplorer.State (SceneConfig)
import PSD3.Internal.FFI (linksForceName_)
import PSD3v2.Simulation.Scene (smoothTransition)
import Data.Maybe (Maybe(..))
import Data.Set as Set

-- | Package Grid Scene - Hierarchical clustering of packages and modules
-- | Shows all nodes with packages arranged in a grid and modules clustered inside
-- | Uses smooth animated transition for professional appearance
packageGridScene :: SceneConfig
packageGridScene = {
  chooseNodes: allNodes
, linksShown: isM2P_Link
, linksActive: const true
, cssClass: "cluster"
, attributes: clusterSceneAttributes
, activeForces: Set.fromFoldable [ "clusterx_P", "clustery_P", "clusterx_M", "clustery_M", "collide2", linksForceName_ ]
, nodeInitializerFunctions: [ unpinAllNodes, packageNodesToGridXY, moduleNodesToContainerXY ]
, transitionConfig: Just smoothTransition  -- Animated transition for pinned layout
}

-- | Package Graph Scene - Force-directed graph of package dependencies
-- | Shows only packages with dependencies radiating from "my-project"
-- | Uses instant transition since forces will animate nodes naturally
packageGraphScene :: SceneConfig
packageGraphScene = {
  chooseNodes: isPackage
, linksShown: isP2P_Link
, linksActive: const true  -- TODO: restore sourcePackageIs filter
, cssClass: "graph"
, attributes: graphSceneAttributes
, activeForces: Set.fromFoldable ["center", "collide2", "charge2", "packageOrbit", linksForceName_ ]
, nodeInitializerFunctions: [ unpinAllNodes, packagesNodesToPhyllotaxis, fixNamedNodeTo "my-project" { x: 0.0, y: 0.0 } ]
, transitionConfig: Nothing  -- Instant transition, let forces animate
}

-- | Layer Swarm Scene - Tree links with horizontal layering forces
-- | Shows module dependency tree with nodes arranged in horizontal layers
-- | Uses instant transition since forces will animate nodes naturally
layerSwarmScene :: SceneConfig
layerSwarmScene = {
  chooseNodes: isUsedModule
, linksShown: isM2M_Tree_Link
, linksActive: const true
, cssClass: "tree"
, attributes: treeSceneAttributes
, activeForces: Set.fromFoldable [ "htreeNodesX", "collide1", "y", linksForceName_ ]
, nodeInitializerFunctions: [ treeNodesToSwarmStart ]  -- Position at (treeX, 0) for swarm effect
, transitionConfig: Nothing  -- Instant transition, let forces animate
}

-- | Radial Module Tree Scene - Tree layout with modules radiating from Main
-- | Uses D3 tree layout in radial coordinates with Main at the center
-- | Uses smooth animated transition for professional appearance
radialTreeScene :: SceneConfig
radialTreeScene = {
  chooseNodes: isUsedModule
, linksShown: isM2M_Tree_Link
, linksActive: const true
, cssClass: "tree radial"
, attributes: treeSceneAttributes
, activeForces: Set.fromFoldable [ "center", "collide2", "chargetree", "charge2", linksForceName_ ]
, nodeInitializerFunctions: [ unpinAllNodes, treeNodesToTreeXY_R, fixNamedNodeTo "PSD3.Main" { x: 0.0, y: 0.0 } ]
, transitionConfig: Just smoothTransition  -- Animated transition for pinned layout
}

-- | Horizontal Module Tree Scene - Tree layout flowing left to right
-- | Uses D3 tree layout with strong positional forces, minimal link force
-- | Uses smooth animated transition for professional appearance
horizontalTreeScene :: SceneConfig
horizontalTreeScene = {
  chooseNodes: isUsedModule
, linksShown: isM2M_Tree_Link
, linksActive: const false
, cssClass: "tree horizontal"
, attributes: treeSceneAttributes
, activeForces: Set.fromFoldable [ "htreeNodesX", "htreeNodesY", "charge1", "collide2", linksForceName_ ]
, nodeInitializerFunctions: [ unpinAllNodes, treeNodesToTreeXY_H ]
, transitionConfig: Just smoothTransition  -- Animated transition for pinned layout
}

-- | Vertical Module Tree Scene - Tree layout flowing top to bottom
-- | Uses D3 tree layout with strong positional forces, minimal link force
-- | Uses smooth animated transition for professional appearance
verticalTreeScene :: SceneConfig
verticalTreeScene = {
  chooseNodes: isUsedModule
, linksShown: isM2M_Tree_Link
, linksActive: const false
, cssClass: "tree vertical"
, attributes: treeSceneAttributes
, activeForces: Set.fromFoldable [ "vtreeNodesX", "vtreeNodesY", "charge1", "collide2", linksForceName_ ]
, nodeInitializerFunctions: [ unpinAllNodes, treeNodesToTreeXY_V ]
, transitionConfig: Just smoothTransition  -- Animated transition for pinned layout
}

-- | Tree Revelation Scene - Progressive reveal from grid to radial tree
-- | Shows all nodes, with modules transitioning from grid (in packages) to tree positions
-- | based on their depth and the current revelation step.
-- |
-- | Step 0: All modules in grid (packages at low opacity)
-- | Step 1-N: Modules at depth <= step move to radial tree positions
-- | Step N+1: Final state - packages fade out, tree relaxes to force layout
-- |
-- | @param step The current revelation step (0 to maxDepth+1)
-- | @param maxDepth The maximum tree depth in the data
treeRevelationScene :: Int -> Int -> SceneConfig
treeRevelationScene step maxDepth
  | step == 0 = {
      -- Step 0: Phyllotaxis with pinned packages, modules clustered by forces
      chooseNodes: allNodes
    , linksShown: const false  -- No links at step 0
    , linksActive: const false
    , cssClass: "revelation step-0"
    , attributes: clusterSceneAttributes
    , activeForces: Set.fromFoldable [ "clusterx_M", "clustery_M", "collide2" ]  -- Module clustering forces
    , nodeInitializerFunctions: [ unpinAllNodes, pinnedPackagesToPhyllotaxis, moduleNodesToContainerXY ]
    , transitionConfig: Just smoothTransition
    }
  | step > maxDepth = {
      -- Final step: Unpin and let force layout relax
      chooseNodes: allNodes
    , linksShown: isM2M_Tree_Link
    , linksActive: const true
    , cssClass: "revelation final"
    , attributes: clusterSceneAttributes
    , activeForces: Set.fromFoldable [ "center", "collide2", "charge2", linksForceName_ ]
    , nodeInitializerFunctions: [ unpinAllNodes, packageNodesToGridXY, moduleNodesToContainerXY, treeNodesToTreeXY_R ]
    , transitionConfig: Just smoothTransition
    }
  | otherwise = {
      -- Steps 1-N: No simulation, just move newly revealed nodes to tree positions
      chooseNodes: allNodes
    , linksShown: isM2M_Tree_Link
    , linksActive: const false  -- No link forces
    , cssClass: "revelation"
    , attributes: clusterSceneAttributes
    , activeForces: Set.empty  -- NO forces - simulation effectively off
    , nodeInitializerFunctions: [ nodesToRevelationXY step ]  -- Only modify newly revealed nodes
    , transitionConfig: Nothing  -- No transition - avoid D3 interpolating all nodes to model positions
    }
