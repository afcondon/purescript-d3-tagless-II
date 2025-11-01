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
import D3.Viz.Spago.Model (allNodes, fixNamedNodeTo, isPackage, isUsedModule, moduleNodesToContainerXY, packageNodesToGridXY, packagesNodesToPhyllotaxis, sourcePackageIs, treeNodesToTreeXY_H, treeNodesToTreeXY_R, treeNodesToTreeXY_V, unpinAllNodes)
import PSD3.CodeExplorer.State (SceneConfig)
import PSD3.Internal.FFI (linksForceName_)
import Data.Set as Set

-- | Package Grid Scene - Hierarchical clustering of packages and modules
-- | Shows all nodes with packages arranged in a grid and modules clustered inside
packageGridScene :: SceneConfig
packageGridScene = {
  chooseNodes: allNodes
, linksShown: isM2P_Link
, linksActive: const true
, cssClass: "cluster"
, attributes: clusterSceneAttributes
, activeForces: Set.fromFoldable [ "clusterx_P", "clustery_P", "clusterx_M", "clustery_M", "collide2", linksForceName_ ]
, nodeInitializerFunctions: [ unpinAllNodes, packageNodesToGridXY, moduleNodesToContainerXY ]
}

-- | Package Graph Scene - Force-directed graph of package dependencies
-- | Shows only packages with dependencies radiating from "my-project"
packageGraphScene :: SceneConfig
packageGraphScene = {
  chooseNodes: isPackage
, linksShown: isP2P_Link
, linksActive: (sourcePackageIs "my-project")
, cssClass: "graph"
, attributes: graphSceneAttributes
, activeForces: Set.fromFoldable ["center", "collide2", "charge2", "packageOrbit", linksForceName_ ]
, nodeInitializerFunctions: [ unpinAllNodes, packagesNodesToPhyllotaxis, fixNamedNodeTo "my-project" { x: 0.0, y: 0.0 } ]
}

-- | Layer Swarm Scene - Tree links with horizontal layering forces
-- | Shows module dependency tree with nodes arranged in horizontal layers
layerSwarmScene :: SceneConfig
layerSwarmScene = {
  chooseNodes: isUsedModule
, linksShown: isM2M_Tree_Link
, linksActive: const true
, cssClass: "tree"
, attributes: treeSceneAttributes
, activeForces: Set.fromFoldable [ "htreeNodesX", "collide1", "y", linksForceName_ ]
, nodeInitializerFunctions: [ unpinAllNodes ]
}

-- | Radial Module Tree Scene - Tree layout with modules radiating from Main
-- | Uses D3 tree layout in radial coordinates with Main at the center
radialTreeScene :: SceneConfig
radialTreeScene = {
  chooseNodes: isUsedModule
, linksShown: isM2M_Tree_Link
, linksActive: const true
, cssClass: "tree radial"
, attributes: treeSceneAttributes
, activeForces: Set.fromFoldable [ "center", "collide2", "chargetree", "charge2", linksForceName_ ]
, nodeInitializerFunctions: [ unpinAllNodes, treeNodesToTreeXY_R, fixNamedNodeTo "PSD3.Main" { x: 0.0, y: 0.0 } ]
}

-- | Horizontal Module Tree Scene - Tree layout flowing left to right
-- | Uses D3 tree layout with strong positional forces, minimal link force
horizontalTreeScene :: SceneConfig
horizontalTreeScene = {
  chooseNodes: isUsedModule
, linksShown: isM2M_Tree_Link
, linksActive: const false
, cssClass: "tree horizontal"
, attributes: treeSceneAttributes
, activeForces: Set.fromFoldable [ "htreeNodesX", "htreeNodesY", "charge1", "collide2", linksForceName_ ]
, nodeInitializerFunctions: [ unpinAllNodes, treeNodesToTreeXY_H ]
}

-- | Vertical Module Tree Scene - Tree layout flowing top to bottom
-- | Uses D3 tree layout with strong positional forces, minimal link force
verticalTreeScene :: SceneConfig
verticalTreeScene = {
  chooseNodes: isUsedModule
, linksShown: isM2M_Tree_Link
, linksActive: const false
, cssClass: "tree vertical"
, attributes: treeSceneAttributes
, activeForces: Set.fromFoldable [ "vtreeNodesX", "vtreeNodesY", "charge1", "collide2", linksForceName_ ]
, nodeInitializerFunctions: [ unpinAllNodes, treeNodesToTreeXY_V ]
}
