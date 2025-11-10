module PSD3.CodeExplorer.Forces where

import Prelude

import PSD3.Internal.Attributes.Instances (Label)
import D3.Viz.Spago.Model (datum_)
import PSD3.Internal.Simulation.Config as F
import PSD3.Internal.Simulation.Forces (createForce, createLinkForce, initialize)
import PSD3.Internal.Simulation.Types (Force, ForceFilter(..), ForceType(..), RegularForceType(..), allNodes) 
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Number (infinity)

-- | table of all the forces that are used in the Spago component
forceLibrary :: forall d. Map Label (Force d)
forceLibrary = initialize [
        createForce "center"       (RegularForce ForceCenter)   allNodes [ F.strengthVal 0.5, F.xVal 0.0, F.yVal 0.0 ]
      , createForce "x"            (RegularForce ForceX)        allNodes [ F.strengthVal 0.05, F.xVal 0.0 ]
      , createForce "y"            (RegularForce ForceY)        allNodes [ F.strengthVal 0.07, F.yVal 0.0 ]

      , createForce "collide1"     (RegularForce ForceCollide)  allNodes [ F.strengthVal 1.0, F.radiusFn (\d _ -> datum_.collideRadius d) ]
      , createForce "collide2"     (RegularForce ForceCollide)  allNodes [ F.strengthVal 0.7, F.radiusFn (\d _ -> datum_.collideRadiusBig d) ]
      , createForce "charge1"      (RegularForce ForceManyBody) allNodes [ F.strengthVal (-50.0), F.thetaVal 0.9, F.distanceMinVal 1.0, F.distanceMaxVal infinity ]
      , createForce "charge2"      (RegularForce ForceManyBody) allNodes [ F.strengthVal (-100.0), F.thetaVal 0.9, F.distanceMinVal 1.0, F.distanceMaxVal 400.0 ]
      , createForce "chargetree"   (RegularForce ForceManyBody) treeExceptLeaves [ F.strengthVal (-100.0), F.thetaVal 0.9, F.distanceMinVal 1.0, F.distanceMaxVal 400.0 ]

      , createForce "clusterx_M"     (RegularForce ForceX)        modulesOnly [ F.strengthVal 0.2, F.xVal datum_.gridPointX ]
      , createForce "clustery_M"     (RegularForce ForceY)        modulesOnly [ F.strengthVal 0.2, F.yVal datum_.gridPointY ]

      , createForce "clusterx_P"     (RegularForce ForceX)        packagesOnly [ F.strengthVal 0.8, F.xVal datum_.gridPointX ]
      , createForce "clustery_P"     (RegularForce ForceY)        packagesOnly [ F.strengthVal 0.8, F.yVal datum_.gridPointY ]

      , createForce "htreeNodesX"  (RegularForce ForceX)  (Just $ ForceFilter "tree only" \d -> datum_.connected d)
          [ F.strengthVal 0.4, F.xVal datum_.treePointX ]
      , createForce "htreeNodesY"  (RegularForce ForceY)  (Just $ ForceFilter "tree only" \d -> datum_.connected d)
          [ F.strengthVal 0.4, F.yVal datum_.treePointY ]
      , createForce "vtreeNodesX"  (RegularForce ForceX)  (Just $ ForceFilter "tree only" \d -> datum_.connected d)
          [ F.strengthVal 0.4, F.xVal datum_.treePointY ]
      , createForce "vtreeNodesY"  (RegularForce ForceY)  (Just $ ForceFilter "tree only" \d -> datum_.connected d)
          [ F.strengthVal 0.4, F.yVal datum_.treePointX ]

      , createForce "packageOrbit" (RegularForce ForceRadial)   packagesOnly
                                   [ F.strengthVal 0.7, F.xVal 0.0, F.yVal 0.0, F.radiusVal 1200.0 ]
      , createForce "unusedOrbit" (RegularForce ForceRadial)   unusedModulesOnly
                                   [ F.strengthVal 0.8, F.xVal 0.0, F.yVal 0.0, F.radiusVal 900.0 ]
      , createForce "moduleOrbit" (RegularForce ForceRadial)   usedModulesOnly
                                   [ F.strengthVal 0.8, F.xVal 0.0, F.yVal 0.0, F.radiusVal 600.0 ]

      , createLinkForce Nothing [ F.strengthVal 0.5, F.distanceVal 0.0, F.numKey (toNumber <<< datum_.id) ]
      ]
  where
    packagesOnly      = Just $ ForceFilter "all packages" datum_.isPackage
    modulesOnly       = Just $ ForceFilter "all modules" datum_.isModule
    unusedModulesOnly = Just $ ForceFilter "unused modules only" datum_.isUnusedModule
    usedModulesOnly   = Just $ ForceFilter "used modules only" datum_.isUsedModule
    treeExceptLeaves  = Just $ ForceFilter "tree parent nodes only" datum_.isTreeParent

    useGridXY d _ = datum_.gridPoint d
    centerXY _ _ = { x: 0.0, y: 0.0 }
    treeXY   d _ = datum_.treePoint d

-- | NOTES

-- gridForceSettings :: Array String
-- gridForceSettings = [ "packageGrid", "clusterx", "clustery", "collide1" ]

-- gridForceSettings2 :: Array String
-- gridForceSettings2 = [ "center", "collide2", "x", "y" ]

-- packageForceSettings :: Array String
-- packageForceSettings = [ "centerNamedNode", "center", "collide2", "charge2", "links"]

-- treeForceSettings :: Array String
-- treeForceSettings = ["links", "center", "charge1", "collide1" ]

-- these are the force settings for the force-layout radial tree in Observables
-- .force("link", d3.forceLink(links).id(d => d.id).distance(0).strength(1))
-- .force("charge", d3.forceManyBody().strength(-50))
-- .force("x", d3.forceX())
-- .force("y", d3.forceY());

