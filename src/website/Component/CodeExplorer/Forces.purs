module PSD3.CodeExplorer.Forces where

import Prelude

import PSD3.Internal.Attributes.Instances (Label)
import D3.Viz.Spago.Model (SpagoSimNode, isPackage, isModule, isUsedModule)
import D3.Viz.Spago.Files (NodeType(..))
import PSD3.Internal.Simulation.Config as F
import PSD3.Internal.Simulation.Forces (createForce, createLinkForce, initialize)
import PSD3.Internal.Simulation.Types (Force, ForceFilter(..), ForceType(..), RegularForceType(..), allNodes)
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (infinity)
import Data.Nullable (toMaybe)
import Unsafe.Coerce (unsafeCoerce)

-- Helper functions for force configuration (phantom type friendly)
collideRadius :: SpagoSimNode -> Number
collideRadius d =
  if d.id == d.containerID
  then 10.0
  else d.r

collideRadiusBig :: SpagoSimNode -> Number
collideRadiusBig d = d.r + 10.0

gridPointX :: SpagoSimNode -> Number
gridPointX d = fromMaybe d.x $ map _.x $ toMaybe d.gridXY

gridPointY :: SpagoSimNode -> Number
gridPointY d = fromMaybe d.y $ map _.y $ toMaybe d.gridXY

treePointX :: SpagoSimNode -> Number
treePointX d = fromMaybe d.x $ map _.x $ toMaybe d.treeXY

treePointY :: SpagoSimNode -> Number
treePointY d = fromMaybe d.y $ map _.y $ toMaybe d.treeXY

isUnusedModule :: SpagoSimNode -> Boolean
isUnusedModule d = case d.nodetype of
  (IsPackage _) -> false
  (IsModule _) -> not d.connected

isTreeParent :: SpagoSimNode -> Boolean
isTreeParent d = case d.links.treeChildren of
  [] -> false
  _ -> true

-- | table of all the forces that are used in the Spago component
forceLibrary :: Map Label (Force SpagoSimNode)
forceLibrary = initialize [
        createForce "center"       (RegularForce ForceCenter)   allNodes [ F.strengthVal 0.5, F.xVal 0.0, F.yVal 0.0 ]
      , createForce "x"            (RegularForce ForceX)        allNodes [ F.strengthVal 0.05, F.xVal 0.0 ]
      , createForce "y"            (RegularForce ForceY)        allNodes [ F.strengthVal 0.07, F.yVal 0.0 ]

      , createForce "collide1"     (RegularForce ForceCollide)  allNodes [ F.strengthVal 1.0, F.radiusFn (\d _ -> collideRadius (unsafeCoerce d)) ]
      , createForce "collide2"     (RegularForce ForceCollide)  allNodes [ F.strengthVal 0.7, F.radiusFn (\d _ -> collideRadiusBig (unsafeCoerce d)) ]
      , createForce "charge1"      (RegularForce ForceManyBody) allNodes [ F.strengthVal (-50.0), F.thetaVal 0.9, F.distanceMinVal 1.0, F.distanceMaxVal infinity ]
      , createForce "charge2"      (RegularForce ForceManyBody) allNodes [ F.strengthVal (-100.0), F.thetaVal 0.9, F.distanceMinVal 1.0, F.distanceMaxVal 400.0 ]
      , createForce "chargetree"   (RegularForce ForceManyBody) treeExceptLeaves [ F.strengthVal (-100.0), F.thetaVal 0.9, F.distanceMinVal 1.0, F.distanceMaxVal 400.0 ]

      , createForce "clusterx_M"     (RegularForce ForceX)        modulesOnly [ F.strengthVal 0.2, F.xFn (\d _ -> gridPointX (unsafeCoerce d)) ]
      , createForce "clustery_M"     (RegularForce ForceY)        modulesOnly [ F.strengthVal 0.2, F.yFn (\d _ -> gridPointY (unsafeCoerce d)) ]

      , createForce "clusterx_P"     (RegularForce ForceX)        packagesOnly [ F.strengthVal 0.8, F.xFn (\d _ -> gridPointX (unsafeCoerce d)) ]
      , createForce "clustery_P"     (RegularForce ForceY)        packagesOnly [ F.strengthVal 0.8, F.yFn (\d _ -> gridPointY (unsafeCoerce d)) ]

      , createForce "htreeNodesX"  (RegularForce ForceX)  (Just $ ForceFilter "tree only" (_.connected <<< unsafeCoerce))
          [ F.strengthVal 0.4, F.xFn (\d _ -> treePointX (unsafeCoerce d)) ]
      , createForce "htreeNodesY"  (RegularForce ForceY)  (Just $ ForceFilter "tree only" (_.connected <<< unsafeCoerce))
          [ F.strengthVal 0.4, F.yFn (\d _ -> treePointY (unsafeCoerce d)) ]
      , createForce "vtreeNodesX"  (RegularForce ForceX)  (Just $ ForceFilter "tree only" (_.connected <<< unsafeCoerce))
          [ F.strengthVal 0.4, F.xFn (\d _ -> treePointY (unsafeCoerce d)) ]
      , createForce "vtreeNodesY"  (RegularForce ForceY)  (Just $ ForceFilter "tree only" (_.connected <<< unsafeCoerce))
          [ F.strengthVal 0.4, F.yFn (\d _ -> treePointX (unsafeCoerce d)) ]

      , createForce "packageOrbit" (RegularForce ForceRadial)   packagesOnly
                                   [ F.strengthVal 0.7, F.xVal 0.0, F.yVal 0.0, F.radiusVal 1200.0 ]
      , createForce "unusedOrbit" (RegularForce ForceRadial)   unusedModulesOnly
                                   [ F.strengthVal 0.8, F.xVal 0.0, F.yVal 0.0, F.radiusVal 900.0 ]
      , createForce "moduleOrbit" (RegularForce ForceRadial)   usedModulesOnly
                                   [ F.strengthVal 0.8, F.xVal 0.0, F.yVal 0.0, F.radiusVal 600.0 ]

      , createLinkForce Nothing [ F.strengthVal 0.5, F.distanceVal 0.0, F.numKey (toNumber <<< _.id <<< unsafeCoerce) ]
      ]
  where
    packagesOnly      = Just $ ForceFilter "all packages" (isPackage <<< unsafeCoerce)
    modulesOnly       = Just $ ForceFilter "all modules" (isModule <<< unsafeCoerce)
    unusedModulesOnly = Just $ ForceFilter "unused modules only" (isUnusedModule <<< unsafeCoerce)
    usedModulesOnly   = Just $ ForceFilter "used modules only" (isUsedModule <<< unsafeCoerce)
    treeExceptLeaves  = Just $ ForceFilter "tree parent nodes only" (isTreeParent <<< unsafeCoerce)
