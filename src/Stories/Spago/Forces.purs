module Stories.Spago.Forces where

import Prelude

import D3.Attributes.Instances (Label)
import D3.Data.Types (Datum_, Index_, PointXY, index_ToInt)
import D3.Examples.Spago.Model (datum_, numberToGridPoint, offsetXY, scalePoint)
import D3.Simulation.Config as F
import D3.Simulation.Forces (createForce, createLinkForce, initialize)
import D3.Simulation.Types (FixForceType(..), Force, ForceFilter(..), ForceStatus, ForceType(..), RegularForceType(..), _name, _status, allNodes)
import Data.Int (toNumber)
import Data.Lens (view)
import Data.Map (Map, fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Number (infinity)
import Data.Tuple (Tuple(..))
import Debug (spy)

-- | table of all the forces that are used in the Spago component
forceLibrary :: Map Label Force
forceLibrary = initialize [
        createForce "collide1"     (RegularForce ForceCollide)  allNodes [ F.strength 1.0, F.radius datum_.collideRadius ]
      , createForce "collide2"     (RegularForce ForceCollide)  allNodes [ F.strength 0.7, F.radius datum_.collideRadiusBig ]
      , createForce "charge1"      (RegularForce ForceManyBody) allNodes [ F.strength (-30.0), F.theta 0.9, F.distanceMin 1.0, F.distanceMax infinity ]
      , createForce "center"       (RegularForce ForceCenter)   allNodes [ F.strength 0.5, F.x 0.0, F.y 0.0 ]
      , createForce "x"            (RegularForce ForceX)        allNodes [ F.strength 0.05, F.x 0.0 ]
      , createForce "y"            (RegularForce ForceY)        allNodes [ F.strength 0.07, F.y 0.0 ]
      , createForce "charge2"      (RegularForce ForceManyBody) allNodes [ F.strength (-100.0), F.theta 0.9, F.distanceMin 1.0, F.distanceMax 100.0 ]
      -- the "xy" cluster force lacks any sugar for setting via a PointXY instead of individually (seems like it'd be worth writing one)
      -- , createForce "clusterxy"    (RegularForce ForceX)        allNodes [ F.strength 0.2, F.x (\d i -> cluster2Point i) ]
      , createForce "clusterx"     (RegularForce ForceX)        allNodes [ F.strength 0.2, F.x (\(_ :: Datum_) i -> _.x $ cluster2Point i) ] -- TODO d:: Datum_ is ugly but needed for typeclass
      , createForce "clustery"     (RegularForce ForceY)        allNodes [ F.strength 0.2, F.y (\(_ :: Datum_) i -> _.y $ cluster2Point i) ]

      , createForce "packageGrid"     (FixForce $ ForceFixPositionXY gridXY)   (Just $ ForceFilter "packages only" datum_.isPackage)            [ ] 
      , createForce "centerNamedNode" (FixForce $ ForceFixPositionXY centerXY) (Just $ ForceFilter "src only"     \d -> datum_.name d == "src") [ ] 
      , createForce "treeNodesPinned" (FixForce $ ForceFixPositionXY treeXY)   (Just $ ForceFilter "tree only"    \d -> datum_.connected d)     [ ] 

      , createForce "packageOrbit" (RegularForce ForceRadial)   (selectivelyApplyForce datum_.isPackage "packages only") 
                                   [ F.strength 0.8, F.x 0.0, F.y 0.0, F.radius 300.0 ]
      , createForce "moduleOrbit1" (RegularForce ForceRadial)   (selectivelyApplyForce datum_.isUnusedModule "unused modules only") 
                                   [ F.strength 0.8, F.x 0.0, F.y 0.0, F.radius 700.0 ]
      , createForce "moduleOrbit2" (RegularForce ForceRadial)   (selectivelyApplyForce datum_.isUsedModule "used modules only")
                                   [ F.strength 0.8, F.x 0.0, F.y 0.0, F.radius 600.0 ]
      , createForce "moduleOrbit3" (RegularForce ForceRadial)   (selectivelyApplyForce datum_.isUsedModule "direct deps of Main")
                                   [ F.strength 0.8, F.x 0.0, F.y 0.0, F.radius 600.0 ]
      , createLinkForce Nothing [ F.strength 1.0, F.distance 0.0, F.numKey (toNumber <<< datum_.id) ]
      ]
  where
    gridXY   _ i = cluster2Point i
    centerXY _ _ = { x: 0.0, y: 0.0 }
    treeXY   d _ = spy "treePoint" $ datum_.treePoint d
    selectivelyApplyForce :: (Datum_ -> Boolean) -> String -> Maybe ForceFilter
    selectivelyApplyForce filterFn description = Just $ ForceFilter description filterFn

-- TODO this is a ridiculously brittle and specific function to distribute package nodes on the screen, general solution needed here
cluster2Point :: Index_ -> PointXY
cluster2Point i =  
  scalePoint 200.0 200.0 $
  offsetXY { x: (-4.5), y: (-2.5) } $ -- center the grid on the (already centered) origin
  numberToGridPoint 10 (index_ToInt i)


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

