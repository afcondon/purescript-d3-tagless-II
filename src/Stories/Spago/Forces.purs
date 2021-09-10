module Stories.Spago.Forces where

import Prelude

import D3.Data.Types (Datum_)
import D3.Examples.Spago.Model (cluster2Point, datum_)
import D3.Simulation.Config as F
import D3.Simulation.Forces (createForce)
import D3.Simulation.Types (FixForceType(..), Force, ForceFilter(..), ForceType(..), RegularForceType(..), allNodes) 
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Number (infinity)

-- | table of all the forces that are used in the Spago component
forces :: Array Force
forces = [
        createForce "collide1"     (RegularForce ForceCollide)  allNodes [ F.strength 1.0, F.radius datum_.collideRadius, F.iterations 1.0 ]
      , createForce "collide2"     (RegularForce ForceCollide)  allNodes [ F.strength 1.0, F.radius datum_.collideRadiusBig, F.iterations 1.0 ]
      , createForce "charge1"      (RegularForce ForceManyBody) allNodes [ F.strength (-30.0), F.theta 0.9, F.distanceMin 1.0, F.distanceMax infinity ]
      , createForce "center"       (RegularForce ForceCenter)   allNodes [ F.strength 0.5, F.x 0.0, F.y 0.0 ]
      , createForce "x"            (RegularForce ForceX)        allNodes [ F.strength 0.05, F.x 0.0 ]
      , createForce "y"            (RegularForce ForceY)        allNodes [ F.strength 0.07, F.y 0.0 ]
      , createForce "charge2"      (RegularForce ForceManyBody) allNodes [ F.strength (-100.0), F.theta 0.9, F.distanceMin 1.0, F.distanceMax 100.0 ]
      , createForce "clusterx"     (RegularForce ForceX)        allNodes [ F.strength 0.2, F.x datum_.clusterPointX ]
      , createForce "clustery"     (RegularForce ForceY)        allNodes [ F.strength 0.2, F.y datum_.clusterPointY ]

      , createForce "packageGrid"     (FixForce $ ForceFixPositionXY gridXY)   (Just $ ForceFilter "packages only" datum_.isPackage) [ ] 
      , createForce "centerNamedNode" (FixForce $ ForceFixPositionXY centerXY) (Just $ ForceFilter "src only" (\d -> (datum_.name d) == "src")) [ ] 

      , createForce "packageOrbit" (RegularForce ForceRadial)   (selectivelyApplyForce datum_.isPackage "packages only") 
                                   [ F.strength 0.8, F.x 0.0, F.y 0.0, F.radius 300.0 ]
      , createForce "moduleOrbit1" (RegularForce ForceRadial)   (selectivelyApplyForce datum_.isUnusedModule "unused modules only") 
                                   [ F.strength 0.8, F.x 0.0, F.y 0.0, F.radius 700.0 ]
      , createForce "moduleOrbit2" (RegularForce ForceRadial)   (selectivelyApplyForce datum_.isUsedModule "used modules only")
                                   [ F.strength 0.8, F.x 0.0, F.y 0.0, F.radius 600.0 ]
      , createForce "moduleOrbit3" (RegularForce ForceRadial)   (selectivelyApplyForce datum_.isUsedModule "direct deps of Main")
                                   [ F.strength 0.8, F.x 0.0, F.y 0.0, F.radius 600.0 ]

      , createForce "links" LinkForce Nothing [ F.strength 1.0, F.distance 0.0, F.numKey (toNumber <<< datum_.id) ]
      ]
  where
    gridXY _ i = cluster2Point i
    centerXY _ _ = { x: 0.0, y: 0.0 }
    selectivelyApplyForce :: (Datum_ -> Boolean) -> String -> Maybe ForceFilter
    selectivelyApplyForce filterFn description = Just $ ForceFilter description filterFn

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

