module D3.Viz.Navigation.Forces where

import Prelude

import D3.Attributes.Instances (Label)
import D3.Viz.Navigation.Model (NavigationSimRecord, NodeType(..))
import D3.Viz.Navigation.Unsafe (unboxD3SimNode)
import D3.Simulation.Config as F
import D3.Simulation.Forces (createForce, createLinkForce, initialize)
import D3.Simulation.Types (Force, ForceType(..), RegularForceType(..), allNodes)
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Number (infinity)

-- | Helper to get collision radius based on node type
collisionRadius :: _ -> Number
collisionRadius d = do
  let node = unboxD3SimNode d
  case node.nodeType of
    _ -> 80.0  -- Use consistent collision radius for spacing

-- | Table of all forces used in the navigation component
forceLibrary :: Map Label Force
forceLibrary = initialize [
    -- Strong charge to spread nodes out
    createForce "charge" (RegularForce ForceManyBody) allNodes [
        F.strength (-800.0)
      , F.theta 0.9
      , F.distanceMin 1.0
      , F.distanceMax infinity
      ]

    -- Center force to keep graph centered
  , createForce "center" (RegularForce ForceCenter) allNodes [
        F.strength 0.5
      , F.x 0.0
      , F.y 0.0
      ]

    -- Collision force to prevent overlap
  , createForce "collision" (RegularForce ForceCollide) allNodes [
        F.strength 1.0
      , F.radius collisionRadius
      ]

    -- Link force to connect parent-child nodes
  , createLinkForce Nothing [
        F.strength 0.5
      , F.distance 150.0
      ]
  ]
