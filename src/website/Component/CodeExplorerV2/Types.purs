-- | Types for CodeExplorerV2
module Component.CodeExplorerV2.Types where

import Prelude

-- | Scene enumeration for tree reveal animation
-- |
-- | Orbit: Initial view with packages in outer ring, modules in inner ring
-- | TreeReveal: Simulation off, nodes transitioning to tree positions
-- | ForceTree: Simulation on with force-driven radial tree
data Scene
  = Orbit        -- Packages outer ring, modules inner ring, main pinned center
  | TreeReveal   -- Nodes transition to tree positions, bezier links appear
  | ForceTree    -- Force simulation with straight links

derive instance Eq Scene

instance Show Scene where
  show Orbit = "Orbit"
  show TreeReveal = "TreeReveal"
  show ForceTree = "ForceTree"
