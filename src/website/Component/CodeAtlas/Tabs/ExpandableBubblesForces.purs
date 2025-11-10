module PSD3.CodeAtlas.Tabs.ExpandableBubblesForces where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import PSD3.Internal.Attributes.Instances (Label)
import PSD3.Internal.Simulation.Config as F
import PSD3.Internal.Simulation.Forces (createForce, createLinkForce, initialize)
import PSD3.Internal.Simulation.Types (Force, ForceFilter(..), ForceStatus(..), ForceType(..), RegularForceType(..), allNodes)

-- | Force library for expandable bubbles visualization
-- | Contains two sets of forces: compact (for overview) and spotlight (for detailed view)
forceLibrary :: forall d. Map.Map Label (Force d)
forceLibrary = initialize
  -- Overview/Compact forces - tight packing for seeing all modules
  [ createForce "manyBody-compact" (RegularForce ForceManyBody) allNodes
      [ F.strengthVal (-30.0)  -- Weak repulsion for compact layout
      , F.distanceMaxVal 200.0
      ]
  , createForce "collision-compact" (RegularForce ForceCollide) allNodes
      [ F.radiusVal 15.0  -- Small fixed radius for compact view
      , F.strengthVal 0.7
      ]

  -- Spotlight forces - spread out for viewing expanded modules with declarations
  , createForce "manyBody-spotlight" (RegularForce ForceManyBody) allNodes
      [ F.strengthVal (-100.0)  -- Stronger repulsion for spread out layout
      , F.distanceMaxVal 400.0
      ]
  , createForce "collision-spotlight" (RegularForce ForceCollide) allNodes
      [ F.radiusVal 50.0  -- Larger radius for expanded nodes
      , F.strengthVal 0.8
      ]

  -- Common forces - always active regardless of mode
  , createForce "center" (RegularForce ForceCenter) allNodes
      [ F.xVal 400.0  -- Will be updated with actual dimensions
      , F.yVal 300.0
      ]
  , createLinkForce allNodes
      [ F.distance 30.0
      , F.strengthVal 0.1  -- Weak link force - just hints at structure
      ]
  ]

-- | Forces active in compact/overview mode
compactForces :: Set.Set Label
compactForces = Set.fromFoldable
  [ "manyBody-compact"
  , "collision-compact"
  , "center"
  , "links"
  ]

-- | Forces active in spotlight mode
spotlightForces :: Set.Set Label
spotlightForces = Set.fromFoldable
  [ "manyBody-spotlight"
  , "collision-spotlight"
  , "center"
  , "links"
  ]
