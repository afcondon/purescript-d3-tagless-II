module PSD3v2.Transition.Scene
  ( EnterBehavior(..)
  , ExitBehavior(..)
  , UpdateBehavior(..)
  , TransitionSpec
  , encodeEnterBehavior
  , encodeExitBehavior
  , encodeUpdateBehavior
  , smoothTransition
  , smoothTransitionPinned
  , quickTransition
  , instantTransition
  ) where

import Prelude

-- ============================================================================
-- Declarative Transition Configuration
-- ============================================================================
-- |
-- | Transitions describe HOW a scene appears, not just WHAT it contains.
-- | By making transitions part of the scene specification, we maintain
-- | the declarative philosophy: users describe the desired end state and
-- | the library handles all sequencing automatically.
-- |
-- | This module provides general-purpose transition types that can be used
-- | for any visualization, not just simulations. For simulation-specific
-- | scene configuration, see PSD3v2.Simulation.Scene.

-- | How nodes should appear when they enter the visualization
data EnterBehavior
  = FadeIn      -- Opacity: 0 → 1 over transition duration
  | ScaleUp     -- Scale: 0 → 1 over transition duration
  | InstantEnter -- Appear immediately at full opacity

-- | How nodes should disappear when they exit the visualization
data ExitBehavior
  = FadeOut      -- Opacity: 1 → 0 over transition duration
  | ScaleDown    -- Scale: 1 → 0 over transition duration
  | InstantExit  -- Disappear immediately

-- | How existing nodes should move to new positions
data UpdateBehavior
  = TransitionMove  -- Smoothly animate to new position over duration
  | InstantMove     -- Snap to new position immediately

-- | Explicit encoding functions for FFI
-- | These ensure we control the string constants passed to JavaScript
encodeEnterBehavior :: EnterBehavior -> String
encodeEnterBehavior FadeIn = "FadeIn"
encodeEnterBehavior ScaleUp = "ScaleUp"
encodeEnterBehavior InstantEnter = "InstantEnter"

encodeExitBehavior :: ExitBehavior -> String
encodeExitBehavior FadeOut = "FadeOut"
encodeExitBehavior ScaleDown = "ScaleDown"
encodeExitBehavior InstantExit = "InstantExit"

encodeUpdateBehavior :: UpdateBehavior -> String
encodeUpdateBehavior TransitionMove = "TransitionMove"
encodeUpdateBehavior InstantMove = "InstantMove"

-- | Complete transition specification for a scene
-- | `Nothing` = instant/no transition (backward compatible)
-- | `Just spec` = declarative transition behavior
type TransitionSpec =
  { duration :: Number           -- Transition duration in milliseconds (e.g., 1500.0)
  , enterNodes :: EnterBehavior  -- How new nodes appear
  , exitNodes :: ExitBehavior    -- How old nodes disappear
  , updateNodes :: UpdateBehavior -- How existing nodes move to new positions
  , pinAfterTransition :: Boolean -- Pin nodes at final positions after transition completes?
  }

-- | Common transition presets

-- | Smooth transition with fading and position animation (1.5 seconds)
-- | Nodes are NOT pinned - forces will continue to act after transition
smoothTransition :: TransitionSpec
smoothTransition =
  { duration: 1500.0
  , enterNodes: FadeIn
  , exitNodes: FadeOut
  , updateNodes: TransitionMove
  , pinAfterTransition: false  -- Let forces work after transition
  }

-- | Smooth transition that pins nodes at their final positions
-- | Use this for tree layouts or other layouts where nodes should stay locked
smoothTransitionPinned :: TransitionSpec
smoothTransitionPinned =
  { duration: 1500.0
  , enterNodes: FadeIn
  , exitNodes: FadeOut
  , updateNodes: TransitionMove
  , pinAfterTransition: true   -- Lock nodes at final positions
  }

-- | Quick transition with scaling effects (0.5 seconds)
quickTransition :: TransitionSpec
quickTransition =
  { duration: 500.0
  , enterNodes: ScaleUp
  , exitNodes: ScaleDown
  , updateNodes: TransitionMove
  , pinAfterTransition: false
  }

-- | Instant transition (no animation)
instantTransition :: TransitionSpec
instantTransition =
  { duration: 0.0
  , enterNodes: InstantEnter
  , exitNodes: InstantExit
  , updateNodes: InstantMove
  , pinAfterTransition: false
  }
