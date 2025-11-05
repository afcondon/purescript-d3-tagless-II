module PSD3.Simulation.Scene where

import Prelude

import PSD3.Internal.Attributes.Instances (Label)
import PSD3.Internal.Types (D3Selection_, Datum_)
import PSD3.Internal.Selection.Types (SelectionAttribute)
import PSD3.Internal.Simulation.Types (Force)
import PSD3.Data.Node (D3Link_Unswizzled, D3_SimulationNode)
import Data.Map (Map)
import Data.Map as Data.Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set

-- ============================================================================
-- Declarative Transition Configuration
-- ============================================================================
-- |
-- | Transitions describe HOW a scene appears, not just WHAT it contains.
-- | By making transitions part of the scene specification, we maintain
-- | the declarative philosophy: users describe the desired end state and
-- | the library handles all sequencing automatically.

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
  }

-- | Common transition presets

-- | Smooth transition with fading and position animation (1.5 seconds)
smoothTransition :: TransitionSpec
smoothTransition =
  { duration: 1500.0
  , enterNodes: FadeIn
  , exitNodes: FadeOut
  , updateNodes: TransitionMove
  }

-- | Quick transition with scaling effects (0.5 seconds)
quickTransition :: TransitionSpec
quickTransition =
  { duration: 500.0
  , enterNodes: ScaleUp
  , exitNodes: ScaleDown
  , updateNodes: TransitionMove
  }

-- | Instant transition (no animation)
instantTransition :: TransitionSpec
instantTransition =
  { duration: 0.0
  , enterNodes: InstantEnter
  , exitNodes: InstantExit
  , updateNodes: InstantMove
  }

-- ============================================================================
-- Scene Configuration
-- ============================================================================

-- | Generic scene configuration for force-directed visualizations
-- | Parameterized over node data type (d) and attributes type (attrs)
-- |
-- | A "scene" is a complete specification of:
-- | - Which data to show (node/link filter predicates)
-- | - Which forces to enable (Set of active force labels)
-- | - Visual appearance (CSS class, custom attributes)
-- | - Initialization (node positioning/pinning functions)
-- | - Transition behavior (how the scene animates in)
-- |
-- | This pattern enables declarative scene switching - just provide a new
-- | SceneConfig and call runSimulation to transition between visualizations.
type SceneConfig d attrs =
  { -- Data filtering
    chooseNodes :: D3_SimulationNode d -> Boolean           -- Which nodes to display
  , linksShown :: D3Link_Unswizzled -> Boolean              -- Which links to render
  , linksActive :: Datum_ -> Boolean                         -- Which links exert force

  -- Force configuration
  , activeForces :: Set Label                                -- Which forces from library to enable

  -- Visual styling
  , cssClass :: String                                       -- Root CSS class for scene
  , attributes :: attrs                                      -- Scene-specific attributes

  -- Node initialization (positioning, pinning, etc.)
  , nodeInitializerFunctions :: Array (Array (D3_SimulationNode d) -> Array (D3_SimulationNode d))

  -- Transition behavior (Nothing = instant, Just = animated)
  , transitionConfig :: Maybe TransitionSpec                 -- How scene transitions in/out
  }

-- | Create a default scene config with minimal settings
-- | Usage: `defaultScene forceLibrary customAttrs`
defaultScene :: forall d attrs. Map Label Force -> attrs -> SceneConfig d attrs
defaultScene forceLibrary attrs =
  { chooseNodes: const true                                  -- Show all nodes
  , linksShown: const false                                  -- Hide all links
  , linksActive: const false                                 -- No link forces
  , activeForces: Set.fromFoldable (Data.Map.keys forceLibrary)  -- All forces enabled
  , cssClass: ""
  , attributes: attrs
  , nodeInitializerFunctions: []                             -- No initialization
  , transitionConfig: Nothing                                -- Instant transition (backward compatible)
  }

-- | Modify node filter in a scene
withNodeFilter :: forall d attrs.
  (D3_SimulationNode d -> Boolean) ->
  SceneConfig d attrs ->
  SceneConfig d attrs
withNodeFilter pred scene = scene { chooseNodes = pred }

-- | Modify link filter in a scene
withLinkFilter :: forall d attrs.
  (D3Link_Unswizzled -> Boolean) ->
  SceneConfig d attrs ->
  SceneConfig d attrs
withLinkFilter pred scene = scene { linksShown = pred }

-- | Set active forces in a scene
withForces :: forall d attrs.
  Set Label ->
  SceneConfig d attrs ->
  SceneConfig d attrs
withForces forces scene = scene { activeForces = forces }

-- | Add a node initializer function
withInitializer :: forall d attrs.
  (Array (D3_SimulationNode d) -> Array (D3_SimulationNode d)) ->
  SceneConfig d attrs ->
  SceneConfig d attrs
withInitializer fn scene =
  scene { nodeInitializerFunctions = scene.nodeInitializerFunctions <> [fn] }

-- | Set CSS class for scene
withCssClass :: forall d attrs.
  String ->
  SceneConfig d attrs ->
  SceneConfig d attrs
withCssClass cls scene = scene { cssClass = cls }

-- | Set transition config for scene
-- | Use `Nothing` for instant transitions, or `Just smoothTransition` for animated
withTransition :: forall d attrs.
  Maybe TransitionSpec ->
  SceneConfig d attrs ->
  SceneConfig d attrs
withTransition trans scene = scene { transitionConfig = trans }
