module PSD3v2.Simulation.Scene
  ( module PSD3v2.Transition.Scene
  , SimSceneConfig
  , defaultSimScene
  , withNodeFilter
  , withLinkFilter
  , withForces
  , withInitializer
  , withCssClass
  , withTransition
  ) where

import Prelude

import PSD3.Internal.Attributes.Instances (Label)
import PSD3.Internal.Types (Datum_)
import PSD3.Internal.Simulation.Types (Force)
import PSD3.Data.Node (Link, SimulationNode)
import PSD3v2.Transition.Scene (TransitionSpec, smoothTransition, smoothTransitionPinned, quickTransition, instantTransition)
import Data.Map (Map)
import Data.Map as Data.Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set

-- Re-export transition types
import PSD3v2.Transition.Scene (EnterBehavior(..), ExitBehavior(..), UpdateBehavior(..), TransitionSpec, encodeEnterBehavior, encodeExitBehavior, encodeUpdateBehavior, smoothTransition, smoothTransitionPinned, quickTransition, instantTransition)

-- ============================================================================
-- Scene Configuration
-- ============================================================================

-- | Generic scene configuration for force-directed visualizations
-- | Parameterized over node data type (a), link data type (linkRow), and attributes type (attrs)
-- |
-- | A "scene" is a complete specification of:
-- | - Which data to show (node/link filter predicates)
-- | - Which forces to enable (Set of active force labels)
-- | - Visual appearance (CSS class, custom attributes)
-- | - Initialization (node positioning/pinning functions)
-- | - Transition behavior (how the scene animates in)
-- |
-- | This pattern enables declarative scene switching - just provide a new
-- | SimSceneConfig and call runSimulation to transition between visualizations.
type SimSceneConfig a id linkRow attrs =
  { -- Data filtering
    chooseNodes :: SimulationNode a -> Boolean           -- Which nodes to display
  , linksShown :: Link id linkRow -> Boolean             -- Which links to render
  , linksActive :: Link id linkRow -> Boolean            -- Which links exert force

  -- Force configuration
  , activeForces :: Set Label                                -- Which forces from library to enable

  -- Visual styling
  , cssClass :: String                                       -- Root CSS class for scene
  , attributes :: attrs                                      -- Scene-specific attributes

  -- Node initialization (positioning, pinning, etc.)
  , nodeInitializerFunctions :: Array (Array (SimulationNode a) -> Array (SimulationNode a))

  -- Transition behavior (Nothing = instant, Just = animated)
  , transitionConfig :: Maybe TransitionSpec                 -- How scene transitions in/out
  }

-- | Create a default simulation scene config with minimal settings
-- | Usage: `defaultSimScene forceLibrary customAttrs`
-- | Note: a is the node data type, Force wraps SimulationNode a
defaultSimScene :: forall a id linkRow attrs. Map Label (Force (SimulationNode a)) -> attrs -> SimSceneConfig a id linkRow attrs
defaultSimScene forceLibrary attrs =
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
withNodeFilter :: forall a id linkRow attrs.
  (SimulationNode a -> Boolean) ->
  SimSceneConfig a id linkRow attrs ->
  SimSceneConfig a id linkRow attrs
withNodeFilter pred scene = scene { chooseNodes = pred }

-- | Modify link filter in a scene
withLinkFilter :: forall a id linkRow attrs.
  (Link id linkRow -> Boolean) ->
  SimSceneConfig a id linkRow attrs ->
  SimSceneConfig a id linkRow attrs
withLinkFilter pred scene = scene { linksShown = pred }

-- | Set active forces in a scene
withForces :: forall a id linkRow attrs.
  Set Label ->
  SimSceneConfig a id linkRow attrs ->
  SimSceneConfig a id linkRow attrs
withForces forces scene = scene { activeForces = forces }

-- | Add a node initializer function
withInitializer :: forall a id linkRow attrs.
  (Array (SimulationNode a) -> Array (SimulationNode a)) ->
  SimSceneConfig a id linkRow attrs ->
  SimSceneConfig a id linkRow attrs
withInitializer fn scene =
  scene { nodeInitializerFunctions = scene.nodeInitializerFunctions <> [fn] }

-- | Set CSS class for scene
withCssClass :: forall a id linkRow attrs.
  String ->
  SimSceneConfig a id linkRow attrs ->
  SimSceneConfig a id linkRow attrs
withCssClass cls scene = scene { cssClass = cls }

-- | Set transition config for scene
-- | Use `Nothing` for instant transitions, or `Just smoothTransition` for animated
withTransition :: forall a id linkRow attrs.
  Maybe TransitionSpec ->
  SimSceneConfig a id linkRow attrs ->
  SimSceneConfig a id linkRow attrs
withTransition trans scene = scene { transitionConfig = trans }
