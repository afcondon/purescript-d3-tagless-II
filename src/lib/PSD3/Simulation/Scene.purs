module PSD3.Simulation.Scene where

import Prelude

import PSD3.Internal.Attributes.Instances (Label)
import PSD3.Internal.Types (D3Selection_, Datum_)
import PSD3.Internal.Selection.Types (SelectionAttribute)
import PSD3.Internal.Simulation.Types (Force)
import PSD3.Data.Node (D3Link_Unswizzled, D3_SimulationNode)
import Data.Map (Map)
import Data.Map as Data.Map
import Data.Set (Set)
import Data.Set as Set

-- | Generic scene configuration for force-directed visualizations
-- | Parameterized over node data type (d) and attributes type (attrs)
-- |
-- | A "scene" is a complete specification of:
-- | - Which data to show (node/link filter predicates)
-- | - Which forces to enable (Set of active force labels)
-- | - Visual appearance (CSS class, custom attributes)
-- | - Initialization (node positioning/pinning functions)
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
