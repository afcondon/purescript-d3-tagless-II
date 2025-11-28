-- | TreeReveal scene - staggered animation to radial tree positions
module Component.CodeExplorerV2.Scenes.TreeReveal where

import Prelude

import Component.CodeExplorerV2.Scenes.Types (LinkStyle(..), SceneConfig)
import D3.Viz.Spago.Files (LinkType(..))
import D3.Viz.Spago.Model (SpagoSimNode, isUsedModule, treeDepthMultiplier)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (toMaybe)
import Data.Number (cos, sin)
import Data.Time.Duration (Milliseconds(..))
import PSD3.Config.Scene as CFG

-- | TreeReveal scene configuration (OLD SYSTEM - using Force handles)
-- | Kept for compatibility during transition
-- | Note: No forces - simulation is stopped during tree reveal
config :: SceneConfig
config =
  { nodeFilter: isUsedModule  -- Only modules in tree
  , linkFilter: Just \l -> l.linktype == M2M_Tree
  , nodeInitializers: []  -- Animation handles positioning
  , forces: []            -- Simulation stopped
  , linkStyle: TreeBezier
  , domSync: Nothing      -- Animated from current positions
  }

-- | NEW: TreeReveal scene using immutable configuration
-- | This uses the new PSD3.Config system
-- | No forces - simulation stopped during animated transition
sceneConfig :: CFG.SceneConfig
sceneConfig = CFG.scene "TreeReveal" []  -- Empty force list - simulation stopped
  # CFG.withDescription "Staggered animation transition to tree layout"
  # CFG.withAlpha 0.0  -- Simulation stopped (alpha = 0)

-- | Calculate transform string for radial tree position
nodeToTreeTransform :: SpagoSimNode -> String
nodeToTreeTransform node =
  case toMaybe node.treeXY of
    Just treeXY ->
      let angle = treeXY.x
          radius = treeXY.y * treeDepthMultiplier
          x = radius * cos angle
          y = radius * sin angle
      in "translate(" <> show x <> "," <> show y <> ")"
    Nothing ->
      "translate(" <> show node.x <> "," <> show node.y <> ")"

-- | Calculate fill color based on tree depth
depthToColor :: SpagoSimNode -> String
depthToColor node =
  let depth = fromMaybe 0 $ toMaybe node.treeDepth
      hue = 30.0 + (toNumber depth * 10.0)  -- Orange to purple
      saturation = 70.0
      lightness = 50.0
  in "hsl(" <> show hue <> ", " <> show saturation <> "%, " <> show lightness <> "%)"

-- | Stagger delay based on tree depth
staggerByDepth :: Number -> SpagoSimNode -> Int -> Milliseconds
staggerByDepth msPerDepth node _ =
  let depth = fromMaybe 0 $ toMaybe node.treeDepth
  in Milliseconds (toNumber depth * msPerDepth)
