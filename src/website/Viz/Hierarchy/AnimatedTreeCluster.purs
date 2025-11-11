module D3.Viz.AnimatedTreeCluster where

-- | Animated transition between Tree and Cluster layouts
-- | Demonstrates pure PureScript hierarchy layout algorithms
-- | Simple approach: toggle between TreeViz and ClusterViz

import Prelude

import D3.Viz.TreeViz as TreeViz
import D3.Viz.ClusterViz as ClusterViz
import D3.Viz.FlareData (HierData)
import PSD3.Internal.Types (D3Selection_, Selector)
import PSD3.Capabilities.Selection (class SelectionM)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)

-- Layout type
data LayoutType = TreeLayout | ClusterLayout

derive instance Eq LayoutType

instance Show LayoutType where
  show TreeLayout = "Tree"
  show ClusterLayout = "Cluster"

-- Toggle layout type
toggleLayout :: LayoutType -> LayoutType
toggleLayout TreeLayout = ClusterLayout
toggleLayout ClusterLayout = TreeLayout

-- Initial draw - draws tree layout
draw :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  HierData -> Selector (D3Selection_ Unit) -> m LayoutType
draw flareData selector = do
  liftEffect $ log "AnimatedTreeCluster: Initial draw (Tree layout)"
  TreeViz.draw flareData selector
  pure TreeLayout

-- Update - clears current and draws the other layout
-- Note: The visualization modules (TreeViz/ClusterViz) handle clearing via their own draw functions
updateLayout :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  HierData -> LayoutType -> Selector (D3Selection_ Unit) -> m LayoutType
updateLayout flareData currentLayout selector = do
  let newLayout = toggleLayout currentLayout
  liftEffect $ log $ "AnimatedTreeCluster: Transitioning from " <> show currentLayout <> " to " <> show newLayout

  -- Just draw the new layout - TreeViz and ClusterViz create fresh SVGs each time
  case newLayout of
    TreeLayout -> TreeViz.draw flareData selector
    ClusterLayout -> ClusterViz.draw flareData selector

  pure newLayout
