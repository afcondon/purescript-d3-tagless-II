module D3.Viz.AnimatedTreeClusterLoop where

-- | Auto-looping animated transition between Tree4 and Cluster4 layouts
-- | For Tour Motion page - no manual controls, just automatic cycling
-- | V2 implementation using PSD3v2 primitives

import Prelude

import PSD3.Shared.FlareData (HierData)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, delay)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import D3.Viz.AnimatedTree4Cluster4v2 as AnimatedTree
import D3.Viz.AnimatedTree4Cluster4v2 (LayoutType(..), VizState)

-- | Toggle layout type
toggleLayout :: LayoutType -> LayoutType
toggleLayout TreeLayout = ClusterLayout
toggleLayout ClusterLayout = TreeLayout

-- | Animation loop that cycles between layouts
animationLoop ::
  VizState ->
  Ref.Ref LayoutType ->
  Aff Unit
animationLoop vizState layoutRef = do
  -- Read current layout and toggle it
  currentLayout <- liftEffect $ Ref.read layoutRef
  let newLayout = toggleLayout currentLayout
  liftEffect $ Ref.write newLayout layoutRef

  -- Apply the new layout with animation (v2 returns Effect directly)
  liftEffect $ AnimatedTree.animationStep
    vizState.dataTree
    vizState.linksGroup
    vizState.nodesGroup
    vizState.chartWidth
    vizState.chartHeight
    newLayout

  -- Wait before next transition (3 seconds)
  delay (Milliseconds 3000.0)

  -- Continue the loop
  animationLoop vizState layoutRef

-- | Initialize and start the auto-looping animation
startAnimatedTreeClusterLoop :: HierData -> String -> Effect Unit
startAnimatedTreeClusterLoop flareData containerSelector = launchAff_ do
  -- Initialize visualization (v2 returns Effect directly)
  vizState <- liftEffect $ AnimatedTree.draw flareData containerSelector

  -- Create ref to track current layout
  layoutRef <- liftEffect $ Ref.new TreeLayout

  -- Perform initial layout (Tree)
  liftEffect $ AnimatedTree.animationStep
    vizState.dataTree
    vizState.linksGroup
    vizState.nodesGroup
    vizState.chartWidth
    vizState.chartHeight
    TreeLayout

  -- Wait a bit before starting the loop
  delay (Milliseconds 3000.0)

  -- Start the infinite animation loop
  animationLoop vizState layoutRef
