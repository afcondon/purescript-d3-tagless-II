module D3.Viz.AnimatedTreeClusterLoop where

-- | Auto-looping animated transition between Tree4 and Cluster4 layouts
-- | For Tour Motion page - no manual controls, just automatic cycling

import Prelude

import PSD3.Shared.FlareData (HierData)
import Data.Time.Duration (Milliseconds(..))
import Data.Tree (Tree)
import Data.Tuple (fst)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, delay)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import PSD3.Interpreter.D3 (runD3M)
import PSD3.Internal.Types (D3Selection_)
import D3.Viz.AnimatedTree4Cluster4 as AnimatedTree
import D3.Viz.AnimatedTree4Cluster4 (LayoutType(..), TreeModel)

-- | Toggle layout type
toggleLayout :: LayoutType -> LayoutType
toggleLayout TreeLayout = ClusterLayout
toggleLayout ClusterLayout = TreeLayout

-- | Type alias for viz state
type VizState =
  { dataTree :: Tree TreeModel
  , linksGroup :: D3Selection_ Unit
  , nodesGroup :: D3Selection_ Unit
  , chartWidth :: Number
  , chartHeight :: Number
  }

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

  -- Apply the new layout with animation
  _ <- liftEffect $ runD3M $ AnimatedTree.animationStep
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
  -- Initialize visualization (runD3M returns Tuple result state, we want the result)
  vizStateTuple <- liftEffect $ runD3M $ AnimatedTree.draw flareData containerSelector
  let vizState = fst vizStateTuple

  -- Create ref to track current layout
  layoutRef <- liftEffect $ Ref.new TreeLayout

  -- Perform initial layout (Tree)
  _ <- liftEffect $ runD3M $ AnimatedTree.animationStep
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
