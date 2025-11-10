module D3.Viz.AnimatedRadialTree where

-- | Animated Radial Tree with smooth transitions between Tidy and Dendrogram layouts
-- | Demonstrates transitions on the same nodes (no enter/exit, just updates)
-- | Includes rotate functionality for label readability

import Prelude

import Data.Array (unsafeIndex)
import Data.Number (pi)
import Data.Tuple (Tuple(..))
import Unsafe.Coerce (unsafeCoerce)
import Data.Time.Duration (Milliseconds(..))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)
import PSD3 (class SelectionM, D3Selection_, Datum_, Index_, Selector, Element(..), appendTo, attach, mergeSelections, openSelection, setAttributes, simpleJoin, updateJoin)
import PSD3.Data.Node (D3_TreeNode)
import PSD3.Data.Tree (TreeJson_, TreeLayoutFn_, TreeType(..))
import PSD3.Internal.Attributes.Sugar (classed, dy, fill, fontFamily, fontSize, radius, strokeColor, strokeOpacity, strokeWidth, text, textAnchor, to, transform, transitionWithDuration, x)
import PSD3.Internal.FFI (descendants_, hierarchyFromJSON_, links_, runLayoutFn_, treeMinMax_, treeSetSeparation_, treeSetSize_)
import PSD3.Internal.Hierarchical (radialLink, radialSeparation)
import PSD3.Shared.TreeHelpers (treeDatum_)
import PSD3.Shared.ZoomableViewbox (ZoomableSVGConfig, zoomableSVG)
import Utility (getWindowWidthHeight)

-- FFI imports for D3 layout algorithms
foreign import d3Tree_ :: Unit -> TreeLayoutFn_
foreign import d3Cluster_ :: Unit -> TreeLayoutFn_

-- Type-safe layout selection using pattern matching
getLayout :: TreeType -> TreeLayoutFn_
getLayout TidyTree = d3Tree_ unit
getLayout Dendrogram = d3Cluster_ unit

-- Key function for tree nodes - uses the hierarchical path as a unique ID
foreign import treeNodeKey_ :: Datum_ -> Index_

-- Key function for tree links - uses the target node's hierarchical path as a unique ID
foreign import treeLinkKey_ :: Datum_ -> Index_

-- Get the size of an array or D3 selection (for debugging)
foreign import selectionSize_ :: forall a. a -> Int

-- Debug helper to inspect container contents
foreign import debugInspectContainer_ :: forall d. D3Selection_ d -> String -> Int

-- Debug helper to log node coordinates
foreign import debugLogCoordinates_ :: forall a. a -> String -> Int

-- Radial transform functions
radialRotate :: Number -> String
radialRotate x = show $ (x * 180.0 / pi - 90.0)

radialRotateCommon :: forall d. d -> String
radialRotateCommon d = "rotate(" <> radialRotate (treeDatum_.x (unsafeCoerce d)) <> ")"

radialTranslate :: forall d. d -> String
radialTranslate d = "translate(" <> show (treeDatum_.y (unsafeCoerce d)) <> ",0)"

rotateRadialLabels :: forall d. d -> String
rotateRadialLabels d =
  "rotate(" <>
    (if treeDatum_.x (unsafeCoerce d) >= pi
    then "180"
    else "0")
    <> ")"

-- | Initialize an animated radial tree
-- | Returns the SVG, group selections, and hierarchy root for later updates
drawAnimatedRadialTree :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  TreeType ->
  TreeJson_ ->
  Selector D3Selection_ ->
  m { svg :: D3Selection_, rotationGroup :: D3Selection_, linksGroup :: D3Selection_, nodesGroup :: D3Selection_, root :: Datum_ }
drawAnimatedRadialTree treeType json selector = do
  -- Get window dimensions
  Tuple w h <- liftEffect getWindowWidthHeight

  -- Build D3 hierarchy from JSON (ONCE - we'll reuse this)
  let root = hierarchyFromJSON_ json

      -- Configure layout algorithm for radial
      layout = ((getLayout treeType) `treeSetSize_` [ 2.0 * pi, (w / 2.0) - 100.0 ])
                                     `treeSetSeparation_` radialSeparation
      laidOutRoot = layout `runLayoutFn_` root

      -- Calculate extents for viewBox
      { yMax } = treeMinMax_ laidOutRoot
      radialRadius = yMax
      radialExtent = 2.0 * radialRadius

      -- Colors
      linkColor = "#94a3b8"
      nodeColor = "#0ea5e9"
      textColor = "#0c4a6e"

  -- Build the SVG structure with zoom
  rootSel <- attach selector
  let zoomConfig :: ZoomableSVGConfig
      zoomConfig = {
        minX: -radialRadius * 1.2,
        minY: -radialRadius * 1.2,
        width: radialExtent * 1.2,
        height: radialExtent * 1.2,
        svgClass: "tree radial-tree animated-radial-tree",
        innerClass: "tree-container",
        innerWidth: w,
        innerHeight: h,
        scaleMin: 0.1,
        scaleMax: 4.0
      }
  { svg, zoomGroup } <- zoomableSVG rootSel zoomConfig

  -- Create rotation group (for label readability feature)
  rotationGroup <- appendTo zoomGroup Group [ classed "rotation-group" ]

  -- Create groups for links and nodes
  linksGroup <- appendTo rotationGroup Group [ classed "links", fontFamily "sans-serif", fontSize 10.0 ]
  nodesGroup <- appendTo rotationGroup Group [ classed "nodes", fontFamily "sans-serif", fontSize 10.0 ]

  -- Draw initial tree (no transitions on first render)
  theLinks <- simpleJoin linksGroup Path (links_ laidOutRoot) treeLinkKey_
  setAttributes theLinks
    [ strokeWidth 1.5
    , strokeColor linkColor
    , strokeOpacity 0.6
    , fill "none"
    , radialLink treeDatum_.x treeDatum_.y
    ]

  nodeGroups <- simpleJoin nodesGroup Group (descendants_ laidOutRoot) treeNodeKey_
  setAttributes nodeGroups
    [ transform [ radialRotateCommon, radialTranslate ] ]  -- No label rotation - all face same way

  -- Add circles to nodes
  _ <- appendTo nodeGroups Circle
    [ fill nodeColor
    , radius 3.0
    , strokeColor "white"
    , strokeWidth 1.5
    ]

  -- Add text labels to nodes (all positioned consistently for rotation feature)
  _ <- appendTo nodeGroups Text
    [ dy 0.31
    , x (\d -> if treeDatum_.hasChildren d then 8.0 else (-8.0))
    , textAnchor (\d -> if treeDatum_.hasChildren d then "start" else "end")
    , text treeDatum_.name
    , fill textColor
    , fontSize 11.0
    ]

  -- Return the hierarchy root (coerced to Datum_ for opaque storage)
  pure { svg, rotationGroup, linksGroup, nodesGroup, root: unsafeCoerce laidOutRoot }

-- | Update the tree to a new layout type, using smooth transitions
-- | Reuses the same hierarchy root object for proper D3 data join
updateToLayout :: forall m d1 d2.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  TreeType ->
  Datum_ ->       -- The hierarchy root to re-layout (NOT reconstructed from JSON)
  D3Selection_ d1 ->
  D3Selection_ d2 ->
  m Unit
updateToLayout newTreeType root linksGroup nodesGroup = do
  liftEffect $ log $ "=== UPDATE TO LAYOUT START - new type: " <> show newTreeType <> " ==="

  -- Get window dimensions
  Tuple w _h <- liftEffect getWindowWidthHeight

  -- Re-run layout algorithm on SAME hierarchy object with new layout type
  -- unsafeCoerce is needed to convert from opaque Datum_ storage type to D3_TreeNode
  let rootNode = unsafeCoerce root :: forall d. D3_TreeNode d

  liftEffect $ log "Calling layout algorithm..."
  let layout = ((getLayout newTreeType) `treeSetSize_` [ 2.0 * pi, (w / 2.0) - 100.0 ])
                                       `treeSetSeparation_` radialSeparation
      laidOutRoot = layout `runLayoutFn_` rootNode

      linksData = links_ laidOutRoot
      nodesData = descendants_ laidOutRoot

  liftEffect $ log $ "Links count: " <> show (selectionSize_ linksData)
  liftEffect $ log $ "Nodes count: " <> show (selectionSize_ nodesData)

  let node1 = unsafeCoerce $ unsafePartial $ unsafeIndex nodesData 1
      node2 = unsafeCoerce $ unsafePartial $ unsafeIndex nodesData 2
      node10 = unsafeCoerce $ unsafePartial $ unsafeIndex nodesData 10
  liftEffect $ log $ "Sample coords - node 1: x=" <> show (treeDatum_.x node1) <> ", y=" <> show (treeDatum_.y node1)
  liftEffect $ log $ "Sample coords - node 2: x=" <> show (treeDatum_.x node2) <> ", y=" <> show (treeDatum_.y node2)
  liftEffect $ log $ "Sample coords - node 10: x=" <> show (treeDatum_.x node10) <> ", y=" <> show (treeDatum_.y node10)

  let -- Colors
      linkColor = "#94a3b8"

      -- Define transition
      transition = transitionWithDuration $ Milliseconds 750.0

  liftEffect $ log "=== UPDATING LINKS ==="
  -- Use updateJoin to get enter/update/exit selections
  linksOpen <- openSelection linksGroup "path"
  linksJoin <- updateJoin linksOpen Path linksData treeLinkKey_
  liftEffect $ log $ "Links join - enter: " <> show (selectionSize_ (unsafeCoerce linksJoin.enter))
                    <> ", update: " <> show (selectionSize_ (unsafeCoerce linksJoin.update))
                    <> ", exit: " <> show (selectionSize_ (unsafeCoerce linksJoin.exit))

  -- Merge enter + update selections for transition
  theLinks <- mergeSelections linksJoin.enter linksJoin.update
  liftEffect $ log $ "Links merged selection size: " <> show (selectionSize_ (unsafeCoerce theLinks))

  -- Set static attributes
  setAttributes theLinks
    [ strokeWidth 1.5
    , strokeColor linkColor
    , strokeOpacity 0.6
    , fill "none"
    ]
  liftEffect $ log "Links static attributes set"

  -- Transition the path
  setAttributes theLinks $
    transition `to` [ radialLink treeDatum_.x treeDatum_.y ]
  liftEffect $ log "Links transition initiated"

  liftEffect $ log "=== UPDATING NODES ==="
  nodesOpen <- openSelection nodesGroup "g"

  nodesJoin <- updateJoin nodesOpen Group nodesData treeNodeKey_
  liftEffect $ log $ "Nodes join - enter: " <> show (selectionSize_ (unsafeCoerce nodesJoin.enter))
                    <> ", update: " <> show (selectionSize_ (unsafeCoerce nodesJoin.update))
                    <> ", exit: " <> show (selectionSize_ (unsafeCoerce nodesJoin.exit))

  -- Merge enter + update selections for transition
  nodeGroups <- mergeSelections nodesJoin.enter nodesJoin.update
  liftEffect $ log $ "Nodes merged selection size: " <> show (selectionSize_ (unsafeCoerce nodeGroups))

  -- Transition node positions (no label rotation for rotate feature)
  setAttributes nodeGroups $
    transition `to` [ transform [ radialRotateCommon, radialTranslate ] ]
  liftEffect $ log "Node groups transition initiated"

  liftEffect $ log "=== UPDATE TO LAYOUT END ==="
  pure unit

-- | Rotate the entire tree by a specified angle
-- | Useful for reading labels without craning your neck!
-- | Pass the current rotation and the angle to add to it
rotateTree :: forall m d.
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  Number ->          -- Current rotation angle in degrees
  Number ->          -- Degrees to rotate by (e.g., 90 for quarter turn)
  D3Selection_ d ->  -- The rotation group
  m Unit
rotateTree currentAngle deltaAngle rotationGroup = do
  let transition = transitionWithDuration $ Milliseconds 500.0
      newAngle = currentAngle + deltaAngle

      -- Rotation transform function (doesn't depend on data)
      rotateTransform :: Datum_ -> String
      rotateTransform _ = "rotate(" <> show newAngle <> ")"

  setAttributes rotationGroup $
    transition `to` [ transform [ rotateTransform ] ]

  pure unit
