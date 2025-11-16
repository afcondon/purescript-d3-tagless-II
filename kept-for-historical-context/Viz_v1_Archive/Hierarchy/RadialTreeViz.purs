-- | Radial tree visualization using Tree4 with polar projection
module D3.Viz.RadialTreeViz where

import Prelude

import Data.Tree (Tree(..))
import Data.List (List(..), fromFoldable)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Number (pi, cos, sin, atan2, sqrt)
import PSD3.Internal.Attributes.Sugar (classed, fill, strokeColor, strokeWidth, viewBox, cx, cy, radius, d, x, y, text, fontSize)
import PSD3.Internal.Types (D3Selection_, Element(..), Selector, Index_)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach, simpleJoin, setAttributes)
import PSD3.Internal.FFI (keyIsID_)
import PSD3.Layout.Hierarchy.Tree4 (tree, defaultTreeConfig)
import D3.Viz.FlareData (HierData, getName, getValue, getChildren)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)

-- | Convert HierData to Data.Tree
hierDataToTree :: HierData -> Tree { name :: String, value :: Number, x :: Number, y :: Number, depth :: Int }
hierDataToTree hierData =
  let
    name = getName hierData
    value = getValue hierData
    childrenMaybe = getChildren hierData
    childrenList = case childrenMaybe of
      Nothing -> Nil
      Just childrenArray -> fromFoldable $ map hierDataToTree childrenArray
  in
    Node { name, value, x: 0.0, y: 0.0, depth: 0 } childrenList

-- | Radial projection: convert (x, y) to polar coordinates
-- | x is mapped to angle, y (depth) is mapped to radius
radialPoint :: forall r. { x :: Number, y :: Number | r } -> Number -> Number -> { x :: Number, y :: Number }
radialPoint node width height =
  let
    -- Map x to angle (0 to 2π)
    angle = (node.x / width) * 2.0 * pi - (pi / 2.0)  -- Start at top (-π/2)
    -- Map y (depth) to radius
    minDim = if width < height then width else height
    radius = (node.y / height) * (minDim / 2.0) * 0.85  -- Scale to 85% of radius
  in
    { x: radius * cos angle
    , y: radius * sin angle
    }

-- | Radial link path generator
-- | Creates cubic Bezier curves that follow the radial structure
-- | Control points are positioned to go radially outward, then curve around
radialLinkPath :: Number -> Number -> Number -> Number -> Number -> Number -> String
radialLinkPath x1 y1 x2 y2 centerX centerY =
  let
    -- Convert cartesian back to polar to calculate proper radial control points
    -- atan2 gives angle, sqrt(x^2 + y^2) gives radius
    angle1 = atan2 y1 x1
    radius1 = sqrt (x1 * x1 + y1 * y1)
    angle2 = atan2 y2 x2
    radius2 = sqrt (x2 * x2 + y2 * y2)

    -- Control points in polar coordinates:
    -- CP1: parent's angle, halfway to child's radius
    -- CP2: child's angle, halfway to child's radius
    midRadius = (radius1 + radius2) / 2.0

    -- Convert control points back to cartesian
    cp1x = midRadius * cos angle1
    cp1y = midRadius * sin angle1
    cp2x = midRadius * cos angle2
    cp2y = midRadius * sin angle2
  in
    "M" <> show (x1 + centerX) <> "," <> show (y1 + centerY) <>
    "C" <> show (cp1x + centerX) <> "," <> show (cp1y + centerY) <>
    " " <> show (cp2x + centerX) <> "," <> show (cp2y + centerY) <>
    " " <> show (x2 + centerX) <> "," <> show (y2 + centerY)

-- | Create links from parent to children
makeLinksAsList :: forall r. Tree { x :: Number, y :: Number | r } -> List { source :: { x :: Number, y :: Number }, target :: { x :: Number, y :: Number } }
makeLinksAsList (Node val children) =
  let
    childLinks = children >>= \(Node childVal _) ->
      Cons { source: { x: val.x, y: val.y }, target: { x: childVal.x, y: childVal.y } } Nil
    grandchildLinks = children >>= makeLinksAsList
  in
    childLinks <> grandchildLinks

makeLinks :: forall r. Tree { x :: Number, y :: Number | r } -> Array { source :: { x :: Number, y :: Number }, target :: { x :: Number, y :: Number } }
makeLinks = Array.fromFoldable <<< makeLinksAsList

-- | Draw radial tree
draw :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  HierData -> Selector (D3Selection_ Unit) -> m Unit
draw flareData selector = do
  let chartWidth = 1600.0
  let chartHeight = 1600.0  -- Square for radial
  let centerX = chartWidth / 2.0
  let centerY = chartHeight / 2.0

  liftEffect $ log "RadialTreeViz: Using Tree4 with radial projection"

  -- Convert to Data.Tree
  let dataTree = hierDataToTree flareData

  -- Apply Tree4 layout
  let config = defaultTreeConfig { size = { width: chartWidth, height: chartHeight } }
  let positioned = tree config dataTree

  -- Apply radial projection to all nodes
  let projectNode :: forall r. Tree { x :: Number, y :: Number | r } -> Tree { x :: Number, y :: Number | r }
      projectNode (Node val children) =
        let
          projected = radialPoint val chartWidth chartHeight
          projectedChildren = map projectNode children
        in
          Node (val { x = projected.x, y = projected.y }) projectedChildren

  let radialTree = projectNode positioned

  -- Flatten for rendering
  let nodes = Array.fromFoldable radialTree
  let links = makeLinks radialTree

  liftEffect $ log $ "RadialTreeViz: Rendering " <> show (Array.length nodes) <> " nodes, " <> show (Array.length links) <> " links"

  -- Create SVG
  root' <- attach selector :: m (D3Selection_ Unit)
  svg <- appendTo root' Svg
    [ viewBox 0.0 0.0 chartWidth chartHeight
    , classed "radial-tree-viz"
    ]

  -- Add title
  _ <- appendTo svg Text
    [ x 10.0
    , y 20.0
    , fill "#333"
    , fontSize 16.0
    , text "Radial Tree Layout (Reingold-Tilford)"
    , classed "title"
    ]

  -- Create groups
  linksGroup <- appendTo svg Group [ classed "links" ]
  nodesGroup <- appendTo svg Group [ classed "nodes" ]

  -- Draw links using data join
  linkElements <- simpleJoin linksGroup Path links keyIsID_
  setAttributes linkElements
    [ d (\(link :: { source :: { x :: Number, y :: Number }, target :: { x :: Number, y :: Number } }) (_ :: Index_) ->
          radialLinkPath link.source.x link.source.y link.target.x link.target.y centerX centerY)
    , fill "none"
    , strokeColor "#555"
    , strokeWidth 1.5
    , classed "link"
    ]

  -- Draw nodes using data join
  nodeElements <- simpleJoin nodesGroup Circle nodes keyIsID_
  setAttributes nodeElements
    [ cx (\(node :: { name :: String, value :: Number, x :: Number, y :: Number, depth :: Int }) (_ :: Index_) -> node.x + centerX)
    , cy (\(node :: { name :: String, value :: Number, x :: Number, y :: Number, depth :: Int }) (_ :: Index_) -> node.y + centerY)
    , radius 3.0
    , fill "#999"
    , strokeColor "#555"
    , strokeWidth 1.5
    , classed "node"
    ]

  liftEffect $ log "RadialTreeViz: Rendering complete!"

  pure unit
