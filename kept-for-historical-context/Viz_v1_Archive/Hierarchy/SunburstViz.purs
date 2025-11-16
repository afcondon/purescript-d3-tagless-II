module D3.Viz.SunburstViz where

import Prelude

import PSD3.Internal.Attributes.Sugar (classed, fill, fillOpacity, fontSize, strokeColor, strokeWidth, text, viewBox, x, y, textAnchor, transform)
import PSD3.Internal.Attributes.Sugar as Sugar
import PSD3.Internal.Types (D3Selection_, Element(..), Selector)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach)
import PSD3.Layout.Hierarchy.Partition (partition, defaultPartitionConfig, PartitionNode(..), hierarchy, HierarchyData(..))
import Data.Array (length, (!!))
import Data.Array as Array
import Data.Foldable (sum)
import Data.Traversable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import D3.Viz.FlareData (HierData, getName, getValue)
import D3.Viz.FlareData (getChildren) as FlareData
import Data.Number (pi, sin, cos)

-- Simple color palette for depth-based coloring
colors :: Array String
colors =
  [ "#e7ba52", "#c7c7c7", "#aec7e8", "#1f77b4", "#9467bd"
  , "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"
  ]

-- Get color by depth
getColor :: Int -> String
getColor depth =
  let idx = depth `mod` (length colors)
  in case colors !! idx of
       Just c -> c
       Nothing -> "#cccccc"

-- Get all nodes (recursive traversal)
getAllNodes :: forall a. PartitionNode a -> Array (PartitionNode a)
getAllNodes node@(PartNode n) =
  if length n.children == 0
  then [node]
  else [node] <> (n.children >>= getAllNodes)

-- Count total nodes
countNodes :: forall a. PartitionNode a -> Int
countNodes (PartNode node) = 1 + sum (map countNodes node.children)

-- Print sunburst structure
printSunburstStructure :: Int -> Int -> String -> PartitionNode HierData -> Effect Unit
printSunburstStructure currentDepth maxDepth indent (PartNode node) = do
  if currentDepth <= maxDepth then do
    let nodeType = if length node.children == 0 then "leaf" else "internal(" <> show (length node.children) <> " children)"
    let nodeName = getName node.data_
    log $ indent <> nodeName <> " [" <> nodeType <> "]: x0=" <> show node.x0 <> ", y0=" <> show node.y0 <> ", x1=" <> show node.x1 <> ", y1=" <> show node.y1 <> ", depth=" <> show node.depth
    if currentDepth < maxDepth then
      traverse_ (printSunburstStructure (currentDepth + 1) maxDepth (indent <> "  ")) node.children
    else
      pure unit
  else
    pure unit

-- Convert FlareData to HierarchyData for Sunburst layout
toHierarchyData :: HierData -> HierarchyData HierData
toHierarchyData node = HierarchyData
  { data_: node
  , value:
      -- Only set explicit value for leaf nodes
      -- Internal nodes will get their value from sum of children
      case FlareData.getChildren node of
        Nothing -> Just (getValue node)  -- Leaf node
        Just _ -> Nothing                 -- Internal node - let it sum children
  , children: map (map toHierarchyData) (FlareData.getChildren node)
  }

-- Convert partition coordinates to arc path for sunburst
-- x0, x1 represent angles (in range 0 to width, which we map to 0 to 2π)
-- y0, y1 represent radii (in range 0 to height)
makeArcPath :: Number -> Number -> Number -> Number -> Number -> Number -> String
makeArcPath chartWidth chartHeight x0 y0 x1 y1 =
  let
    -- Convert x coordinates to angles (0 to 2π)
    startAngle = (x0 / chartWidth) * 2.0 * pi
    endAngle = (x1 / chartWidth) * 2.0 * pi

    -- Convert y coordinates to radii
    innerRadius = y0
    outerRadius = y1

    -- Calculate arc points
    -- Start at inner radius, start angle
    x0' = innerRadius * cos startAngle
    y0' = innerRadius * sin startAngle

    -- End of inner arc (inner radius, end angle)
    x1' = innerRadius * cos endAngle
    y1' = innerRadius * sin endAngle

    -- Start of outer arc (outer radius, start angle)
    x2' = outerRadius * cos startAngle
    y2' = outerRadius * sin startAngle

    -- End of outer arc (outer radius, end angle)
    x3' = outerRadius * cos endAngle
    y3' = outerRadius * sin endAngle

    -- Determine if we need large arc flag (arc > 180 degrees)
    largeArcFlag = if (endAngle - startAngle) > pi then 1 else 0

    -- SVG arc path: Move to inner start, arc to inner end, line to outer end, arc back to outer start, close
    path = "M" <> show x0' <> "," <> show y0'
         <> " A" <> show innerRadius <> "," <> show innerRadius <> " 0 " <> show largeArcFlag <> ",1 " <> show x1' <> "," <> show y1'
         <> " L" <> show x3' <> "," <> show y3'
         <> " A" <> show outerRadius <> "," <> show outerRadius <> " 0 " <> show largeArcFlag <> ",0 " <> show x2' <> "," <> show y2'
         <> " Z"
  in
    path

-- Calculate label position (middle of arc)
type LabelPos = { x :: Number, y :: Number, angle :: Number }

getLabelPosition :: Number -> Number -> Number -> Number -> Number -> Number -> LabelPos
getLabelPosition chartWidth chartHeight x0 y0 x1 y1 =
  let
    -- Convert to angles and radii
    startAngle = (x0 / chartWidth) * 2.0 * pi
    endAngle = (x1 / chartWidth) * 2.0 * pi
    innerRadius = y0
    outerRadius = y1

    -- Middle angle and radius
    midAngle = (startAngle + endAngle) / 2.0
    midRadius = (innerRadius + outerRadius) / 2.0

    -- Position
    x = midRadius * cos midAngle
    y = midRadius * sin midAngle

    -- Convert angle to degrees for text rotation
    angleDeg = midAngle * 180.0 / pi
  in
    { x, y, angle: angleDeg }

-- Main drawing function for sunburst layout
draw :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  HierData -> Selector (D3Selection_ Unit) -> m Unit
draw flareData selector = do
  let chartWidth = 1600.0
  let chartHeight = 1200.0
  let radius = 500.0  -- Max radius for the sunburst

  -- Log the root data to verify data was loaded
  _ <- liftEffect $ log $ "Loaded JSON - root name: " <> getName flareData

  -- Convert FlareData to HierarchyData and create hierarchy
  let hierData = toHierarchyData flareData
  let root = hierarchy hierData

  -- Apply partition layout - use 2*pi for width (full circle) and radius for height
  let config = defaultPartitionConfig
        { size = { width: 2.0 * pi, height: radius }
        }
  let layout = partition config root

  -- Get all nodes for visualization
  let nodes = getAllNodes layout

  -- Debug: log the sunburst structure
  _ <- liftEffect $ log "\n╔═══════════════════════════════════════════════════════════════╗"
  _ <- liftEffect $ log "║   SUNBURST VISUALIZATION: FULL FLARE HIERARCHY (RADIAL)      ║"
  _ <- liftEffect $ log "╚═══════════════════════════════════════════════════════════════╝\n"
  _ <- liftEffect $ printSunburstStructure 0 2 "" layout
  _ <- liftEffect $ log $ "\nRendering " <> show (length nodes) <> " nodes (full hierarchy)"
  _ <- liftEffect $ log $ "Total node count: " <> show (countNodes layout)

  root' <- attach selector :: m (D3Selection_ Unit)
  svg <- appendTo root' Svg
    [ viewBox 0.0 0.0 chartWidth chartHeight
    , classed "sunburst"
    ]

  -- Add title
  _ <- appendTo svg Text
    [ x 10.0
    , y 20.0
    , fill "#333"
    , fontSize 16.0
    , text "Sunburst Layout (Radial Partition)"
    , classed "title"
    ]

  -- Create group for sunburst, centered in the view
  sunburstGroup <- appendTo svg Group
    [ classed "sunburst-arcs"
    , transform [ \_ -> "translate(" <> show (chartWidth / 2.0) <> "," <> show (chartHeight / 2.0) <> ")" ]
    ]

  -- Render arcs for all nodes (except root which would be invisible at center)
  let renderNode :: PartitionNode HierData -> m Unit
      renderNode (PartNode node) = do
        let nodeName = getName node.data_

        -- Skip root (depth 0) as it would be a point at the center
        if node.depth > 0
          then do
            -- Calculate arc angle span
            let angleSpan = node.x1 - node.x0
            let radiusSpan = node.y1 - node.y0

            -- Only render if arc has size
            if angleSpan > 0.001 && radiusSpan > 0.5
              then do
                -- Draw arc
                _ <- appendTo sunburstGroup Path
                  [ Sugar.d $ makeArcPath (2.0 * pi) radius node.x0 node.y0 node.x1 node.y1
                  , fill $ getColor node.depth
                  , strokeColor "#fff"
                  , strokeWidth 1.0
                  , fillOpacity 0.7
                  , classed "arc"
                  , classed $ "depth-" <> show node.depth
                  ]

                -- Add label if arc is large enough
                -- Check both angle span (> 0.1 radians) and radius span (> 30)
                if angleSpan > 0.1 && radiusSpan > 30.0
                  then do
                    let labelPos = getLabelPosition (2.0 * pi) radius node.x0 node.y0 node.x1 node.y1

                    -- Adjust text anchor and rotation for readability
                    let textRotation =
                          if labelPos.angle > 90.0 && labelPos.angle < 270.0
                          then labelPos.angle + 180.0  -- Flip text on left side
                          else labelPos.angle

                    let anchor =
                          if labelPos.angle > 90.0 && labelPos.angle < 270.0
                          then "end"  -- Right-align flipped text
                          else "start"  -- Left-align normal text

                    _ <- appendTo sunburstGroup Text
                      [ x labelPos.x
                      , y labelPos.y
                      , transform [ \_ -> "rotate(" <> show textRotation <> " " <> show labelPos.x <> " " <> show labelPos.y <> ")" ]
                      , fill "#333"
                      , fontSize 10.0
                      , text nodeName
                      , textAnchor anchor
                      , classed "label"
                      ]
                    pure unit
                  else pure unit

                pure unit
              else pure unit
          else pure unit

        -- Recursively render children
        traverse_ renderNode node.children

  -- Render all nodes
  _ <- renderNode layout

  pure unit
