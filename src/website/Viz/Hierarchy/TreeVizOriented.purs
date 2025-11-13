module D3.Viz.TreeVizOriented where

import Prelude

import PSD3.Internal.Attributes.Sugar (classed, fill, fillOpacity, fontSize, strokeColor, strokeWidth, text, viewBox, cx, cy, radius, x, y, textAnchor, d)
import PSD3.Internal.Types (D3Selection_, Element(..), Selector)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach)
import PSD3.Layout.Hierarchy.Core (hierarchy)
import PSD3.Layout.Hierarchy.Tree (tree, defaultTreeConfig, TreeNode(..))
import PSD3.Layout.Hierarchy.Projection (verticalX, verticalY, verticalLinkPath, horizontalX, horizontalY, horizontalLinkPath, radialX, radialY, radialLinkPath, isometricX, isometricY, isometricLinkPath, isometricTreeLinkPath, isometricScale)
import D3.Viz.FlareData (HierData, getName, getValue)
import D3.Viz.FlareData (getChildren) as FlareData
import Data.Array (length)
import Data.Foldable (minimum, maximum)
import Data.Ord (max, abs)
import Data.Traversable (traverse_)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)

-- | Tree orientation type
data TreeOrientation
  = Vertical
  | Horizontal
  | Radial
  | Isometric

derive instance eqTreeOrientation :: Eq TreeOrientation

instance showTreeOrientation :: Show TreeOrientation where
  show Vertical = "Vertical"
  show Horizontal = "Horizontal"
  show Radial = "Radial"
  show Isometric = "Isometric"

-- | Type alias for coordinate accessor function
type CoordAccessor = TreeNode HierData -> Number

-- | Type alias for link path generator
type LinkPathFn = Number -> Number -> Number -> Number -> String

-- | Get coordinate accessors and link path generator for orientation
getProjection :: TreeOrientation ->
  { getX :: CoordAccessor
  , getY :: CoordAccessor
  , linkPath :: LinkPathFn
  , title :: String
  , scale :: Number
  }
getProjection orientation = case orientation of
  Vertical ->
    { getX: verticalX
    , getY: verticalY
    , linkPath: verticalLinkPath
    , title: "Tree Layout - Vertical (Reingold-Tilford)"
    , scale: 1.0
    }
  Horizontal ->
    { getX: horizontalX
    , getY: horizontalY
    , linkPath: horizontalLinkPath
    , title: "Tree Layout - Horizontal (Reingold-Tilford)"
    , scale: 1.0
    }
  Radial ->
    { getX: radialX
    , getY: radialY
    , linkPath: radialLinkPath
    , title: "Tree Layout - Radial (Reingold-Tilford)"
    , scale: 1.0
    }
  Isometric ->
    { getX: isometricX
    , getY: isometricY
    , linkPath: isometricLinkPath
    , title: "Tree Layout - Isometric 2.5D (Reingold-Tilford)"
    , scale: isometricScale
    }

-- | Get all nodes (recursive traversal)
getAllNodes :: forall a. TreeNode a -> Array (TreeNode a)
getAllNodes node@(TreeNode n) =
  if length n.children == 0
  then [node]
  else [node] <> (n.children >>= getAllNodes)

-- | Calculate bounds of projected coordinates
calculateBounds :: CoordAccessor -> CoordAccessor -> Array (TreeNode HierData) ->
  { minX :: Number, maxX :: Number, minY :: Number, maxY :: Number }
calculateBounds getX getY nodes =
  let
    xCoords = map getX nodes
    yCoords = map getY nodes
    minX = fromMaybe 0.0 $ minimum xCoords
    maxX = fromMaybe 0.0 $ maximum xCoords
    minY = fromMaybe 0.0 $ minimum yCoords
    maxY = fromMaybe 0.0 $ maximum yCoords
  in
    { minX, maxX, minY, maxY }

-- | Main drawing function with orientation parameter
draw :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  TreeOrientation -> HierData -> Selector (D3Selection_ Unit) -> m Unit
draw orientation flareData selector = do
  let chartWidth = 1600.0
  let chartHeight = 1200.0

  -- Get projection functions for this orientation
  let proj = getProjection orientation
  let getX = proj.getX
  let getY = proj.getY
  let linkPath = proj.linkPath

  -- Create hierarchy from blessed Flare data
  let root = hierarchy flareData FlareData.getChildren

  -- Apply tree layout with custom size
  let config = defaultTreeConfig
        { size = { width: chartWidth * proj.scale, height: chartHeight * proj.scale }
        }
  let layout = tree config root

  -- Get all nodes for visualization
  let nodes = getAllNodes layout

  -- Calculate bounds of projected coordinates
  let bounds = calculateBounds getX getY nodes

  -- Determine viewBox based on orientation
  -- For isometric and radial, we need to accommodate potentially negative coordinates
  let padding = 50.0  -- Add padding around content
  let viewBoxSpec = case orientation of
        Isometric ->
          -- Calculate width/height from bounds and add padding
          let width = bounds.maxX - bounds.minX + (2.0 * padding)
              height = bounds.maxY - bounds.minY + (2.0 * padding)
          in { x: bounds.minX - padding, y: bounds.minY - padding, width, height }
        Radial ->
          -- Radial should be centered, find max radius
          let maxRadius = max (abs bounds.minX) (max (abs bounds.maxX) (max (abs bounds.minY) (abs bounds.maxY)))
              side = (maxRadius + padding) * 2.0
          in { x: -(maxRadius + padding), y: -(maxRadius + padding), width: side, height: side }
        _ ->
          -- Vertical and horizontal use standard 0-based viewBox
          { x: 0.0, y: 0.0, width: chartWidth, height: chartHeight }

  -- Debug: log the tree structure
  _ <- liftEffect $ log $ "\n╔══════════════════════════════════════════════════════════════╗"
  _ <- liftEffect $ log $ "║   TREE VISUALIZATION: " <> show orientation <> " orientation"
  _ <- liftEffect $ log $ "╚══════════════════════════════════════════════════════════════╝"
  _ <- liftEffect $ log $ "Rendering " <> show (length nodes) <> " nodes"
  _ <- liftEffect $ log $ "Bounds: minX=" <> show bounds.minX <> ", maxX=" <> show bounds.maxX
                        <> ", minY=" <> show bounds.minY <> ", maxY=" <> show bounds.maxY
  _ <- liftEffect $ log $ "ViewBox: x=" <> show viewBoxSpec.x <> ", y=" <> show viewBoxSpec.y
                        <> ", width=" <> show viewBoxSpec.width <> ", height=" <> show viewBoxSpec.height

  root' <- attach selector :: m (D3Selection_ Unit)
  svg <- appendTo root' Svg
    [ viewBox viewBoxSpec.x viewBoxSpec.y viewBoxSpec.width viewBoxSpec.height
    , classed "tree"
    , classed $ "tree-" <> (show orientation)
    ]

  -- Add title (positioned relative to viewBox origin)
  _ <- appendTo svg Text
    [ x $ viewBoxSpec.x + 10.0
    , y $ viewBoxSpec.y + 20.0
    , fill "#333"
    , fontSize 16.0
    , text proj.title
    , classed "title"
    ]

  -- Create group for links (so they appear behind nodes)
  linksGroup <- appendTo svg Group [ classed "links" ]

  -- Create group for nodes (so they appear in front)
  nodesGroup <- appendTo svg Group [ classed "nodes" ]

  -- Render links first
  let renderLinks :: TreeNode HierData -> m Unit
      renderLinks parent@(TreeNode node) = do
        -- Render links to all children
        traverse_ (\child -> do
          let pathData = case orientation of
                Isometric -> isometricTreeLinkPath parent child
                _ -> linkPath (getX parent) (getY parent) (getX child) (getY child)
          _ <- appendTo linksGroup Path
            [ d pathData
            , fill "none"
            , strokeColor "#555"
            , fillOpacity 0.4
            , strokeWidth 1.5
            , classed "link"
            ]
          pure unit
        ) node.children

        -- Recursively render children's links
        traverse_ renderLinks node.children

  -- Render nodes (circles and labels)
  let renderNode :: TreeNode HierData -> m Unit
      renderNode treeNode@(TreeNode node) = do
        let nodeName = getName node.data_
        let isLeaf = length node.children == 0
        let nodeRadius = if isLeaf then 3.0 else 4.0

        -- Draw circle using projection accessors
        _ <- appendTo nodesGroup Circle
          [ cx $ getX treeNode
          , cy $ getY treeNode
          , radius nodeRadius
          , fill "#999"
          , strokeColor "#555"
          , strokeWidth 1.5
          , classed "node"
          , classed $ "depth-" <> show node.depth
          , classed $ if isLeaf then "leaf" else "internal"
          ]

        -- Add label for internal nodes (non-leaf)
        -- Only for vertical/horizontal (radial/isometric would be too cluttered)
        if not isLeaf && (orientation == Vertical || orientation == Horizontal)
          then do
            _ <- appendTo nodesGroup Text
              [ x $ getX treeNode + 8.0
              , y $ getY treeNode
              , fill "#333"
              , fontSize 10.0
              , text nodeName
              , classed "label"
              ]
            pure unit
          else pure unit

        -- Recursively render children
        traverse_ renderNode node.children

  -- Render all links
  _ <- renderLinks layout

  -- Render all nodes
  _ <- renderNode layout

  pure unit
