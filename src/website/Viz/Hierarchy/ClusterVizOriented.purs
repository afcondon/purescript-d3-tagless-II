module D3.Viz.ClusterVizOriented where

import Prelude

import PSD3.Internal.Attributes.Sugar (classed, fill, fillOpacity, fontSize, strokeColor, strokeWidth, text, viewBox, cx, cy, radius, x, y, d)
import PSD3.Internal.Types (D3Selection_, Element(..), Selector)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach)
import PSD3.Layout.Hierarchy.Core (hierarchy)
import PSD3.Layout.Hierarchy.Core as Hierarchy
import PSD3.Layout.Hierarchy.Cluster (cluster, defaultClusterConfig, ClusterNode(..))
import PSD3.Layout.Hierarchy.Projection (clusterVerticalX, clusterVerticalY, verticalLinkPath, clusterHorizontalX, clusterHorizontalY, horizontalLinkPath, clusterRadialX, clusterRadialY, radialLinkPath, clusterIsometricX, clusterIsometricY, isometricLinkPath, isometricClusterLinkPath, isometricScale)
import PSD3.Layout.Hierarchy.Types (HierarchyNode(..), ValuedNode(..))
import Data.Int (toNumber)
import Data.Array (length)
import Data.Array as Array
import Data.Foldable (minimum, maximum)
import Data.Ord (max, abs)
import Data.Traversable (traverse_)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Data.Foldable (sum)
import Data.Either (Either(..))
import D3.Viz.FlareData (HierData, getName)
import D3.Viz.FlareData (getChildren, getValue) as FlareData

-- | Cluster orientation type
data ClusterOrientation
  = Vertical
  | Horizontal
  | Radial
  | Isometric

derive instance eqClusterOrientation :: Eq ClusterOrientation

instance showClusterOrientation :: Show ClusterOrientation where
  show Vertical = "Vertical"
  show Horizontal = "Horizontal"
  show Radial = "Radial"
  show Isometric = "Isometric"

-- | Type alias for coordinate accessor function
type CoordAccessor = ClusterNode HierData -> Number

-- | Type alias for link path generator
type LinkPathFn = Number -> Number -> Number -> Number -> String

-- | Get coordinate accessors and link path generator for orientation
getProjection :: ClusterOrientation ->
  { getX :: CoordAccessor
  , getY :: CoordAccessor
  , linkPath :: LinkPathFn
  , title :: String
  , scale :: Number
  }
getProjection orientation = case orientation of
  Vertical ->
    { getX: clusterVerticalX
    , getY: clusterVerticalY
    , linkPath: verticalLinkPath
    , title: "Cluster Layout - Vertical Dendrogram"
    , scale: 1.0
    }
  Horizontal ->
    { getX: clusterHorizontalX
    , getY: clusterHorizontalY
    , linkPath: horizontalLinkPath
    , title: "Cluster Layout - Horizontal Dendrogram"
    , scale: 1.0
    }
  Radial ->
    { getX: clusterRadialX
    , getY: clusterRadialY
    , linkPath: radialLinkPath
    , title: "Cluster Layout - Radial Dendrogram"
    , scale: 1.0
    }
  Isometric ->
    { getX: clusterIsometricX
    , getY: clusterIsometricY
    , linkPath: isometricLinkPath
    , title: "Cluster Layout - Isometric 2.5D Dendrogram"
    , scale: isometricScale
    }

-- | Convert ValuedNode back to HierarchyNode (discarding value information)
valuedToHierarchy :: forall a. ValuedNode a -> HierarchyNode a
valuedToHierarchy (VNode vn) =
  HNode
    { data_: vn.data_
    , depth: vn.depth
    , height: vn.height
    , parent: Nothing
    , children: map valuedToHierarchy vn.children
    }

-- | Sort ValuedNode by height (descending) then value (descending)
sortValuedByHeightAndValue :: forall a. ValuedNode a -> ValuedNode a
sortValuedByHeightAndValue (VNode vn) =
  if length vn.children == 0
  then VNode vn
  else
    let
      sortedGrandchildren = map sortValuedByHeightAndValue vn.children
      sortedChildren = Array.sortBy comparator sortedGrandchildren
    in
      VNode (vn { children = sortedChildren })
  where
    comparator (VNode a) (VNode b) =
      case compare b.height a.height of
        EQ -> compare b.value a.value
        other -> other

-- | Get all nodes (recursive traversal)
getAllNodes :: forall a. ClusterNode a -> Array (ClusterNode a)
getAllNodes node@(ClusterNode n) =
  if length n.children == 0
  then [node]
  else [node] <> (n.children >>= getAllNodes)

-- | Calculate bounds of projected coordinates
calculateBounds :: CoordAccessor -> CoordAccessor -> Array (ClusterNode HierData) ->
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
  ClusterOrientation -> HierData -> Selector (D3Selection_ Unit) -> m Unit
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

  -- Use sum (total value of descendants) for sorting - matches D3 pattern
  let valued = Hierarchy.sum root FlareData.getValue

  -- Sort by height then value (descending) to minimize crossings
  let sortedValued = sortValuedByHeightAndValue valued

  -- Convert back to HierarchyNode for cluster layout
  let sorted = valuedToHierarchy sortedValued

  -- Apply cluster layout with custom size
  let config = defaultClusterConfig
        { size = { width: chartWidth * proj.scale, height: chartHeight * proj.scale }
        }
  let layout = cluster config sorted

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

  -- Debug: log the cluster structure
  _ <- liftEffect $ log $ "\n╔══════════════════════════════════════════════════════════════╗"
  _ <- liftEffect $ log $ "║   CLUSTER VISUALIZATION: " <> show orientation <> " orientation"
  _ <- liftEffect $ log $ "╚══════════════════════════════════════════════════════════════╝"
  _ <- liftEffect $ log $ "Rendering " <> show (length nodes) <> " nodes"
  _ <- liftEffect $ log $ "Bounds: minX=" <> show bounds.minX <> ", maxX=" <> show bounds.maxX
                        <> ", minY=" <> show bounds.minY <> ", maxY=" <> show bounds.maxY
  _ <- liftEffect $ log $ "ViewBox: x=" <> show viewBoxSpec.x <> ", y=" <> show viewBoxSpec.y
                        <> ", width=" <> show viewBoxSpec.width <> ", height=" <> show viewBoxSpec.height

  root' <- attach selector :: m (D3Selection_ Unit)
  svg <- appendTo root' Svg
    [ viewBox viewBoxSpec.x viewBoxSpec.y viewBoxSpec.width viewBoxSpec.height
    , classed "cluster"
    , classed $ "cluster-" <> (show orientation)
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
  let renderLinks :: ClusterNode HierData -> m Unit
      renderLinks parent@(ClusterNode node) = do
        -- Render links to all children using projection accessors
        traverse_ (\child -> do
          let pathData = case orientation of
                Isometric -> isometricClusterLinkPath parent child
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
  let renderNode :: ClusterNode HierData -> m Unit
      renderNode clusterNode@(ClusterNode node) = do
        let nodeName = getName node.data_
        let isLeaf = length node.children == 0
        let nodeRadius = if isLeaf then 3.0 else 4.0

        -- Draw circle using projection accessors
        _ <- appendTo nodesGroup Circle
          [ cx $ getX clusterNode
          , cy $ getY clusterNode
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
              [ x $ getX clusterNode + 8.0
              , y $ getY clusterNode
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
