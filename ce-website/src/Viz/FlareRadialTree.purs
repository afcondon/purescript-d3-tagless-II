-- | Flare Radial Tree - Test module
-- |
-- | Direct copy of RadialTreeViz from demo-website to validate
-- | radial tree rendering works in ce-website.
module Viz.FlareRadialTree
  ( renderFlareRadialTree
  , loadAndRenderFlareTree
  , loadAndRenderDependencyTree
  , loadAndRenderDependencyTreeVertical
  ) where

import Prelude

import Affjax.Web as AJAX
import Affjax.ResponseFormat as ResponseFormat
import Data.Array as Array
import Data.Either (Either(..))
import Data.List (List(..), fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Data.Number (pi, cos, sin, atan2, sqrt)
import Data.Tree (Tree(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Data.Loader as Loader
import PSD3.Layout.Hierarchy.Tree4 (tree, defaultTreeConfig)
import PSD3v2.Attribute.Types (viewBox, class_, cx, cy, radius, fill, stroke, strokeWidth, d, transform)
import PSD3v2.Capabilities.Selection (select, appendChild, appendData)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2Selection_)
import PSD3v2.Selection.Types (ElementType(..), SEmpty)
import Web.DOM.Element (Element)

-- =============================================================================
-- FFI for loading Flare JSON
-- =============================================================================

foreign import data HierData :: Type
foreign import parseFlareJson :: String -> HierData
foreign import getName :: HierData -> String
foreign import getValue :: HierData -> Number
foreign import getChildren_ :: HierData -> Nullable (Array HierData)

getChildren :: HierData -> Maybe (Array HierData)
getChildren = toMaybe <<< getChildren_

-- | Hierarchy node type
type HierNode = { name :: String, value :: Number, x :: Number, y :: Number, depth :: Int, height :: Int }

-- | Convert HierData to Data.Tree
hierDataToTree :: HierData -> Tree HierNode
hierDataToTree hierData =
  let
    name = getName hierData
    value = getValue hierData
    childrenMaybe = getChildren hierData
    childrenList = case childrenMaybe of
      Nothing -> Nil
      Just childrenArray -> fromFoldable $ map hierDataToTree childrenArray
  in
    Node { name, value, x: 0.0, y: 0.0, depth: 0, height: 0 } childrenList

-- | Load Flare JSON
loadFlareData :: Aff (Either String (Tree HierNode))
loadFlareData = do
  log "[FlareRadialTree] Loading flare-2.json..."
  response <- AJAX.get ResponseFormat.string "./data/flare-2.json"
  case response of
    Left err -> pure $ Left $ "Failed to load: " <> AJAX.printError err
    Right { body } -> do
      let hierData = parseFlareJson body
      let tree' = hierDataToTree hierData
      pure $ Right tree'

-- =============================================================================
-- Radial projection (copied from RadialTreeViz)
-- =============================================================================

-- | Radial projection: convert (x, y) to polar coordinates
radialPoint :: forall r. { x :: Number, y :: Number | r } -> Number -> Number -> { x :: Number, y :: Number }
radialPoint node w h =
  let
    -- Map x to angle (0 to 2π)
    angle = (node.x / w) * 2.0 * pi - (pi / 2.0)  -- Start at top (-π/2)
    -- Map y (depth) to radius
    minDim = if w < h then w else h
    rad = (node.y / h) * (minDim / 2.0) * 0.85  -- Scale to 85% of radius
  in
    { x: rad * cos angle
    , y: rad * sin angle
    }

-- | Apply radial projection to a tree
projectRadial :: forall r. Number -> Number -> Tree { x :: Number, y :: Number | r } -> Tree { x :: Number, y :: Number | r }
projectRadial w h (Node val children) =
  let
    projected = radialPoint val w h
    projectedChildren = map (projectRadial w h) children
  in
    Node (val { x = projected.x, y = projected.y }) projectedChildren

-- | Radial link path generator
radialLinkPath :: Number -> Number -> Number -> Number -> String
radialLinkPath x1 y1 x2 y2 =
  let
    angle1 = atan2 y1 x1
    radius1 = sqrt (x1 * x1 + y1 * y1)
    angle2 = atan2 y2 x2
    radius2 = sqrt (x2 * x2 + y2 * y2)
    midRadius = (radius1 + radius2) / 2.0
    cp1x = midRadius * cos angle1
    cp1y = midRadius * sin angle1
    cp2x = midRadius * cos angle2
    cp2y = midRadius * sin angle2
  in
    "M" <> show x1 <> "," <> show y1 <>
    "C" <> show cp1x <> "," <> show cp1y <>
    " " <> show cp2x <> "," <> show cp2y <>
    " " <> show x2 <> "," <> show y2

-- | Link data type
type LinkDatum = { source :: { x :: Number, y :: Number }, target :: { x :: Number, y :: Number } }

-- | Generate path for a link
linkPathFn :: LinkDatum -> String
linkPathFn link = radialLinkPath link.source.x link.source.y link.target.x link.target.y

-- | Create links from tree
makeLinks :: forall r. Tree { x :: Number, y :: Number | r } -> Array LinkDatum
makeLinks tree' = Array.fromFoldable $ makeLinksList tree'
  where
  makeLinksList :: Tree { x :: Number, y :: Number | r } -> List LinkDatum
  makeLinksList (Node val children) =
    let
      childLinks = children >>= \(Node childVal _) ->
        pure { source: { x: val.x, y: val.y }, target: { x: childVal.x, y: childVal.y } }
      grandchildLinks = children >>= makeLinksList
    in
      childLinks <> grandchildLinks

-- =============================================================================
-- Rendering
-- =============================================================================

-- | Render a radial tree from a Tree structure
renderFlareRadialTree :: String -> Tree HierNode -> Effect Unit
renderFlareRadialTree selector flareTree = runD3v2M do
  container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)

  let chartSize = 600.0
  let centerX = chartSize / 2.0
  let centerY = chartSize / 2.0

  -- Apply Tree4 layout with rectangular coordinates
  let config = defaultTreeConfig
        { size = { width: chartSize, height: chartSize } }
  let positioned = tree config flareTree

  -- Apply radial projection to all nodes
  let radialTree = projectRadial chartSize chartSize positioned

  -- Flatten to arrays
  let nodes = Array.fromFoldable radialTree
  let links = makeLinks radialTree

  liftEffect $ log $ "[FlareRadialTree] Rendering: " <> show (Array.length nodes) <> " nodes, " <> show (Array.length links) <> " links"

  -- Create SVG
  svg <- appendChild SVG
    [ viewBox ("0 0 " <> show chartSize <> " " <> show chartSize)
    , class_ "flare-radial-tree"
    ]
    container

  -- Create centered group
  chartGroup <- appendChild Group
    [ transform ("translate(" <> show centerX <> "," <> show centerY <> ")")
    , class_ "chart-group"
    ]
    svg

  -- Render links
  linksGroup <- appendChild Group [ class_ "links" ] chartGroup
  _ <- appendData Path links
    [ d linkPathFn
    , fill (\(_ :: LinkDatum) -> "none")
    , stroke (\(_ :: LinkDatum) -> "#999")
    , strokeWidth (\(_ :: LinkDatum) -> 1.0)
    ]
    linksGroup

  -- Render nodes
  nodesGroup <- appendChild Group [ class_ "nodes" ] chartGroup
  _ <- appendData Circle nodes
    [ cx (_.x :: HierNode -> Number)
    , cy (_.y :: HierNode -> Number)
    , radius (\(_ :: HierNode) -> 3.0)
    , fill (\(_ :: HierNode) -> "#69b3a2")
    , stroke (\(_ :: HierNode) -> "#fff")
    , strokeWidth (\(_ :: HierNode) -> 1.5)
    ]
    nodesGroup

  liftEffect $ log "[FlareRadialTree] Render complete"

-- | Load and render the Flare radial tree
loadAndRenderFlareTree :: String -> Effect Unit
loadAndRenderFlareTree selector = launchAff_ do
  result <- loadFlareData
  case result of
    Left err -> liftEffect $ log $ "[FlareRadialTree] Error: " <> err
    Right flareTree -> liftEffect $ renderFlareRadialTree selector flareTree

-- | Load and render the dependency tree using same radial rendering
loadAndRenderDependencyTree :: String -> Effect Unit
loadAndRenderDependencyTree selector = launchAff_ do
  log "[FlareRadialTree] Loading dependency tree..."
  result <- Loader.getDependencyTree
  case result of
    Left err -> liftEffect $ log $ "[FlareRadialTree] Dependency tree error: " <> err
    Right depTree -> liftEffect $ renderFlareRadialTree selector depTree

-- | Render a VERTICAL tidy tree (no radial projection) for debugging
renderVerticalTree :: String -> Tree HierNode -> Effect Unit
renderVerticalTree selector inputTree = runD3v2M do
  container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)

  let chartWidth = 2000.0  -- Wide to accommodate many nodes
  let chartHeight = 800.0

  -- Apply Tree4 layout (vertical: x is horizontal spread, y is depth)
  let config = defaultTreeConfig
        { size = { width: chartWidth, height: chartHeight } }
  let positioned = tree config inputTree

  -- Flatten to arrays (NO radial projection)
  let nodes = Array.fromFoldable positioned
  let links = makeLinks positioned

  liftEffect $ log $ "[FlareRadialTree] Vertical tree: " <> show (Array.length nodes) <> " nodes, " <> show (Array.length links) <> " links"

  -- Create SVG with scrolling viewbox
  svg <- appendChild SVG
    [ viewBox ("0 0 " <> show chartWidth <> " " <> show chartHeight)
    , class_ "vertical-tree"
    ]
    container

  -- Render links as simple lines
  linksGroup <- appendChild Group [ class_ "links" ] svg
  _ <- appendData Path links
    [ d verticalLinkPathFn
    , fill (const "none" :: LinkDatum -> String)
    , stroke (const "#999" :: LinkDatum -> String)
    , strokeWidth (const 1.0 :: LinkDatum -> Number)
    ]
    linksGroup

  -- Render nodes
  nodesGroup <- appendChild Group [ class_ "nodes" ] svg
  _ <- appendData Circle nodes
    [ cx (_.x :: HierNode -> Number)
    , cy (_.y :: HierNode -> Number)
    , radius (const 3.0 :: HierNode -> Number)
    , fill (const "#69b3a2" :: HierNode -> String)
    , stroke (const "#fff" :: HierNode -> String)
    , strokeWidth (const 1.5 :: HierNode -> Number)
    ]
    nodesGroup

  liftEffect $ log "[FlareRadialTree] Vertical tree render complete"

-- | Simple vertical link path (straight lines for clarity)
verticalLinkPathFn :: LinkDatum -> String
verticalLinkPathFn link =
  "M" <> show link.source.x <> "," <> show link.source.y <>
  "L" <> show link.target.x <> "," <> show link.target.y

-- | Load and render dependency tree as vertical tidy tree
loadAndRenderDependencyTreeVertical :: String -> Effect Unit
loadAndRenderDependencyTreeVertical selector = launchAff_ do
  log "[FlareRadialTree] Loading dependency tree (vertical)..."
  result <- Loader.getDependencyTree
  case result of
    Left err -> liftEffect $ log $ "[FlareRadialTree] Dependency tree error: " <> err
    Right depTree -> liftEffect $ renderVerticalTree selector depTree
