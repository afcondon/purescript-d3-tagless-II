module D3.Viz.SankeyDiagram where

-- | Sankey diagram visualization using pure PureScript layout

import Prelude

import PSD3.Layout.Sankey (computeLayout, generateLinkPath, SankeyNode, SankeyLink)
import PSD3.Layout.Sankey.CSV (parseSankeyCSV)
import PSD3.Internal.Attributes.Sugar (classed, d, fill, fillOpacity, height, strokeColor, strokeOpacity, strokeWidth, text, textAnchor, viewBox, width, x, y, dy)
import PSD3.Internal.Types (D3Selection_, Datum_, Element(..), Index_, Selector)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach, setAttributes, simpleJoin)
import D3.Viz.Sankey.Unsafe (unboxSankeyNode, unboxSankeyLink)
import Data.Tuple (Tuple(..))
import Data.Set as Set
import Data.Foldable (for_)
import Data.Array as Array
import Data.Array.NonEmpty as NEArray
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Utility (getWindowWidthHeight)
import Unsafe.Coerce (unsafeCoerce)

-- Key functions for data joins (work with opaque Datum_ type)
keyForNode :: Datum_ -> Index_
keyForNode d = unsafeCoerce ((unsafeCoerce d :: SankeyNode).name)

keyForLink :: Datum_ -> Index_
keyForLink d =
  let link = unsafeCoerce d :: SankeyLink
  in unsafeCoerce (show link.index)

-- Main drawing function for Sankey diagram using pure PureScript layout
draw :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  String -> Selector D3Selection_ -> m Unit
draw csvData selector = do
  (Tuple w h) <- liftEffect getWindowWidthHeight
  _ <- liftEffect $ log $ "Window dimensions: w=" <> show w <> ", h=" <> show h

  root <- attach selector :: m D3Selection_
  svg <- appendTo root Svg [ viewBox 0.0 0.0 w h, width w, height h, classed "sankey" ]

  -- Create groups for links and nodes
  linksGroup <- appendTo svg Group [ classed "links" ]
  nodesGroup <- appendTo svg Group [ classed "nodes" ]
  labelsGroup <- appendTo svg Group [ classed "labels" ]

  -- Parse CSV and compute layout in pure PureScript!
  let linkInputs = parseSankeyCSV csvData
  _ <- liftEffect $ log $ "Parsed " <> show (Array.length linkInputs) <> " link inputs from CSV"

  let layoutResult = computeLayout linkInputs w h
  _ <- liftEffect $ log $ "Layout computed: " <> show (Array.length layoutResult.nodes) <> " nodes, "
                                              <> show (Array.length layoutResult.links) <> " links"

  -- Debug: log ALL nodes with depth/height/layer
  _ <- liftEffect $ for_ layoutResult.nodes \node -> do
    let sourceCount = "sources=" <> show (Set.size node.sourceLinks)
    let targetCount = "targets=" <> show (Set.size node.targetLinks)
    log $ "Node: \"" <> node.name <> "\""
    log $ "  Position: x0=" <> show node.x0 <> ", y0=" <> show node.y0 <> ", x1=" <> show node.x1 <> ", y1=" <> show node.y1
    log $ "  Depth: " <> show node.depth <> ", Height: " <> show node.nodeHeight <> ", Layer: " <> show node.layer
    log $ "  " <> sourceCount <> ", " <> targetCount

  -- Debug: Count nodes per layer
  _ <- liftEffect $ do
    let sorted = Array.sortBy (\n1 n2 -> compare n1.layer n2.layer) layoutResult.nodes
    let layers = Array.groupBy (\n1 n2 -> n1.layer == n2.layer) sorted
    for_ layers \layerNodes -> do
      let firstNode = NEArray.head layerNodes
      log $ "Layer " <> show firstNode.layer <> ": " <> show (NEArray.length layerNodes) <> " nodes"

  -- Join and render links
  -- Links are now filled paths (ribbons), not stroked lines
  linksSelection <- simpleJoin linksGroup Path layoutResult.links keyForLink
  setAttributes linksSelection
    [ classed "sankey-link"
    , fill (\(datum :: Datum_) (_ :: Index_) -> (unboxSankeyLink datum).color)
    , fillOpacity 0.5
    , d (\(datum :: Datum_) (_ :: Index_) -> generateLinkPath layoutResult.nodes (unboxSankeyLink datum))
    ]

  -- Join and render nodes
  nodesSelection <- simpleJoin nodesGroup Rect layoutResult.nodes keyForNode
  setAttributes nodesSelection
    [ classed "sankey-node"
    , x (\(datum :: Datum_) (_ :: Index_) -> (unboxSankeyNode datum).x0)
    , y (\(datum :: Datum_) (_ :: Index_) -> (unboxSankeyNode datum).y0)
    , width (\(datum :: Datum_) (_ :: Index_) -> let node = unboxSankeyNode datum in node.x1 - node.x0)
    , height (\(datum :: Datum_) (_ :: Index_) -> let node = unboxSankeyNode datum in node.y1 - node.y0)
    , fill (\(datum :: Datum_) (_ :: Index_) -> (unboxSankeyNode datum).color)
    , strokeColor "#000"
    , strokeWidth 1.0
    , strokeOpacity 0.3
    ]

  -- Join and render node labels
  labelsSelection <- simpleJoin labelsGroup Text layoutResult.nodes keyForNode
  setAttributes labelsSelection
    [ classed "sankey-label"
    , x (\(datum :: Datum_) (_ :: Index_) -> let node = unboxSankeyNode datum in if node.x0 < w / 2.0 then node.x1 + 6.0 else node.x0 - 6.0)
    , y (\(datum :: Datum_) (_ :: Index_) -> let node = unboxSankeyNode datum in (node.y0 + node.y1) / 2.0)
    , textAnchor (\(datum :: Datum_) (_ :: Index_) -> let node = unboxSankeyNode datum in if node.x0 < w / 2.0 then "start" else "end")
    , text (\(datum :: Datum_) (_ :: Index_) -> (unboxSankeyNode datum).name)
    , fill "#000"
    ]

  pure unit
