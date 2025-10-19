module D3.Examples.SankeyDiagram where

import Prelude

import Control.Monad.State (class MonadState)
import D3.Attributes.Instances (AttributeSetter(..), toAttr)
import D3.Attributes.Sugar (classed, d, fill, fillOpacity, height, strokeColor, strokeOpacity, strokeWidth, text, textAnchor, viewBox, width, x, y, dy)
import D3.Data.Types (D3Selection_, Datum_, Element(..), Index_, Selector)
import D3.Examples.Sankey.Model (SankeyData)
import D3.Examples.Sankey.Unsafe (unboxSankeyLink, unboxSankeyNode)
import D3.Layouts.Sankey.Functions (sankeyLinkPath_)
import D3.Layouts.Sankey.Types (SankeyConfig, SankeyLayoutState_)
import D3.Selection (SelectionAttribute(..))
import D3Tagless.Capabilities (class SankeyM, appendTo, attach, setAttributes, setSankeyData, setSankeyDataWithConfig, simpleJoin)
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect, liftEffect)
import Unsafe.Coerce (unsafeCoerce)
import Utility (getWindowWidthHeight)

-- Type-safe accessors for Sankey data
-- After the data goes through D3's Sankey layout, nodes and links get additional properties
node_ ::
  { color :: Datum_ -> String
    , name :: Datum_ -> String
    , value :: Datum_ -> Number
    , x0 :: Datum_ -> Number
    , x1 :: Datum_ -> Number
    , y0 :: Datum_ -> Number
    , y1 :: Datum_ -> Number
    }
node_ = {
    name: _.name <<< unboxSankeyNode
  , x0: _.x0 <<< unboxSankeyNode
  , y0: _.y0 <<< unboxSankeyNode
  , x1: _.x1 <<< unboxSankeyNode
  , y1: _.y1 <<< unboxSankeyNode
  , value: _.value <<< unboxSankeyNode
  , color: _.color <<< unboxSankeyNode
}

link_ :: 
  { color :: Datum_ -> String
  , source :: Datum_
              -> { color :: String
                  , name :: String
                  , value :: Number
                  , x0 :: Number
                  , x1 :: Number
                  , y0 :: Number
                  , y1 :: Number
                  }
  , target :: Datum_
              -> { color :: String
                  , name :: String
                  , value :: Number
                  , x0 :: Number
                  , x1 :: Number
                  , y0 :: Number
                  , y1 :: Number
                  }
  , value :: Datum_ -> Number
  , width :: Datum_ -> Number
  , path :: Datum_  -> String
      }
link_ = {
    source: _.source <<< unboxSankeyLink
  , target: _.target <<< unboxSankeyLink
  , value: _.value <<< unboxSankeyLink
  , width: _.width <<< unboxSankeyLink
  , color: _.color <<< unboxSankeyLink
  , path: _.path  <<< unboxSankeyLink
}

-- Snippet_Start
-- Name: SankeyDraw
-- Main drawing function for Sankey diagram
draw :: forall row m.
  Bind m =>
  MonadEffect m =>
  MonadState { sankeyLayout :: SankeyLayoutState_ | row } m =>
  SankeyM D3Selection_ m =>
  SankeyData -> Selector D3Selection_ -> m Unit
draw sankeyData selector = do
  (Tuple w h) <- liftEffect getWindowWidthHeight

  (root :: D3Selection_) <- attach selector
  svg  <- appendTo root Svg [ viewBox 0.0 0.0 w h, classed "sankey" ]

  -- Create groups for links and nodes
  linksGroup <- appendTo svg Group [ classed "links" ]
  nodesGroup <- appendTo svg Group [ classed "nodes" ]
  labelsGroup <- appendTo svg Group [ classed "labels" ]

  -- Pass data through Sankey layout generator
  -- This computes positions and dimensions for nodes and links
  layoutResult <- setSankeyData sankeyData w h

  -- Join and render links (as paths with custom link path generator)
  linksSelection <- simpleJoin linksGroup Path layoutResult.links keyForLink
  setAttributes linksSelection
    [ classed "sankey-link"
    , fill "none"
    , strokeWidth link_.width
    , strokeOpacity 0.5
    , d sankeyLinkPath_
    , strokeColor link_.color
    ]

  -- Join and render nodes (as rectangles)
  nodesSelection <- simpleJoin nodesGroup Rect layoutResult.nodes keyForNode
  setAttributes nodesSelection
    [ classed "sankey-node"
    , x node_.x0
    , y node_.y0
    , width (\n -> node_.x1 n - node_.x0 n)
    , height (\n -> node_.y1 n - node_.y0 n)
    , fill node_.color
    , fillOpacity 0.8
    ]

  -- Add labels for nodes
  labelsSelection <- simpleJoin labelsGroup Text layoutResult.nodes keyForNode
  setAttributes labelsSelection
    [ classed "sankey-label"
    , x (\n -> if node_.x0 n < w / 2.0 then node_.x1 n + 6.0 else node_.x0 n - 6.0)
    , y (\n -> (node_.y0 n + node_.y1 n) / 2.0)
    , dy 4.0  -- TODO: should be "0.35em" but dy is not polymorphic in our library
    , textAnchor (\n -> if node_.x0 n < w / 2.0 then "start" else "end")
    , text node_.name
    ]

  pure unit
-- Snippet_End


-- TODO these functions do not belong here, move to D3.FFI and check if they can use generic versions
-- such as keyIsSourceTarget_ instead
-- Key functions for data joins (needed for update pattern)
-- These work with the opaque Datum_ type and need to unbox to access fields
keyForNode :: Datum_ -> Index_
keyForNode d = unsafeCoerce $ node_.name d

keyForLink :: Datum_ -> Index_
keyForLink d = unsafeCoerce $ node_.name (unsafeCoerce $ link_.source d) <> "-" <> node_.name (unsafeCoerce $ link_.target d)

-- | Draw Sankey diagram with custom configuration
drawWithConfig :: forall row m.
  Bind m =>
  MonadEffect m =>
  MonadState { sankeyLayout :: SankeyLayoutState_ | row } m =>
  SankeyM D3Selection_ m =>
  SankeyData -> Selector D3Selection_ -> SankeyConfig -> m Unit
drawWithConfig sankeyData selector config = do
  (Tuple w h) <- liftEffect getWindowWidthHeight

  (root :: D3Selection_) <- attach selector
  svg  <- appendTo root Svg [ viewBox 0.0 0.0 w h, classed "sankey" ]

  -- Create groups for links and nodes
  linksGroup <- appendTo svg Group [ classed "links" ]
  nodesGroup <- appendTo svg Group [ classed "nodes" ]
  labelsGroup <- appendTo svg Group [ classed "labels" ]

  -- Pass data through Sankey layout generator with config
  layoutResult <- setSankeyDataWithConfig sankeyData w h config

  -- Join and render links (as paths with custom link path generator)
  linksSelection <- simpleJoin linksGroup Path layoutResult.links keyForLink
  setAttributes linksSelection
    [ classed "sankey-link"
    , fill "none"
    , strokeWidth link_.width
    , strokeOpacity 0.5
    , d sankeyLinkPath_
    , strokeColor link_.color
    ]

  -- Join and render nodes (as rectangles)
  nodesSelection <- simpleJoin nodesGroup Rect layoutResult.nodes keyForNode
  setAttributes nodesSelection
    [ classed "sankey-node"
    , x node_.x0
    , y node_.y0
    , width (\n -> node_.x1 n - node_.x0 n)
    , height (\n -> node_.y1 n - node_.y0 n)
    , fill node_.color
    , fillOpacity 0.8
    ]

  -- Add labels for nodes
  labelsSelection <- simpleJoin labelsGroup Text layoutResult.nodes keyForNode
  setAttributes labelsSelection
    [ classed "sankey-label"
    , x (\n -> if node_.x0 n < w / 2.0 then node_.x1 n + 6.0 else node_.x0 n - 6.0)
    , y (\n -> (node_.y0 n + node_.y1 n) / 2.0)
    , dy 4.0  -- TODO: should be "0.35em" but dy is not polymorphic in our library
    , textAnchor (\n -> if node_.x0 n < w / 2.0 then "start" else "end")
    , text node_.name
    ]

  pure unit
