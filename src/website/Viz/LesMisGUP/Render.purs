module D3.Viz.LesMiserablesGUP.Render where

import Prelude

import PSD3.Internal.Attributes.Sugar (classed, cx, cy, fill, radius, remove, strokeColor, strokeOpacity, strokeWidth, x1, x2, y1, y2)
import PSD3.Internal.Types (D3Selection_, Element(..))
import PSD3.Internal.Selection.Types (Behavior(..), DragBehavior(..), SelectionAttribute)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, on, setAttributes)
import PSD3.Simulation.Update (RenderCallbacks)
import D3.Viz.LesMiserablesGUP.Unsafe (unboxD3SimLink, unboxD3SimNode)
import PSD3.Internal.FFI (simdrag_)
import PSD3.Internal.Scales.Scales (d3SchemeCategory10N_)
import Data.Int (toNumber)
import Data.Number (sqrt)

-- | Type-safe accessors for LesMis data
-- | These unwrap the D3 simulation types to access the original data fields
link_ = {
    source: _.source <<< unboxD3SimLink
  , target: _.target <<< unboxD3SimLink
  , value:  _.value <<< unboxD3SimLink
  , color:  d3SchemeCategory10N_ <<< toNumber <<< _.target.group <<< unboxD3SimLink
}

datum_ = {
    x: _.x <<< unboxD3SimNode
  , y: _.y <<< unboxD3SimNode
  , colorByGroup: d3SchemeCategory10N_ <<< toNumber <<< _.group <<< unboxD3SimNode
}

-- | Attributes for LesMis visualization
-- | Much simpler than Spago - no nested structure, just direct circle/line styling
type LesMisAttributes = {
  nodeRadius :: Number
, nodeStrokeColor :: String
, nodeStrokeWidth :: Number
}

-- | Default attributes for LesMis
defaultLesMisAttributes :: LesMisAttributes
defaultLesMisAttributes = {
  nodeRadius: 5.0
, nodeStrokeColor: "#fff"
, nodeStrokeWidth: 1.5
}

-- | Render callbacks for LesMis visualization
-- |
-- | This is much simpler than Spago:
-- | - Nodes are just Circle elements (no Group wrapper, no text labels)
-- | - Links are just Line elements
-- | - Simple attribute structure
-- |
-- | This demonstrates how easy it is to create a new visualization using the library.
lesMisRenderCallbacks :: forall d m.
  Monad m =>
  SelectionM D3Selection_ m =>
  RenderCallbacks LesMisAttributes D3Selection_ m d
lesMisRenderCallbacks = {
  -- Node rendering: Just circles, no complex structure
  onNodeEnter: \enterSel attrs -> do
    -- Create circles directly
    nodeEnter <- appendTo enterSel Circle
      [ radius attrs.nodeRadius
      , fill datum_.colorByGroup
      , strokeColor attrs.nodeStrokeColor
      , strokeOpacity attrs.nodeStrokeWidth
      , classed "enter"
      ]

    -- Add drag behavior
    _ <- nodeEnter `on` Drag (CustomDrag "lesmis" simdrag_)

    pure nodeEnter

  , onNodeUpdate: \updateSel attrs ->
      setAttributes updateSel
        [ fill datum_.colorByGroup
        , classed "update"
        ]

  , onNodeExit: \exitSel ->
      setAttributes exitSel [ remove ]

  -- Link rendering: Simple lines
  , onLinkEnter: \enterSel _attrs -> do
      linkEnter <- appendTo enterSel Line
        [ strokeWidth (sqrt <<< link_.value)
        , strokeColor link_.color
        , classed "enter"
        ]
      pure linkEnter

  , onLinkUpdate: \updateSel _attrs ->
      setAttributes updateSel [ classed "update" ]

  , onLinkExit: \exitSel ->
      setAttributes exitSel [ remove ]

  -- Tick function attributes: Position circles and lines
  , nodeTickAttrs: \_attrs ->
      [ cx datum_.x, cy datum_.y ]

  , linkTickAttrs:
      [ x1 (_.x <<< link_.source)
      , y1 (_.y <<< link_.source)
      , x2 (_.x <<< link_.target)
      , y2 (_.y <<< link_.target)
      ]
}
