module D3.Viz.LesMiserablesGUP.Render where

import Prelude

import PSD3.Attributes (DatumFn(..), DatumFnI(..))
import PSD3.Internal.Attributes.Sugar (classed, cx, cy, fill, radius, remove, strokeColor, strokeOpacity, strokeWidth, x1, x2, y1, y2)
import PSD3.Internal.Types (D3Selection_, Element(..))
import PSD3.Internal.Selection.Types (Behavior(..), DragBehavior(..), SelectionAttribute)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, on, setAttributes)
import PSD3.Simulation.Update (RenderCallbacks)
import PSD3.Internal.FFI (simdrag_)
import PSD3.Internal.Scales.Scales (d3SchemeCategory10N_)
import Data.Int (toNumber)
import Data.Number (sqrt)
import Unsafe.Coerce (unsafeCoerce)

-- | Direct record accessors with minimal unsafeCoerce at FFI boundary
-- | SimulationNode is row-polymorphic, so we can access fields directly after coercion

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
    -- Create circles directly with inline accessors
    nodeEnter <- appendTo enterSel Circle
      [ radius attrs.nodeRadius
      , fill (DatumFn \d -> d3SchemeCategory10N_ (toNumber (unsafeCoerce d).group) :: String)
      , strokeColor attrs.nodeStrokeColor
      , strokeOpacity attrs.nodeStrokeWidth
      , classed "enter"
      ]

    -- Add drag behavior
    _ <- nodeEnter `on` Drag (CustomDrag "lesmis" simdrag_)

    pure nodeEnter

  , onNodeUpdate: \updateSel attrs ->
      setAttributes updateSel
        [ fill (DatumFn \d -> d3SchemeCategory10N_ (toNumber (unsafeCoerce d).group) :: String)
        , classed "update"
        ]

  , onNodeExit: \exitSel ->
      setAttributes exitSel [ remove ]

  -- Link rendering: Simple lines
  , onLinkEnter: \enterSel _attrs -> do
      linkEnter <- appendTo enterSel Line
        [ strokeWidth (DatumFn \d -> sqrt (unsafeCoerce d).value :: Number)
        , strokeColor (DatumFn \d -> d3SchemeCategory10N_ (toNumber (unsafeCoerce d).target.group) :: String)
        , classed "enter"
        ]
      pure linkEnter

  , onLinkUpdate: \updateSel _attrs ->
      setAttributes updateSel [ classed "update" ]

  , onLinkExit: \exitSel ->
      setAttributes exitSel [ remove ]

  -- Tick function attributes: Position circles and lines
  , nodeTickAttrs: \_attrs ->
      [ cx (DatumFn \d -> (unsafeCoerce d).x :: Number)
      , cy (DatumFn \d -> (unsafeCoerce d).y :: Number)
      ]

  , linkTickAttrs:
      [ x1 (DatumFn \d -> (unsafeCoerce d).source.x :: Number)
      , y1 (DatumFn \d -> (unsafeCoerce d).source.y :: Number)
      , x2 (DatumFn \d -> (unsafeCoerce d).target.x :: Number)
      , y2 (DatumFn \d -> (unsafeCoerce d).target.y :: Number)
      ]
}
