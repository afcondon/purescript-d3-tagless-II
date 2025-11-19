module D3.Viz.LesMis.LesMisRenderCallbacks where

import Prelude

import D3.Viz.LesMiserables.Model (LesMisSimNode)
import Data.Int (toNumber)
import Data.Number (sqrt)
import PSD3.Data.Node (D3Link_Swizzled)
import PSD3.Internal.Scales.Scales (d3SchemeCategory10N_)
import PSD3v2.Attribute.Types (Attribute, cx, cy, fill, radius, stroke, strokeWidth, x1, x2, y1, y2)
import PSD3v2.Behavior.Types (Behavior(..), simulationDrag)
import PSD3v2.Capabilities.Selection (class SelectionM, append, setAttrs, setAttrsExit, remove, on)
import PSD3v2.Selection.Types (ElementType(..), SBoundOwns, SBoundInherits, SPending, SExiting)
import PSD3v2.Simulation.Update (RenderCallbacks)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (Element)

-- | Type alias for attributes passed to render callbacks
-- | Can be extended with additional fields as needed
type LesMisAttributes = {
  -- Placeholder for future attribute extensions (e.g., colors, sizes, etc.)
}

-- | Default attributes for LesMis visualization
defaultLesMisAttributes :: LesMisAttributes
defaultLesMisAttributes = {}

-- | Render callbacks for LesMis force-directed graph
-- |
-- | Implements the declarative RenderCallbacks pattern:
-- | - Nodes are simple circles colored by group
-- | - Links are lines with thickness based on value
-- | - Enter/update/exit behaviors are clearly separated
-- |
-- | This shows how to use the generic updateSimulation with simple DOM structures.
lesMisRenderCallbacks :: forall m sel.
  Monad m =>
  SelectionM sel m =>
  RenderCallbacks LesMisAttributes sel m LesMisSimNode
lesMisRenderCallbacks = {
  -- Node rendering: Simple circles
  onNodeEnter: \enterSel _attrs -> do
    nodeEnter <- append Circle [
      cx (\(d :: LesMisSimNode) -> d.x)
    , cy (\(d :: LesMisSimNode) -> d.y)
    , radius 5.0
    , fill (\(d :: LesMisSimNode) -> d3SchemeCategory10N_ (toNumber d.group))
    , stroke "#fff"
    , strokeWidth 2.0
    ] enterSel

    -- Add drag behavior to circles
    _ <- on (Drag $ simulationDrag "lesmis") nodeEnter

    pure nodeEnter

  , onNodeUpdate: \updateSel _attrs -> do
      -- Update colors (in case group changed)
      _ <- setAttrs [
        fill (\(d :: LesMisSimNode) -> d3SchemeCategory10N_ (toNumber d.group))
      ] updateSel
      pure unit

  , onNodeExit: \exitSel -> do
      -- Style exit nodes before removal
      _ <- setAttrsExit [
        fill ("gray" :: String)
      , stroke ("black" :: String)
      ] exitSel
      -- Remove from DOM
      remove exitSel

  -- Link rendering: Simple lines
  , onLinkEnter: \enterSel _attrs ->
      append Line [
        strokeWidth (\(il :: D3Link_Swizzled) -> sqrt (unsafeCoerce il).value :: Number)
      , stroke (\(il :: D3Link_Swizzled) -> d3SchemeCategory10N_ (toNumber (unsafeCoerce il).target.group) :: String)
      ] enterSel

  , onLinkUpdate: \updateSel _attrs -> do
      -- Update link colors (in case target group changed)
      _ <- setAttrs [
        stroke (\(il :: D3Link_Swizzled) -> d3SchemeCategory10N_ (toNumber (unsafeCoerce il).target.group) :: String)
      ] updateSel
      pure unit

  , onLinkExit: \exitSel -> do
      -- Just remove links
      remove exitSel

  -- Tick function attributes (update positions on each tick)
  , nodeTickAttrs: \_attrs -> [
      cx (\(d :: LesMisSimNode) -> d.x)
    , cy (\(d :: LesMisSimNode) -> d.y)
    ]

  , linkTickAttrs: [
      x1 (\(il :: D3Link_Swizzled) -> (unsafeCoerce il).source.x :: Number)
    , y1 (\(il :: D3Link_Swizzled) -> (unsafeCoerce il).source.y :: Number)
    , x2 (\(il :: D3Link_Swizzled) -> (unsafeCoerce il).target.x :: Number)
    , y2 (\(il :: D3Link_Swizzled) -> (unsafeCoerce il).target.y :: Number)
    ]
}
