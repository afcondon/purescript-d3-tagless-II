module D3.Viz.LesMis.LesMisRenderCallbacks where

import Prelude

import D3.Viz.LesMiserables.Model (LesMisSimNode, LesMisNodeRow, LesMisLinkRow)
import Data.Int (toNumber)
import Data.Number (sqrt)
import PSD3.Data.Node (SwizzledLink)
import PSD3.Internal.Scales.Scales (d3SchemeCategory10N_)
import PSD3v2.Attribute.Types (Attribute, cx, cy, fill, radius, stroke, strokeWidth, x1, x2, y1, y2)
import PSD3v2.Behavior.Types (Behavior(..), simulationDrag)
import PSD3v2.Capabilities.Selection (class SelectionM, append, setAttrs, setAttrsExit, remove, on)
import PSD3v2.Selection.Types (ElementType(..), SBoundOwns, SBoundInherits, SPending, SExiting)

import PSD3v2.Simulation.Update (RenderCallbacks)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (Element)

-- | Type alias for LesMis swizzled links
type LesMisSwizzledLink = SwizzledLink LesMisNodeRow LesMisLinkRow

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
  RenderCallbacks LesMisAttributes sel m LesMisNodeRow LesMisLinkRow
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
        strokeWidth (\(il :: LesMisSwizzledLink) -> sqrt il.value)
      , stroke (\(il :: LesMisSwizzledLink) -> d3SchemeCategory10N_ (toNumber il.target.group))
      ] enterSel

  , onLinkUpdate: \updateSel _attrs -> do
      -- Update link colors (in case target group changed)
      _ <- setAttrs [
        stroke (\(il :: LesMisSwizzledLink) -> d3SchemeCategory10N_ (toNumber il.target.group))
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
      x1 (\(il :: LesMisSwizzledLink) -> il.source.x)
    , y1 (\(il :: LesMisSwizzledLink) -> il.source.y)
    , x2 (\(il :: LesMisSwizzledLink) -> il.target.x)
    , y2 (\(il :: LesMisSwizzledLink) -> il.target.y)
    ]
}
