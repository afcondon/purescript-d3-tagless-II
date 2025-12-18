module PSD3.Internal.Behavior.FFI
  ( attachZoom_
  , attachZoomWithTransform_
  , getZoomTransform_
  , ZoomTransform
  , attachSimpleDrag_
  , attachSimulationDrag_
  , attachSimulationDragById_
  , attachSimulationDragNestedById_
  , attachClick_
  , attachClickWithDatum_
  , attachMouseEnter_
  , attachMouseLeave_
  , attachHighlight_
  -- Simulation registry
  , registerSimulation_
  , unregisterSimulation_
  -- Pure web-events versions (preferred)
  , attachMouseMoveWithEvent_
  , attachMouseEnterWithEvent_
  , attachMouseLeaveWithEvent_
  -- DEPRECATED D3 versions
  , attachMouseMoveWithInfo_
  , attachMouseEnterWithInfo_
  , attachMouseLeaveWithInfo_
  , MouseEventInfoJS
  ) where

import Prelude

import Data.Nullable (Nullable)
import Effect (Effect)
import Effect.Uncurried (EffectFn2)
import PSD3.Internal.Types (D3Simulation_)
import Web.DOM.Element (Element)
import Web.UIEvent.MouseEvent (MouseEvent)

-- | Attach zoom behavior to a DOM element
-- |
-- | Parameters:
-- | - element: The DOM element to attach zoom to (typically SVG)
-- | - scaleMin: Minimum zoom scale (e.g., 0.5 for 50%)
-- | - scaleMax: Maximum zoom scale (e.g., 4.0 for 400%)
-- | - targetSelector: CSS selector for the element to transform (e.g., ".zoom-group")
-- |
-- | Returns the element for chaining.
foreign import attachZoom_
  :: Element
  -> Number
  -> Number
  -> String
  -> Effect Element

-- | Zoom transform record {k, x, y} for scale and translation
type ZoomTransform = { k :: Number, x :: Number, y :: Number }

-- | Get the current zoom transform from a DOM element
-- | Returns identity transform {k:1, x:0, y:0} if none exists
foreign import getZoomTransform_
  :: Element
  -> Effect ZoomTransform

-- | Attach zoom behavior and restore a previous transform
-- |
-- | Like attachZoom_ but also applies the given transform after setup.
-- | Use this to preserve zoom state across re-renders.
foreign import attachZoomWithTransform_
  :: Element
  -> Number
  -> Number
  -> String
  -> ZoomTransform
  -> Effect Element

-- | Attach simple drag behavior to a DOM element
-- |
-- | Enables dragging with transform translation.
-- | Returns the element for chaining.
foreign import attachSimpleDrag_
  :: Element
  -> Unit
  -> Effect Element

-- | Attach simulation-aware drag behavior to a DOM element
-- |
-- | Enables dragging with force simulation reheat.
-- | When dragging:
-- | - Dragstart: Sets fx/fy fixed positions, reheats simulation
-- | - Drag: Updates fx/fy to follow mouse
-- | - Dragend: Clears fx/fy, cools simulation
-- |
-- | Returns the element for chaining.
foreign import attachSimulationDrag_
  :: Element
  -> Nullable D3Simulation_
  -> String  -- Label for drag event namespace
  -> Effect Element

-- | Attach click handler without datum access
-- |
-- | Attaches a simple click handler that doesn't access the bound datum.
-- | Also sets cursor to pointer for clickable elements.
-- |
-- | Returns the element for chaining.
foreign import attachClick_
  :: Element
  -> Effect Unit
  -> Effect Element

-- | Attach click handler with datum access
-- |
-- | Attaches a click handler that receives the datum bound to the element.
-- | The datum is recovered from D3's __data__ property.
-- | Type safety is preserved through the Selection's phantom type.
-- | Also sets cursor to pointer for clickable elements.
-- |
-- | Returns the element for chaining.
foreign import attachClickWithDatum_
  :: forall datum
   . Element
  -> (datum -> Effect Unit)
  -> Effect Element

-- | Attach mouseenter handler with datum access
-- |
-- | Attaches a handler that fires when mouse enters the element.
-- | Does not bubble (unlike mouseover).
-- | The datum is recovered from D3's __data__ property.
-- |
-- | Returns the element for chaining.
foreign import attachMouseEnter_
  :: forall datum
   . Element
  -> (datum -> Effect Unit)
  -> Effect Element

-- | Attach mouseleave handler with datum access
-- |
-- | Attaches a handler that fires when mouse leaves the element.
-- | Does not bubble (unlike mouseout).
-- | The datum is recovered from D3's __data__ property.
-- |
-- | Returns the element for chaining.
foreign import attachMouseLeave_
  :: forall datum
   . Element
  -> (datum -> Effect Unit)
  -> Effect Element

-- | Attach hover highlight behavior
-- |
-- | Applies styles on mouseenter and resets on mouseleave.
-- | Also raises the element to front on hover.
-- |
-- | Parameters:
-- | - element: DOM element to attach to
-- | - enterStyles: Array of {attr, value} to apply on enter
-- | - leaveStyles: Array of {attr, value} to apply on leave
-- |
-- | Returns the element for chaining.
foreign import attachHighlight_
  :: Element
  -> Array { attr :: String, value :: String }
  -> Array { attr :: String, value :: String }
  -> Effect Element

-- | Mouse event info type (matches PureScript MouseEventInfo)
-- | DEPRECATED: Will be removed once Operations.purs is updated to use MouseEvent
type MouseEventInfoJS =
  { clientX :: Number
  , clientY :: Number
  , pageX :: Number
  , pageY :: Number
  , offsetX :: Number
  , offsetY :: Number
  }

-- | Attach mousemove handler with event info (pure web-events version)
-- |
-- | Uses standard addEventListener instead of D3's .on()
-- | Handler receives raw MouseEvent - use Web.UIEvent.MouseEvent accessors
foreign import attachMouseMoveWithEvent_
  :: forall datum
   . Element
  -> EffectFn2 datum MouseEvent Unit
  -> Effect Element

-- | Attach mouseenter handler with event info (pure web-events version)
foreign import attachMouseEnterWithEvent_
  :: forall datum
   . Element
  -> EffectFn2 datum MouseEvent Unit
  -> Effect Element

-- | Attach mouseleave handler with event info (pure web-events version)
foreign import attachMouseLeaveWithEvent_
  :: forall datum
   . Element
  -> EffectFn2 datum MouseEvent Unit
  -> Effect Element

-- | DEPRECATED: Old D3-based versions - keeping for backwards compatibility during transition
foreign import attachMouseMoveWithInfo_
  :: forall datum
   . Element
  -> (datum -> MouseEventInfoJS -> Effect Unit)
  -> Effect Element

foreign import attachMouseEnterWithInfo_
  :: forall datum
   . Element
  -> (datum -> MouseEventInfoJS -> Effect Unit)
  -> Effect Element

foreign import attachMouseLeaveWithInfo_
  :: forall datum
   . Element
  -> (datum -> MouseEventInfoJS -> Effect Unit)
  -> Effect Element

-- =============================================================================
-- Simulation Registry
-- =============================================================================

-- | Register a simulation with the global registry
-- |
-- | This enables SimulationDrag to look up simulations by ID.
-- | The reheat function is called when dragging starts.
-- |
-- | Usage:
-- | ```purescript
-- | registerSimulation_ "my-viz" (Sim.reheat sim)
-- | ```
foreign import registerSimulation_
  :: String         -- Simulation ID
  -> Effect Unit    -- Reheat function
  -> Effect Unit

-- | Unregister a simulation from the global registry
-- |
-- | Should be called when the visualization is destroyed.
foreign import unregisterSimulation_
  :: String
  -> Effect Unit

-- | Attach simulation-aware drag using the simulation registry
-- |
-- | Looks up the simulation by ID and calls its reheat function on drag start.
-- | This enables declarative SimulationDrag in TreeAPI.
-- |
-- | When dragging:
-- | - Dragstart: Sets fx/fy, calls registered reheat function
-- | - Drag: Updates fx/fy to follow mouse
-- | - Dragend: Clears fx/fy
foreign import attachSimulationDragById_
  :: Element
  -> String     -- Simulation ID to look up in registry
  -> Effect Element

-- | Attach simulation-aware drag for nested datum structure
-- |
-- | Like attachSimulationDragById_, but for datums that have a `.node` field
-- | containing the actual simulation node. Used when the bound datum is a
-- | wrapper (like RenderNode) that contains the simulation node.
-- |
-- | Sets `event.subject.node.fx/fy` instead of `event.subject.fx/fy`
foreign import attachSimulationDragNestedById_
  :: Element
  -> String     -- Simulation ID to look up in registry
  -> Effect Element
