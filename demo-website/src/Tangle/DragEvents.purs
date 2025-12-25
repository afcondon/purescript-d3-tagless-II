-- | Document-level mouse event listeners for drag tracking
-- |
-- | Used by Tangle adjust controls for drag-to-adjust behavior.
-- | These create Halogen subscriptions that fire while dragging.
module Tangle.DragEvents
  ( dragEventSource
  , clickEndDragEventSource
  ) where

import Prelude

import Effect (Effect)
import Halogen.Subscription as HS

-- | FFI for document mousemove listener
foreign import documentMouseMoveImpl :: (Int -> Effect Unit) -> Effect (Effect Unit)

-- | FFI for document mouseup listener
foreign import documentMouseUpImpl :: Effect Unit -> Effect (Effect Unit)

-- | FFI for document click listener (for click-to-end drag mode)
foreign import documentClickImpl :: Effect Unit -> Effect (Effect Unit)

-- | Action type for drag events
data DragEvent
  = DragMove Int    -- Mouse X position during drag
  | DragEnd         -- Mouse released

-- | Create an event source for drag events (mousemove and mouseup on document)
-- |
-- | Returns an Emitter that yields DragMove on mousemove and DragEnd on mouseup.
-- | The finalizer removes both listeners when the subscription is closed.
dragEventSource :: forall action. (Int -> action) -> action -> HS.Emitter action
dragEventSource onMove onEnd = HS.makeEmitter \emitter -> do
  -- Set up mousemove listener
  removeMoveListener <- documentMouseMoveImpl \clientX -> do
    emitter (onMove clientX)

  -- Set up mouseup listener
  removeUpListener <- documentMouseUpImpl do
    emitter onEnd

  -- Return finalizer that removes both listeners
  pure do
    removeMoveListener
    removeUpListener

-- | Create an event source for click-to-end drag mode
-- |
-- | Like dragEventSource but uses click instead of mouseup to end the drag.
-- | This is useful in SVG contexts where D3 zoom/pan captures mouseup events.
-- | User clicks on number to start drag, moves mouse, clicks anywhere to end.
clickEndDragEventSource :: forall action. (Int -> action) -> action -> HS.Emitter action
clickEndDragEventSource onMove onEnd = HS.makeEmitter \emitter -> do
  -- Set up mousemove listener
  removeMoveListener <- documentMouseMoveImpl \clientX -> do
    emitter (onMove clientX)

  -- Set up click listener (with small delay to avoid initial click)
  removeClickListener <- documentClickImpl do
    emitter onEnd

  -- Return finalizer that removes both listeners
  pure do
    removeMoveListener
    removeClickListener
