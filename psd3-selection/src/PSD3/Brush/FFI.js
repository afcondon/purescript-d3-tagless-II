// FFI for D3 brush behavior
// D3 dependencies: d3-selection, d3-brush
import { select } from "d3-selection";
import { brush, brushX, brushY, brushSelection } from "d3-brush";

/**
 * Create and attach a 2D brush to an element
 * @param {Element} element - The DOM element (typically a <g>) to attach brush to
 * @param {Object} extent - Brush extent { x0, y0, x1, y1 }
 * @param {Function} onStart - Handler for brush start (selection -> Effect Unit)
 * @param {Function} onBrush - Handler for brush move (selection -> Effect Unit)
 * @param {Function} onEnd - Handler for brush end (selection -> Effect Unit)
 * @returns {Object} Brush handle with methods { clear, move, getSelection }
 */
export function attachBrush_(element) {
  return extent => onStart => onBrush => onEnd => () => {
    const selection = select(element);

    // Convert selection [[x0,y0],[x1,y1]] to PureScript record or null
    function toRecord(sel) {
      if (!sel) return null;
      return { x0: sel[0][0], y0: sel[0][1], x1: sel[1][0], y1: sel[1][1] };
    }

    const brushBehavior = brush()
      .extent([[extent.x0, extent.y0], [extent.x1, extent.y1]])
      .on('start', function(event) {
        onStart(toRecord(event.selection))();
      })
      .on('brush', function(event) {
        onBrush(toRecord(event.selection))();
      })
      .on('end', function(event) {
        onEnd(toRecord(event.selection))();
      });

    selection.call(brushBehavior);

    // Return brush handle for programmatic control
    return {
      clear: () => {
        selection.call(brushBehavior.move, null);
      },
      move: (newSelection) => () => {
        if (newSelection) {
          selection.call(brushBehavior.move,
            [[newSelection.x0, newSelection.y0], [newSelection.x1, newSelection.y1]]);
        } else {
          selection.call(brushBehavior.move, null);
        }
      },
      getSelection: () => {
        const sel = brushSelection(element);
        return toRecord(sel);
      }
    };
  };
}

/**
 * Create and attach a 1D horizontal brush (brushX)
 * @param {Element} element - The DOM element to attach brush to
 * @param {Object} extent - Brush extent { x0, y0, x1, y1 }
 * @param {Function} onStart - Handler for brush start
 * @param {Function} onBrush - Handler for brush move
 * @param {Function} onEnd - Handler for brush end
 * @returns {Object} Brush handle
 */
export function attachBrushX_(element) {
  return extent => onStart => onBrush => onEnd => () => {
    const selection = select(element);

    // Convert selection [x0, x1] to PureScript record or null
    function toRecord(sel) {
      if (!sel) return null;
      return { x0: sel[0], x1: sel[1] };
    }

    const brushBehavior = brushX()
      .extent([[extent.x0, extent.y0], [extent.x1, extent.y1]])
      .on('start', function(event) {
        onStart(toRecord(event.selection))();
      })
      .on('brush', function(event) {
        onBrush(toRecord(event.selection))();
      })
      .on('end', function(event) {
        onEnd(toRecord(event.selection))();
      });

    selection.call(brushBehavior);

    return {
      clear: () => {
        selection.call(brushBehavior.move, null);
      },
      move: (newSelection) => () => {
        if (newSelection) {
          selection.call(brushBehavior.move, [newSelection.x0, newSelection.x1]);
        } else {
          selection.call(brushBehavior.move, null);
        }
      },
      getSelection: () => {
        const sel = brushSelection(element);
        return toRecord(sel);
      }
    };
  };
}

/**
 * Create and attach a 1D vertical brush (brushY)
 * @param {Element} element - The DOM element to attach brush to
 * @param {Object} extent - Brush extent { x0, y0, x1, y1 }
 * @param {Function} onStart - Handler for brush start
 * @param {Function} onBrush - Handler for brush move
 * @param {Function} onEnd - Handler for brush end
 * @returns {Object} Brush handle
 */
export function attachBrushY_(element) {
  return extent => onStart => onBrush => onEnd => () => {
    const selection = select(element);

    // Convert selection [y0, y1] to PureScript record or null
    function toRecord(sel) {
      if (!sel) return null;
      return { y0: sel[0], y1: sel[1] };
    }

    const brushBehavior = brushY()
      .extent([[extent.x0, extent.y0], [extent.x1, extent.y1]])
      .on('start', function(event) {
        onStart(toRecord(event.selection))();
      })
      .on('brush', function(event) {
        onBrush(toRecord(event.selection))();
      })
      .on('end', function(event) {
        onEnd(toRecord(event.selection))();
      });

    selection.call(brushBehavior);

    return {
      clear: () => {
        selection.call(brushBehavior.move, null);
      },
      move: (newSelection) => () => {
        if (newSelection) {
          selection.call(brushBehavior.move, [newSelection.y0, newSelection.y1]);
        } else {
          selection.call(brushBehavior.move, null);
        }
      },
      getSelection: () => {
        const sel = brushSelection(element);
        return toRecord(sel);
      }
    };
  };
}

/**
 * Clear brush selection programmatically
 * @param {Object} brushHandle - The brush handle returned from attachBrush_
 */
export function clearBrush_(brushHandle) {
  return () => {
    brushHandle.clear();
  };
}

/**
 * Move brush selection programmatically
 * @param {Object} brushHandle - The brush handle
 * @param {Object|null} selection - New selection or null to clear
 */
export function moveBrush_(brushHandle) {
  return selection => () => {
    brushHandle.move(selection)();
  };
}

/**
 * Get current brush selection
 * @param {Object} brushHandle - The brush handle
 * @returns {Object|null} Current selection or null
 */
export function getBrushSelection_(brushHandle) {
  return () => {
    return brushHandle.getSelection();
  };
}
