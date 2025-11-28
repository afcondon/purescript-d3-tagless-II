// FFI for D3 zoom and drag behaviors
import * as d3 from 'd3';

/**
 * Attach zoom behavior to an element
 * @param {Element} element - The DOM element to attach zoom to (typically SVG)
 * @param {number} scaleMin - Minimum zoom scale
 * @param {number} scaleMax - Maximum zoom scale
 * @param {string} targetSelector - CSS selector for the element to transform
 * @returns {Element} The element (for chaining)
 */
export function attachZoom_(element) {
  return scaleMin => scaleMax => targetSelector => () => {
    // Create D3 selection from element
    const selection = d3.select(element);

    // Find the target element to transform
    const target = selection.select(targetSelector);

    function zoomed(event) {
      target.attr('transform', event.transform);
    }

    const zoom = d3.zoom()
      .scaleExtent([scaleMin, scaleMax])
      .on('zoom', zoomed);

    selection.call(zoom);

    return element;
  };
}

/**
 * Attach simple drag behavior to an element
 * @param {Element} element - The DOM element to attach drag to
 * @returns {Element} The element (for chaining)
 */
export function attachSimpleDrag_(element) {
  return () => () => {
    // Create D3 selection from element
    const selection = d3.select(element);

    let transform = { x: 0, y: 0 };

    function dragstarted(event) {
      d3.select(this).raise();
    }

    function dragged(event) {
      transform.x += event.dx;
      transform.y += event.dy;
      d3.select(this).attr('transform', `translate(${transform.x},${transform.y})`);
    }

    const drag = d3.drag()
      .on('start', dragstarted)
      .on('drag', dragged);

    selection.call(drag);

    return element;
  };
}

/**
 * Attach simulation-aware drag behavior to an element
 * @param {Element} element - The DOM element to attach drag to
 * @param {d3.Simulation|null} simulation - The D3 force simulation
 * @param {string} label - Event namespace label
 * @returns {Element} The element (for chaining)
 */
export function attachSimulationDrag_(element) {
  return simulation => label => () => {
    // Create D3 selection from element
    const selection = d3.select(element);

    function dragstarted(event) {
      if (simulation) {
        // Always restart if simulation has stopped (alpha = 0)
        // Only reheat if this is the first concurrent drag (!event.active)
        if (!event.active || simulation.alpha() < 0.001) {
          simulation.alphaTarget(0.3).restart();
        }
      }
      event.subject.fx = event.subject.x;
      event.subject.fy = event.subject.y;
    }

    function dragged(event) {
      event.subject.fx = event.x;
      event.subject.fy = event.y;
    }

    function dragended(event) {
      if (simulation && !event.active) {
        simulation.alphaTarget(0);
      }
      event.subject.fx = null;
      event.subject.fy = null;
    }

    const drag = d3.drag()
      .on('start.' + label, dragstarted)
      .on('drag.' + label, dragged)
      .on('end.' + label, dragended);

    selection
      .call(drag)
      .style('cursor', 'pointer');  // Set cursor to pointer for draggable elements

    return element;
  };
}

/**
 * Attach click handler without datum access
 * @param {Element} element - The DOM element to attach click handler to
 * @param {Function} handler - The PureScript Effect Unit handler
 * @returns {Element} The element (for chaining)
 */
export function attachClick_(element) {
  return handler => () => {
    // Create D3 selection from element
    const selection = d3.select(element);

    // Attach click event listener
    selection.on('click', function(event) {
      // Call PureScript handler (it's already an Effect, so invoke it)
      handler();
    });

    // Set cursor to pointer for clickable elements
    selection.style('cursor', 'pointer');

    return element;
  };
}

/**
 * Attach click handler with datum access
 * @param {Element} element - The DOM element to attach click handler to
 * @param {Function} handler - The PureScript (datum -> Effect Unit) handler
 * @returns {Element} The element (for chaining)
 */
export function attachClickWithDatum_(element) {
  return handler => () => {
    // Create D3 selection from element
    const selection = d3.select(element);

    // Attach click event listener
    selection.on('click', function(event, d) {
      // D3 v6+ passes datum as second argument
      // Call PureScript handler with datum (it returns an Effect, so invoke it)
      handler(d)();
    });

    // Set cursor to pointer for clickable elements
    selection.style('cursor', 'pointer');

    return element;
  };
}

/**
 * Attach mouseenter handler with datum access
 * @param {Element} element - The DOM element to attach handler to
 * @param {Function} handler - The PureScript (datum -> Effect Unit) handler
 * @returns {Element} The element (for chaining)
 */
export function attachMouseEnter_(element) {
  return handler => () => {
    // Create D3 selection from element
    const selection = d3.select(element);

    // Attach mouseenter event listener
    selection.on('mouseenter', function(event, d) {
      // D3 v6+ passes datum as second argument
      // Call PureScript handler with datum (it returns an Effect, so invoke it)
      handler(d)();
    });

    return element;
  };
}

/**
 * Attach mouseleave handler with datum access
 * @param {Element} element - The DOM element to attach handler to
 * @param {Function} handler - The PureScript (datum -> Effect Unit) handler
 * @returns {Element} The element (for chaining)
 */
export function attachMouseLeave_(element) {
  return handler => () => {
    // Create D3 selection from element
    const selection = d3.select(element);

    // Attach mouseleave event listener
    selection.on('mouseleave', function(event, d) {
      // D3 v6+ passes datum as second argument
      // Call PureScript handler with datum (it returns an Effect, so invoke it)
      handler(d)();
    });

    return element;
  };
}

/**
 * Attach hover highlight behavior
 * @param {Element} element - The DOM element to attach to
 * @param {Array} enterStyles - Array of {attr, value} to apply on enter
 * @param {Array} leaveStyles - Array of {attr, value} to apply on leave
 * @returns {Element} The element (for chaining)
 */
export function attachHighlight_(element) {
  return enterStyles => leaveStyles => () => {
    // Create D3 selection from element
    const selection = d3.select(element);

    // Attach mouseenter handler
    selection.on('mouseenter', function(event) {
      const sel = d3.select(this);
      // Raise element to front
      sel.raise();
      // Apply enter styles
      enterStyles.forEach(style => {
        sel.attr(style.attr, style.value);
      });
    });

    // Attach mouseleave handler
    selection.on('mouseleave', function(event) {
      const sel = d3.select(this);
      // Apply leave styles
      leaveStyles.forEach(style => {
        sel.attr(style.attr, style.value);
      });
    });

    return element;
  };
}

/**
 * Attach mousemove handler using pure web-events (no D3)
 * @param {Element} element - The DOM element to attach handler to
 * @param {Function} handler - The PureScript EffectFn2 datum MouseEvent Unit handler
 * @returns {Element} The element (for chaining)
 */
export function attachMouseMoveWithEvent_(element) {
  return handler => () => {
    element.addEventListener('mousemove', function(event) {
      // Get datum from element's __data__ property (D3 convention)
      const datum = this.__data__;
      handler(datum, event);
    });
    return element;
  };
}

/**
 * DEPRECATED: D3-based version - use attachMouseMoveWithEvent_ instead
 */
export function attachMouseMoveWithInfo_(element) {
  return handler => () => {
    const selection = d3.select(element);

    selection.on('mousemove', function(event, d) {
      const info = {
        clientX: event.clientX,
        clientY: event.clientY,
        pageX: event.pageX,
        pageY: event.pageY,
        offsetX: event.offsetX,
        offsetY: event.offsetY
      };
      handler(d)(info)();
    });

    return element;
  };
}

/**
 * Attach mouseenter handler using pure web-events (no D3)
 */
export function attachMouseEnterWithEvent_(element) {
  return handler => () => {
    element.addEventListener('mouseenter', function(event) {
      const datum = this.__data__;
      handler(datum, event);
    });
    return element;
  };
}

/**
 * DEPRECATED: D3-based version
 */
export function attachMouseEnterWithInfo_(element) {
  return handler => () => {
    const selection = d3.select(element);

    selection.on('mouseenter', function(event, d) {
      const info = {
        clientX: event.clientX,
        clientY: event.clientY,
        pageX: event.pageX,
        pageY: event.pageY,
        offsetX: event.offsetX,
        offsetY: event.offsetY
      };
      handler(d)(info)();
    });

    return element;
  };
}

/**
 * Attach mouseleave handler using pure web-events (no D3)
 */
export function attachMouseLeaveWithEvent_(element) {
  return handler => () => {
    element.addEventListener('mouseleave', function(event) {
      const datum = this.__data__;
      handler(datum, event);
    });
    return element;
  };
}

/**
 * DEPRECATED: D3-based version
 */
export function attachMouseLeaveWithInfo_(element) {
  return handler => () => {
    const selection = d3.select(element);

    selection.on('mouseleave', function(event, d) {
      const info = {
        clientX: event.clientX,
        clientY: event.clientY,
        pageX: event.pageX,
        pageY: event.pageY,
        offsetX: event.offsetX,
        offsetY: event.offsetY
      };
      handler(d)(info)();
    });

    return element;
  };
}

/**
 * Attach line chart tooltip behavior
 * Shows series name and interpolated value at mouse position
 */
export function attachLineTooltip_(svgElement) {
  return pathElement => seriesName => points => margin => () => {
    const svg = d3.select(svgElement);
    const path = d3.select(pathElement);

    // Get or create tooltip div
    let tooltip = d3.select('body').select('.line-tooltip');
    if (tooltip.empty()) {
      tooltip = d3.select('body')
        .append('div')
        .attr('class', 'line-tooltip')
        .style('position', 'absolute')
        .style('background', 'rgba(0, 0, 0, 0.8)')
        .style('color', 'white')
        .style('padding', '8px 12px')
        .style('border-radius', '4px')
        .style('font-size', '12px')
        .style('pointer-events', 'none')
        .style('opacity', 0)
        .style('z-index', 1000);
    }

    // Sort points by x for binary search
    const sortedPoints = [...points].sort((a, b) => a.x - b.x);

    // Find nearest point by x coordinate
    function findNearestPoint(mouseX) {
      // Adjust for margin
      const x = mouseX - margin.left;

      // Binary search for nearest point
      let left = 0;
      let right = sortedPoints.length - 1;

      while (right - left > 1) {
        const mid = Math.floor((left + right) / 2);
        if (sortedPoints[mid].x < x) {
          left = mid;
        } else {
          right = mid;
        }
      }

      // Return closer of the two
      if (right >= sortedPoints.length) return sortedPoints[left];
      if (left < 0) return sortedPoints[right];

      const dLeft = Math.abs(sortedPoints[left].x - x);
      const dRight = Math.abs(sortedPoints[right].x - x);
      return dLeft < dRight ? sortedPoints[left] : sortedPoints[right];
    }

    // Attach events
    path
      .on('mouseenter.tooltip', function(event) {
        // Highlight the line
        d3.select(this)
          .raise()
          .attr('stroke', '#333')
          .attr('stroke-width', 2.5);

        tooltip
          .style('opacity', 1);
      })
      .on('mousemove.tooltip', function(event) {
        // Get mouse position relative to SVG
        const [mouseX, mouseY] = d3.pointer(event, svgElement);

        // Find nearest data point
        const point = findNearestPoint(mouseX);

        // Update tooltip content
        tooltip
          .html(`<strong>${seriesName}</strong><br/>${point.label}: ${point.y.toFixed(1)}%`)
          .style('left', (event.pageX + 15) + 'px')
          .style('top', (event.pageY - 10) + 'px');
      })
      .on('mouseleave.tooltip', function(event) {
        // Reset line style
        d3.select(this)
          .attr('stroke', '#ddd')
          .attr('stroke-width', 1.5);

        tooltip
          .style('opacity', 0);
      });

    return pathElement;
  };
}
