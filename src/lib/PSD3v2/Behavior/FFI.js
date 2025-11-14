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
      if (simulation && !event.active) {
        simulation.alphaTarget(0.3).restart();
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

    selection.call(drag);

    return element;
  };
}
