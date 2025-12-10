// SPLOM Rendering FFI
// Brushable Scatterplot Matrix using d3-brush
import { select } from "d3-selection";
import { scaleLinear } from "d3-scale";
import { brush } from "d3-brush";
import { extent } from "d3-array";
import { line } from "d3-shape";

// Configuration
const CELL_SIZE = 120;
const CELL_PADDING = 12;
const POINT_RADIUS = 2.5;
const MATRIX_PADDING = 50;  // Space for axis labels
const TICK_COUNT = 5;

// Species colors
const speciesColors = {
  "Adelie": "#1f77b4",
  "Gentoo": "#ff7f0e",
  "Chinstrap": "#2ca02c"
};

/**
 * Render the SPLOM visualization
 * @param {string} selector - Container selector
 * @param {Array} data - Penguin data
 * @param {Array} dimensions - Array of {key, label}
 * @returns {Object} Handle with methods for interaction
 */
export function renderSPLOM_(selector) {
  return data => dimensions => () => {
    const container = select(selector);
    container.selectAll("*").remove();

    // Filter data to only penguins with all numeric values
    const validData = data.filter(d =>
      dimensions.every(dim => d[dim.key] != null)
    );

    const n = dimensions.length;
    const size = CELL_SIZE;
    const padding = CELL_PADDING;
    const width = n * size + MATRIX_PADDING * 2;
    const height = n * size + MATRIX_PADDING * 2;

    // Create scales for each dimension
    const scales = {};
    dimensions.forEach(dim => {
      const values = validData.map(d => d[dim.key]);
      scales[dim.key] = scaleLinear()
        .domain(extent(values))
        .range([padding, size - padding])
        .nice();
    });

    // Create SVG
    const svg = container
      .append("svg")
      .attr("width", width)
      .attr("height", height)
      .attr("viewBox", `0 0 ${width} ${height}`)
      .style("max-width", "100%")
      .style("height", "auto")
      .style("font", "10px sans-serif");

    // Background
    svg.append("rect")
      .attr("width", width)
      .attr("height", height)
      .attr("fill", "white");

    // State for brush selection
    let brushCell = null;
    let brushSelection = null;

    // Create cells
    const cells = [];
    for (let i = 0; i < n; i++) {
      for (let j = 0; j < n; j++) {
        cells.push({
          row: i,
          col: j,
          dimX: dimensions[j],
          dimY: dimensions[i]
        });
      }
    }

    // Create cell groups
    const cellGroups = svg.selectAll(".cell")
      .data(cells)
      .enter()
      .append("g")
      .attr("class", "cell")
      .attr("transform", d =>
        `translate(${MATRIX_PADDING + d.col * size},${MATRIX_PADDING + d.row * size})`
      );

    // Cell backgrounds with frame
    cellGroups.append("rect")
      .attr("class", "cell-frame")
      .attr("width", size)
      .attr("height", size)
      .attr("fill", "none")
      .attr("stroke", "#aaa")
      .attr("stroke-width", 0.5);

    // Add grid lines to each cell
    cellGroups.each(function(cell) {
      const g = select(this);
      const xScale = scales[cell.dimX.key];
      const yScale = scales[cell.dimY.key];
      const xTicks = xScale.ticks(TICK_COUNT);
      const yTicks = yScale.ticks(TICK_COUNT);

      // Vertical grid lines
      g.selectAll(".grid-v")
        .data(xTicks)
        .enter()
        .append("line")
        .attr("class", "grid-v")
        .attr("x1", d => xScale(d))
        .attr("x2", d => xScale(d))
        .attr("y1", padding)
        .attr("y2", size - padding)
        .attr("stroke", "#eee")
        .attr("stroke-width", 0.5);

      // Horizontal grid lines
      g.selectAll(".grid-h")
        .data(yTicks)
        .enter()
        .append("line")
        .attr("class", "grid-h")
        .attr("x1", padding)
        .attr("x2", size - padding)
        .attr("y1", d => yScale(d))
        .attr("y2", d => yScale(d))
        .attr("stroke", "#eee")
        .attr("stroke-width", 0.5);
    });

    // Diagonal cells: label + identity scatter line (SW-NE orientation)
    const diagonalCells = cellGroups.filter(d => d.row === d.col);

    diagonalCells.each(function(cell) {
      const g = select(this);
      const scale = scales[cell.dimX.key];
      const domain = scale.domain();

      // For SW-NE diagonal: X increases left-to-right, Y increases bottom-to-top
      // So we need an inverted Y scale for the diagonal
      const yScaleInverted = scaleLinear()
        .domain(domain)
        .range([size - padding, padding]);  // Inverted: high values at top

      // Add label text
      g.append("text")
        .attr("class", "cell-label")
        .attr("x", padding + 4)
        .attr("y", padding + 12)
        .attr("font-size", "11px")
        .attr("font-weight", "bold")
        .attr("fill", "#333")
        .text(cell.dimX.label.replace(" (mm)", "").replace(" (g)", ""));

      // Add identity line (dotted diagonal SW-NE)
      g.append("line")
        .attr("class", "identity-line")
        .attr("x1", scale(domain[0]))
        .attr("y1", yScaleInverted(domain[0]))  // Bottom-left
        .attr("x2", scale(domain[1]))
        .attr("y2", yScaleInverted(domain[1]))  // Top-right
        .attr("stroke", "#333")
        .attr("stroke-width", 1)
        .attr("stroke-dasharray", "2,2");

      // Add small scatter points on the identity line (colored by species)
      g.selectAll(".diag-point")
        .data(validData)
        .enter()
        .append("circle")
        .attr("class", "diag-point")
        .attr("cx", d => scale(d[cell.dimX.key]))
        .attr("cy", d => yScaleInverted(d[cell.dimX.key]))
        .attr("r", 1.5)
        .attr("fill", d => speciesColors[d.species] || "#999")
        .attr("fill-opacity", 0.7);
    });

    // Off-diagonal: scatter points
    const offDiagonalCells = cellGroups.filter(d => d.row !== d.col);

    // Points in each cell
    offDiagonalCells.each(function(cell) {
      const g = select(this);
      const xScale = scales[cell.dimX.key];
      const yScale = scales[cell.dimY.key];

      g.selectAll(".point")
        .data(validData)
        .enter()
        .append("circle")
        .attr("class", "point")
        .attr("cx", d => xScale(d[cell.dimX.key]))
        .attr("cy", d => yScale(d[cell.dimY.key]))
        .attr("r", POINT_RADIUS)
        .attr("fill", d => speciesColors[d.species] || "#999")
        .attr("fill-opacity", 0.7)
        .attr("stroke", "none");
    });

    // Add axis labels on edges
    // Bottom axis labels (X)
    dimensions.forEach((dim, i) => {
      const scale = scales[dim.key];
      const ticks = scale.ticks(TICK_COUNT);
      const g = svg.append("g")
        .attr("class", "axis-bottom")
        .attr("transform", `translate(${MATRIX_PADDING + i * size}, ${MATRIX_PADDING + n * size})`);

      ticks.forEach(tick => {
        g.append("text")
          .attr("x", scale(tick))
          .attr("y", 12)
          .attr("text-anchor", "middle")
          .attr("font-size", "8px")
          .attr("fill", "#666")
          .text(formatTick(tick));
      });
    });

    // Left axis labels (Y)
    dimensions.forEach((dim, i) => {
      const scale = scales[dim.key];
      const ticks = scale.ticks(TICK_COUNT);
      const g = svg.append("g")
        .attr("class", "axis-left")
        .attr("transform", `translate(${MATRIX_PADDING}, ${MATRIX_PADDING + i * size})`);

      ticks.forEach(tick => {
        g.append("text")
          .attr("x", -4)
          .attr("y", scale(tick))
          .attr("text-anchor", "end")
          .attr("dominant-baseline", "middle")
          .attr("font-size", "8px")
          .attr("fill", "#666")
          .text(formatTick(tick));
      });
    });

    // Create brush behavior
    const brushBehavior = brush()
      .extent([[padding, padding], [size - padding, size - padding]])
      .on("start", brushstarted)
      .on("brush", brushed)
      .on("end", brushended);

    // Attach brush to off-diagonal cells
    offDiagonalCells.append("g")
      .attr("class", "brush")
      .call(brushBehavior);

    // Helper to get cell data from brush event context
    function getCellData(element, datum) {
      if (datum && datum.dimX && datum.dimY) return datum;
      if (element && element.__data__) return element.__data__;
      if (element && element.parentNode && element.parentNode.__data__) {
        return element.parentNode.__data__;
      }
      return null;
    }

    function brushstarted(event, datum) {
      if (brushCell !== this) {
        select(brushCell).call(brushBehavior.move, null);
        brushCell = this;
      }
    }

    function brushed(event, datum) {
      const cell = getCellData(this, datum);

      if (!event.selection) {
        brushSelection = null;
        updatePointOpacity();
        return;
      }

      if (!cell || !cell.dimX || !cell.dimY) return;

      const [[x0, y0], [x1, y1]] = event.selection;
      const xScale = scales[cell.dimX.key];
      const yScale = scales[cell.dimY.key];

      brushSelection = {
        dimX: cell.dimX.key,
        dimY: cell.dimY.key,
        xMin: xScale.invert(x0),
        xMax: xScale.invert(x1),
        yMin: yScale.invert(y0),
        yMax: yScale.invert(y1)
      };

      updatePointOpacity();
    }

    function brushended(event, datum) {
      if (!event.selection) {
        brushSelection = null;
        updatePointOpacity();
      }
    }

    function updatePointOpacity() {
      if (!brushSelection) {
        svg.selectAll(".point")
          .attr("fill-opacity", 0.7)
          .attr("r", POINT_RADIUS);
      } else {
        svg.selectAll(".point")
          .attr("fill-opacity", d => isSelected(d) ? 0.9 : 0.1)
          .attr("r", d => isSelected(d) ? POINT_RADIUS : 1.5);
      }
    }

    function isSelected(d) {
      if (!brushSelection) return true;
      const xVal = d[brushSelection.dimX];
      const yVal = d[brushSelection.dimY];
      return xVal >= brushSelection.xMin &&
             xVal <= brushSelection.xMax &&
             yVal >= brushSelection.yMin &&
             yVal <= brushSelection.yMax;
    }

    function formatTick(value) {
      if (value >= 1000) return (value / 1000).toFixed(1) + "k";
      if (Number.isInteger(value)) return value.toString();
      return value.toFixed(1);
    }

    // Selection change callback (set by caller)
    let onSelectionChange = null;

    function notifySelectionChange() {
      if (onSelectionChange) {
        const count = brushSelection ? validData.filter(isSelected).length : validData.length;
        onSelectionChange(count);
      }
    }

    // Update updatePointOpacity to notify on change
    const originalUpdatePointOpacity = updatePointOpacity;
    updatePointOpacity = function() {
      if (!brushSelection) {
        svg.selectAll(".point")
          .attr("fill-opacity", 0.7)
          .attr("r", POINT_RADIUS);
      } else {
        svg.selectAll(".point")
          .attr("fill-opacity", d => isSelected(d) ? 0.9 : 0.1)
          .attr("r", d => isSelected(d) ? POINT_RADIUS : 1.5);
      }
      notifySelectionChange();
    };

    // Return handle
    return {
      clearBrush: () => {
        brushSelection = null;
        svg.selectAll(".brush").call(brushBehavior.move, null);
        updatePointOpacity();
      },
      getSelectedCount: () => {
        if (!brushSelection) return validData.length;
        return validData.filter(isSelected).length;
      },
      getTotalCount: () => validData.length,
      setOnSelectionChange: (callback) => {
        onSelectionChange = callback;
      }
    };
  };
}

/**
 * Clear brush and reset visualization
 */
export function clearSPLOMBrush_(handle) {
  return () => {
    handle.clearBrush();
  };
}

/**
 * Get selected point count
 */
export function getSelectedCount_(handle) {
  return () => handle.getSelectedCount();
}

/**
 * Get total point count
 */
export function getTotalCount_(handle) {
  return () => handle.getTotalCount();
}

/**
 * Set selection change callback
 */
export function setOnSelectionChange_(handle) {
  return callback => () => {
    handle.setOnSelectionChange(count => callback(count)());
  };
}
