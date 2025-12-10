// SPLOM Rendering FFI
// Brushable Scatterplot Matrix using d3-brush
import { select, selectAll } from "d3-selection";
import { scaleLinear } from "d3-scale";
import { brush } from "d3-brush";
import { extent } from "d3-array";

// Configuration
const CELL_SIZE = 120;
const CELL_PADDING = 10;
const POINT_RADIUS = 3;
const MATRIX_PADDING = 40;

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
      .style("height", "auto");

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

    // Cell backgrounds
    cellGroups.append("rect")
      .attr("class", "cell-bg")
      .attr("width", size)
      .attr("height", size)
      .attr("fill", d => d.row === d.col ? "#f5f2e8" : "white")
      .attr("stroke", "#ccc")
      .attr("stroke-width", 0.5);

    // Diagonal labels
    cellGroups.filter(d => d.row === d.col)
      .append("text")
      .attr("class", "cell-label")
      .attr("x", size / 2)
      .attr("y", size / 2)
      .attr("text-anchor", "middle")
      .attr("dominant-baseline", "middle")
      .attr("font-size", "10px")
      .attr("font-family", "monospace")
      .attr("fill", "#333")
      .text(d => d.dimX.label.replace(" (mm)", "").replace(" (g)", ""));

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

    function brushstarted(event, cell) {
      // Clear brush in other cells
      if (brushCell !== this) {
        select(brushCell).call(brushBehavior.move, null);
        brushCell = this;
      }
    }

    function brushed(event, cell) {
      if (!event.selection) {
        brushSelection = null;
        updatePointOpacity();
        return;
      }

      const [[x0, y0], [x1, y1]] = event.selection;
      const xScale = scales[cell.dimX.key];
      const yScale = scales[cell.dimY.key];

      // Invert scales to get data range
      const xMin = xScale.invert(x0);
      const xMax = xScale.invert(x1);
      const yMin = yScale.invert(y1); // y is inverted
      const yMax = yScale.invert(y0);

      brushSelection = {
        dimX: cell.dimX.key,
        dimY: cell.dimY.key,
        xMin, xMax, yMin, yMax
      };

      updatePointOpacity();
    }

    function brushended(event, cell) {
      if (!event.selection) {
        brushSelection = null;
        updatePointOpacity();
      }
    }

    function updatePointOpacity() {
      if (!brushSelection) {
        // No selection - show all points
        svg.selectAll(".point")
          .attr("fill-opacity", 0.7)
          .attr("r", POINT_RADIUS);
      } else {
        // Filter points based on brush selection
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

    // Add legend
    const legend = svg.append("g")
      .attr("class", "legend")
      .attr("transform", `translate(${width - 100}, 10)`);

    const species = ["Adelie", "Gentoo", "Chinstrap"];
    species.forEach((sp, i) => {
      const g = legend.append("g")
        .attr("transform", `translate(0, ${i * 18})`);

      g.append("circle")
        .attr("cx", 6)
        .attr("cy", 6)
        .attr("r", 5)
        .attr("fill", speciesColors[sp])
        .attr("fill-opacity", 0.7);

      g.append("text")
        .attr("x", 16)
        .attr("y", 10)
        .attr("font-size", "11px")
        .attr("font-family", "monospace")
        .attr("fill", "#333")
        .text(sp);
    });

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
      getTotalCount: () => validData.length
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
