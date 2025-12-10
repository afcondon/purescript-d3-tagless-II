// FFI for SPLOM visualization
import { select } from "d3-selection";

// Map from dimensionKey strings to data attribute names
const dimensionAttrMap = {
  "billLength": "data-bill-length",
  "billDepth": "data-bill-depth",
  "flipperLength": "data-flipper-length",
  "bodyMass": "data-body-mass"
};

// Update point visibility based on selection bounds
// This updates fill-opacity and r attributes on all .point circles
// without destroying the brush elements
export function updatePointVisibility_(containerSelector) {
  return function(maybeSelection) {
    return function(selectedRadius) {
      return function() {
        const container = select(containerSelector);
        const points = container.selectAll(".point");

        if (maybeSelection === null || maybeSelection.value0 === undefined) {
          // No selection - show all points at full opacity
          points
            .attr("fill-opacity", 0.7)
            .attr("r", selectedRadius);
        } else {
          // Has selection - update each point based on whether it's selected
          const sel = maybeSelection.value0;
          const xAttr = dimensionAttrMap[sel.dimX];
          const yAttr = dimensionAttrMap[sel.dimY];
          const xMin = Math.min(sel.xMin, sel.xMax);
          const xMax = Math.max(sel.xMin, sel.xMax);
          const yMin = Math.min(sel.yMin, sel.yMax);
          const yMax = Math.max(sel.yMin, sel.yMax);

          if (!xAttr || !yAttr) {
            console.error("Unknown dimension key:", sel.dimX, sel.dimY);
            return;
          }

          points.each(function() {
            const point = select(this);

            // Get the values for the selection's dimensions from data attributes
            const xVal = parseFloat(this.getAttribute(xAttr));
            const yVal = parseFloat(this.getAttribute(yAttr));

            if (isNaN(xVal) || isNaN(yVal)) {
              point.attr("fill-opacity", 0.1).attr("r", 1.5);
              return;
            }

            // Check if point is within selection bounds
            const isSelected = xVal >= xMin && xVal <= xMax && yVal >= yMin && yVal <= yMax;

            point
              .attr("fill-opacity", isSelected ? 0.7 : 0.1)
              .attr("r", isSelected ? selectedRadius : 1.5);
          });
        }
      };
    };
  };
}
