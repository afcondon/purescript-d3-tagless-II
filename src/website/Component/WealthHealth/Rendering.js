import * as d3 from "d3";

// Clear the container of any existing SVG
export function clearContainer(containerId) {
  return function() {
    const container = document.getElementById(containerId);
    if (container) {
      container.innerHTML = '';
    }
  };
}

// Create SVG element
export function createSVG(containerId) {
  return function(width) {
    return function(height) {
      return function() {
        const container = document.getElementById(containerId);
        if (!container) {
          console.error(`Container ${containerId} not found`);
          return null;
        }

        const svg = d3.select(container)
          .append("svg")
          .attr("width", width)
          .attr("height", height)
          .attr("viewBox", [0, 0, width, height])
          .attr("style", "max-width: 100%; height: auto;");

        return svg.node();
      };
    };
  };
}

// Create logarithmic scale
export function createLogScale(domainMin) {
  return function(domainMax) {
    return function(rangeMin) {
      return function(rangeMax) {
        return d3.scaleLog()
          .domain([domainMin, domainMax])
          .range([rangeMin, rangeMax]);
      };
    };
  };
}

// Create linear scale
export function createLinearScale(domainMin) {
  return function(domainMax) {
    return function(rangeMin) {
      return function(rangeMax) {
        return d3.scaleLinear()
          .domain([domainMin, domainMax])
          .range([rangeMin, rangeMax]);
      };
    };
  };
}

// Create square root scale
export function createSqrtScale(domainMin) {
  return function(domainMax) {
    return function(rangeMin) {
      return function(rangeMax) {
        return d3.scaleSqrt()
          .domain([domainMin, domainMax])
          .range([rangeMin, rangeMax]);
      };
    };
  };
}

// Apply a scale to a value
export function applyScale(scale) {
  return function(value) {
    return scale(value);
  };
}

// Render X axis
export function renderXAxis(svg) {
  return function(xScale) {
    return function(yPosition) {
      return function(xMin) {
        return function(xMax) {
          return function() {
            const svgSelection = d3.select(svg);

            const xAxis = d3.axisBottom(xScale)
              .ticks(10, ",.0f")
              .tickSizeOuter(0);

            svgSelection.append("g")
              .attr("class", "x-axis")
              .attr("transform", `translate(0,${yPosition})`)
              .call(xAxis)
              .call(g => g.select(".domain").remove());
          };
        };
      };
    };
  };
}

// Render Y axis
export function renderYAxis(svg) {
  return function(yScale) {
    return function(yMin) {
      return function(yMax) {
        return function(xPosition) {
          return function() {
            const svgSelection = d3.select(svg);

            const yAxis = d3.axisLeft(yScale)
              .ticks(10)
              .tickSizeOuter(0);

            svgSelection.append("g")
              .attr("class", "y-axis")
              .attr("transform", `translate(${xPosition},0)`)
              .call(yAxis)
              .call(g => g.select(".domain").remove());
          };
        };
      };
    };
  };
}

// Render X axis label
export function renderXAxisLabel(svg) {
  return function(label) {
    return function(width) {
      return function(yPosition) {
        return function() {
          const svgSelection = d3.select(svg);

          svgSelection.append("text")
            .attr("class", "x-axis-label")
            .attr("text-anchor", "middle")
            .attr("x", width / 2)
            .attr("y", yPosition + 30)
            .attr("fill", "currentColor")
            .text(label);
        };
      };
    };
  };
}

// Render Y axis label
export function renderYAxisLabel(svg) {
  return function(label) {
    return function(xPosition) {
      return function(height) {
        return function() {
          const svgSelection = d3.select(svg);

          svgSelection.append("text")
            .attr("class", "y-axis-label")
            .attr("text-anchor", "middle")
            .attr("transform", `rotate(-90)`)
            .attr("x", -height / 2)
            .attr("y", xPosition - 30)
            .attr("fill", "currentColor")
            .text(label);
        };
      };
    };
  };
}

// Render gridlines
export function renderGridlines(svg) {
  return function(xScale) {
    return function(yScale) {
      return function(config) {
        return function() {
          const svgSelection = d3.select(svg);

          // Vertical gridlines
          const xTicks = xScale.ticks(10);
          svgSelection.append("g")
            .attr("class", "grid-lines-x")
            .selectAll("line")
            .data(xTicks)
            .join("line")
            .attr("x1", d => xScale(d))
            .attr("x2", d => xScale(d))
            .attr("y1", config.marginTop)
            .attr("y2", config.height - config.marginBottom)
            .attr("stroke", "#ccc")
            .attr("stroke-opacity", 0.3)
            .attr("stroke-dasharray", "2,2");

          // Horizontal gridlines
          const yTicks = yScale.ticks(10);
          svgSelection.append("g")
            .attr("class", "grid-lines-y")
            .selectAll("line")
            .data(yTicks)
            .join("line")
            .attr("x1", config.marginLeft)
            .attr("x2", config.width - config.marginRight)
            .attr("y1", d => yScale(d))
            .attr("y2", d => yScale(d))
            .attr("stroke", "#ccc")
            .attr("stroke-opacity", 0.3)
            .attr("stroke-dasharray", "2,2");
        };
      };
    };
  };
}

// Add a circle to the SVG
export function addCircle(svg) {
  return function(x) {
    return function(y) {
      return function(r) {
        return function(color) {
          return function(name) {
            return function(onHover) {
              return function(onLeave) {
                return function() {
                  const svgSelection = d3.select(svg);

                  svgSelection.append("circle")
                    .attr("class", "nation-circle")
                    .attr("cx", x)
                    .attr("cy", y)
                    .attr("r", r)
                    .attr("fill", color)
                    .attr("fill-opacity", 0.7)
                    .attr("stroke", "#333")
                    .attr("stroke-width", 0.5)
                    .attr("data-nation", name)
                    .on("mouseenter", function() {
                      onHover(name)();
                    })
                    .on("mouseleave", function() {
                      onLeave()();
                    });
                };
              };
            };
          };
        };
      };
    };
  };
}

// Update circles with transition
export function updateCircles(containerId) {
  return function(nations) {
    return function(xScale) {
      return function(yScale) {
        return function(rScale) {
          return function(onHover) {
            return function(onLeave) {
              return function() {
                const container = document.getElementById(containerId);
                if (!container) return;

                const svg = d3.select(container).select("svg");

                // Update existing circles with transitions
                svg.selectAll(".nation-circle")
                  .data(nations, d => d ? d.name : this.getAttribute("data-nation"))
                  .join(
                    enter => enter.append("circle")
                      .attr("class", "nation-circle")
                      .attr("data-nation", d => d.name)
                      .attr("cx", d => xScale(d.income))
                      .attr("cy", d => yScale(d.lifeExpectancy))
                      .attr("r", 0)
                      .attr("fill", d => getRegionColor(d.region))
                      .attr("fill-opacity", 0.7)
                      .attr("stroke", "#333")
                      .attr("stroke-width", 0.5)
                      .on("mouseenter", function(event, d) {
                        onHover(d.name)();
                      })
                      .on("mouseleave", function() {
                        onLeave()();
                      })
                      .call(enter => enter.transition()
                        .duration(300)
                        .attr("r", d => rScale(d.population))),
                    update => update
                      .on("mouseenter", function(event, d) {
                        onHover(d.name)();
                      })
                      .on("mouseleave", function() {
                        onLeave()();
                      })
                      .call(update => update.transition()
                        .duration(300)
                        .attr("cx", d => xScale(d.income))
                        .attr("cy", d => yScale(d.lifeExpectancy))
                        .attr("r", d => rScale(d.population))
                        .attr("fill", d => getRegionColor(d.region))),
                    exit => exit
                      .call(exit => exit.transition()
                        .duration(300)
                        .attr("r", 0)
                        .remove())
                  );
              };
            };
          };
        };
      };
    };
  };
}

// Helper to get region color from the region object
function getRegionColor(region) {
  const colorMap = {
    "EastAsiaAndPacific": "#5ab4ac",
    "Europe": "#d8b365",
    "LatinAmericaAndCaribbean": "#8c510a",
    "MiddleEastAndNorthAfrica": "#f6e8c3",
    "SouthAsia": "#c7eae5",
    "SubSaharanAfrica": "#01665e",
    "NorthAmerica": "#dfc27d"
  };

  return colorMap[region.constructor] || "#999";
}
