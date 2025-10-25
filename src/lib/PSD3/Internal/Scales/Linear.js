import * as d3 from "d3";

export function createLinearScale_(config) {
  return () => {
    return d3.scaleLinear()
      .domain(config.domain)
      .range(config.range);
  };
}

export function applyScale_(scale) {
  return (value) => scale(value);
}

export function getTicks_(scale) {
  return (count) => () => scale.ticks(count);
}
