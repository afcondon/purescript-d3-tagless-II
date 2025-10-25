import * as d3 from "d3";

export function createLinearScale(config) {
  return () => {
    return d3.scaleLinear()
      .domain(config.domain)
      .range(config.range);
  };
}

export function applyScale(scale) {
  return (value) => scale(value);
}

export function getTicks(scale) {
  return (count) => () => scale.ticks(count);
}
