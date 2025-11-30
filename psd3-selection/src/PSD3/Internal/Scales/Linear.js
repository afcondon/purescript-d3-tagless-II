// D3 dependencies: d3-scale
import { scaleLinear } from "d3-scale";

export function createLinearScale_(config) {
  return () => {
    return scaleLinear()
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
