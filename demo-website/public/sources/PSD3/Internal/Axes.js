import * as d3 from "d3";

export function axisBottom_(scale) {
  return d3.axisBottom(scale);
}

export function axisLeft_(scale) {
  return d3.axisLeft(scale);
}

export function callAxis_(selection) {
  return (axis) => () => {
    selection.call(axis);
  };
}
