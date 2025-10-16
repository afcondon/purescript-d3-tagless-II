import * as d3 from "d3";

export function axisBottom(scale) {
  return d3.axisBottom(scale);
}

export function axisLeft(scale) {
  return d3.axisLeft(scale);
}

export function callAxis(selection) {
  return (axis) => () => {
    selection.call(axis);
  };
}
